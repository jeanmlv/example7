# example7

#!/usr/bin/env python3
"""Read-only Girder/Thea.View video inventory extraction across studies.

Requires: pip install requests truststore
Authentication: set THEA_API_KEY or THEA_TOKEN in your environment.
Never paste secrets into this file or commit them to Git.
"""
from __future__ import annotations

import argparse
import csv
import json
import logging
import os
import re
import sys
import time
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

import truststore

# Use the operating system trust store (corporate Windows CA certificates).
# Do not disable TLS verification.
truststore.inject_into_ssl()

import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

API_URL = "https://dsa-staging.jnj.com/api/v1"
COLLECTION_ID = "687fe830e4f52ab6376b2785"
VIDEO_EXTENSIONS = {".avi", ".mp4", ".mov", ".mkv", ".wmv", ".mpeg", ".mpg", ".m4v", ".webm"}
FIELDS = [
    "study", "study_folder", "study_folder_id", "collection_id", "data_type", "video_class", "processor",
    "batch", "site", "patient_id", "visit", "examination_date", "anatomical_region",
    "filename", "extension", "item_id", "folder_id", "size_bytes",
    "source_video", "transcoder", "codec", "fps", "import_source",
    "import_path", "relative_path", "updated", "needs_review",
]


def configure_logging(output_dir: Path) -> None:
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s | %(levelname)s | %(message)s",
        handlers=[logging.StreamHandler(sys.stdout), logging.FileHandler(output_dir / "extraction.log", encoding="utf-8")],
    )


def make_session() -> requests.Session:
    """Authenticate via Girder API key exchange or an existing Girder token.

    Secrets are read only from environment variables. Never log response URLs
    from the key-exchange endpoint because they contain the API key.
    """
    api_key = os.environ.get("THEA_API_KEY", "").strip()
    token = os.environ.get("THEA_TOKEN", "").strip()
    if bool(api_key) == bool(token):
        raise RuntimeError("Set exactly one environment variable: THEA_API_KEY or THEA_TOKEN.")

    session = requests.Session()
    session.headers.update({"Accept": "application/json", "User-Agent": "thea-video-inventory/1.1"})
    retry = Retry(total=4, connect=4, read=4, backoff_factor=1.0,
                  status_forcelist=[429, 500, 502, 503, 504], allowed_methods=["GET"])
    session.mount("https://", HTTPAdapter(max_retries=retry))

    if api_key:
        try:
            response = session.post(
                f"{API_URL.rstrip('/')}/api_key/token",
                params={"key": api_key},
                timeout=(15, 60),
            )
            response.raise_for_status()
            payload = response.json()
        except requests.HTTPError as exc:
            status = exc.response.status_code if exc.response is not None else "unknown"
            raise RuntimeError(f"API key token exchange failed (HTTP {status}).") from None
        except (requests.RequestException, ValueError):
            # Suppress chained exception: requests exception strings may expose
            # the key included in the URL query string.
            raise RuntimeError("API key token exchange failed (connection or invalid JSON).") from None

        if not isinstance(payload, dict):
            raise RuntimeError("Unexpected response from API key token exchange.")
        auth_token = payload.get("authToken")
        token = auth_token.get("token") if isinstance(auth_token, dict) else payload.get("token")
        if not isinstance(token, str) or not token:
            raise RuntimeError("API key token exchange returned no authentication token.")

    session.headers.update({"Girder-Token": token})
    return session


def get_json(session: requests.Session, endpoint: str, params: dict[str, Any] | None = None) -> Any:
    url = f"{API_URL.rstrip('/')}/{endpoint.lstrip('/')}"
    try:
        response = session.get(url, params=params, timeout=(15, 90))
        response.raise_for_status()
        return response.json()
    except requests.HTTPError as exc:
        status = exc.response.status_code if exc.response is not None else "unknown"
        raise RuntimeError(f"API request failed: GET /{endpoint} (HTTP {status}). Check permissions/authentication.") from exc
    except (requests.RequestException, ValueError) as exc:
        raise RuntimeError(f"API request failed: GET /{endpoint}: {type(exc).__name__}") from exc


def paginated(session: requests.Session, endpoint: str, params: dict[str, Any], page_size: int = 100) -> list[dict]:
    results: list[dict] = []
    offset = 0
    while True:
        batch = get_json(session, endpoint, {**params, "limit": page_size, "offset": offset,
                                             "sort": "lowerName", "sortdir": 1})
        if not isinstance(batch, list):
            raise RuntimeError(f"Unexpected API response for /{endpoint}: expected JSON list")
        results.extend(batch)
        if len(batch) < page_size:
            return results
        offset += len(batch)


def list_folders(session: requests.Session, folder_id: str) -> list[dict]:
    return paginated(session, "folder", {"parentType": "folder", "parentId": folder_id})


def list_items(session: requests.Session, folder_id: str) -> list[dict]:
    return paginated(session, "item", {"folderId": folder_id})


def parse_path(data_type: str, parts: tuple[str, ...]) -> dict[str, str]:
    result = dict(processor="", batch="", site="", patient_id="", visit="",
                  examination_date="", anatomical_region="")
    components = list(parts)
    if data_type == "processed" and components and not re.match(r"^BC\d+", components[0], re.I):
        result["processor"] = components.pop(0)
    for idx, component in enumerate(components):
        if re.match(r"^BC\d+", component, re.I):
            result["batch"] = component
        elif re.match(r"^Site\s+", component, re.I):
            result["site"] = re.sub(r"^Site\s+", "", component, flags=re.I).strip()
        elif re.match(r"^Patient\s+", component, re.I):
            result["patient_id"] = re.sub(r"^Patient\s+", "", component, flags=re.I).strip()
            if idx + 1 < len(components):
                result["visit"] = components[idx + 1]
        elif component.lower().startswith("endoscopy"):
            match = re.search(r"Endoscopy\s*-\s*(.+)", component, re.I)
            if match:
                result["examination_date"] = match.group(1).strip()
        elif component.upper().startswith("ES -"):
            result["anatomical_region"] = component
    return result


def classify(data_type: str, filename: str, meta: dict) -> tuple[str, bool]:
    source = meta.get("source_video")
    if data_type == "processed":
        return "PROCESSED", False
    if source is True:
        return "RAW_ORIGINAL", False
    if "transcoded" in filename.casefold():
        return "RAW_TRANSCODED", False
    return "RAW_UNCLASSIFIED", True


def item_to_row(item: dict, data_type: str, path_parts: tuple[str, ...], study: dict) -> dict | None:
    name = str(item.get("name") or "")
    extension = Path(name).suffix.lower()
    if extension not in VIDEO_EXTENSIONS:
        return None
    meta = item.get("meta") or {}
    if not isinstance(meta, dict):
        meta = {}
    parsed = parse_path(data_type, path_parts)
    category, review = classify(data_type, name, meta)
    import_path = str(meta.get("import_path") or "")
    if not parsed["patient_id"] or not parsed["visit"]:
        review = True
    return {
        "study": study["label"], "study_folder": study["name"], "study_folder_id": study["_id"],
        "collection_id": COLLECTION_ID, "data_type": data_type,
        "video_class": category, **parsed, "filename": name, "extension": extension,
        "item_id": str(item.get("_id") or ""), "folder_id": str(item.get("folderId") or ""),
        "size_bytes": item.get("size", ""), "source_video": meta.get("source_video", ""),
        "transcoder": meta.get("transcoder", ""), "codec": meta.get("codec", ""),
        "fps": meta.get("fps", meta.get("originalFps", "")),
        "import_source": meta.get("import_source", ""),
        "import_path": import_path, "relative_path": "/".join((*path_parts, name)),
        "updated": item.get("updated", ""), "needs_review": review,
    }


def write_csv(path: Path, rows: list[dict]) -> None:
    # utf-8-sig helps Excel display column headers and accents correctly.
    with path.open("w", newline="", encoding="utf-8-sig") as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDS, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def scan(session: requests.Session, root_id: str, data_type: str, mode: str,
         max_folders: int, pause: float, study: dict) -> tuple[list[dict], dict]:
    stack: list[tuple[str, tuple[str, ...]]] = [(root_id, ())]
    visited: set[str] = set()
    rows: list[dict] = []
    errors: list[dict] = []
    folder_count = 0
    item_count = 0
    while stack:
        if mode == "test" and folder_count >= max_folders:
            break
        folder_id, path_parts = stack.pop()
        if folder_id in visited:
            continue
        visited.add(folder_id)
        folder_count += 1
        try:
            items = list_items(session, folder_id)
            children = list_folders(session, folder_id)
        except RuntimeError as exc:
            logging.error("Could not read folder %s: %s", folder_id, exc)
            errors.append({"folder_id": folder_id, "error": str(exc)})
            continue
        item_count += len(items)
        for item in items:
            row = item_to_row(item, data_type, path_parts, study)
            if row:
                rows.append(row)
        # Sorting + LIFO ensures deterministic traversal.
        for child in sorted(children, key=lambda x: str(x.get("name", "")).casefold(), reverse=True):
            child_id = str(child.get("_id") or "")
            if child_id:
                stack.append((child_id, (*path_parts, str(child.get("name") or ""))))
        if folder_count % 25 == 0 or (mode == "test" and folder_count == max_folders):
            logging.info("%s: folders=%s, items=%s, videos=%s, pending=%s",
                         data_type, folder_count, item_count, len(rows), len(stack))
        if pause:
            time.sleep(pause)
    return rows, {"folders_scanned": folder_count, "items_seen": item_count,
                  "videos_found": len(rows), "folders_pending": len(stack),
                  "complete": not stack and not errors, "errors": errors}


def discover_studies(session: requests.Session) -> list[dict]:
    folders = paginated(session, "folder", {"parentType": "collection", "parentId": COLLECTION_ID})
    studies = []
    for folder in folders:
        name = str(folder.get("name") or "")
        folder_id = str(folder.get("_id") or "")
        if not folder_id:
            continue
        match = re.search(r"\(([^()]+)\)\s*$", name)
        label = match.group(1).strip() if match else name
        studies.append({"_id": folder_id, "name": name, "label": label})
    return studies


def main() -> int:
    parser = argparse.ArgumentParser(description="Read-only multistudy video inventory from Thea.View")
    parser.add_argument("--mode", choices=["test", "full"], default="test")
    parser.add_argument("--study", default="all", help="Study name substring (e.g. QUASAR), or all")
    parser.add_argument("--folder-prefix", default="", help="Optional study folder name prefix, e.g. IMM_Endoscopy")
    parser.add_argument("--max-studies", type=int, default=2, help="Max studies in test mode")
    parser.add_argument("--max-folders", type=int, default=30, help="Max folders PER data root in test mode")
    parser.add_argument("--pause", type=float, default=0.05)
    parser.add_argument("--output-dir", default="video_inventory_output")
    args = parser.parse_args()
    if args.max_folders < 1 or args.max_studies < 1 or args.pause < 0:
        parser.error("--max-folders and --max-studies must be >=1; --pause must be >=0")
    out = Path(args.output_dir)
    out.mkdir(parents=True, exist_ok=True)
    configure_logging(out)
    try:
        session = make_session()
        user = get_json(session, "user/me")
        if not isinstance(user, dict) or not user.get("_id"):
            raise RuntimeError("Authentication test returned no valid user record")
        studies = discover_studies(session)
        if args.study.casefold() != "all":
            studies = [s for s in studies if args.study.casefold() in s["name"].casefold()]
        if args.folder_prefix:
            studies = [s for s in studies if s["name"].casefold().startswith(args.folder_prefix.casefold())]
        if not studies:
            raise RuntimeError("No matching study folders found")
        if args.mode == "test":
            studies = studies[:args.max_studies]
        if args.study.casefold() != "all" and len(studies) != 1:
            raise RuntimeError(f"Expected exactly one study folder for {args.study!r}; found {len(studies)}. Refine --folder-prefix.")
        logging.info("Authenticated; %d study folders selected; mode=%s", len(studies), args.mode)
    except RuntimeError as exc:
        logging.error("Startup failed: %s", exc)
        return 2

    all_rows: list[dict] = []
    stats: list[dict] = []
    failures = False
    for index, study in enumerate(studies, 1):
        logging.info("Study %d/%d: %s", index, len(studies), study["name"])
        try:
            children = list_folders(session, study["_id"])
        except RuntimeError as exc:
            logging.error("Study root inaccessible: %s", exc)
            stats.append({"study": study["label"], "study_folder": study["name"],
                          "status": "ERROR", "error": str(exc)})
            failures = True
            continue
        roots = {str(f.get("name", "")).lower(): str(f.get("_id", "")) for f in children}
        for data_type in ("rawdata", "processed"):
            root_id = roots.get(data_type)
            if not root_id:
                stats.append({"study": study["label"], "study_folder": study["name"],
                              "data_type": data_type, "status": "ROOT_NOT_FOUND",
                              "videos_found": 0})
                continue
            rows, summary = scan(session, root_id, data_type, args.mode,
                                 args.max_folders, args.pause, study)
            all_rows.extend(rows)
            status = "COMPLETE" if summary["complete"] else "INCOMPLETE"
            stats.append({"study": study["label"], "study_folder": study["name"],
                          "data_type": data_type, "status": status, **summary})
            failures |= bool(summary["errors"])
            logging.info("%s/%s: %d videos, %d folders, %s", study["label"],
                         data_type, len(rows), summary["folders_scanned"], status)

    write_csv(out / "all_video_inventory.csv", all_rows)
    for data_type in ("rawdata", "processed"):
        write_csv(out / f"{data_type}_video_inventory.csv",
                  [r for r in all_rows if r["data_type"] == data_type])
    summary = {
        "collection_id": COLLECTION_ID, "mode": args.mode,
        "study_filter": args.study, "studies_selected": len(studies),
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "counts_by_video_class": dict(Counter(r["video_class"] for r in all_rows)),
        "review_count": sum(bool(r["needs_review"]) for r in all_rows),
        "results": stats,
        "note": "Counts are Girder video item counts, not deduplicated clinical recordings. "
                "Test mode is incomplete. ROOT_NOT_FOUND does not prove video absence. "
                "Folders with API errors invalidate completeness."
    }
    (out / "extraction_summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    logging.info("Finished: %d video items. Output: %s", len(all_rows), out.resolve())
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())

