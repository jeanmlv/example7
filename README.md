# example7

1. Regra de prioridade que eu utilizaria
Em vez de escolher o último valor concatenado, utilizaria uma seleção hierárquica:
Prioridade	Critério	Regra proposta
1	ANL01FL	Priorizar Y, quando essa for a flag relevante para a análise
2	DTYPE	Priorizar observado ou derivado conforme a definição do parâmetro
3	ADT	Utilizar a data conforme a janela analítica definida
4	ASEQ	Utilizar apenas como critério técnico de desempate, se justificado
5	AVAL / AVALC	Preservar o valor do registro selecionado

Essa hierarquia é uma proposta inicial, não uma regra universal CDISC. Por exemplo, ANL01FL pode não ser a flag adequada para todas as variáveis, e registros derivados podem ser exatamente aqueles exigidos pela análise.
