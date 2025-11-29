
install.packages("DiagrammeR")
library(DiagrammeR)



grViz("
digraph kingdon {
  graph [
    layout = dot,
    rankdir = TB,
    labelloc = b,          # coloca o texto na parte de baixo
    label = '\n\\n\\nElaborado por: Amanda Ravelly a partir de CAPELLA, 2007, p. 98',
    fontsize = 12
  ]

  node [shape = box, style = filled, fillcolor = white, fontsize = 12]

  Problem  [label = 'PROBLEM STREAM\\n(Fluxo de problemas)\\n\\nIndicadores; Crises;\\nEventos focalizadores;\\nFeedback de ações.']
  Policy   [label = 'POLICY STREAM\\n(Fluxo de soluções)\\n\\nViabilidade técnica;\\nAceitação pela comunidade;\\nCustos toleráveis.']
  Political[label = 'POLITICAL STREAM\\n(Fluxo político)\\n\\n“Humor nacional”;\\nForças políticas organizadas;\\nMudanças no governo.']

  Window   [label = 'OPORTUNIDADE DE MUDANÇA\\n(Window)\\n\\nConvergência dos fluxos\\n(impulsionada pelos empreendedores\\nde políticas – policy entrepreneurs)']
  Agenda   [label = 'AGENDA-SETTING\\n\\nAcesso de uma questão à agenda']

  Problem -> Window
  Policy  -> Window
  Political -> Window
  Window -> Agenda
}
")
