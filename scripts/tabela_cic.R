# =========================================================
# PACOTES
# =========================================================

install.packages(c("kableExtra", "dplyr"))

library(kableExtra)
library(dplyr)

# =========================================================
# BASE DA TABELA
# =========================================================

dados <- data.frame(
  Subdimensoes = c(
    "Mobilidade inteligente",
    "Tecnologia",
    "Ambiente inteligente",
    "Vida inteligente",
    "Planejamento urbano/\nurbanismo/\narquitetura",
    
    "Pessoas inteligentes",
    "Capital humano",
    "Capital social",
    "Capital cultural",
    "Inovação",
    "Indústrias criativas",
    "Classes criativas",
    "Tolerância",
    
    "Governança inteligente",
    "Coesão social",
    "Conectividade social",
    
    "Economia inteligente",
    "Economia",
    "Empreendedorismo"
  ),
  
  Indicadores = c(
    "Rede de transporte público por habitante (Giffinger et al., 2007)",
    "Porcentagem de banda larga de alta velocidade (Urban Systems, 2019)",
    "",
    "",
    "",
    
    "Patentes (Urban Systems, 2019; Castro-Higueras & Aguilera-Moyano, 2018)",
    "Número de teatros por cidade (Berrone & Ricart, 2019)",
    "Participação de estrangeiros (Giffinger et al., 2007)",
    "",
    "",
    "",
    "",
    "",
    
    "Parcela de representantes femininos nas cidades (Giffinger et al., 2007)",
    "Satisfação com a luta contra a corrupção (Giffinger et al., 2007)",
    "",
    
    "Transporte aéreo de passageiros (Giffinger et al., 2007; Figueiredo et al., 2019)",
    "Empregabilidade (Urban Systems, 2019)",
    ""
  )
)

# =========================================================
# TABELA
# =========================================================

kbl(
  dados,
  col.names = c(
    "SUBDIMENSÕES",
    "EXEMPLO DE INDICADORES E AUTORES"
  ),
  align = c("l", "l"),
  booktabs = TRUE,
  caption = "Tabela 1 – Dimensões, subdimensões e indicadores atrelados"
) %>%
  
  kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 14
  ) %>%
  
  row_spec(0, bold = TRUE) %>%
  
  # =====================================================
# AGRUPAMENTOS DAS DIMENSÕES
# =====================================================

pack_rows(
  "Suporte tecnológico",
  1, 5,
  bold = FALSE,
  background = "#F5F5F5"
) %>%
  
  pack_rows(
    "Criatividade social",
    6, 13,
    bold = FALSE,
    background = "#F5F5F5"
  ) %>%
  
  pack_rows(
    "Governança participativa",
    14, 16,
    bold = FALSE,
    background = "#F5F5F5"
  ) %>%
  
  pack_rows(
    "Economia e negócios",
    17, 19,
    bold = FALSE,
    background = "#F5F5F5"
  ) %>%
  
  footnote(
    general = "Fonte: Os autores (2025).",
    general_title = ""
  )
