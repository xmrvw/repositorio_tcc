# =========================================================
# POPULAÇÃO DE TERESINA
# IBGE/SIDRA - Tabela 6579
# =========================================================

# ---------------------------------------------------------
# 1. Pacotes
# ---------------------------------------------------------

library(sidrar)
library(tidyverse)
library(janitor)
library(scales)

# ---------------------------------------------------------
# 2. Consulta da tabela no SIDRA
# ---------------------------------------------------------

search_sidra("população")
info_sidra(6579)

# ---------------------------------------------------------
# 3. Coleta dos dados
# ---------------------------------------------------------

pop <- get_sidra(
  x = 6579,
  variable = 9324,
  geo = "City",
  geo.filter = list("City" = 2211001),
  period = "all"
)

# ---------------------------------------------------------
# 4. Limpeza e organização dos dados
# ---------------------------------------------------------

pop_the <- pop |> 
  clean_names()

names(pop_the)

pop_grafico <- pop_the |> 
  transmute(
    ano = as.numeric(ano),
    populacao = as.numeric(valor)
  ) |> 
  filter(
    !is.na(ano),
    !is.na(populacao)
  ) |> 
  arrange(ano)

# ---------------------------------------------------------
tema_tcc <- theme_minimal() +
  theme(
    text = element_text(size = 10),
    
    axis.title = element_text(
      size = 10,
      face = "plain"
    ),
    
    axis.text = element_text(
      size = 10,
      face = "plain"
    ),
    
    axis.title.x = element_text(
      margin = margin(t = 10)
    ),
    
    axis.title.y = element_text(
      margin = margin(r = 15)
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.3
    ),
    
    plot.caption = element_text(
      size = 7,
      color = "gray50",
      hjust = 0,
      margin = margin(t = 8)
    ),
    
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )
  )

# ---------------------------------------------------------
# 5. Gráfico
# ---------------------------------------------------------

p_pop <- ggplot(
  pop_grafico,
  aes(x = ano, y = populacao)
) +
  geom_line(
    linewidth = 0.7,
    colour = "black"
  ) +
  
  scale_x_continuous(
    breaks = seq(
      min(pop_grafico$ano, na.rm = TRUE),
      max(pop_grafico$ano, na.rm = TRUE),
      by = 5
    )
  ) +
  
  scale_y_continuous(
    labels = label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  
  labs(
    x = "Ano",
    y = "População",
    caption = "Fonte: IBGE/SIDRA, Tabela 6579. Elaboração própria."
  ) +
  
  tema_tcc


p_pop

# ---------------------------------------------------------
# 6. Salvar
# ---------------------------------------------------------

dir.create("figuras", showWarnings = FALSE)

ggsave(
  filename = "figuras/populacao_residente_teresina.png",
  plot = p_pop,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)