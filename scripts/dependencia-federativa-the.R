# ============================================================
# DEPENDÊNCIA FEDERATIVA DE TERESINA
# Participação das transferências correntes nas receitas correntes
# Fonte: SICONFI / RREO / STN
# Período: 2015-2024
# ============================================================


# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(scales)


# ------------------------------------------------------------
# 2. Parâmetros da análise
# ------------------------------------------------------------

cod_teresina <- 2211001
anos <- 2015:2024


# ------------------------------------------------------------
# 3. Função para baixar dados do RREO no SICONFI
# ------------------------------------------------------------

baixar_rreo_ano <- function(ano) {
  
  message("Baixando ano: ", ano)
  
  resposta <- GET(
    "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rreo",
    query = list(
      an_exercicio = ano,
      nr_periodo = 6,
      co_tipo_demonstrativo = "RREO",
      no_anexo = "RREO-Anexo 01",
      co_esfera = "M",
      id_ente = cod_teresina
    )
  )
  
  if (status_code(resposta) != 200) {
    warning("Erro no ano: ", ano)
    return(tibble())
  }
  
  conteudo <- content(resposta, as = "text", encoding = "UTF-8") |>
    fromJSON(flatten = TRUE)
  
  conteudo$items |>
    as_tibble() |>
    clean_names() |>
    mutate(ano = ano)
}


# ------------------------------------------------------------
# 4. Baixar dados de todos os anos
# ------------------------------------------------------------

rreo_teresina <- map_dfr(anos, baixar_rreo_ano)


# ------------------------------------------------------------
# 5. Conferir estrutura dos dados
# ------------------------------------------------------------

glimpse(rreo_teresina)

rreo_teresina |>
  distinct(coluna) |>
  print(n = 100)

rreo_teresina |>
  filter(str_detect(str_to_lower(conta), "receita|transfer")) |>
  distinct(conta) |>
  print(n = 200)


# ------------------------------------------------------------
# 6. Selecionar contas usadas no indicador
# ------------------------------------------------------------

receitas_dependencia <- rreo_teresina |>
  filter(
    coluna == "Até o Bimestre (c)",
    conta %in% c(
      "RECEITAS CORRENTES",
      "RECEITA TRIBUTÁRIA",
      "TRANSFERÊNCIAS CORRENTES"
    )
  ) |>
  group_by(ano, conta) |>
  summarise(
    valor = sum(as.numeric(valor), na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = conta,
    values_from = valor
  ) |>
  mutate(
    perc_receita_tributaria =
      `RECEITA TRIBUTÁRIA` / `RECEITAS CORRENTES`,
    
    perc_transferencias =
      `TRANSFERÊNCIAS CORRENTES` / `RECEITAS CORRENTES`
  )


# ------------------------------------------------------------
# 7. Visualizar tabela final
# ------------------------------------------------------------

receitas_dependencia


# ------------------------------------------------------------
# 8. Calcular médias do período
# ------------------------------------------------------------

medias_dependencia <- receitas_dependencia |>
  summarise(
    media_receita_tributaria = mean(perc_receita_tributaria, na.rm = TRUE),
    media_transferencias = mean(perc_transferencias, na.rm = TRUE)
  )

medias_dependencia_formatada <- medias_dependencia |>
  mutate(
    media_receita_tributaria = percent(
      media_receita_tributaria,
      accuracy = 0.1,
      decimal.mark = ","
    ),
    media_transferencias = percent(
      media_transferencias,
      accuracy = 0.1,
      decimal.mark = ","
    )
  )

medias_dependencia_formatada


# ------------------------------------------------------------
# 9. Preparar dados para o gráfico
# ------------------------------------------------------------

grafico_transferencias <- receitas_dependencia |>
  mutate(
    ano = factor(ano),
    dependencia = perc_transferencias
  )


# ------------------------------------------------------------
# 10. Gráfico final
# ------------------------------------------------------------

media_transferencias <- mean(
  grafico_transferencias$dependencia,
  na.rm = TRUE
)

p_dependencia <- ggplot(
  grafico_transferencias,
  aes(x = ano, y = dependencia)
) +
  
  # Barras
  geom_col(
    fill = "#457B9D",
    width = 0.75
  ) +
  
  # Percentuais sobre as barras
  geom_text(
    aes(
      label = scales::percent(
        dependencia,
        accuracy = 0.1,
        decimal.mark = ","
      )
    ),
    vjust = -0.35,
    size = 3.8,
    fontface = "bold",
    colour = "black"
  ) +
  
  # Linha da média
  geom_hline(
    yintercept = media_transferencias,
    colour = "gray35",
    linewidth = 0.8,
    linetype = "22"
  ) +
  
  # Texto da média
  annotate(
    "text",
    x = 5.5,
    y = media_transferencias + 0.085,
    label = paste0(
      "Média do período: ",
      scales::percent(
        media_transferencias,
        accuracy = 0.1,
        decimal.mark = ","
      )
    ),
    size = 4.3,
    fontface = "italic",
    colour = "gray30"
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(decimal.mark = ","),
    limits = c(0, 0.85),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    x = NULL,
    y = "Transferências nas receitas correntes (%)"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    
    text = element_text(family = "sans"),
    
    axis.title.y = element_text(
      size = 14,
      face = "bold"
    ),
    
    axis.text = element_text(
      size = 12,
      colour = "black"
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    plot.margin = margin(
      t = 20,
      r = 25,
      b = 10,
      l = 10
    )
  )

p_dependencia


# ------------------------------------------------------------
# 11. Salvar gráfico
# ------------------------------------------------------------

ggsave(
  "figuras/dependencia_federativa_teresina.png",
  p_dependencia,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)