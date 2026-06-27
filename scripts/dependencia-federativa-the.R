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
  geom_col(
    fill = "#C0392B",
    width = 0.7
  ) +
  geom_text(
    aes(
      label = percent(
        dependencia,
        accuracy = 0.1,
        decimal.mark = ","
      )
    ),
    vjust = -0.3,
    size = 3
  ) +
  geom_hline(
    yintercept = media_transferencias,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = 2,
    y = media_transferencias + 0.050,
    label = paste0(
      "Média do período: ",
      percent(
        media_transferencias,
        accuracy = 0.1,
        decimal.mark = ","
      )
    ),
    hjust = 0,
    size = 3,
    color = "gray30"
  ) +
  scale_y_continuous(
    labels = percent_format(decimal.mark = ","),
    limits = c(0, 0.80)
  ) +
  labs(
    x = "",
    y = "Quanto das receitas vem de transferências (%)",
    caption = "Fonte: SICONFI/RREO/STN. Elaboração própria."
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    plot.caption = element_text(
      size = 7,
      color = "gray50",
      hjust = 0
    )
  )

p_dependencia


# ------------------------------------------------------------
# 11. Salvar gráfico
# ------------------------------------------------------------

ggsave(
  filename = "grafico_dependencia_federativa_teresina.png",
  plot = p_dependencia,
  width = 9,
  height = 5,
  dpi = 300
)