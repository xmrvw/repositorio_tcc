# =========================================================
# TCC — SANEAMENTO BÁSICO EM TERESINA
# Fonte: SNIS / Base dos Dados
# Objetivo: analisar atendimento de água, esgoto,
# tratamento de esgoto e perdas na distribuição de água
# =========================================================


# =========================================================
# 1. PACOTES
# =========================================================

library(tidyverse)
library(janitor)
library(scales)
library(writexl)


# =========================================================
# 2. CAMINHOS DO PROJETO
# =========================================================

caminho_dados <- "C:/Projetos R/repositorio_tcc/repositorio_tcc/dados"
caminho_graficos <- "C:/Projetos R/repositorio_tcc/repositorio_tcc/graficos"

dir.create(caminho_graficos, showWarnings = FALSE)


# =========================================================
# 3. IMPORTAÇÃO DOS DADOS
# =========================================================

snis <- read_csv(
  file.path(caminho_dados, "br_mdr_snis_municipio_agua_esgoto.csv"),
  show_col_types = FALSE
) |>
  clean_names()


# Conferir estrutura geral da base
dim(snis)
names(snis)


# =========================================================
# 4. FILTRAR TERESINA
# =========================================================

# Código IBGE de Teresina: 2211001

snis_the <- snis |>
  filter(id_municipio == 2211001) |>
  arrange(ano)

dim(snis_the)
glimpse(snis_the)


# =========================================================
# 5. CRIAR BASE DE ANÁLISE
# =========================================================

saneamento_final <- snis_the |>
  select(
    ano,
    agua = indice_atendimento_total_agua,
    esgoto = indice_atendimento_esgoto_agua,
    esgoto_tratado = indice_esgotamento_agua_consumida,
    perdas = indice_perda_distribuicao_agua,
    hidrometracao = indice_hidrometracao,
    volume_agua_produzido,
    volume_agua_consumido,
    volume_agua_faturado
  ) |>
  arrange(ano)


# Visualizar a base final
print(saneamento_final, n = Inf)


# =========================================================
# 6. AUDITORIA DOS DADOS
# =========================================================

# 6.1 Valores ausentes
saneamento_final |>
  summarise(
    ausentes_agua = sum(is.na(agua)),
    ausentes_esgoto = sum(is.na(esgoto)),
    ausentes_esgoto_tratado = sum(is.na(esgoto_tratado)),
    ausentes_perdas = sum(is.na(perdas))
  )


# 6.2 Duplicidades por ano
saneamento_final |>
  count(ano) |>
  filter(n > 1)


# 6.3 Estatísticas descritivas
summary(saneamento_final)


# 6.4 Valores percentuais acima de 100%
saneamento_final |>
  filter(
    agua > 100 |
      esgoto > 100 |
      esgoto_tratado > 100 |
      perdas > 100
  )


# =========================================================
# 7. VERIFICAÇÃO DO ANO DE 2006
# =========================================================

# O ano de 2006 apresentou valor atípico em perdas.
# Por isso, verificamos os volumes e a hidrometração.

saneamento_final |>
  filter(ano %in% c(2005, 2006, 2007)) |>
  select(
    ano,
    perdas,
    volume_agua_produzido,
    volume_agua_consumido,
    volume_agua_faturado,
    hidrometracao
  )


# Decisão metodológica:
# O valor de 2006 será mantido, pois é compatível com os volumes
# declarados ao SNIS, embora deva ser interpretado com cautela.


# =========================================================
# 8. EXPORTAR TABELA FINAL
# =========================================================

write_xlsx(
  saneamento_final,
  file.path(dados, "tabela_saneamento_teresina_final.xlsx")
)


# =========================================================
# 9. GRÁFICO 1 — ATENDIMENTO DE ÁGUA E ESGOTO
# =========================================================

grafico_cobertura <- saneamento_final |>
  select(
    ano,
    agua,
    esgoto
  ) |>
  pivot_longer(
    cols = c(agua, esgoto),
    names_to = "indicador",
    values_to = "valor"
  ) |>
  mutate(
    indicador = recode(
      indicador,
      agua = "Atendimento total de água",
      esgoto = "Atendimento total de esgoto"
    )
  ) |>
  filter(!is.na(valor))


p_cobertura <- ggplot(
  grafico_cobertura,
  aes(x = ano, y = valor, color = indicador)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = label_percent(scale = 1)
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2022, 2)
  ) +
  labs(
    title = "Atendimento de água e esgoto em Teresina",
    subtitle = "Evolução dos serviços de saneamento básico",
    x = "Ano",
    y = "Percentual da população atendida",
    color = "",
    caption = "Fonte: SNIS/Base dos Dados"
  ) +
  theme_minimal(base_size = 14)

p_cobertura

ggsave(
  file.path(caminho_graficos, "grafico_agua_esgoto_teresina.png"),
  p_cobertura,
  width = 10,
  height = 6,
  dpi = 300
)


# =========================================================
# 10. GRÁFICO 2 — TRATAMENTO DE ESGOTO
# =========================================================

grafico_tratamento <- saneamento_final |>
  select(
    ano,
    esgoto_tratado
  ) |>
  filter(!is.na(esgoto_tratado))


p_tratamento <- ggplot(
  grafico_tratamento,
  aes(x = ano, y = esgoto_tratado)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    limits = c(0, 50),
    labels = label_percent(scale = 1)
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2022, 2)
  ) +
  labs(
    title = "Tratamento de esgoto em Teresina",
    subtitle = "Percentual de esgoto tratado em relação à água consumida",
    x = "Ano",
    y = "Percentual (%)",
    caption = "Fonte: SNIS/Base dos Dados"
  ) +
  theme_minimal(base_size = 14)

p_tratamento

ggsave(
  file.path(caminho_graficos, "grafico_tratamento_esgoto_teresina.png"),
  p_tratamento,
  width = 10,
  height = 6,
  dpi = 300
)


# =========================================================
# 11. GRÁFICO 3 — PERDAS NA DISTRIBUIÇÃO DE ÁGUA
# =========================================================

grafico_perdas <- saneamento_final |>
  select(
    ano,
    perdas
  ) |>
  filter(!is.na(perdas))


p_perdas <- ggplot(
  grafico_perdas,
  aes(x = ano, y = perdas)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_point(
    data = grafico_perdas |> filter(ano == 2006),
    aes(x = ano, y = perdas),
    size = 4
  ) +
  annotate(
    "text",
    x = 2006,
    y = 18,
    label = "2006: valor atípico",
    size = 4
  ) +
  scale_y_continuous(
    limits = c(0, 70),
    labels = label_percent(scale = 1)
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2022, 2)
  ) +
  labs(
    title = "Perdas na distribuição de água em Teresina",
    subtitle = "Percentual de perdas no sistema de abastecimento",
    x = "Ano",
    y = "Perdas na distribuição (%)",
    caption = "Fonte: SNIS/Base dos Dados"
  ) +
  theme_minimal(base_size = 14)

p_perdas

ggsave(
  file.path(caminho_graficos, "grafico_perdas_agua_teresina.png"),
  p_perdas,
  width = 10,
  height = 6,
  dpi = 300
)


# =========================================================
# 12. GRÁFICO 4 — HIDROMETRAÇÃO
# =========================================================

grafico_hidrometracao <- saneamento_final |>
  select(
    ano,
    hidrometracao
  ) |>
  filter(!is.na(hidrometracao))


p_hidrometracao <- ggplot(
  grafico_hidrometracao,
  aes(x = ano, y = hidrometracao)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = label_percent(scale = 1)
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2022, 2)
  ) +
  labs(
    title = "Índice de hidrometração em Teresina",
    subtitle = "Percentual de ligações de água com hidrômetro",
    x = "Ano",
    y = "Hidrometração (%)",
    caption = "Fonte: SNIS/Base dos Dados"
  ) +
  theme_minimal(base_size = 14)

p_hidrometracao

ggsave(
  file.path(caminho_graficos, "grafico_hidrometracao_teresina.png"),
  p_hidrometracao,
  width = 10,
  height = 6,
  dpi = 300
)


# =========================================================
# 13. RESUMO FINAL PARA INTERPRETAÇÃO
# =========================================================

resumo_saneamento <- saneamento_final |>
  summarise(
    agua_min = min(agua, na.rm = TRUE),
    agua_media = mean(agua, na.rm = TRUE),
    agua_max = max(agua, na.rm = TRUE),
    
    esgoto_min = min(esgoto, na.rm = TRUE),
    esgoto_media = mean(esgoto, na.rm = TRUE),
    esgoto_max = max(esgoto, na.rm = TRUE),
    
    tratamento_min = min(esgoto_tratado, na.rm = TRUE),
    tratamento_media = mean(esgoto_tratado, na.rm = TRUE),
    tratamento_max = max(esgoto_tratado, na.rm = TRUE),
    
    perdas_min = min(perdas, na.rm = TRUE),
    perdas_media = mean(perdas, na.rm = TRUE),
    perdas_max = max(perdas, na.rm = TRUE),
    
    hidrometracao_min = min(hidrometracao, na.rm = TRUE),
    hidrometracao_media = mean(hidrometracao, na.rm = TRUE),
    hidrometracao_max = max(hidrometracao, na.rm = TRUE)
  )

resumo_saneamento
