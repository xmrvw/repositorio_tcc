# =============================================================================
# CAPÍTULO 4.3.1 – DIMENSÃO MEIO AMBIENTE
# Dados: SNIS/SINISA, NASA POWER (clima), MapBiomas (cobertura vegetal), IBGE
# Município: Teresina (PI) | Código IBGE: 2211001 | UF: 22
# =============================================================================

# ─── 0. PACOTES NECESSÁRIOS ──────────────────────────────────────────────────
install.packages(c("tidyverse","geobr","sf","ggplot2","leaflet","terra",
                   "nasapower","httr2","readxl","janitor","scales","patchwork"))

library(tidyverse)
library(geobr)          # geometrias oficiais IBGE
library(sf)             # operações vetoriais
library(ggplot2)
library(scales)
library(patchwork)
library(nasapower)      # NASA POWER – temperatura, precipitação, radiação
library(httr2)          # requisições HTTP para SNIS API
library(readxl)         # leitura de planilhas SNIS baixadas
library(janitor)        # limpeza de nomes de colunas
library(lubridate)
library(dplyr)

# Constantes
COD_IBGE <- 2211001     # Teresina
UF_IBGE  <- 22          # Piauí
MUNICIPIO <- "Teresina"

# =============================================================================
# BLOCO 1 – GEOMETRIA DO MUNICÍPIO
# =============================================================================

# Polígono de Teresina (ano 2022)
teresina_geo <- geobr::read_municipality(code_muni = COD_IBGE, year = 2022)

# Setor censitário de Teresina (granularidade maior para mapas de calor/vegetação)
setores_the <- geobr::read_census_tract(
  code_tract = "PI",   # carrega PI todo; filtrar depois
  year = 2022,
  simplified = FALSE
) |>
  filter(code_muni == COD_IBGE)

# Visualização rápida
plot(st_geometry(teresina_geo), main = "Teresina – Limite Municipal")
plot(st_geometry(setores_the),  add = TRUE, col = "lightblue", border = "grey50")

# =============================================================================
# BLOCO 2 – TEMPERATURA E PRECIPITAÇÃO: NASA POWER
# =============================================================================
# NASA POWER fornece dados climáticos diários de reanálise (1981–hoje)
# Parâmetros mais úteis:
#   T2M      = temperatura do ar a 2 m (°C)
#   T2M_MAX  = temp. máxima diária
#   T2M_MIN  = temp. mínima diária
#   PRECTOTCORR = precipitação corrigida (mm/dia)
#   RH2M     = umidade relativa a 2 m (%)

clima_the <- nasapower::get_power(
  community  = "AG",           # agroclimatologia (inclui temperatura superficial)
  lonlat     = c(-42.8019, -5.0892),  # lon, lat de Teresina
  pars       = c("T2M", "T2M_MAX", "T2M_MIN", "PRECTOTCORR", "RH2M"),
  dates      = c("1990-01-01", "2024-12-31"),
  temporal_api = "DAILY"
)

# Limpeza e tipagem
clima_the <- clima_the |>
  janitor::clean_names() |>
  mutate(data = as.Date(paste(year, doy), format = "%Y %j")) |>
  filter(!is.na(t2m))

# Médias anuais de temperatura máxima
temp_anual <- clima_the |>
  group_by(year) |>
  summarise(
    tmax_media  = mean(t2m_max, na.rm = TRUE),
    tmin_media  = mean(t2m_min, na.rm = TRUE),
    prec_total  = sum(prectotcorr, na.rm = TRUE),
    .groups = "drop"
  )

# Gráfico de tendência de temperatura
p_temp <- ggplot(temp_anual, aes(x = year)) +
  geom_line(aes(y = tmax_media, colour = "Máxima"), linewidth = 1) +
  geom_line(aes(y = tmin_media, colour = "Mínima"), linewidth = 1) +
  geom_smooth(aes(y = tmax_media), method = "lm", se = TRUE,
              colour = "red", linetype = "dashed", alpha = 0.2) +
  scale_colour_manual(values = c("Máxima" = "#E63946", "Mínima" = "#457B9D")) +
  labs(title = "Temperatura média anual em Teresina (PI) – 1990–2024",
       subtitle = "Fonte: NASA POWER Reanalysis",
       x = NULL, y = "Temperatura (°C)", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p_temp)
ggsave("figuras/01_temperatura_anual_teresina.png", p_temp,
       width = 10, height = 5, dpi = 300)

# Distribuição mensal da precipitação (regra dos 75% jan–abr)
class(clima_the$data)

clima_the <- clima_the |>
  mutate(data = as.Date(data)) 

clima_the <- clima_the |>
  mutate(month = month(data))

p_prec <- ggplot(prec_mensal, aes(x = month, y = prec_media)) +
  geom_col(fill = "#2196F3") +
  labs(
    title = "Precipitação média mensal – Teresina (1990–2024)",
    subtitle = "Fonte: NASA POWER",
    x = "Mês",
    y = "Precipitação (mm/mês)"
  ) +
  theme_minimal(base_size = 12)

print(p_prec)

prec_mensal$month <- factor(
  prec_mensal$month,
  levels = 1:12,
  labels = c(
    "Jan","Fev","Mar","Abr",
    "Mai","Jun","Jul","Ago",
    "Set","Out","Nov","Dez"
  )
)

# =============================================================================
# BLOCO 3 – SANEAMENTO BÁSICO: SNIS / SINISA
# =============================================================================
# OPÇÃO A: Download manual do SNIS (recomendada)
# 1. Acesse: http://app4.saude.gov.br/snis/
# 2. Selecione: Água e Esgotos → Série histórica → Município: Teresina-PI
# 3. Baixe como CSV ou XLSX
# 4. Salve em: dados/snis_teresina_serie.xlsx



# ── Leitura do arquivo baixado manualmente ──────────────────────────────────
snis_raw <- readr::read_delim(
  "C:/Projetos R/repositorio_tcc/repositorio_tcc/dados/agua_esgoto_the.csv",
  delim = ";",
  locale = readr::locale(encoding = "UTF-16LE"),
  show_col_types = FALSE,
  name_repair = "unique"
) |>
  janitor::clean_names()

snis_raw <- snis_raw |>
  dplyr::select(-dplyr::last_col())

names(snis_raw)[stringr::str_detect(names(snis_raw),
                                    "in055|in015|municip|ano")]

names(snis_raw)
problems(snis_raw)


snis_the <- snis_raw |>
  dplyr::filter(stringr::str_detect(municipio, stringr::regex("teresina", ignore_case = TRUE))) |>
  dplyr::select(
    ano = ano_de_referencia,
    pct_agua = in055_indice_de_atendimento_total_de_agua,
    pct_esgoto = in015_indice_de_coleta_de_esgoto,
    pct_esgoto_tratado = in016_indice_de_tratamento_de_esgoto,
    pop_agua = ag001_populacao_total_atendida_com_abastecimento_de_agua,
    pop_esgoto = es001_populacao_total_atendida_com_esgotamento_sanitario
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(pct_agua, pct_esgoto, pct_esgoto_tratado, pop_agua, pop_esgoto),
      ~ as.numeric(stringr::str_replace(as.character(.x), ",", "."))
    ),
    ano = as.integer(ano)
  )

snis_the

----------------------------------------------------
p_san <- snis_the |>
  tidyr::pivot_longer(
    cols = c(pct_agua, pct_esgoto),
    names_to = "servico",
    values_to = "pct"
  ) |>
  dplyr::mutate(
    servico = dplyr::recode(
      servico,
      pct_agua = "Água (% atendida)",
      pct_esgoto = "Esgoto coletado (% atendida)"
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = pct, colour = servico, group = servico)) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 110)
  ) +
  ggplot2::scale_colour_manual(values = c("#2196F3", "#FF5722")) +
  ggplot2::labs(
    title = "Cobertura de saneamento em Teresina (PI)",
    subtitle = "Fonte: SNIS/SINISA",
    x = NULL,
    y = "% da população atendida",
    colour = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "bottom")

print(p_san)

list.files()
names(snis_raw)

# Indicadores-chave do SNIS para Teresina:
# IN015_PCT_POPULACAO_ATENDIDA_COM_ESGOTO_COLETADO
# IN016_PCT_POPULACAO_ATENDIDA_COM_ESGOTO_TRATADO
# IN055_PCT_POPULACAO_ATENDIDA_COM_ABASTECIMENTO_AGUA
# GE12_POPULACAO_TOTAL_ATENDIDA_COM_ESGOTO_COLETADO
# ES001_POPULACAO_ATENDIDA_COM_ESGOTO_COLETADO

snis_the <- snis_raw |>
  filter(str_detect(municipio, regex("teresina", ignore_case = TRUE))) |>
  select(
    ano = ano_de_referencia,
    pct_agua = contains("in055"),
    pct_esgoto = contains("in015")
  ) |>
  mutate(
    pct_agua = as.numeric(str_replace(pct_agua, ",", ".")),
    pct_esgoto = as.numeric(str_replace(pct_esgoto, ",", ".")),
    ano = as.integer(ano)
  )

# Evolução histórica do saneamento
p_san <- snis_the |>
  pivot_longer(cols = c(pct_agua, pct_esgoto),
               names_to = "servico", values_to = "pct") |>
  mutate(servico = recode(servico,
                          pct_agua   = "Água (% atendida)",
                          pct_esgoto = "Esgoto coletado (% atendida)")) |>
  ggplot(aes(x = ano, y = pct, colour = servico, group = servico)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 110)) +
  scale_colour_manual(values = c("#2196F3", "#FF5722")) +
  labs(title = "Cobertura de saneamento em Teresina (PI)",
       subtitle = "Fonte: SNIS/SINISA",
       x = NULL, y = "% da população atendida", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p_san)
ggsave("figuras/03_saneamento_historico_teresina.png", p_san,
       width = 9, height = 5, dpi = 300)

# =============================================================================
# BLOCO 4 – COBERTURA VEGETAL (MapBiomas)
# =============================================================================
# MapBiomas disponibiliza coleções anuais de uso e cobertura do solo via
# Google Earth Engine (GEE) ou download de arquivos municipais.
#
# OPÇÃO MAIS ACESSÍVEL: tabela de estatísticas por município (CSV)
# Download: https://mapbiomas.org/estatisticas → filtro: Teresina-PI
# Baixe: "Área por município por classe de cobertura (1985–2023)"

mapbio_raw <- readr::read_csv("dados/mapbiomas_teresina.csv",
                              locale = locale(encoding = "UTF-8")) |>
  janitor::clean_names()

# Classes de interesse para Teresina (código MapBiomas Coleção 9):
# 3  = Formação florestal
# 4  = Formação savânica (cerrado)
# 11 = Campo alagado / Várzea
# 24 = Área urbanizada
# 25 = Outras áreas não vegetadas

cobertura_the <- mapbio_raw |>
  filter(geocodigo == COD_IBGE) |>
  pivot_longer(cols = starts_with("x"),
               names_to = "ano", values_to = "area_ha") |>
  mutate(ano = as.integer(str_remove(ano, "x")))

# Evolução da área urbanizada vs. vegetação nativa
p_cob <- cobertura_the |>
  filter(classe %in% c("Área Urbanizada", "Formação Florestal",
                       "Formação Savânica", "Campo Alagado e Área Pantanosa")) |>
  ggplot(aes(x = ano, y = area_ha / 1000, colour = classe, group = classe)) +
  geom_line(linewidth = 1) +
  labs(title = "Dinâmica de uso e cobertura do solo – Teresina (1985–2023)",
       subtitle = "Fonte: MapBiomas Coleção 9",
       x = NULL, y = "Área (× 1000 ha)", colour = "Classe") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p_cob)
ggsave("figuras/04_cobertura_vegetal_teresina.png", p_cob,
       width = 10, height = 5, dpi = 300)

# =============================================================================
# BLOCO 5 – ÍNDICE DE VEGETAÇÃO (NDVI) POR SETOR CENSITÁRIO
# =============================================================================
# Esta análise requer acesso ao Google Earth Engine (GEE) ou
# rasters LANDSAT/MODIS baixados localmente.
# Abaixo o script para processar um raster NDVI já baixado (.tif):
install.packages("terra")
library(terra)

ndvi_raster <- terra::rast("dados/ndvi_teresina_2023.tif")
setores_the_reproj <- st_transform(setores_the, crs(ndvi_raster))

 ndvi_por_setor <- terra::extract(ndvi_raster, vect(setores_the_reproj),
                                   fun = mean, na.rm = TRUE, bind = TRUE) |>
   as_tibble() |>
   rename(ndvi_medio = last_col())

 setores_ndvi <- left_join(setores_the, ndvi_por_setor, by = "code_tract")

 ggplot(setores_ndvi) +
   geom_sf(aes(fill = ndvi_medio), colour = NA) +
   scale_fill_viridis_c(option = "YlGn", name = "NDVI médio") +
   labs(title = "NDVI médio por setor censitário – Teresina (2023)",
        subtitle = "Fonte: USGS Landsat 8/9 via GEE") +
   theme_minimal()

# =============================================================================
# BLOCO 6 – SÍNTESE DA DIMENSÃO AMBIENTAL
# =============================================================================

cat("========== SÍNTESE – DIMENSÃO MEIO AMBIENTE ==========\n")
cat(sprintf("Município: %s | Cód. IBGE: %d\n\n", MUNICIPIO, COD_IBGE))

# Temperatura máxima média nos últimos 5 anos
cat(">> Temperatura máxima média (2020–2024):\n")
temp_anual |>
  filter(year >= 2020) |>
  summarise(media = mean(tmax_media, na.rm = TRUE)) |>
  pull(media) |>
  cat(sprintf("   %.1f°C\n\n", x = _))

# Tendência de temperatura (regressão linear 1990–2024)
modelo_temp <- lm(tmax_media ~ year, data = temp_anual)
cat(sprintf(">> Tendência de aquecimento (1990–2024): +%.3f°C/ano\n\n",
            coef(modelo_temp)["year"]))

# Último dado de saneamento disponível
cat(">> Cobertura de esgoto (último ano SNIS):\n")
snis_the |> slice_max(ano) |>
  select(ano, pct_esgoto) |> print()

message("\n✅ Bloco 01_meio_ambiente.R executado com sucesso.")