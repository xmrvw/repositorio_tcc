library(tidyverse)
library(readxl)
library(janitor)
library(scales)

mapbiomas <- read_excel(
  "C:/Projetos R/repositorio_tcc/repositorio_tcc/dados/mapbiomas_teresina.xlsx.xlsx"
) |>
  clean_names()

names(mapbiomas)

mapbiomas <- read_excel(
  "C:/Projetos R/repositorio_tcc/repositorio_tcc/dados/mapbiomas_teresina.xlsx"
) |>
  clean_names()

mapbiomas |>
  select(class_level_2) |>
  distinct()


library(stringr)

mapbiomas_long <- mapbiomas |>
  pivot_longer(
    cols = starts_with("x"),
    names_to = "ano",
    values_to = "area_ha"
  ) |>
  mutate(
    ano = as.numeric(str_remove(ano, "x"))
  )

glimpse(mapbiomas_long)


mapbiomas_long |>
  select(class_level_2) |>
  distinct()


mapbiomas_the <- mapbiomas |>
  filter(municipality == "Teresina") |>
  pivot_longer(
    cols = starts_with("x"),
    names_to = "ano",
    values_to = "area_ha"
  ) |>
  mutate(
    ano = as.numeric(str_remove(ano, "x")),
    grupo = case_when(
      class_level_2 == "4.2. Urban Area" ~ "Área urbanizada",
      class_level_2 %in% c(
        "1.1. Forest Formation",
        "1.2. Savanna Formation"
      ) ~ "Vegetação",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(grupo)) |>
  group_by(ano, grupo) |>
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop")

mapbiomas_the

p_mapbiomas <- ggplot(mapbiomas_the, aes(x = ano, y = area_ha, color = grupo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.2) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_x_continuous(
    breaks = seq(1985, 2024, 5)
  ) +
  labs(
    title = "Mudança na cobertura da terra em Teresina",
    subtitle = "Área urbanizada e vegetação entre 1985 e 2024",
    x = "Ano",
    y = "Área em hectares",
    color = "",
    caption = "Fonte: MapBiomas"
  ) +
  theme_minimal(base_size = 14)

p_mapbiomas


mapbiomas_the |>
  group_by(grupo) |>
  summarise(
    area_1985 = area_ha[ano == 1985],
    area_2024 = area_ha[ano == 2024],
    variacao_pct = ((area_2024 / area_1985) - 1) * 100
  )


#------------
#Gráfico 2

mapbiomas_indice <- mapbiomas_the |>
  group_by(grupo) |>
  mutate(
    indice = area_ha / area_ha[ano == 1985] * 100
  )

ggplot(mapbiomas_indice,
       aes(ano, indice, color = grupo)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  labs(
    title = "Evolução relativa da cobertura da terra em Teresina",
    subtitle = "Índice base 1985 = 100",
    x = "Ano",
    y = "Índice",
    color = ""
  ) +
  theme_minimal(base_size = 14)



