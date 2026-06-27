# =========================================================
# IDHM DAS CAPITAIS DO NORDESTE
# =========================================================

library(tidyverse)
library(scales)


# ---------------------------------------------------------
# 1. Base de dados
# ---------------------------------------------------------

df_idhm_capitais_ne <- tibble(
  Municipio = c(
    "São Luís", "Teresina", "Fortaleza", "Natal", "João Pessoa",
    "Recife", "Maceió", "Aracaju", "Salvador"
  ),
  UF = c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"),
  IDHM = c(0.768, 0.751, 0.754, 0.763, 0.763, 0.772, 0.721, 0.770, 0.759)
) |>
  mutate(
    destaque = if_else(Municipio == "Teresina", "Teresina", "Outras capitais"),
    nome = paste0(Municipio, " (", UF, ")")
  )

# ---------------------------------------------------------
# 2. Média das capitais do Nordeste
# ---------------------------------------------------------

media_ne <- mean(df_idhm_capitais_ne$IDHM)

media_label <- paste0("Média das capitais do Nordeste: ", 
                      format(round(media_ne, 3), nsmall = 3))

# ---------------------------------------------------------
# 3. Posição de Teresina no gráfico
# ---------------------------------------------------------

df_teresina <- df_idhm_capitais_ne |> 
  filter(Municipio == "Teresina")

pos_teresina <- which(
  levels(reorder(df_idhm_capitais_ne$nome, df_idhm_capitais_ne$IDHM)) == 
    df_teresina$nome
)

# ---------------------------------------------------------
# 4. Gráfico
# ---------------------------------------------------------

p_idhm <- ggplot(
  df_idhm_capitais_ne,
  aes(x = reorder(nome, IDHM), y = IDHM, fill = destaque)
) +
  geom_col(width = 0.8) +
  
  geom_text(
    aes(label = format(IDHM, nsmall = 3)),
    hjust = -0.15,
    size = 3.5
  ) +
  
  geom_hline(
    yintercept = media_ne,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.5
  ) +
  
  annotate(
    "text",
    x = 1,
    y = media_ne + 0.006,
    label = media_label,
    hjust = 0,
    vjust = 0,
    size = 3.2,
    color = "gray40"
  ) +
  
  annotate(
    "text",
    x = pos_teresina,
    y = 0.70,
    label = "Teresina: capital piauiense",
    hjust = 0,
    size = 3.2,
    color = "gray25",
    fontface = "italic"
  ) +
  
  scale_fill_manual(
    values = c(
      "Teresina" = "#C0392B",
      "Outras capitais" = "#D9D9D9"
    )
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    limits = c(0.68, 0.79),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "",
    y = "IDHM"
  ) +
  
  theme_minimal() +
  theme(
    text = element_text(size = 11),
    
    axis.title = element_text(
      size = 11,
      face = "plain"
    ),
    
    axis.text = element_text(
      size = 11,
      face = "plain"
    ),
    
    legend.position = "none",
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.3
    )
  )

p_idhm
