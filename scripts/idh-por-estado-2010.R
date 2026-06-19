ggplot(df_idh, aes(x = reorder(Estado, IDH), y = IDH, fill = destaque)) +
  geom_bar(stat = "identity") +
  
  geom_text(
    data = df_pi,
    aes(label = format(IDH, nsmall = 3)),
    hjust = -0.2, size = 3, color = "#C0392B"
  ) +
  
  geom_hline(yintercept = media, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  annotate("text", x = 1, y = media + 0.01,
           label = media_label,
           hjust = 0, vjust = 0, size = 2.8, color = "gray40") +
  
  annotate("text", x = pos_pi, y = 0.42,
           label = "PI – 24ª posição nacional",
           hjust = 0, size = 2.8, color = "gray25", fontface = "italic") +
  
  scale_fill_manual(values = c("Piauí" = "#C0392B", "Outros" = "#D9D9D9")) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 0.88), expand = c(0, 0)) +
  theme_minimal() +
  labs(
    x = "",
    y = "IDH",
    caption = "Fonte: Atlas do Desenvolvimento Humano no Brasil (PNUD, IPEA e FJP, 2010). Elaboração própria."
  ) +
  theme(
    text = element_text(size = 10),
    legend.position = "none",
    plot.caption = element_text(size = 7, color = "gray50", hjust = 0, margin = margin(t = 8))
  )
