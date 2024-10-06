extreme_cox_padj_bubble_plot <- extreme_cox_anno_padj_df_ls %>% 
  bind_rows() %>% 
  mutate(
    anno = if_else(
      padj < 0.05, exposure, NA
    ),
    outcome = outcome %>%
      factor(
        levels = c(
          "cvd", "ihd", "mi", "stroke", "hf", "t2d"
        )
      )
  ) %>%
  ggplot(
    aes(
      x = food_group, y = -log10(padj), color = food_group
    )
  ) +
  geom_jitter(
    aes(size = ifelse(padj < 0.05, sqrt(log10padj) * 1.2, 1)),
    width = 0.5
  ) +
  scale_size_identity() +
  geom_label(
    aes(
      label = anno, color = food_group
    ), size = 4
  ) +
  scale_color_manual(
    values = c(
      "Alcohol/Beverage" = "#FFA333",
      "Coffee/Tea" = "#4DAF4A",
      "Dairy/Cheese" = "#417C60",
      "Dessert" = "#E87B1E",
      "Flavour/Taste preference" = "#0076B9",
      "Fruit" = "#F898CB",
      "Grain" = "#3D505A",
      "Processed products" = "#D65190",
      "Total Animal Products" = "#699ECA",
      "Vegetable/Beans" = "#731A73"
    )
  ) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "grey") +
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.2))) +
  labs(
    x = "",
    y = "-log<sub>10</sub>(padj)"
  ) +
  facet_grid(vars(outcome), scales = "free") +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#faf3eb"),
    legend.position = "none",
    axis.text.x = element_text(angle = 20, hjust = 1),
    axis.title.y = element_markdown()
  )


ggsave(
  plot = extreme_cox_padj_bubble_plot,
  filename = "extreme_cox_padj_bubble_plot.pdf",
  height = 2000,
  width = 2000,
  units = "px",
  device = "pdf", dpi = 72
)