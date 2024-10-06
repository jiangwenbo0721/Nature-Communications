tm <- forest_theme(
  base_size = 10,        # 字体大小为10
  refline_col = "red",   # 参考线颜色红色
  arrow_type = "closed", # 箭头类型为闭合式箭头
  footnote_col = "blue",
  core = list(
    bg_params = list(fill = "white")
  )
)

food_group_and_color_df <- uniMR_and_multiMR_df %>%
  select(food_group) %>%
  distinct() %>%
  inner_join(
    tribble(
      ~ food_group, ~ color,
      "Alcohol/Beverage", "#FFA333",
      "Coffee/Tea", "#4DAF4A",
      "Dairy/Cheese" ,"#417C60",
      "Dessert", "#E87B1E",
      "Flavour/Taste preference", "#0076B9",
      "Fruit", "#F898CB",
      "Grain", "#3D505A",
      "Processed products", "#D65190",
      "Total Animal Products", "#699ECA",
      "Vegetable/Beans", "#731A73"
    )
  ) %>%
  arrange(food_group)

uni_multi_mr_res_color_df <- uniMR_and_multiMR_df %>%
  mutate(n_row = 1:nrow(.)) %>%
  select(food_group, n_row) %>%
  left_join(
    food_group_and_color_df
  )

uniMR_and_multiMR_df <- uniMR_and_multiMR_df %>% 
  mutate(
    `          ` = "                   ", .after = Outcome
  ) %>% 
  mutate(
    `           ` = "                   ", .before = `multiMR OR (95% CI)`
  )

uni_multi_mr_res_forest_plot <- forest(
  uniMR_and_multiMR_df %>% 
    select(
      Exposure, food_group, Outcome, `          `, `uniMR OR (95% CI)`,
      `uniMR FDR`, `           `, `multiMR OR (95% CI)`, `multiMR P-value`
    ),
  est = list(
    uniMR_and_multiMR_df$`uniMR or`, uniMR_and_multiMR_df$`multiMR or`
  ),
  lower = list(
    uniMR_and_multiMR_df$`uniMR conflow`, uniMR_and_multiMR_df$`multiMR conflow`
  ),
  upper = list(
    uniMR_and_multiMR_df$`uniMR confhigh`, uniMR_and_multiMR_df$`multiMR confhigh`
  ),
  ci_column = c(4, 7),
  ref_line = c(1, 1),
  theme = tm
)

for (i in 1:nrow(uniMR_and_multiMR_df)) {
  uni_multi_mr_res_forest_plot <- uni_multi_mr_res_forest_plot %>%
    edit_plot(
      row = i, part = "body", which = "text", gp = gpar(
        col = uni_multi_mr_res_color_df$color[i]
      )
    ) %>%
    edit_plot(
      row = i, col = c(4, 7), part = "body", which = "ci", gp = gpar(
        col = uni_multi_mr_res_color_df$color[i], fill = uni_multi_mr_res_color_df$color[i]
      )
    )
}

get_wh(uni_multi_mr_res_forest_plot, unit = "in")

ggsave(
  filename = "uni_multi_mr_res_forest_plot.pdf",
  device = "pdf",
  plot = uni_multi_mr_res_forest_plot,
  units = "in",
  width = get_wh(uni_multi_mr_res_forest_plot, unit = "in")[1],
  height = get_wh(uni_multi_mr_res_forest_plot, unit = "in")[2],
  dpi = 72
)