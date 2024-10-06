liking_consumption_group_mean_radar_plot <- liking_consumption_group_cor_df %>% 
  select(
    food_liking, food_consumption_group, consumption_group_average
  ) %>% 
  pivot_wider(
    names_from = food_consumption_group, values_from = consumption_group_average
  ) %>% 
  mutate(
    across(where(is.numeric), ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)))
  ) %>%
  pivot_longer(
    -food_liking, names_to = "category", values_to = "consumption"
  ) %>%
  mutate(
    category = factor(
      category,
      levels = c(
        "Nuts and Seeds",
        "Juices and Plant-based Drinks",
        "Vegetables",
        "Decaffeinated Drinks",
        "Fruits",
        "Seafood and Fish",
        "Meat Substitutes",
        "Grains and Breads",
        "Sweets and Desserts",
        "Sugary and Non-sugar Drinks",
        "Red Meat",
        "White Meat",
        "Alcoholic Beverages",
        "Condiments and Snacks",
        "Potatoes and Starchy Foods",
        "Caffeinated Drinks",
        "Dairy"
      )
    ),
    food_liking = factor(
      food_liking,
      levels = c(
        "broccoli_like", "lentils_beans_like", "prawns_like", "pizza_like",
        "bacon_like", "diet_fizzy_drinks_like"
      )
    ),
    category_num = as.integer(category)
  ) %>% 
  ggplot(
    aes(x = category, y = consumption, color = food_liking)
  ) +
  geom_point(size = 4) +
  geom_line(
    aes(x = category_num, y = consumption), linewidth = 2
  ) +
  see::coord_radar() +
  scale_color_manual(
    values = c(
      "bacon_like" = "#E64B35",
      "broccoli_like" = "#4DBBD5",
      "diet_fizzy_drinks_like" = "#00A087",
      "lentils_beans_like" = "#3C5488",
      "pizza_like" = "#F39B7F",
      "prawns_like" = "#8491B4"
    )
  ) +
  theme_clean()

ggsave(
  plot = liking_consumption_group_mean_radar_plot,
  device = "pdf",
  units = "px",
  width = 2100,
  height = 1080,
  dpi = 144,
  filename = "liking_consumption_group_mean_radar_plot.pdf"
)