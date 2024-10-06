library(tidyverse)
food_liking_olink_lm_res_df <- map2(
  intersection_deps_liking_diet %>% 
    mutate(
      food_fill = case_when(
        food_fill == "drinks" ~ "diet_fizzy_drinks",
        food_fill == "beans" ~ "lentils_beans",
        TRUE ~ food_fill
      )
    ) %>% 
    select(gene, food_fill) %>% 
    split(.$food_fill) %>% 
    map(
      \(food) {
        food %>% 
          select(gene) %>% 
          unlist() %>% 
          unique()
      }
    ),
  c("bacon", "broccoli", "diet_fizzy_drinks", "pizza"),
  \(gene_ls, liking) {
    gene_ls %>% 
      map(
        \(gene) {
          temp_df <- inner_join(
            olink_wide_df %>% 
              select(
                eid, !!sym(gene)
              ) %>% 
              drop_na(),
            food_liking_6_df %>% 
              select(
                eid, !!sym(liking)
              ) %>% 
              drop_na(),
            by = "eid"
          ) %>% 
            inner_join(
              age_sex_activity_tdi_bmi_df,
              by = "eid"
            )
          temp_formula <- str_c(
            gene, " ~ ",
            liking,  " + age + sex + tdi + met_time"
          ) %>% 
            as.formula()
          temp_lm_res <- lm(
            temp_formula,
            data = temp_df
          )
          temp_res_df <- temp_lm_res %>% 
            broom::tidy(conf.int = TRUE) %>% 
            slice(2) %>% 
            mutate(
              food = liking, gene_name = gene
            ) %>% 
            select(
              food, gene_name, term, food, beta = estimate, pval = p.value, conf.low ,conf.high
            )
        }
      ) %>% 
      bind_rows()
  }
) %>% 
  bind_rows() %>% 
  mutate(
    fdr = pval %>% 
      p.adjust(method = "fdr"),
    .after = pval
  )

food_liking_olink_lm_res_df <- food_liking_olink_lm_res_df %>% 
  mutate(
    beta_cha = beta %>% 
      round(3) %>% 
      format(nsmall = 3),
    id = 1:nrow(.) %>% 
      as.character(),
    pval_cha = case_when(
      pval < 0.001 ~ "***",
      pval < 0.01 ~ "**",
      pval < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

food_liking_olink_lm_res_df %>% 
  mutate(
    beta_num = str_c(food, "_beta"),
    beta_scale = scale(beta),
    beta_scale_cha = beta_scale %>% 
      round(2) %>% 
      format(nsmall = 2),
    food_name = "food"
  ) %>% 
  group_by(food) %>% 
  arrange(beta_scale) %>% 
  ungroup() %>% 
  mutate(
    gene_name = as_factor(gene_name),
    x_food = "food",
    x_label = "label"
  ) %>% 
  ggplot(
    aes(
      x = x_food, y = gene_name, fill = beta_scale
    )
  ) +
  geom_tile(color = "grey90") +
  geom_text(aes(label = pval_cha) , fontface = "bold" ) + 
  geom_text(aes(x = x_label, y = gene_name, label = beta_scale_cha)) +
  scale_fill_gradientn(
    colors = c("#10457e", "white", "#a01228"),  # 使用红色、白色和蓝色
    values = scales::rescale(c(-1.9, 0, 2.3)),  # 定义断点
    limits = c(-1.9, 2.3),
    na.value = "grey95"
  ) +
  facet_grid(vars(food), scales = "free", space = "free") +
  theme(
    panel.background = element_blank(),  # 去掉面板背景
    axis.ticks.x = element_blank(),      # 去掉x轴刻度线
    axis.text.x = element_blank(),       # 去掉x轴文本
    axis.title.x = element_blank()      # 去掉x轴标题
  )

ggsave(
  plot = last_plot(),
  filename = "deps_diet_intake_liking_binded_lm_scale_plot.pdf",
  device = "pdf",
  units = "px",
  height = 3200,
  width = 2200,
  dpi = 144
)