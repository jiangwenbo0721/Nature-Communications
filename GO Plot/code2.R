# 加载必要的R包
library(readxl)
library(stringr)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggtext)
library(writexl)

# 读取 Excel 文件
dataD <- read_excel("E:\\Work\\8.27tu\\8.27sixtu\\-log10(p)zhuanhuan\\dataD.xlsx")

# 将 dataD 中的 pvalue 列进行负对数转换，并生成新列 log_p
dataD$log_p <- -log10(dataD$pvalue)

# 截短GOterm列中的文本
dataD$GOterm <- str_trunc(dataD$GOterm, width = 60, ellipsis = "...")

# 按log_p降序排列GOterm并保证每个子组的排序
dataD <- dataD %>%
  arrange(subgroup, desc(log_p)) %>%
  group_by(subgroup) %>%
  mutate(GOterm = factor(GOterm, levels = unique(GOterm[order(-log_p)])))

# 只保留 "BP"、"CC" 和 "MF" 子组的前10个，更新原始的 dataD 数据框
dataD <- dataD %>%
  group_by(subgroup) %>%
  filter((subgroup == "BP" | subgroup == "MF") & row_number() <= 10) %>%
  bind_rows(
    dataD %>%
      filter(subgroup == "CC") %>%
      slice_head(n = 10)
  ) %>%
  ungroup()

# 修改子图函数中的 Y 轴刻度设置
create_plot <- function(data, title, color) {
  data <- data %>% mutate(log_p_adjusted = log_p - 1.2) # 调整数据，使柱状图从1.2开始
  
  ggplot(data, aes(x = GOterm, y = log_p_adjusted, fill = subgroup)) +
    geom_col(width = 0.7, position = position_dodge()) + # 使用 geom_col 绘制柱状图
    scale_fill_manual(values = setNames(color, unique(data$subgroup))) +
    labs(title = title, x = NULL, y = "-log(p)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  # 倾斜角度改为45度
          axis.text.y = if (title == "BP") element_text(size = 10) else element_blank(),
          axis.title.y = if (title == "BP") element_text(size = 12) else element_blank(),
          plot.title = element_textbox_simple(size = 10, hjust = 0.5, color = "black",
                                              fill = "darkgrey", padding = margin(5, 10, 5, 10),
                                              linewidth = 0.5, box.color = "black"),
          legend.position = "none",
          plot.margin = margin(t = 5, r = 2, b = 5, l = 2),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          panel.grid.major = element_line(color = "lightgrey", linewidth = 0.5),
          panel.grid.minor = element_line(color = "lightgrey", linewidth = 0.25)) +
    scale_y_continuous(
      breaks = c(0, 0.8, 1.8, 2.8, 3.8),  # 设置实际数据的刻度
      labels = c("1.2", "2", "3", "4", "5")  # 设置显示的标签
    ) +
    coord_cartesian(ylim = c(0, 3.8))  # 使用 coord_cartesian 调整范围
}

# 创建各个子图
plot_BP <- create_plot(subset(dataD, subgroup == "BP"), "BP", "#66AFD8")
plot_CC <- create_plot(subset(dataD, subgroup == "CC"), "CC", "#D86A82")
plot_MF <- create_plot(subset(dataD, subgroup == "MF"), "MF", "#F4A582")

# 使用patchwork合并图像
final_plot <- (plot_BP | plot_CC | plot_MF) + 
  plot_layout(ncol = 3, widths = c(5, 4, 5)) +  # 调整宽度比例
  plot_annotation(title = "GO Enrichment BarPlot", caption = "GO Term", 
                  theme = theme(plot.title = element_text(size = 16, hjust = 0.5),
                                plot.caption = element_text(size = 12, hjust = 0.5),
                                plot.margin = margin(t = 20, b = 20)))  # 总标题和说明文字样式

# 打印最终的合并图
print(final_plot)

# 将 dataD 保存为 Excel 文件
write_xlsx(dataD, path = "E:/Work/8.27tu/8.27sixtu/-log10(p)zhuanhuan/T2D_bacon.GO.GO.xlsx")

# 保存最终图像为PDF格式
ggsave("E:\\Work\\10.3figure\\T2D_bacon.GO.GO.pdf", final_plot, width = 12, height = 8, units = "in", dpi = 600)
