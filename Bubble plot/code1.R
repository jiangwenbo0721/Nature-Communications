library(RColorBrewer)
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(patchwork)
library(cowplot)
setwd("F:/")
data_21<-read.csv("food_liking_cox_results1.csv")
data_22<-read.csv("food_liking_cox_results2.csv")
data_21$postive <- factor(data_21$postive, levels = c(labels = c("<0.0001","<0.001", "<0.01", "<0.05", ">0.05")))
data_21$negative <- factor(data_21$negative, levels = c(labels = c("<0.0001","<0.001", "<0.01", "<0.05", ">0.05")))
data_22$postive <- factor(data_22$postive, levels = c(labels = c("<0.0001","<0.001", "<0.01", "<0.05", ">0.05")))
data_22$negative <- factor(data_22$negative, levels = c(labels = c("<0.0001","<0.001", "<0.01", "<0.05", ">0.05")))
pdf(file = "heatmap1002.pdf", width = 16.5, height = 9.5)
group_colors1 = c( "Alcohol/Beverage" ="#E64835FF", "Coffee/Tea" ="#4DBBD5FF", "Dairy/Cheese" ="#7E6148FF",
                   "Dessert/Processed products" ="#3C5488FF","Flavour/Taste preference" ="#F39B7FFF" )
# 
p1 <- ggplot() +
    geom_point(data = subset(data_21, beta > 0), shape = 19, stroke = 0,
               aes(x = food_liking, y = disease, size = abs(beta), color = postive)) +
    geom_point(data = subset(data_21, beta < 0), shape = 21, stroke = 0.1,
               aes(x = food_liking, y = disease, size = abs(beta), fill = negative)) +
    scale_size_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    scale_fill_manual(values = c("darkblue", "#5C9BEB", "#94BBEC", "#C8D8ED", "#FEF7EF")) +
    scale_color_manual(values = c("darkred", "#E85450", "#F08E88", "#F8C3BC", "#FEF7EF")) +
    cowplot::theme_cowplot() + 
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 60, hjust = 1,size = 12,face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    facet_grid(. ~ category, scales = 'free', space = "free", margins = "vs") +
    theme(
        strip.text = element_text(size = 13, color = "white",face = "bold"),  
        panel.spacing = unit(0.5, "lines"), 
        #strip.background = element_rect(fill = group_colors1[levels(as.factor(data_21$category))])  
    )
p2 <- ggplot() +
    geom_point(data = subset(data_22, beta > 0), shape = 19, stroke = 0,
               aes(x = food_liking, y = disease, size = abs(beta), color = postive)) +
    geom_point(data = subset(data_22, beta < 0), shape = 21, stroke = 0.1,
               aes(x = food_liking, y = disease, size = abs(beta), fill = negative)) +
    scale_size_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    scale_fill_manual(values = c("darkblue", "#5C9BEB", "#94BBEC", "#C8D8ED", "#FEF7EF")) +
    scale_color_manual(values = c("darkred", "#E85450", "#F08E88", "#F8C3BC", "#FEF7EF")) +
    cowplot::theme_cowplot() + 
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 60, hjust = 1,size = 12,face = "bold"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          plot.title = element_text(hjust = 0.5)) +
    facet_grid(. ~ category, scales = 'free', space = "free", margins = "vs") +
    theme(
        strip.text = element_text(size = 13, color = "white",face = "bold"),  # 
        panel.spacing = unit(0.5, "lines"),  #
        # strip.background = element_rect(fill = group_colors2[levels(as.factor(data_22$category))]) 
    )

p3 <- p1 / p2
print(p3)

dev.off()
