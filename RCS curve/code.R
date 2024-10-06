library(rms)
library(ggplot2)
library(dplyr)
library(broom)
library(infotheo)
library(foreign)
library(survival)
setwd("") 
aa <- readRDS("protein_Disease.RDS")

nbins<-4
age_4 <- discretize(aa$age,"equalfreq",nbins)
M4<- aa$age[order(aa$age)]
age4 <- as.data.frame(table(age_4))$Freq
colnames(age_4)[1]<-"age_4"
aa<-cbind(aa,age_4)
aa$age_4<-factor(aa$age_4)
aa$age_4_m<-ave(aa$age,aa$age_4,FUN = median)
aa <- aa %>%
  mutate(age_4 = case_when(
    age_4 %in% c("-3", "-1") ~ "9",
    age_4 == "1" ~ "1",
    age_4 == "2" ~ "2",
    age_4 == "3" ~ "3",
    age_4 == "4" ~ "4",
    is.na(age_4) ~ "9",
    TRUE ~ as.character(age_4)
  ))
aa$age_4<-as.factor(aa$age_4)
aa <- aa %>%
  mutate(race = case_when(
    race == "-3" ~ "2",
    race == "-1" ~ "2",
    race == "1" ~ "2",
    race == "2" ~ "2",
    race == "3" ~ "2",
    race == "4" ~ "2",
    race == "5" ~ "2",
    race == "6" ~ "2",
    race == "1001" ~ "1",
    race == "1002" ~ "2",
    race == "1003" ~ "2",
    race == "2001" ~ "2",
    race == "2002" ~ "2",
    race == "2003" ~ "2",
    race == "2004" ~ "2",
    race == "3001" ~ "2",
    race == "3002" ~ "2",
    race == "3003" ~ "2",
    race == "3004" ~ "2",
    race == "4001" ~ "2",
    race == "4002" ~ "2",
    race == "4003" ~ "2",
    is.na(race) ~ "9",
    TRUE ~ as.character(race)
  ))
aa$race<-as.factor(aa$race)
colnames(aa)[colnames(aa) == "education"] <- "edu"
aa <- aa %>%
  mutate(edu = case_when(
    edu %in% c("-3", "-7") ~ "9",
    edu == "1" ~ "1",
    edu == "2" ~ "2",
    edu == "6" ~ "6",
    edu == "3" ~ "3",
    edu == "4" ~ "4",
    edu == "5" ~ "5",
    is.na(edu) ~ "9",
    TRUE ~ as.character(edu)
  ))
aa$edu<-as.factor(aa$edu)
aa <- aa %>%
  mutate(sex = case_when(
    sex %in% c("-3", "-1") ~ "9",
    sex == "1" ~ "1",
    sex == "0" ~ "0",
    is.na(sex) ~ "9",
    TRUE ~ as.character(sex)
  ))
aa$sex<-as.factor(aa$sex)
colnames(aa)[colnames(aa) == "smoke"] <- "smoking"
aa <- aa %>%
  mutate(smoking = case_when(
    smoking %in% c("-3") ~ "9",
    smoking == "1" ~ "1",
    smoking == "2" ~ "2",
    smoking == "0" ~ "0",
    is.na(smoking) ~ "9",
    TRUE ~ as.character(smoking)
  ))
aa$smoking<-as.factor(aa$smoking)
colnames(aa)[colnames(aa) == "drink"] <- "drinking"
aa <- aa %>%
  mutate(drinking = case_when(
    drinking %in% c("-3") ~ "9",
    drinking == "1" ~ "1",
    drinking == "2" ~ "2",
    drinking == "0" ~ "0",
    is.na(drinking) ~ "9",
    TRUE ~ as.character(drinking)
  ))
aa$drinking<-as.factor(aa$drinking)
colnames(aa)[colnames(aa) == "TDI"] <- "Townsend.index"
num_bins <- 3
Townsend.index_bins <- cut(aa$Townsend.index, breaks = quantile(aa$Townsend.index, probs = seq(0, 1, 1/num_bins), na.rm = TRUE), labels = FALSE)
aa$Townsend.index_3 <- Townsend.index_bins
table(aa$Townsend.index_3)
unique(aa$Townsend.index_3)
aa <- aa %>%
  mutate(Townsend.index_3 = case_when(
    Townsend.index_3 %in% c("-3", "-1", "0") ~ "9",
    Townsend.index_3 == "1" ~ "1",
    Townsend.index_3 == "2" ~ "2",
    Townsend.index_3 == "3" ~ "3",
    is.na(Townsend.index_3) ~ "9",
    TRUE ~ as.character(Townsend.index_3)
  ))
aa$Townsend.index_3<-as.factor(aa$Townsend.index_3)
aa <- aa %>%
  mutate(bmi_3 = case_when(
    bmi < 25 ~ 1,
    bmi >= 25 & bmi < 30 ~ 2,
    bmi >= 30 ~ 3
  ))
sum(is.na(aa$bmi_3))
aa <- aa %>%
  mutate(bmi_3 = case_when(
    bmi_3 %in% c("-3", "-1", "-7") ~ "9",
    bmi_3 == "0" ~ "0",
    bmi_3 == "1" ~ "1",
    bmi_3 == "2" ~ "2",
    bmi_3 == "3" ~ "3",
    is.na(bmi_3) ~ "9",
    TRUE ~ as.character(bmi_3)
  ))
aa$bmi_3<-as.factor(aa$bmi_3)
aa <- aa %>%
  mutate(moderate_exe_per_week = case_when(
    moderate_exe_per_week %in% c("-3", "-1") ~ "9",
    moderate_exe_per_week == "0" ~ "0",
    moderate_exe_per_week == "1" ~ "1",
    moderate_exe_per_week == "2" ~ "2",
    moderate_exe_per_week == "3" ~ "3",
    moderate_exe_per_week == "4" ~ "4",
    moderate_exe_per_week == "5" ~ "5",
    moderate_exe_per_week == "6" ~ "6",
    moderate_exe_per_week == "7" ~ "7",
    is.na(moderate_exe_per_week) ~ "9",
    TRUE ~ as.character(moderate_exe_per_week)
  ))
aa$moderate_exe_per_week<-as.factor(aa$moderate_exe_per_week)
unique(aa$vigorous_exe_per_week)
aa <- aa %>%
  mutate(vigorous_exe_per_week = case_when(
    vigorous_exe_per_week %in% c("-3", "-1") ~ "9",
    vigorous_exe_per_week == "0" ~ "0",
    vigorous_exe_per_week == "1" ~ "1",
    vigorous_exe_per_week == "2" ~ "2",
    vigorous_exe_per_week == "3" ~ "3",
    vigorous_exe_per_week == "4" ~ "4",
    vigorous_exe_per_week == "5" ~ "5",
    vigorous_exe_per_week == "6" ~ "6",
    vigorous_exe_per_week == "7" ~ "7",
    is.na(vigorous_exe_per_week) ~ "9",
    TRUE ~ as.character(vigorous_exe_per_week)
  ))
aa$vigorous_exe_per_week<-as.factor(aa$vigorous_exe_per_week)


dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$SSC4D), ]

density_data <- density(filtered_data$SSC4D)
density_df <- data.frame(
  SSC4D = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(SSC4D, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)
p_value <- fit_anova["SSC4D", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, SSC4D, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$SSC4D, na.rm = TRUE)

p <- ggplot() +

  geom_area(data = density_df, aes(x = SSC4D, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = SSC4D, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  

  geom_line(data = HR, aes(x = SSC4D, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = SSC4D, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +

  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.2, 0.1))
  ) +

  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
 
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +

  labs(x = "SSC4D")

print(p)

ggsave("HF_SSC4D.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)



dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$UPB1), ]

density_data <- density(filtered_data$UPB1)
density_df <- data.frame(
  UPB1 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(UPB1, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)
fit_anova <- anova(fit)
p_value <- fit_anova["UPB1", "P"]


if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}
HR <- Predict(fit, UPB1, fun = exp, ref.zero = TRUE)


y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$UPB1, na.rm = TRUE)


p <- ggplot() +

  geom_area(data = density_df, aes(x = UPB1, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = UPB1, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  

  geom_line(data = HR, aes(x = UPB1, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = UPB1, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +

  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +

  labs(x = "UPB1")


print(p)

ggsave("HF_UPB1.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')


filtered_data <- subset(aa, HF_status == 1)


filtered_data <- filtered_data[complete.cases(filtered_data$SERPINE1), ]

density_data <- density(filtered_data$SERPINE1)
density_df <- data.frame(
  SERPINE1 = density_data$x,
  density = density_data$y
)


fit <- cph(Surv(HF_interval, HF_status) ~ rcs(SERPINE1, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)


fit_anova <- anova(fit)


p_value <- fit_anova["SERPINE1", "P"]


if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}


HR <- Predict(fit, SERPINE1, fun = exp, ref.zero = TRUE)


y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$SERPINE1, na.rm = TRUE)


p <- ggplot() +
 
  geom_area(data = density_df, aes(x = SERPINE1, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = SERPINE1, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  

  geom_line(data = HR, aes(x = SERPINE1, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = SERPINE1, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +

  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.5, 0.1))
  ) +
  
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +

  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
 
  labs(x = "SERPINE1")

print(p)

ggsave("HF_SERPINE1.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

# 筛选 T2D_status == 1 的数据
filtered_data <- subset(aa, HF_status == 1)

# 去除 SELE 列中的缺失值
filtered_data <- filtered_data[complete.cases(filtered_data$SELE), ]

# 计算密度值
density_data <- density(filtered_data$SELE)
density_df <- data.frame(
  SELE = density_data$x,
  density = density_data$y
)

# 创建 Cox 回归模型并生成 RCS 曲线数据
fit <- cph(Surv(HF_interval, HF_status) ~ rcs(SELE, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

# 使用 anova 提取模型结果并提取 SELE 的 P 值
fit_anova <- anova(fit)

# 提取 SELE 的 P 值（假设 'SELE' 对应的行是第1行，需根据实际调整行索引）
p_value <- fit_anova["SELE", "P"]

# 检查并格式化 P 值
if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

# 生成 HR (风险比) 曲线数据
HR <- Predict(fit, SELE, fun = exp, ref.zero = TRUE)

# 计算 HR 的范围
y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$SELE, na.rm = TRUE)

# 创建图形
p <- ggplot() +
  # 绘制密度曲线在底层
  geom_area(data = density_df, aes(x = SELE, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = SELE, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  # 绘制 HR 曲线在上层
  geom_line(data = HR, aes(x = SELE, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = SELE, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  # 设置 X 轴和 Y 轴的范围
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.6, 0.2))
  ) +
  # 使用经典主题样式
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  # 添加 P 值文本
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  # 添加 X 轴标签
  labs(x = "SELE")

# 显示图形
print(p)

ggsave("HF_SELE.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

# 筛选 T2D_status == 1 的数据
filtered_data <- subset(aa, HF_status == 1)

# 去除 PTGR1 列中的缺失值
filtered_data <- filtered_data[complete.cases(filtered_data$PTGR1), ]

# 计算密度值
density_data <- density(filtered_data$PTGR1)
density_df <- data.frame(
  PTGR1 = density_data$x,
  density = density_data$y
)

# 创建 Cox 回归模型并生成 RCS 曲线数据
fit <- cph(Surv(HF_interval, HF_status) ~ rcs(PTGR1, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

# 使用 anova 提取模型结果并提取 PTGR1 的 P 值
fit_anova <- anova(fit)

# 提取 PTGR1 的 P 值（假设 'PTGR1' 对应的行是第1行，需根据实际调整行索引）
p_value <- fit_anova["PTGR1", "P"]

# 检查并格式化 P 值
if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

# 生成 HR (风险比) 曲线数据
HR <- Predict(fit, PTGR1, fun = exp, ref.zero = TRUE)

# 计算 HR 的范围
y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$PTGR1, na.rm = TRUE)

# 创建图形
p <- ggplot() +
  # 绘制密度曲线在底层
  geom_area(data = density_df, aes(x = PTGR1, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = PTGR1, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  # 绘制 HR 曲线在上层
  geom_line(data = HR, aes(x = PTGR1, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = PTGR1, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  # 设置 X 轴和 Y 轴的范围
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  # 使用经典主题样式
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  # 添加 P 值文本
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  # 添加 X 轴标签
  labs(x = "PTGR1")

# 显示图形
print(p)

ggsave("HF_PTGR1.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$NIT2), ]

density_data <- density(filtered_data$NIT2)
density_df <- data.frame(
  NIT2 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(NIT2, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["NIT2", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, NIT2, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$NIT2, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = NIT2, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = NIT2, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = NIT2, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = NIT2, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.6, 0.2))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "NIT2")
print(p)
ggsave("HF_NIT2.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)
dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$MYDGF), ]

density_data <- density(filtered_data$MYDGF)
density_df <- data.frame(
  MYDGF = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(MYDGF, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["MYDGF", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, MYDGF, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$MYDGF, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = MYDGF, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = MYDGF, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = MYDGF, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = MYDGF, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "MYDGF")
print(p)
ggsave("HF_MYDGF.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$MECR), ]

density_data <- density(filtered_data$MECR)
density_df <- data.frame(
  MECR = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(MECR, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["MECR", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, MECR, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$MECR, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = MECR, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = MECR, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = MECR, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = MECR, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "MECR")
print(p)
ggsave("HF_MECR.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$LONP1), ]

density_data <- density(filtered_data$LONP1)
density_df <- data.frame(
  LONP1 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(LONP1, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["LONP1", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, LONP1, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$LONP1, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = LONP1, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = LONP1, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = LONP1, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = LONP1, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "LONP1")
print(p)
ggsave("HF_LONP1.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$LEP), ]

density_data <- density(filtered_data$LEP)
density_df <- data.frame(
  LEP = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(LEP, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["LEP", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, LEP, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$LEP, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = LEP, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = LEP, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = LEP, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = LEP, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.3, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "LEP")
print(p)
ggsave("HF_LEP.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$KHK), ]

density_data <- density(filtered_data$KHK)
density_df <- data.frame(
  KHK = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(KHK, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["KHK", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, KHK, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$KHK, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = KHK, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = KHK, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = KHK, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = KHK, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.6, 0.2))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "KHK")
print(p)
ggsave("HF_KHK.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$IGSF9), ]

density_data <- density(filtered_data$IGSF9)
density_df <- data.frame(
  IGSF9 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(IGSF9, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["IGSF9", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, IGSF9, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$IGSF9, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = IGSF9, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = IGSF9, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = IGSF9, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = IGSF9, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "IGSF9")
print(p)
ggsave("HF_IGSF9.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$IGFBP2), ]

density_data <- density(filtered_data$IGFBP2)
density_df <- data.frame(
  IGFBP2 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(IGFBP2, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["IGFBP2", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, IGFBP2, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$IGFBP2, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = IGFBP2, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = IGFBP2, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = IGFBP2, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = IGFBP2, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "IGFBP2")
print(p)
ggsave("HF_IGFBP2.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$IGFBP1), ]

density_data <- density(filtered_data$IGFBP1)
density_df <- data.frame(
  IGFBP1 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(IGFBP1, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["IGFBP1", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, IGFBP1, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$IGFBP1, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = IGFBP1, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = IGFBP1, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = IGFBP1, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = IGFBP1, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.2, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "IGFBP1")
print(p)
ggsave("HF_IGFBP1.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$HPSE), ]

density_data <- density(filtered_data$HPSE)
density_df <- data.frame(
  HPSE = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(HPSE, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["HPSE", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, HPSE, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$HPSE, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = HPSE, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = HPSE, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = HPSE, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = HPSE, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "HPSE")
print(p)
ggsave("HF_HPSE.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$GUSB), ]

density_data <- density(filtered_data$GUSB)
density_df <- data.frame(
  GUSB = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(GUSB, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["GUSB", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, GUSB, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$GUSB, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = GUSB, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = GUSB, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = GUSB, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = GUSB, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.2))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "GUSB")
print(p)
ggsave("HF_GUSB.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$GSTA1), ]

density_data <- density(filtered_data$GSTA1)
density_df <- data.frame(
  GSTA1 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(GSTA1, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["GSTA1", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, GSTA1, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$GSTA1, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = GSTA1, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = GSTA1, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = GSTA1, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = GSTA1, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "GSTA1")
print(p)
ggsave("HF_GSTA1.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$GP2), ]

density_data <- density(filtered_data$GP2)
density_df <- data.frame(
  GP2 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(GP2, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["GP2", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, GP2, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$GP2, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = GP2, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = GP2, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = GP2, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = GP2, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "GP2")
print(p)
ggsave("HF_GP2.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$GHRL), ]

density_data <- density(filtered_data$GHRL)
density_df <- data.frame(
  GHRL = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(GHRL, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["GHRL", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, GHRL, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$GHRL, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = GHRL, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = GHRL, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = GHRL, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = GHRL, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "GHRL")
print(p)
ggsave("HF_GHRL.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$FSHB), ]

density_data <- density(filtered_data$FSHB)
density_df <- data.frame(
  FSHB = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(FSHB, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["FSHB", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, FSHB, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$FSHB, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = FSHB, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = FSHB, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = FSHB, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = FSHB, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.6, 0.2))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "FSHB")
print(p)
ggsave("HF_FSHB.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$FCAMR), ]

density_data <- density(filtered_data$FCAMR)
density_df <- data.frame(
  FCAMR = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(FCAMR, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["FCAMR", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, FCAMR, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$FCAMR, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = FCAMR, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = FCAMR, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = FCAMR, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = FCAMR, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "FCAMR")
print(p)
ggsave("HF_FCAMR.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$FABP4), ]

density_data <- density(filtered_data$FABP4)
density_df <- data.frame(
  FABP4 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(FABP4, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["FABP4", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, FABP4, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$FABP4, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = FABP4, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = FABP4, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = FABP4, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = FABP4, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "FABP4")
print(p)
ggsave("HF_FABP4.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$ECHS1), ]

density_data <- density(filtered_data$ECHS1)
density_df <- data.frame(
  ECHS1 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(ECHS1, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["ECHS1", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, ECHS1, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$ECHS1, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = ECHS1, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = ECHS1, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = ECHS1, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = ECHS1, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "ECHS1")
print(p)
ggsave("HF_ECHS1.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$CNST), ]

density_data <- density(filtered_data$CNST)
density_df <- data.frame(
  CNST = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(CNST, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["CNST", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, CNST, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$CNST, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = CNST, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = CNST, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = CNST, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = CNST, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.3, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "CNST")
print(p)
ggsave("HF_CNST.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$CKB), ]

density_data <- density(filtered_data$CKB)
density_df <- data.frame(
  CKB = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(CKB, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["CKB", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, CKB, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$CKB, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = CKB, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = CKB, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = CKB, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = CKB, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.5, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "CKB")
print(p)
ggsave("HF_CKB.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$CES1), ]

density_data <- density(filtered_data$CES1)
density_df <- data.frame(
  CES1 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(CES1, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["CES1", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, CES1, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$CES1, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = CES1, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = CES1, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = CES1, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = CES1, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "CES1")
print(p)
ggsave("HF_CES1.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$CCL5), ]

density_data <- density(filtered_data$CCL5)
density_df <- data.frame(
  CCL5 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(CCL5, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["CCL5", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, CCL5, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$CCL5, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = CCL5, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = CCL5, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = CCL5, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = CCL5, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.3, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "CCL5")
print(p)
ggsave("HF_CCL5.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)
dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$BPIFB2), ]

density_data <- density(filtered_data$BPIFB2)
density_df <- data.frame(
  BPIFB2 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(BPIFB2, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["BPIFB2", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, BPIFB2, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$BPIFB2, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = BPIFB2, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = BPIFB2, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = BPIFB2, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = BPIFB2, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.6, 0.2))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "BPIFB2")
print(p)
ggsave("HF_BPIFB2.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, HF_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$BPIFB1), ]

density_data <- density(filtered_data$BPIFB1)
density_df <- data.frame(
  BPIFB1 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(HF_interval, HF_status) ~ rcs(BPIFB1, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["BPIFB1", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, BPIFB1, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$BPIFB1, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = BPIFB1, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = BPIFB1, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = BPIFB1, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = BPIFB1, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.5, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "BPIFB1")
print(p)
ggsave("HF_BPIFB1.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, T2D_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$SSC4D), ]

density_data <- density(filtered_data$SSC4D)
density_df <- data.frame(
  SSC4D = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(T2D_interval, T2D_status) ~ rcs(SSC4D, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["SSC4D", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, SSC4D, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$SSC4D, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = SSC4D, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = SSC4D, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = SSC4D, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = SSC4D, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.25, 0.05))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "SSC4D")
print(p)
ggsave("T2D_SSC4D.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, T2D_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$LPL), ]

density_data <- density(filtered_data$LPL)
density_df <- data.frame(
  LPL = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(T2D_interval, T2D_status) ~ rcs(LPL, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["LPL", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, LPL, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$LPL, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = LPL, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = LPL, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = LPL, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = LPL, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.6, 0.2))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "LPL")
print(p)
ggsave("T2D_LPL.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)
dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, T2D_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$GH1), ]

density_data <- density(filtered_data$GH1)
density_df <- data.frame(
  GH1 = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(T2D_interval, T2D_status) ~ rcs(GH1, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["GH1", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, GH1, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$GH1, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = GH1, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = GH1, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = GH1, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = GH1, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.15, 0.05))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "GH1")
print(p)
ggsave("T2D_GH1.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)
dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, T2D_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$FSHB), ]

density_data <- density(filtered_data$FSHB)
density_df <- data.frame(
  FSHB = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(T2D_interval, T2D_status) ~ rcs(FSHB, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["FSHB", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, FSHB, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$FSHB, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = FSHB, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = FSHB, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = FSHB, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = FSHB, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.6, 0.2))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "FSHB")
print(p)
ggsave("T2D_FSHB.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)
dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, CVD_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$SSC4D), ]

density_data <- density(filtered_data$SSC4D)
density_df <- data.frame(
  SSC4D = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(CVD_interval, CVD_status) ~ rcs(SSC4D, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["SSC4D", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, SSC4D, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$SSC4D, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = SSC4D, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = SSC4D, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = SSC4D, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = SSC4D, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.2, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "SSC4D")
print(p)
ggsave("CVD_SSC4D.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

dd <- datadist(aa)
options(datadist = 'dd')

filtered_data <- subset(aa, CVD_status == 1)

filtered_data <- filtered_data[complete.cases(filtered_data$BGLAP), ]

density_data <- density(filtered_data$BGLAP)
density_df <- data.frame(
  BGLAP = density_data$x,
  density = density_data$y
)

fit <- cph(Surv(CVD_interval, CVD_status) ~ rcs(BGLAP, 4)+age_4 + sex + race + smoking + edu + drinking + 
             Townsend.index_3 + moderate_exe_per_week + bmi_3+DBP
           +SBP+Glucose+HDL+LDL+Total_Cholesterol+Triglycerides, data = aa)

fit_anova <- anova(fit)

p_value <- fit_anova["BGLAP", "P"]

if (p_value < 0.001) {
  p_value_text <- "P for linear < 0.001"
} else {
  p_value_text <- paste("P for linear =", format(p_value, digits = 3))
}

HR <- Predict(fit, BGLAP, fun = exp, ref.zero = TRUE)

y_range <- range(HR$yhat, HR$lower, HR$upper, na.rm = TRUE)
x_range <- range(HR$BGLAP, na.rm = TRUE)

p <- ggplot() +
  geom_area(data = density_df, aes(x = BGLAP, y = density * (max(y_range) / max(density_df$density))), fill = "#ADD8E6", alpha = 0.7) +  
  geom_line(data = density_df, aes(x = BGLAP, y = density * (max(y_range) / max(density_df$density))), color = "#0073C2", size = 1) +  
  geom_line(data = HR, aes(x = BGLAP, y = yhat), linetype = 1, linewidth = 1, alpha = 0.9, color = '#EB5E2B') +  
  geom_ribbon(data = HR, aes(x = BGLAP, ymin = lower, ymax = upper), alpha = 0.3, fill = '#F1948A' ) +  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(limits = x_range) + 
  scale_y_continuous(
    name = "HR (95% CI)",
    breaks = seq(0, max(y_range)),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * (max(density_df$density) / max(y_range)), name = "Density", breaks = seq(0, 0.4, 0.1))
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  annotate("text", x = max(x_range) * 0.7, y = max(y_range) * 0.9, label = p_value_text, size = 5, color = "black") +
  labs(x = "BGLAP")
print(p)
ggsave("CVD_BGLAP.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)
