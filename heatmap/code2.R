library("ggplotify")
library(patchwork)
library(tidyr)
df_liking<-read.csv("food_liking_cor.csv")
df_liking_p<-read.csv("food_liking_p.csv")
M <- pivot_wider(df_liking, 
                 names_from = food_liking,    
                 values_from = cor)
M1 <- pivot_wider(df_liking_p, 
                  names_from = food_liking,   
                  values_from = p)
df_prs_cor<-read.csv("food_prs_cor.csv")
df_prs_p<-read.csv("food_prs_p.csv")
df_wide <- pivot_wider(df_prs_cor, 
                       names_from = prs,    
                       values_from = cor)
df_widep <- pivot_wider(df_prs_p, 
                        names_from = prs,    
                        values_from = p)
df_wide <- as.data.frame(df_wide)
rownames(df_wide) <- df_wide$food_consumption_group  
df_wide <- df_wide[-1]  
df_wide <- as.matrix(df_wide)
df_widep <- as.data.frame(df_widep)
rownames(df_widep) <- df_widep$food_consumption_group  
df_widep <- df_widep[-1] 
df_widep <- as.matrix(df_widep)
M <- as.data.frame(M)
rownames(M) <- M$food_consumption_group 
M <- M[-1]  
M <- as.matrix(M)
M1 <- as.data.frame(M1)
rownames(M1) <- M1$food_consumption_group  
M1 <- M1[-1] 
M1 <- as.matrix(M1)
library(corrplot)
library(patchwork)
col1=colorRampPalette(c("darkblue", "#5C9BEB", "#94BBEC", "#C8D8ED",
                        "#FEF7EF", "#F8C3BC", "#F08E88", "#E85450","darkred"))
pdf(file ="heatmap-0913-2.pdf", width = 8, height =10)
corrplot(df_wide,method = 'square', 
         col = col1(10),
         type = "full",  
         is.corr = FALSE,
         diag = T,
         p.mat = df_widep,sig.level = c(0.001, 0.01, 0.05),pch.cex = 1,pch.col = 'black',
         tl.cex=1, tl.col="black",tl.srt = 45,tl.offset=0.5,
         addgrid.col = 'grey',
         cl.pos = "b",cl.length = 5,cl.ratio = 0.1
         
)
corrplot(M,method = 'square',
         col = col1(10),
         type = "full", 
         is.corr = FALSE,
         diag = T, 
         p.mat = M1,sig.level = c(0.001, 0.01, 0.05),pch.cex = 1,pch.col = 'black',
         tl.cex=1, tl.col="black",tl.srt = 45,tl.offset=0.5,
         addgrid.col = 'grey',
         cl.pos = "b",cl.length = 5,cl.ratio = 0.1
         
)
dev.off()
