setwd("F:/work/")
install.packages("openxlsx")
library(openxlsx)
bc <- read.xlsx("forest plot.xlsx", sheet = 1)  
library(stringr)
cl<-bc$`95%CI`
bc$low<-str_sub(cl,1,4)
bc$hi<-str_sub(cl,6,9)
library(grid)
library(forestploter)
bc$Outcome<-ifelse(!is.na(bc$`HR`), bc$Outcome, paste0(" ", bc$Outcome))
bc$se<- (log(as.numeric(bc$hi)) - log(as.numeric(bc$HR)))/1.96
bc$hi<-as.numeric(bc$hi)
bc$low<-as.numeric(bc$low)
bc$`HR(95% CI)` <- ifelse(is.na(bc$se), "",
                          sprintf("%.2f(%.2f, %.2f)", bc$HR,bc$low, bc$hi))
bc$` `<- paste(rep(" ", 20), collapse = " ")
tm <- forest_theme(base_size = 4,  
                   ci_pch = 19,   
                   ci_col = "black",   
                   ci_fill = "black",  
                   ci_alpha = 1,        
                   ci_lty = 1,            
                   ci_lwd = 0.8,         
                   ci_Theight = 0.15, 
                   refline_lwd = 1,       
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   vertline_lwd = 1,             
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   summary_fill = "yellow",      
                   summary_col = "#4575b4")

forest(bc[,c(1,8,9,4)],
       est = bc$HR,      
       lower = bc$low,     
       upper = bc$hi,      
       sizes = 0.35,          
       ci_column = 3,   
       ref_line = 1,
       xlim = c(0.5,2),
       ticks_at = c(0.5,1,1.5,2),
       theme = tm)

