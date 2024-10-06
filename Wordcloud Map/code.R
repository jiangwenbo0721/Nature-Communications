###install
devtools::install_github("lchiffon/wordcloud2")

###library
library(tidyverse)            
library(openxlsx)             
library(wordcloud2)           



cname = c('HF-diet','T2D-bacon','T2D-letilsbean')
for(c in cname){
  cdata = read.xlsx('WordCloud.xlsx', sheet = c)
  
  cdata$pair_trait = c
  result <- cdata %>%
    group_by(pair_trait, trait_class2) %>%
    summarise(Count = n(), .groups = 'drop')
  
  res_1 = result[,2:3]
  shi = quantile(res_1$Count, probs = seq(0, 1, by = 0.1))
  res_1 <- res_1 %>%
    mutate(Color = case_when(
      Count > shi[10] ~ '#E64B35FF',
      Count > shi[9] ~ '#4DBBD5FF',
      Count > shi[6] ~ '#00A087FF',
      Count > shi[3] ~ '#3C5488FF',
      TRUE ~ '#F39B7FFF'      #other
    ))
  
  
  j00 = wordcloud2(res_1,size = 1,minSize = 2,
                   color = res_1$Color,
                   shape = 'pentagon',
                   rotateRatio = 0)
  
  htmlwidgets::saveWidget(j00, paste0('N-',c,'-cloud.html')) 
}


