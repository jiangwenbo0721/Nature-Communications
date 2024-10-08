rm(list=ls())
gc()
library(NetWeaver)
library(ggplot2)
library(qqman)
inputFile = c("T2D_bacon","T2D_letilsbeans")
data(ucsc.hg38.cytoband)
build=38
cytoband <- force(ucsc.hg38.cytoband)
GenomicRiskLoci_path='GenomicRiskLoci/'
col_path="col_path/"
col_result = data.table::fread(paste0(col_path,"/GenomicRiskLoci_T2D.txt"))
all_Region <- c()
for (i in 1:nrow(col_result)) {
  Region <- as.character(subset(cytoband,Chr==paste0("chr",col_result[i,]$chr)&
                                  Start <= col_result[i,]$pos & End >= col_result[i,]$pos )$Band)
  all_Region <- c(all_Region,Region)
}

col_result$Region <- paste0(col_result$chr,all_Region)
col_result$Nearest_genes <- "NA"
allmerge <- data.frame()
for (phen in inputFile) {
  map_gene <- data.table::fread(paste0(GenomicRiskLoci_path,phen,"/genes.txt"))
  cphen = strsplit(phen,'_')[[1]][2] 
  col_result_phen <- subset(col_result,phen2==cphen) 
  cmerge <- merge(col_result_phen,map_gene,by="GenomicLocus")
  allmerge <- rbind(allmerge,cmerge)
}
allmerge$Nearest_genes <- allmerge$symbol
alldata <- data.frame()
for (phen in inputFile) {
  map_gene <- data.table::fread(paste0(GenomicRiskLoci_path,phen,"/genes.txt"))
  cphen = strsplit(phen,'_')[[1]][2] 
  col_result_phen <- subset(col_result,phen2==cphen)

  for (i in col_result_phen$GenomicLocus) {
    col_result_phen[col_result_phen$GenomicLocus==i,]$Nearest_genes <-
      paste0(map_gene[map_gene$GenomicLocus==i,]$symbol,collapse = " ")
  }
  alldata <- rbind(col_result_phen,alldata)
}
data.table::fwrite(allmerge,
                   file = "plot_circular_diagram_T2D.txt",
                   na = "NA",
                   quote = FALSE,
                   sep = "\t")



library(webshot2)
library(networkD3)
library(htmlwidgets)
cdata <- data.table::fread("plot_circular_diagram_T2D.txt")
df <- data.frame(
  Level1 = cdata$phen1,
  Level2 = cdata$phen2, 
  Level3 = cdata$Region,
  Level4 = cdata$symbol
)
create_hierarchy <- function(df, levels) {
  if (length(levels) == 1) {
    csj = unique(df[[levels]])
    return( lapply(csj, function(s) list(name = s)) ) 
  }
  
  lapply(unique(df[[levels[1]]]), function(x) {
    sub_df <- df[df[[levels[1]]] == x, ]
    list(name = x, children = create_hierarchy(sub_df, levels[-1]))
  })
}

hierarchy <- create_hierarchy(df, colnames(df))

mcol = c("#4197d8","#f8c120","#413496","#495226","#d60b6f",
         "#e66519","#d581b7","#83d3ad","#7c162c","#26755d")

print(hierarchy)
p=radialNetwork(List = hierarchy[[1]], fontSize = 12, opacity = 0.8, margin=0, 
                linkColour = 'grey',  nodeStroke = '#4197d8', textColour = 'black'
)
p

saveWidget(p, file="01-circus_T2D.html")
webshot("01-circus_T2D.html", "circus_T2D.pdf")



library(Fast2TWAS)
library(NetWeaver)
library(ggplot2)
library(qqman)
inputFile = c("HF_diet-fizzy-drink")
data(ucsc.hg38.cytoband)

build=38
cytoband <- force(ucsc.hg38.cytoband)

GenomicRiskLoci_path='GenomicRiskLoci/'
col_result = data.table::fread(paste0(col_path,"/GenomicRiskLoci_HF.txt"))

all_Region <- c()
for (i in 1:nrow(col_result)) {
  Region <- as.character(subset(cytoband,Chr==paste0("chr",col_result[i,]$chr)&
                                  Start <= col_result[i,]$pos & End >= col_result[i,]$pos )$Band)
  all_Region <- c(all_Region,Region)
}
col_result$Region <- paste0(col_result$chr,all_Region)
col_result$Nearest_genes <- "NA"

allmerge <- data.frame()
for (phen in inputFile) {
  map_gene <- data.table::fread(paste0(GenomicRiskLoci_path,phen,"/genes.txt"))
  cphen = strsplit(phen,'_')[[1]][2] 
  col_result_phen <- subset(col_result,phen2==cphen)
  cmerge <- merge(col_result_phen,map_gene,by="GenomicLocus")
  allmerge <- rbind(allmerge,cmerge)
}
allmerge$Nearest_genes <- allmerge$symbol

alldata <- data.frame()
for (phen in inputFile) {
  map_gene <- data.table::fread(paste0(GenomicRiskLoci_path,phen,"/genes.txt"))
  cphen = strsplit(phen,'_')[[1]][2] 
  col_result_phen <- subset(col_result,phen2==cphen)
  for (i in col_result_phen$GenomicLocus) {
    col_result_phen[col_result_phen$GenomicLocus==i,]$Nearest_genes <-
      paste0(map_gene[map_gene$GenomicLocus==i,]$symbol,collapse = " ")
  }
  alldata <- rbind(col_result_phen,alldata)
}

data.table::fwrite(allmerge,
                   file = "plot_circular_diagram_HF.txt",
                   na = "NA",
                   quote = FALSE,
                   sep = "\t")


library(webshot2)
library(networkD3)
library(htmlwidgets)
cdata <- data.table::fread("plot_circular_diagram_HF.txt")

df <- data.frame(
  Level1 = cdata$phen1,
  Level2 = cdata$phen2,
  Level3 = cdata$Region,
  Level4 = cdata$symbol
)


create_hierarchy <- function(df, levels) {
  if (length(levels) == 1) {
    csj = unique(df[[levels]]) 
    return( lapply(csj, function(s) list(name = s)) ) 
  }
  
  lapply(unique(df[[levels[1]]]), function(x) {
    sub_df <- df[df[[levels[1]]] == x, ]
    list(name = x, children = create_hierarchy(sub_df, levels[-1]))
  })
}


hierarchy <- create_hierarchy(df, colnames(df))

print(hierarchy)
p=radialNetwork(List = hierarchy[[1]], fontSize = 12, opacity = 0.8, margin=0, 
                linkColour = 'grey',  nodeStroke = '#7c162c', textColour = 'black'
)
p

saveWidget(p, file="01-circus_HF.html")
webshot("01-circus_HF.html", "circus_HF.pdf")

