library(limma)
library(DESeq2)
library(tidyverse)

kk <- readRDS("../darkEnrichApp/deseq.Rds")
colData(kk)
assay(kk) %>% head %>% View()
kkmatrix <- assay(kk) # matriz de cuentas

samples <- colnames(kkmatrix) 

definegroup <- rep(c("a","b","c"), each=length(colnames(kkmatrix))/3 )
definegender <- rep(c("m","f"), each = length(colnames(kkmatrix))/2)

sampleData <- data.frame(samples = samples, group = definegroup)

rownames(sampleData) <- sampleData$samples

kkdeseq <- DESeq2::DESeqDataSetFromMatrix(countData = kkmatrix, colData = DataFrame(sampleData), design = ~1)
design(kkdeseq) <- formula(~ group)

kkdelrt <- DESeq(kkdeseq, test = "LRT", reduced = ~1)
results(kkdelrt) %>% as.data.frame %>% head(2)
kkde <- DESeq(kkdeseq, test = "Wald")
kkres_ab <- results(kkde, contrast = c("group", "b", "a") )
kkres_ac <- results(kkde, contrast = c("group", "c", "a") )
kkres_bc <- results(kkde, contrast = c("group", "c", "b") )

kkres_ab %>% as.data.frame() %>% head(2)
kkres_ac %>% as.data.frame() %>% head(2)
kkres_bc %>% as.data.frame() %>% head(2)

resultsNames(kkde)
library(gtools)
combi <- combinations(3, 2, definegroup)
combi %>% as.data.frame 


