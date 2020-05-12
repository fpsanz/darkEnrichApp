## 
library(msigdbr)
library(clusterProfiler)
library(fgsea)
library(GSEABase)
library(phenoTest)
library(org.Mm.eg.db)
library("GO.db")
m_df <- msigdbr(species = "Mus musculus")
View(m_df)

res <- readRDS("res.Rds")
st <- res$log2FoldChange
names(st) <- res$GeneName_Symbol
geneNames <- geneIdConverter( rownames(res) )
names(st) <- geneNames$ENTREZID

m_df <- msigdbr(species = "Mus musculus", subcategory = "CP:KEGG")
m_list <- m_df %>% split(x = .$gene_symbol, f = .$gs_name)
fgs <- fgsea(pathways = m_list, stats = st, nperm = 1000)

gseaRes <- gsea(st, gsets = m_list)
plot.gseaData(gseaRes, selGsets = "KEGG_ADHERENS_JUNCTION")
summary(gseaRes)

gsea.go1 <- gsea.go(st, species = "Mm", ontologies = "BP")
View(summary(gsea.go1))
plot.gseaData(gsea.go1, selGsets = "GO:0006805")

library(dplyr)
pathwayDataSet <- readRDS(paste0("./resources/","Mm","/GSEA/keggDataGSEA.Rds"))
kk <- pathwayDataSet %>% group_by(Id) %>% summarise(gene =  paste(GeneID, collapse = ",")) %>% as.data.frame()
rownames(kk) <- kk$Id
