library(networkD3)
library(dplyr)
 
# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("DEseq","DEseq","DEseq","DEseq","DEseq","DEseq",
           "UserGeneList","UserGeneList",
           "ExprsMatrix","ExprsMatrix","ExprsMatrix",
           "SampleData",
           "GeneList+Pval+logFC","GeneList+Pval+logFC","GeneList+Pval+logFC","GeneList+Pval+logFC",
           "Exprs","ColData","logFC","Pval","logFC","Pval","Exprs","ColData",
           "Exprs","ColData","Exprs","ColData","Exprs","ColData","logFC","Pval",
           "EnrichObj","EnrichObj","EnrichObj","EnrichObj","EnrichObj",
           "EnrichObj","EnrichObj","EnrichObj","Pval","logFC","EnrichObj","Pval","logFC",
           "GeneList","Pval","logFC"), 
  target=c("Exprs","ColData","EnrichObj","logFC","Pval","GeneList",
           "GeneList","EnrichObj",
           "Exprs","GeneList","EnrichObj",
           "ColData",
           "GeneList","Pval","logFC","EnrichObj",
           "PCA","PCA","Volcano","Volcano","MA","MA","Boxplot/Violin","Boxplot/Violin",
           "Heatmap","Heatmap","Cluster","Cluster","TopGene","TopGene","Karyoplot","Karyoplot",
           "KeggBarplot","KeggChorplot","KeggDotplot","KeggHeatmap","KeggNetplot",
           "GOBar","GODot","GOplotBar","GOplotBar","GOplotBar","GOCircle","GOCircle","GOCircle",
           "GSEA","GSEA","GSEA"), 
  value=rep(1,48)
  )
 
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
  as.character(links$target)) %>% unique()
)
 
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
 
# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, fontSize = 13)
p

