heatmapKeggLogFC <- function(kdt, res, nr){
    kdt <- kdt[nr, ]
    kk <- kdt %>% dplyr::select(Pathway, genes) %>% separate_rows(Pathway, genes, sep=",")
    kk$genes <- gsub(" ", "", kk$genes)
    resSig <- res[ which(res$GeneName_Symbol %in% kk$genes), ]
    kk2 <- resSig %>% dplyr::select(GeneName_Symbol, log2FoldChange, padj)
    kk3 <- left_join(kk, kk2, by = c("genes"="GeneName_Symbol"))
    kk3$padj <- format(kk3$padj, scientific = TRUE, digits = 3)
    yNum <- length(unique(kdt$Pathway))
    if(yNum <=35){ySize=12}else if(yNum>35 | yNum <=50){ySize=10}else{ySize=0}
    xNum <- length(unique(kdt$genes))
    if(xNum <=60){xSize=8}else if(xNum>60 | yNum <=80){xSize=7}else{xSize=0}
    
    kk3 %>% ggplot(aes_(~genes, ~Pathway)) + 
    geom_tile(aes_(fill = ~log2FoldChange, label= ~padj), color = 'black', size =0.2) +
    xlab(NULL) + ylab(NULL) +
    theme_minimal() +
    theme(panel.grid.major = element_line(colour = "gray88", size = 0.8),
          axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5, size=xSize))+
    scale_fill_gradient2(low="blue", mid = "gray88", high="red", name = "Log2FC")+
    # scale_fill_brewer(palette = "YlOrRd")
    # scale_fill_manual(values = getPalette(colourCount))+
    theme(text = element_text(size=ySize, angle=0), plot.margin = unit(c(15,25,15,15), "pt"))
}


res <- readRDS("res.Rds")
    genesUp <- getSigUpregulated(res, 0.05, 0.5, "Mm" ) 
    genesDown <- getSigDownregulated(res, 0.05, -0.5, "Mm" ) 
    genesall <- rbind(genesUp, genesDown)

resSig <- res[ which(res$GeneName_Symbol %in% genesall$SYMBOL), ]

    kgg <- customKegg(genesall, species = "Mm" ) #"Mm"), species.KEGG = "mmu")
    kggDT <- kegg2DT(kgg, genesall)
    names(kggDT) <- c("Pathway", "N"    ,   "DEG"    ,  "P.DE" ,   "genes" ,  "url")
    
    heatmapKegg(kggDT, 1:10 ) 
    
    kdt <- kggDT
    kk <- kdt %>% dplyr::select(Pathway, genes) %>% separate_rows(Pathway, genes, sep=",")
    kk$genes <- gsub(" ", "", kk$genes)
    resSig <- res[ which(res$GeneName_Symbol %in% kk$genes), ]
    kk2 <- resSig %>% dplyr::select(GeneName_Symbol, log2FoldChange, padj)
    kk3 <- left_join(kk, kk2, by = c("genes"="GeneName_Symbol"))


kk <- heatmapKeggLogFC(kdt, res, 1:10 ) 
    kk %>% ggplotly()
          
    yNum <- length(unique(kdt$Pathway))
    if(yNum <=35){ySize=12}else if(yNum>35 | yNum <=50){ySize=10}else{ySize=0}
    xNum <- length(unique(kdt$genes))
    if(xNum <=60){xSize=8}else if(xNum>60 | yNum <=80){xSize=7}else{xSize=0}
     
    kk3 %>% ggplot(aes_(~genes, ~Pathway)) + 
    geom_tile(aes_(fill = ~log2FoldChange), color = 'black', size =0.2) +
    xlab(NULL) + ylab(NULL) +
    theme_minimal() +
    theme(panel.grid.major = element_line(colour = "gray88", size = 0.8),
          axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5, size=xSize))+
     scale_fill_gradient2(low="blue", mid="white", high="red", name = "Log2FC")+
    # scale_fill_brewer(palette = "YlOrRd")
    #scale_fill_manual(values = getPalette(colourCount))+
    theme(text = element_text(size=ySize, angle=0), plot.margin = unit(c(15,25,15,15), "pt"))
    
     