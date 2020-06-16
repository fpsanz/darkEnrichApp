res <- readRDS("res.Rds")
    genesUp <- getSigUpregulated(res, 0.05, 0.5, "Mm" ) 
    genesDown <- getSigDownregulated(res, 0.05, -0.5, "Mm" ) 
    genesall <- rbind(genesUp, genesDown)
  kgg <- customKegg(genesall, species = "Mm" ) #"Mm"), species.KEGG = "mmu")
    kggDT <- kegg2DT(kgg, genesall)
   customCnetKegg(kgg, 1:10)
 nr <- 1:10; category=NULL; nPath=NULL; byDE=FALSE

 customCnetKegg <- function(kgg, category=NULL, nPath=NULL, byDE=FALSE, nr){
    if(! "ggraph" %in% .packages()) require("ggraph")
    if(! "igraph" %in% .packages()) require("igraph")
    if(! "dplyr" %in% .packages()) require("dplyr")
    if(! "tidyr" %in% .packages()) require("tidyr")
    kgg <- kgg[nr,]
    color_palette <- function(colors) colorRampPalette(colors)(n = 299)
    if(byDE){
        kgg <- kgg %>% arrange(-DE)
    }
    if(!is.null(category)){
        if(is.numeric(category)){
            tmp <- kgg[category,]
        } else{
            tmp <- kgg[kgg$Pathway %in% category,]
        }
    } else{
        tmp <- kgg
    }
    if(!is.null(nPath)){
        if(is.numeric(nPath)){
            tmp <- tmp[1:nPath, ]
        } else{ stop("nPath must be numeric or NULL")}
    }
    genesUp$dir <- "#ffa200"
    genesDown$dir <- "#91ebff"
    genesAll <- rbind(genesUp,genesDown)
    pval <- tmp[,c(1,4)]
    tmp <- tmp[,c(1,6)]
    tmp2 <- tmp %>%  separate_rows( genes, convert=TRUE)
    g <- igraph::graph.data.frame(tmp2, directed=FALSE)
    size <- tmp2 %>% dplyr::select(Pathway) %>% group_by(Pathway) %>%  summarise(n=n())
    size <- left_join(tmp, size, by=c("Pathway"))
    pval <- left_join(tmp, pval, by=c("Pathway"))
    size <- size$n
    pval <- pval$P.DE
    V(g)$size <- min(size)/2
    n <- dim(tmp)[1]
    V(g)$size[1:n] <- size
    V(g)$pval <- NA
    V(g)$pval[1:n] <- pval
    edge_layer <- geom_edge_link(alpha=.8, colour='darkgrey')
    fc <- V(g)$pval
    V(g)$color <- fc
    palette <- color_palette(c("red", "blue"))
    p <- ggraph(g, layout="stress", circular=FALSE) +
        edge_layer +
        geom_node_point( aes_(color=~pval, size=~size) ) +
        scale_size(range=c(3, 10), breaks=unique(round(seq(min(size), max(size), length.out=4)))) +
        theme_void()
    ##
    ptmp <- p$data
    ptmp2 <- left_join(ptmp, genesAll, by = c("name"="ENTREZID"))
    genesColor <- ptmp2$dir[!is.na(ptmp2$dir)]
    ##
    p <- p + geom_node_text(aes_(label=~name), data = p$data[1:n,]) +
        scale_color_gradientn(name = "pval", colors=palette, na.value = genesColor )
    return(p)
}

 
visData <- customVisNet(kgg, nTerm=1:10, kggDT,
                             up = genesUp$SYMBOL, down = genesDown$SYMBOL ) 

ledges <- data.frame(color = c("lightblue", "red"),
 label = c("reverse", "depends"), arrows =c("to", "from"))
lnodes <- data.frame(id = 1:10, group = c("B", "A"))
visNetwork(visData$nodes, visData$edges, background = "#ffffff") %>%
    visOptions(highlightNearest = list(enabled=TRUE, hover=TRUE),
                nodesIdSelection = TRUE) %>% 
    visGroups(groupname = "pval", color = "red") %>%
  visLegend(addEdges = ledges, useGroups = FALSE, addNodes = lnodes)

customVisNet <- function( enrich, kggDT, nTerm = NULL, up = NULL, down = NULL ){
    require(visNetwork)
    require(scales)
    enrich$genes <- kggDT$genes
    enrich <- enrich %>% arrange(P.DE)
    enrich <- enrich[nTerm, ]
    enrich$genes <- gsub(",", ";", enrich$genes)
    enrich$genes <- gsub(" ", "", enrich$genes)
    if( dim(enrich)[2]==8 ){
        names(enrich) <- c("Term","Ont","N","DE","P.DE","id","genes","level")
    } else if(dim(enrich)[2]==6){
        names(enrich) <- c("Term","N","DE","P.DE","id","genes")
    }
    edges <- separate_rows(enrich, Term, genes, sep=";")
    if( dim(enrich)[2]==8 ){
        edgesf <- edges[, c(1, 7)]
        edges <- edges %>% dplyr::select(-Ont)    
    } else if(dim(enrich)[2]==6){
        edgesf <- edges[, c(1, 6)]
    }
    names(edgesf) <- c("from", "to")
    edgesf$to <- gsub(" ", "", edgesf$to)
    nd1 <- edges[, 1:5]
    nd1$P.DE <- nd1$P.DE
    nd2 <- as.data.frame( 
    cbind( Term = edges$genes, N = NA, DE = NA, P.DE = NA, id = NA))
    nd2$Term <- gsub(" ", "", nd2$Term)
    nd2$P.DE <- NA
    nd2$id <- nd2$Term
    nd3 <- rbind(nd1, nd2)
    nd3 <- dplyr::distinct(nd3)
        
    nd3$title <- paste0( nd3$id, "<br>", "pval: ",
                         formatC(nd3$P.DE, format = "e", digits = 3),"<br>",
                         "DE: ", nd3$DE )
    nd3$DE <- ifelse( is.na(nd3$DE), 
                      min( as.numeric( nd3$DE[ !is.na( nd3$DE ) ] ) ),
                      nd3$DE )
    nd3$title <- gsub("<br>pval:   NA<br>DE: NA", "", nd3$title)
        
    pvalCol <- rescale(nd3$P.DE, to = c(0, 1))
    colores <- scales::seq_gradient_pal("red", "blue")(pvalCol)
    colores <- ifelse(is.na(colores), "#bbceed", colores)
        
    nodesf <- data.frame( id = nd3$Term, label = nd3$Term, group = 1,
            value = nd3$DE, color = colores, shadow = F, title = nd3$title,
            stringsAsFactors = F )
    if(!is.null(up) ){
      genesUp <- which(nodesf$id %in% up)
      nodesf$color[genesUp] <- "#ffa200"
      }
    if(!is.null(down)){
      genesDown <- which(nodesf$id %in% down)
      nodesf$color[genesDown] <- "#91ebff"
      }
    return(list(nodes = nodesf, edges = edgesf))
} 


require(colorspace)
require("grid")
grid.rect(x = unit(seq(0,0.5, length=100), "npc"), y = unit(0.8, "npc"),
          width = unit(0.5, "npc"), height = unit(0.05, "npc"), 
          just = "centre",
          gp = gpar(col = NA, fill =colorRampPalette(c("red", "yellow", "blue"), (100))))

grid.rect(x = unit(0,0.42, "npc"), y = unit(0.35, "npc"),
          width = unit(0.2, "npc"), height = unit(0.2, "npc"),
        gp=gpar(col="NA", fill = "red") )
popViewport()
