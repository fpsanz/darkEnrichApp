customCnetGo <- function(gos, category=NULL, nTerm=NULL, byDE=FALSE, ont="BP"){
    if(! "ggraph" %in% .packages()) require("ggraph")
    if(! "igraph" %in% .packages()) require("igraph")
    if(! "dplyr" %in% .packages()) require("dplyr")
    if(! "tidyr" %in% .packages()) require("tidyr")
    gos <- gos[gos$Ont == ont, ]
    color_palette <- function(colors) colorRampPalette(colors)(n = 299)
    if(byDE){
        gos <- gos %>% arrange(-DE)
    }
    if(!is.null(category)){
        if(is.numeric(category)){
            tmp <- gos[category,]
        } else{
            tmp <- gos[gos$Pathway %in% category,]
        }
    } else{
        tmp <- gos
    }
    if(!is.null(nTerm)){
        if(is.numeric(nTerm)){
            tmp <- tmp[1:nTerm, ]
        } else{ stop("nTerm must be numeric or NULL")}
    }
    pval <- tmp[,c(1,5)]
    tmp <- tmp[,c(1,7)]
    tmp2 <- tmp %>% separate_rows( genes, convert=FALSE)
    g <- igraph::graph.data.frame(tmp2, directed=FALSE)
    size <- tmp2 %>% dplyr::select(Term) %>% group_by(Term) %>%  summarise(n=n())
    size <- left_join(tmp, size, by=c("Term"))
    pval <- left_join(tmp, pval, by=c("Term"))
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
    p <- p + geom_node_text(aes_(label=~name), data = p$data ) +
        scale_color_gradientn(name = "pval", colors=palette, na.value = "#E5C494")
    return(p)
}
library(readxl)
library(dplyr)
library(plotly)
df <-  read_xlsx("/datos/miriamCollection/Proteostasis_Chaperone_arreglado.xlsx")
kk <- df %>% data.frame()

library(visNetwork)

edges <- separate_rows(kk, Term, genes, sep = ",")
edgesf <- edges[,c(1,7)]
names(edgesf) <- c("from","to")
edgesf$to <- gsub(" ", "", edgesf$to)

nd1 <- edges[,1:6]
nd1$P.DE <- nd1$P.DE
nd2 <- as.data.frame(cbind( Term=edges$genes, Ont=NA, N=NA, DE=NA, P.DE=NA, go_id=NA))
#names(nd2)
nd2$Term <- gsub(" ", "", nd2$Term)
nd2$P.DE <- NA
nd2$go_id <- nd2$Term
nd3 <- rbind(nd1,nd2)
nd3 <- dplyr::distinct(nd3)

nd3$title <- paste0(nd3$go_id,"<br>","pval: ",formatC(nd3$P.DE, format="e",digits = 3),
                    "<br>","DE: ",nd3$DE)
nd3$DE <- ifelse( is.na(nd3$DE), min(as.numeric(nd3$DE[!is.na(nd3$DE)])), nd3$DE)
nd3$title <- gsub("<br>pval:   NA<br>DE: NA", "", nd3$title)
library(scales)
pvalCol <- rescale(nd3$P.DE, to=c(0,1) )
colores <- scales::seq_gradient_pal("red","blue")(pvalCol)
colores <- ifelse( is.na(colores), "#bbceed", colores)

nodesf <- data.frame(id=nd3$Term, label=nd3$Term, group=1, value=nd3$DE, 
                     color = colores, shadow=F, 
                     title = nd3$title,
                     stringsAsFactors = F)


visNetwork(nodesf, edgesf) %>%
    visOptions(highlightNearest = list(enabled=TRUE, hover=TRUE), nodesIdSelection = TRUE)

res <- readRDS("./res.Rds")
data <- getSigUpregulated(res, 0.05, 0.1, "Mm" )
kgg <- customKegg(data, species = "Mm" )
go <- customGO(data, species = "Mm")

enrich=kgg
customVisNet <- function( enrich, nTerm = 10 ){
    require(visNetwork)
    require(scales)
    enrich <- enrich %>% arrange(P.DE)
    enrich <- enrich[seq_len(nTerm), ]
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
    return(list(nodes = nodesf, edges = edgesf))
} 

visData <- customVisNet(go, nTerm=5)
visNetwork(visData$nodes, visData$edges) %>%
    visOptions(highlightNearest = list(enabled=TRUE, hover=TRUE), nodesIdSelection = TRUE)
