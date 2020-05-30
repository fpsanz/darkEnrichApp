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
df <-  read_xlsx("Proteostasis_Chaperone_arreglado.xlsx")
kk <- df %>% data.frame()
kkita <- customCnetGo(kk)
kkita %>% ggplotly()

str(kkita)

library(visNetwork)
nodes <- data.frame(id = 1:6, title = paste("node", 1:6), 
                    shape = c("dot", "square"),
                    size = 10:15, color = c("blue", "red"))
edges <- data.frame(from = 1:5, to = c(5, 4, 6, 3, 3))



edges <- separate_rows(kk, Term, genes, sep = ",")
edgesf <- edges[,c(1,7)]
names(edgesf) <- c("from","to")
edgesf$to <- gsub(" ", "", edgesf$to)

nd1 <- edges[,1:6]
nd1$P.DE <- nd1$P.DE + 2
nd2 <- as.data.frame(cbind( Term=edges$genes, Ont=NA, N=NA, DE=NA, P.DE=NA, go_id=NA))
names(nd2)
nd2$Term <- gsub(" ", "", nd2$Term)
nd2$P.DE <- 1
nd2$go_id <- nd2$Term
nd3 <- rbind(nd1,nd2)
nd3 <- dplyr::distinct(nd3)
nodesf <- data.frame(id=nd3$Term, label=nd3$Term, group=1, value=nd3$P.DE, stringsAsFactors = F)

visNetwork(nodesf, edgesf) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)



