goBarplot <- function(enrichGO=NULL, resGO=NULL, genes=NULL, category=NULL ){
    require(GOplot)
    go <- enrichGO
    res <- resGO
    go2 <- go %>% group_by(Ont) %>% sample_n(size = 30)
    goDT <- go2DT(go2, genes)
    # preparar tabla GO
    go2$genes <- goDT$genes
    go2 <- go2 %>% dplyr::select(Ont,go_id,Term,genes, P.DE)
    names(go2) <- c("Category","ID", "Term", "Genes", "adj_pval")
    #preparar tabla genelist
    names(res)
    res2 <- res %>% dplyr::select(GeneName_Symbol, log2FoldChange, padj)
    names(res2) <- c("ID","logFC","adj.P.Val")
    # crear objeto circ
    library(GOplot)
    circ <- circle_dat(go2, res2)
    GOBar(subset(circ, category=category))
}
goBarplot(enrichGO = go, resGO = res, genes= data, category = "MF")



library(tidyverse)
res <- readRDS("./res.Rds")
up <- getSigUpregulated(res, 0.05, 0.5, "Mm" )
down <- getSigDownregulated(res, 0.05, -0.5, "Mm" )
data <- rbind(up, down)

go <- customGO(data, species = "Mm")
go2 <- go %>% group_by(Ont) %>% sample_n(size = 30)
goDT <- go2DT(go2, data)

# preparar tabla GO
go2$genes <- goDT$genes
go2 <- go2 %>% dplyr::select(Ont,go_id,Term,genes, P.DE)
names(go2) <- c("Category","ID", "Term", "Genes", "adj_pval")

#preparar tabla genelist
names(res)
res2 <- res %>% dplyr::select(GeneName_Symbol, log2FoldChange, padj)
names(res2) <- c("ID","logFC","adj.P.Val")

# crear objeto circ
library(GOplot)
circ <- circle_dat(go2, res2)
GOBar(subset(circ, category="MF"))



circle <- function (data, title, nsub, rad1, rad2, table.legend = T, zsc.col, 
          lfc.col, label.size, label.fontface) 
{
    require(GOplot)
    xmax <- y1 <- zscore <- y2 <- ID <- logx <- logy2 <- logy <- logFC <- NULL
    if (missing(title))
        title <- ""
    if (missing(nsub))
        if (dim(data)[1] > 10)
            nsub <- 10
        else
            nsub <- dim(data)[1]
        if (missing(rad1))
            rad1 <- 2
        if (missing(rad2))
            rad2 <- 3
        if (missing(zsc.col))
            zsc.col <- c("red", "white", "blue")
        if (missing(lfc.col))
            lfc.col <- c("cornflowerblue", "firebrick1")
        else
            lfc.col <- rev(lfc.col)
        if (missing(label.size))
            label.size = 5
        if (missing(label.fontface))
            label.fontface = "bold"
        data$adj_pval <- -log(data$adj_pval, 10)
        suby <- data[!duplicated(data$term),]
        if (is.numeric(nsub) == T) {
            suby <- suby[1:nsub,]
        }
        else {
            if (strsplit(nsub[1], ":")[[1]][1] == "GO") {
                suby <- suby[suby$ID %in% nsub,]
            }
            else {
                suby <- suby[suby$term %in% nsub,]
            }
            nsub <- length(nsub)
        }
        N <- dim(suby)[1]
        r_pval <- round(range(suby$adj_pval), 0) + c(-2, 2)
        ymax <- c()
        for (i in 1:length(suby$adj_pval)) {
            val <- (suby$adj_pval[i] - r_pval[1]) / (r_pval[2] - r_pval[1])
            ymax <- c(ymax, val)
        }
        df <- data.frame(
                x = seq(0, 10 - (10 / N), length = N),
                xmax = rep(10 / N -
                               0.2, N),
                y1 = rep(rad1, N),
                y2 = rep(rad2, N),
                ymax = ymax,
                zscore = suby$zscore,
                ID = suby$ID
            )
        scount <- data[!duplicated(data$term), which(colnames(data) ==
                                                   "count")][1:nsub]
        idx_term <- which(!duplicated(data$term) == T)
        xm <- c()
        logs <- c()
        for (sc in 1:length(scount)) {
            idx <- c(idx_term[sc], idx_term[sc] + scount[sc] - 1)
            val <-
                stats::runif(scount[sc], df$x[sc] + 0.06, (df$x[sc] +
                                                               df$xmax[sc] - 0.06))
            xm <- c(xm, val)
            r_logFC <- round(range(data$logFC[idx[1]:idx[2]]), 0) +
                c(-1, 1)
            for (lfc in idx[1]:idx[2]) {
                val <- (data$logFC[lfc] - r_logFC[1]) / (r_logFC[2] -
                                                             r_logFC[1])
                logs <- c(logs, val)
            }
        }
        cols <- c()
        for (ys in 1:length(logs))
            cols <- c(cols, ifelse(data$logFC[ys] >
                                       0, "upregulated", "downregulated"))
        dfp <- data.frame(
                logx = xm,
                logy = logs,
                logFC = factor(cols),
                logy2 = rep(rad2, length(logs))
            )
        c <- ggplot() + geom_rect(
                data = df,
                aes(
                    xmin = x,
                    xmax = x +
                        xmax,
                    ymin = y1,
                    ymax = y1 + ymax,
                    fill = zscore
                ),
                colour = "black"
            ) +
            geom_rect(data = df,
                      aes(
                          xmin = x,
                          xmax = x + xmax,
                          ymin = y2,
                          ymax = y2 + 1
                      ),
                      fill = "gray70") + geom_rect(
                          data = df,
                          aes(
                              xmin = x,
                              xmax = x + xmax,
                              ymin = y2 + 0.5,
                              ymax = y2 +
                                  0.5
                          ),
                          colour = "white"
                      ) + geom_rect(
                          data = df,
                          aes(
                              xmin = x,
                              xmax = x + xmax,
                              ymin = y2 + 0.25,
                              ymax = y2 + 0.25
                          ),
                          colour = "white"
                      ) + geom_rect(
                          data = df,
                          aes(
                              xmin = x,
                              xmax = x + xmax,
                              ymin = y2 + 0.75,
                              ymax = y2 + 0.75
                          ),
                          colour = "white"
                      ) + geom_text( data = df,
                                     aes( x = x + (xmax /2), y = y2 + 1.5, label = ID, 
                                          angle = 360/(2*pi)*rev( pi/2 + seq( pi/14, 2*pi-pi/14, len=20)) ),
                          size = label.size,
                          fontface = label.fontface
                      ) +
            coord_polar() + labs(title = title) +
            ylim(1, rad2 + 1.6) + xlim(0, 10) + 
            GOplot:::theme_blank + scale_fill_gradient2("z-score",
                                                 space = "Lab",
                                                 low = zsc.col[3],
                                                 mid = zsc.col[2],
                                                 high = zsc.col[1],
                                                 guide = guide_colourbar(title.position = "top", title.hjust = 0.5),
                                                 breaks = c(min(df$zscore), max(df$zscore)),
                                                 labels = c("decreasing",
                                                                          "increasing")
                                                           ) + theme(
                                                               legend.position = "bottom",
                                                               legend.background = element_rect(fill = "transparent"),
                                                               legend.box = "horizontal",
                                                               legend.direction = "horizontal"
                                                           ) +
            geom_point(
                data = dfp,
                aes(x = logx, y = logy2 + logy),
                pch = 21,
                fill = "transparent",
                colour = "black",
                size = 3
            ) + geom_point(data = dfp,
                           aes(x = logx,
                               y = logy2 + logy, colour = logFC),
                           size = 2.5) + scale_colour_manual(values = lfc.col,
                                                             guide = guide_legend(title.position = "top", title.hjust = 0.5))
        if (table.legend) {
            table <- GOplot:::draw_table(suby)
            graphics::par(mar = c(0.1, 0.1, 0.1, 0.1))
            grid.arrange(c, table, ncol = 2)
        }
        else {
            c + theme(
                plot.background = element_rect(fill = "aliceblue"),
                panel.background = element_rect(fill = "white")
            )
        }
}


circle(circ, label.size = 3, nsub = 20, table.legend = TRUE)
library(ggplot2)
