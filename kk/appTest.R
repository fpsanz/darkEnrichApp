library(DESeq2)
library(dplyr)
library(ggplot2)
library(limma)
library(plotly)
dds <- readRDS("deseq.Rds")

res <- as.data.frame(lfcShrink(dds, coef=2, type="apeglm", res = results(dds)))
        conversion <- geneIdConverter(rownames(res), "Mm" )
        res$baseMean <- round(res$baseMean,4)
        res$lfcSE <- round(res$lfcSE,4)
        res$log2FoldChange <- round(res$log2FoldChange,4)
        res <- cbind(`Description`=conversion$description, res)
        res <- cbind(`GeneName_Symbol`=conversion$consensus, res)
        res <-  res %>% dplyr::select(-c(pvalue))
        if("Mm" == "Mm" ){spc = "Mus_musculus"} else {spc = "Homo_sapiens"}
        links = paste0("<a href='http://www.ensembl.org/",spc,"/Gene/Summary?db=core;g=",
                       rownames(res),"' target='_blank'>",rownames(res),"</a>")
        res <- cbind(`GeneName_Ensembl`= links, res)
    genesUp <- getSigUpregulated(res, 0.05, 0.5, "Mm" ) 
    genesDown <- getSigDownregulated(res, 0.05, -0.5, "Mm" ) 
    genesall <- rbind(genesUp, genesDown)
    
    kgg <- customKegg(genesall, species = "Mm" ) 
# chordPlot(kgg[1:10, ], nRows = length(1:10), orderby = "P.DE")
#     
# 
# variables <- "AAV"
#     topGenes <- rownames(res)[order(res$padj)][1:6]
#     topSymbol <- as.character(res$GeneName_Symbol)[order(res$padj)][1:6]
#     z <- lapply(topGenes, function(x) plotCounts(dds=dds, gene=x,
#                                                  res=res, intgroup = variables,
#                                                  returnData = TRUE))
#     for(i in 1:6) z[[i]]$gene <- rep(topGenes[i], nrow(colData(dds)))
#     z <- do.call(rbind, z)
#     z$symbol <- rep(topSymbol, each =(nrow(z)/6) ) 
#     ggplot(z, aes_(as.name(variables) , ~count, colour = as.name(variables) )) + 
#       scale_y_log10() +
#       geom_point(position = position_jitter(width = 0.1, height = 0), size = 2) +
#       facet_wrap(~symbol) + scale_color_manual( values = c(1:12)) +
#       ggtitle("Top 6 most significant gene")

# 
#         CustomVolcano(toptable[1:5000,], lab = toptable$GeneName_Symbol[1:5000], 
#                   x = 'log2FoldChange',
#                   y = 'padj',
#                   pCutoff = 0.05,
#                   FCcutoffUP = 1.5,
#                   FCcutoffDOWN = -1.5,
#                   xlim = c(-8, 8),
#                   col = c("red", "blue", "green", "gray")
#                   )

    # try(rgl.close(), silent = TRUE)
    # #rgl.open(useNULL = TRUE) 
    # x = d$PC1; y = d$PC2; z = d$PC3
    # spheres3d(x,y,x, radius = 2)
    # # plot3d(x,y,x, size = 2, type="s", col = (d$labels),
    # #        box=FALSE, axes=FALSE, xlab = names(d)[1],
    # #        ylab=names(d)[2], names(d)[3])
    # rgl.bbox(color=c("#333377","black"), emission="#333377",
    #      specular="#3333FF", shininess=5, alpha=0.8, nticks = 3 ) 
    # #bg3d(sphere = FALSE, fogtype = "none", color = "#dadee3" )
    # #rgl.bbox(xlen=0, ylen=0, zlen=0)
    # rgl.lines(c(min(x), max(x)), c(0, 0), c(0, 0), color = "black")
    # rgl.lines(c(0, 0), c(min(y),max(y)), c(0, 0), color = "black")
    # rgl.lines(c(0, 0), c(0, 0), c(min(z),max(z)), color = "black")
    # rglwidget()
    
    # df <- data.frame(rutas = c("a","b","c"), colores = c("red", "blue", "gray"),
    #                  xmin = rep(-Inf, 3), xmax = rep(0.05, 3), 
    #                  ymin = c(0.05, 0.1, 0.15), 
    #                  ymax= c(0.1, 0.15, 0.20) )
    # df %>% ggplot() + 
    #     geom_rect(aes(xmin = xmin, xmax=xmax, ymin=ymin, ymax = ymax, fill = colores)) + 
    #     geom_text(aes(x = xmax+0.05, y = ymin+0.05,  label = rutas)) + 
    #     xlim(0,0.5) + ylim(0,0.5) + theme_minimal() + theme(legend.position = "none") +
    #     theme(panel.grid.major = element_blank()) + 
    #     theme(panel.grid.minor = element_blank()) +
    #     theme(axis.text = element_blank()) +
    #     theme(axis.title = element_blank())
    # 
    # plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    # legend("topleft", legend = df$rutas, pch=16, pt.cex = 3, cex = 1.5, bty="n", col = df$colores)
    # 
    # 
    ## TOP 1 pruebas ########################33
#     variables <- c("AAV")
#     gene ="ENSMUSG00000053113"
#   z <- plotCounts(dds = dds, gene = gene, returnData = TRUE, intgroup = variables[1] )
#     symbol = as.character(res$GeneName_Symbol[rownames(res)==gene])
#     z[[variables[1]]] <- as.character(z[[variables[1]]] )
#     z <- z %>% group_by(!!as.name(variables[1])) %>%
#         mutate(mean = round(mean(count),2), sem = round(sd(count)/sqrt(n()),3 ), n = n() ) %>% 
#         mutate(text = paste0("Mean: ",mean,"\n","SEM: ",sem))
#     p <- z %>% ggplot(aes_(as.name(variables[1]), ~count, colour = as.name(variables[1] ),
#                            text =  ~text)  ) +
#       scale_y_log10() +
#       geom_point(position = position_jitter(width = 0.1, height = 0), size = 2)+
#         scale_color_manual( values = c("red","blue") )+
#         ggtitle(symbol )
# p %>% ggplotly(tooltip = c("x","y","text") )
# 
# texto <- as.data.frame(unique(z[ ,c(variables[1],"text") ] ))
# txt <- paste0(apply(texto, 1, function(x){x} ), collapse = "\n")
    #### TOP 6 pruebas ###################################################
    # topGenes <- rownames(res)[order(res$padj)][1:6]
    # topSymbol <- as.character(res$GeneName_Symbol)[order(res$padj)][1:6]
    # z <- lapply(topGenes, function(x) plotCounts(dds=dds, gene=x,
    #                                              res=res, intgroup = as.character(variables[1]),
    #                                              returnData = TRUE))
    # 
    # z <- lapply(z, function(x){x %>% group_by(!!as.name(variables[1])) %>%
    #     mutate(mean = round(mean(count),2), sem = round(sd(count)/sqrt(n()),3 ), n = n() ) %>% 
    #     mutate(text = paste0("Mean: ",mean,"\n","SEM: ",sem)) } )
    # 
    # z <- do.call(rbind, z)
    # z$symbol <- rep(topSymbol, each =(nrow(z)/6) ) 
    # z[[variables[1]]] <- as.factor(z[[variables[1]]])
    # 
    # z <- z %>% arrange(symbol, !!as.name(variables[1]))
    # p <- ggplot(z, aes_(as.name(variables), ~count, colour = as.name(variables), text=~text ) ) + 
    #   scale_y_log10() +
    #   geom_point(position = position_jitter(width = 0.1, height = 0), size = 1) +
    #   facet_wrap(~symbol) + scale_color_manual( values = c("red","black") ) +
    #   ggtitle("Expression of top 6 most significant genes")
    # p %>% ggplotly(tooltip = c("x","y", "text"))
## Arreglar volcano plot ################
    #     res$GeneName_Symbol <- as.character(res$GeneName_Symbol)
    # p <- CustomVolcano(res, lab = res$GeneName_Symbol, 
    #               x = 'log2FoldChange',
    #               y = 'padj',
    #               pCutoff = 0.05,
    #               FCcutoffUP = 1.5,
    #               FCcutoffDOWN = -1.5,
    #               xlim = c(-8, 8),
    #               col = c("gray", "#7cccc3", "#d99c01", "red", "blue")
    #               )

##### MA plot
 # p <-       MA(res, main = 'MA plot applying the DESeq2 Shrinkage normalization for Foldchange',
 #       fdr = 0.05, fcDOWN = -1.5, fcUP = 1.5, size = 1.5,
 #       palette = c("red","green", "gray"),
 #       genenames = res$GeneName_Symbol,
 #       legend = "top", top = 15, select.top.method = c('padj','fc'),
 #       font.label = c("plain", 12),
 #       font.legend = c("plain", 15),
 #       font.main = "plain",
 #       cex.axis = 1.1, cex.lab = 1.3,
 #       ggtheme = theme_classic()
 #    )

# p %>% ggplotly()
 ######## pruebas import objeto deseq ########
library(DESeq2)
dd <- readRDS("../deseqCustom.rds")
# ann <- read.table("../geneannots.txt")
head(assay(dd))
colData(dd)
design(dd)
re <- results(dd)
resultsNames(dd)

se <- assay(dd)
exp <- as.data.frame(colData(dd))
kk <- DESeqDataSetFromMatrix(countData = se, colData = exp, design = ~replicates)
#design(kk) <- formula(~replicates)
kkdd <- DESeq(kk)
design(kkdd)
fct <- gsub("~","", design(kkdd))[2]
rN <- resultsNames(kkdd)
conditions <- levels(colData(dd)[[fct]])
res <- results(kkdd, contrast = list(rN[2]) )
res2 <- results(kkdd, contrast = c("replicates","Astro_GFP_WT","Astro_GFP_AD") ) 
#aplicar shrikcomosellame
res <- lfcShrink(kkdd, coef = "replicates_Astro_SOCS3_AD_vs_Astro_GFP_WT",
                 type = "apeglm", res = results(kkdd,
                                                contrast = c("replicates","Astro_SOCS3_AD","Astro_GFP_WT") ) )
gsub(" ", "_", sub("log2 fold change \\(MLE\\): ", "", mcols(res)[2, 2]))
gsub(" ", "_", sub("log2 fold change \\(MLE\\): ", "", mcols(res2)[2, 2]))

resultsNames(kkdd) <- append(resultsNames(kkdd),"replicates_Astro_SOCS3_AD_vs_Astro_GFP_WT" )




dsg <- DataFrame( factor(colData(dd)[[fct]] ) )
names(dsg) <- fct
md <- model.matrix( as.formula(paste0("~",fct ) ), dsg  )


res <- results(kkdd,  contrast=c("replicates","Astro_SOCS3_AD","Astro_GFP_WT"))
resultsNames(kkdd)
edit(exp)
lfcShrink(datos$dds, coef=2, type="apeglm", res = results(datos$dds))

d <- DataFrame(AAV=factor(exp$AAV), type=factor(exp$Background))
m1 <- model.matrix(~AAV*type, d)
colnames(m1)

## Ideogram #####################
library(karyoploteR)
library(DESeq2)
library(dplyr)
dds <- readRDS("deseq.Rds")
res <- lfcShrink(dds, coef=2, res=results(dds))
chrom <- readRDS("./resources/Mm_chromset.rds")
names(chrom)<- c("chr","start","end")
annot <- readRDS("./resources/Mm_annot.rds")
res2 <- res[ res$padj <0.05 & (res$log2FoldChange<(-0.5) | res$log2FoldChange>0.5),]
res3 <- as.data.frame(res2)
res3$genes <- rownames(res3)
genes <- left_join(annot, res3, by = c("V1"="genes"))
sig <- which( !is.na(genes$padj) )
genes <- genes[sig,]
custom.gene <- toGRanges(chrom)
A <- data.frame(chr = paste0("chr",genes$V2), start = genes$V3, end=genes$V4, x = genes$V1, y = genes$log2FoldChange)
genesSig <- toGRanges(A)
col.over <- "#FFBD07AA"
col.under <- "#00A6EDAA"
sign.col <- rep(col.over, length(genesSig))
sign.col[genesSig$y <0] <- col.under
fc.ymax <- ceiling(max(abs(range(genesSig$y))))
fc.ymin <- -fc.ymax
points.top <- 0.5

one <- getDefaultPlotParams(2)
one$ideogramheight <- 300
kp <- plotKaryotype(genome  = custom.gene, plot.params = one, plot.type = 2,
                          use.cache = FALSE)
kpPlotRegions(kp, genesSig[genesSig$y>0,], col = "red", data.panel = 1)
kpPlotRegions(kp, genesSig[genesSig$y<0,], col = "blue", data.panel = 2)


#  # #
krtp <- function(res, specie="Mm", pval, fcdown, fcup){
  require(karyoploteR)
  fileAnnot <- paste0("./resources/",specie,"_annot.rds")
  if(specie == "Mm"){organism = "mm10"}
  annot <- readRDS(fileAnnot)
  res2 <- res[ res$padj <pval & (res$log2FoldChange<(fcdown) | res$log2FoldChange>fcup),]
  res3 <- as.data.frame(res2)
  res3$genes <- rownames(res3)
  genes <- left_join(annot, res3, by = c("V1"="genes"))
  sig <- which( !is.na(genes$padj) )
  genes <- genes[sig,]
  A <- data.frame(chr = paste0("chr",genes$V2), start = genes$V3,
                  end=genes$V4, x = genes$V1, y = genes$log2FoldChange)
  genesSig <- toGRanges(A)
  one <- getDefaultPlotParams(2)
  one$ideogramheight <- 200

  kp <- plotKaryotype(genome  = "mm10" ,plot.params = one, plot.type = 2)
  
  %>%
  kpPlotRegions(genesSig[genesSig$y>0,], col = "red", data.panel = 1) %>% 
  kpPlotRegions(genesSig[genesSig$y<0,], col = "blue", data.panel = 2)
  return(kp)
}
krtp(res,"Mm", 0.05, -0.5,0.5)

## ggbio ############################
library(ggbio)
p.ideo <- Ideogram(genome = "hg19", subchr = c("chr2"))
p.ideo  + xlim(GRanges("chr2", IRanges(1e8, 1e8+10000)))

library(GenomicRanges)
library(ggplot2)
## it's a 'ideogram'
data(hg19IdeogramCyto, package = "biovizBase")
hg19 <- keepSeqlevels(hg19IdeogramCyto, paste0("chr", c(1:22, "X", "Y")))
biovizBase::isIdeogram(hg19)
## set to FALSE
ggbio::autoplot(hg19, layout = "karyogram", cytoband = FALSE, aes(fill=gieStain)) +
  ggbio::scale_fill_giemsa()

## ideograma ###############################
# devtools::install_github("freestatman/ideogRam")
library(ideogRam)
example("ideogRam-shiny")
ideogRam(organism="mouse", orientation="horizontal",
         showBandLabels=FALSE)

genesSig$color <- ifelse(genesSig$y<0, "#08519c", "#a50f15")
ideogRam(organism="mouse", orientation="horizontal",showBandLabels=FALSE) %>%
    set_option(showAnnotTooltip=TRUE) %>% 
    set_option(annotationsLayout="overlay") %>% 
    set_option(chrWidth=15) %>% 
    set_option(chrHeight=600) %>% 
    add_track(genesSig)

###############################################################
gsea <- gseaKegg(res)
    mygsea <- gsea
    #saveRDS(mygsea, "tmpResources/gsea.Rds")
    if( length(which(mygsea@result$p.adjust<=0.05))==0 ){
        createAlert(session, anchorId = "gsea", title = "Oops!!", 
          content = "Sorry, I didn't get any significant results for this analysis",
          append=FALSE, style = "info")
    } else{
    table <- mygsea@result[mygsea@result$p.adjust<=0.05 ,2:9] %>% 
      mutate_at(vars(3:7), ~round(., 4))

    tituloTabla <- paste0("Table: GSEA pathway | ","log2FC: ",logfc()[1],"_",logfc()[2]," | ","padj: ",padj()," | ",
                          "Num genes Up/down: ",numgenesDE$up,"/",numgenesDE$down)
    customButtons <- list(
        list(extend = "copy", title=tituloTabla),
        list(extend = "excel",
            filename = "GSEAkegg",
            title = tituloTabla),
        list(extend = "pdf",
            filename = "GSEAkegg",
            title = tituloTabla),
        list(extend = "print", title=tituloTabla)
    )
    }


## plot1 ####################################################################
plotCountsSymbol <- function (dds, gene, intgroup = "condition", normalized = TRUE,
                              transform = TRUE, main, xlab = "group", returnData = FALSE,
                              replaced = FALSE, pc, org, ...){
  stopifnot(length(gene) == 1 & (is.character(gene) | (is.numeric(gene) &
                                                         (gene >= 1 & gene <= nrow(dds)))))
  if (!all(intgroup %in% names(colData(dds))))
    stop("all variables in 'intgroup' must be columns of colData")
  if (!returnData) {
    if (!all(sapply(intgroup, function(v) is(colData(dds)[[v]],
                                             "factor")))) {
      stop("all variables in 'intgroup' should be factors, or choose returnData=TRUE and plot manually")
    }
  }
  if (missing(pc)) {
    pc <- if(transform) 0.5
    else 0
  }
  if (is.null(sizeFactors(dds)) & is.null(normalizationFactors(dds))) {
    dds <- estimateSizeFactors(dds)
  }

  geneE <- mapIds(orgdb, keys = gene, column = "ENSEMBL", keytype = "SYMBOL")
  cnts <- counts(dds, normalized = normalized, replaced = replaced)[geneE,]
  intgroup <- intgroup[1]
  group <- if (length(intgroup) == 1) {
    colData(dds)[[intgroup]]
  }
  else if (length(intgroup) == 2) {
    lvls <- as.vector(t(outer(levels(colData(dds)[[intgroup[1]]]),
                              levels(colData(dds)[[intgroup[2]]]), function(x,
                                                                            y) paste(x, y, sep = ":"))))
    droplevels(factor(apply(as.data.frame(colData(dds)[,
                                                       intgroup, drop = FALSE]), 1, paste, collapse = ":"),
                      levels = lvls))
  }
  else {
    factor(apply(as.data.frame(colData(dds)[, intgroup,
                                            drop = FALSE]), 1, paste, collapse = ":"))
  }
  #rownames(cnts) <- res$GeneName_Symbol
  data <- data.frame(count = cnts + pc, group = as.integer(group))
  logxy <- if (transform) "y" else ""
  ylab <- ifelse(normalized, "normalized counts")
  #colors = c("#008000","#800080")
  if (returnData)
    return(data.frame(count = data$count, colData(dds)[intgroup]))
  plot(data$group + runif(ncol(dds), -0.05, 0.05), data$count, #col=colors[(dds)[intergroup]],
       xlim = c(0.5, max(data$group) + 0.5), log = logxy, xaxt = "n",
       xlab = xlab, ylab = ylab, main = paste0("Expression of ", gene), ...)
  axis(1, at = seq_along(levels(group)), levels(group))
  #text(data$group + runif(ncol(dds), -0.05, 0.05), data$count, labels=colnames(dds))
}


    
    
# 1. curl data USCS
cytoBandCreate <- function(specie = "Mm"){
  library(dplyr)
  dat <- curl::curl(url = "https://api.genome.ucsc.edu/getData/track?genome=mm10;track=cytoBandIdeo")
  open(dat)
  out <- readLines(dat)
  datos <- jsonlite::prettify(out)
  dfIdeoBand <- jsonlite::fromJSON(datos)
  dfIdeoBanda <- as.data.frame.list(dfIdeoBand$cytoBandIdeo[[1]])
  dfIdeoBandaLimpio <- dfIdeoBanda %>% select(-c(4, 5)) %>%
    group_by(chrom) %>%
    summarise(min = min(chromStart), max = max(chromEnd)) %>%
    filter(!grepl("_", chrom)) %>% mutate(chrom = sub("chr", "", chrom))
  
  dfIdeoSort <- dfIdeoBandaLimpio[gtools::mixedorder(dfIdeoBandaLimpio$chrom), ] %>%
    mutate(chrom = paste0("chr", chrom)) %>% as.data.frame()
  dfRanges <- regioneR::toGRanges(dfIdeoSort)
  saveRDS(dfRanges, "./resources/mm10_genomicRanges")
  saveRDS(dfIdeoBanda, "./resources/mm10_cytoBand")
  
  # curl -L ftp://ftp.ensembl.org/pub/release-100/gff3/homo_sapiens/Homo_sapiens.GRCh38.100.chr.gff3.gz >mm10.gtf.gz
  # zcat mm10.gtf.gz | awk '$3=="gene"{print $1,$4,$5,$9}' | awk 'BEGIN{OFS="\t"}{split($4,a,";");print a[1],$1,$2,$3}' | sed 's/ID=gene://g' >Mm_annot.txt
  library(ensembldb)
  library(EnsDb.Mmusculus.v79)
  edb = EnsDb.Mmusculus.v79
  genes <- genes(edb, columns = c("seq_name", "gene_name"))
  seqlevels(genes) <- paste0("chr", seqlevels(genes))
  genes <- regioneR::toGRanges(as.data.frame(genes))
  saveRDS(genes, "./resources/mm10_annot")
}
#
pval=0.05
fcdown=-0.5
fcup = 0.5
res2 <- res[ res$padj <pval & (res$log2FoldChange<(fcdown) | res$log2FoldChange>fcup),]
res3 <- as.data.frame(res2)
res3$genes <- rownames(res3)
annot <- as.data.frame(readRDS("./resources/mm10_annot"))
genes <- left_join(annot, res3, by = c("gene_id"="genes"))
sig <- which( !is.na(genes$padj) )
genes <- genes[sig,]
A <- data.frame(chr = paste0(genes$seqnames), start = genes$start,
                  end=genes$end, x = genes$log2FoldChange)
genesSig <- toGRanges(A)

one <- getDefaultPlotParams(2)
one$ideogramheight <- 200
dfRanges <- readRDS("./resources/mm10_genomicRanges")
dfIdeoBanda <- readRDS("./resources/mm10_cytoBand")
library(karyoploteR)
kp <- karyoploteR::plotKaryotype( genome = dfRanges, cytobands = dfIdeoBanda,
                                  plot.type = 2, use.cache = FALSE, plot.params = one) %>% 
    kpPlotRegions(genesSig[genesSig$x>0,], col = "red", data.panel = 1) %>% 
  kpPlotRegions(genesSig[genesSig$x<0,], col = "blue", data.panel = 2)
#

dot <- function(data, n = 20){
  names(data) <- gsub("P.DE", "p-val", names(data) )
  names(data) <- gsub("DE", "DEG", names(data) )
  data$ratio <- data$DEG/data$N
  data <- data[order(data$ratio, decreasing = F), ]
  data <- data[seq_len(n),]
  data$Pathway <- factor(data$Pathway, levels = data$Pathway)
  p <- ggplot(data, aes(y=Pathway, x=ratio, color=`p-val`))+
    geom_point( aes( size=DEG))+
    scale_radius()+
    theme_bw()+
    labs(x = "ratio (DEG/N)") +
    scale_color_continuous(low = "red", high = "blue",
                           guide = guide_colorbar(reverse = TRUE))+
    theme(text = element_text(size=20))
  return(p)
}



heatmapKegg <- function(kdt, nr){
  kdt <- kdt[nr, ]
  colourCount <- length(unique(kdt$DEG)) # number of levels
  getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))
  kdt <- kdt %>% dplyr::select(Pathway, genes, DEG) %>% 
    separate_rows(genes) %>%
    mutate(Pathway = fct_inorder(Pathway)) %>% 
    mutate(Pathway = fct_rev(Pathway)) %>% 
    mutate(genes = fct_infreq(genes)) %>% 
    mutate(DEG = factor(DEG))
    yNum <- length(unique(kdt$Pathway))
    if(yNum <=35){ySize=12}else if(yNum>35 | yNum <=50){ySize=10}else{ySize=0}
    xNum <- length(unique(kdt$genes))
    if(xNum <=60){xSize=8}else if(xNum>60 | yNum <=80){xSize=6}else{xSize=0}
    kdt %>% ggplot(aes_(~genes, ~Pathway)) + 
    geom_tile(aes_(fill = ~DEG), color = 'black', size =0.2) +
    xlab(NULL) + ylab(NULL) +
    theme_minimal() +
    theme(panel.grid.major = element_line(colour = "gray88", size = 0.8),
          axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5, size=xSize))+
    # scale_fill_continuous(low="blue", high="red", name = "N")
    # scale_fill_brewer(palette = "YlOrRd")
    scale_fill_manual(values = getPalette(colourCount))+
    theme(text = element_text(size=ySize, angle=0), plot.margin = unit(c(15,25,15,15), "pt"))
}





    heat <- function (vsd, n = 40, intgroup = "AAV", sampleName = "condition",
                      specie="Mm", customColor = c("red","blue")) 
    {
      require("EnsDb.Mmusculus.v79")
      require("org.Mm.eg.db")
      require("EnsDb.Hsapiens.v86")
      require("org.Hs.eg.db")
      require("EnsDb.Rnorvegicus.v79")
      require("org.Rn.eg.db") 
      
      if(specie=="Mm"){
        ensdb <- EnsDb.Mmusculus.v79
        orgdb <- org.Mm.eg.db
      }
      else{
        ensdb <- EnsDb.Hsapiens.v86
        orgdb <- org.Hs.eg.db
      }
    #vsd <- vst(data)
    topVarGenes <- head(order(rowVars(assay(vsd)), decreasing = TRUE), n)
    mat  <- assay(vsd)[ topVarGenes, ]
    mat  <- mat - rowMeans(mat)
    if (!all(intgroup %in% names(colData(vsd)))) {
      stop("the argument 'intgroup' should specify columns of colData(dds)")
    }

    if(length(intgroup)>1){
      df <- as.data.frame(colData(vsd)[, intgroup[1:2], drop = FALSE])
    } else{
      df <- as.data.frame(colData(vsd)[, intgroup, drop = FALSE])
    }
    
    annot <- NULL
    annot$ENSEMBL <- rownames(mat)
    annot$SYMBOL <-  mapIds(ensdb, keys=rownames(mat), column="SYMBOL",keytype="GENEID")
    annot$SYMBOL1 <- mapIds(orgdb, keys = rownames(mat), column = 'SYMBOL', keytype = 'ENSEMBL', multiVals = 'first') 
    annot$description <- mapIds(orgdb, keys = rownames(mat), column = 'GENENAME', keytype = 'ENSEMBL', multiVals = 'first')
    annot <- as.data.frame(annot)
    consensus <- data.frame('Symbol'= ifelse(!is.na(annot$SYMBOL), as.vector(annot$SYMBOL),
                                             ifelse(!is.na(annot$SYMBOL1),as.vector(annot$SYMBOL1),
                                                    as.vector(annot$ENSEMBL))), stringsAsFactors = F)
    ann_colors<-list()
    ann_colors[[intgroup[1]]] <- customColor
    names(ann_colors[[intgroup[1]]]) <- c(levels(df[[intgroup[1]]]))
    sizesDf <- data.frame( ch = c(rep(14,20), rep(12,20),rep(10,10), 
                                  rep(8,10), rep(7,10), rep(6,10), rep(5,20), rep(4,20)), 
                           fsr = c(rep(10,50), rep(8,10), rep(7,10), rep(6,10), rep(0.1, 40) ))
    ch <- sizesDf$ch[ nrow(mat) ]
    fsr <- sizesDf$fsr[ nrow(mat) ]
    pheatmap(mat, cluster_rows=TRUE, cluster_cols=TRUE,
             show_colnames=TRUE, show_rownames = TRUE, annotation_col = df,
             labels_col = as.character(vsd[[sampleName]]),
             labels_row = as.character(consensus$Symbol),
             cellwidth = 14, cellheight = ch,
             fontsize_row = fsr,
             annotation_colors = ann_colors,
             main = "Heatmap top variant genes on normalized data")
    }

