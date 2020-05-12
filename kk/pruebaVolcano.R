# volcany <- function(res, padj = padj(), fcup = logfc()[2],
#                     fcdown = logfc()[1], col = c(input$upColor, input$downColor), genes=NULL ){
# 
# geneNames <- data.frame(ens=as.character(rownames(res)), symbol=as.character(res$GeneName_Symbol), stringsAsFactors = F)
# res2 <- res %>% dplyr::select(GeneName_Symbol, log2FoldChange, padj) %>% as.data.frame()
# rownames(res2) <- rownames(res)
# res2$group <- "NS"
# res2[which( res2$padj<=padj & ( res2$log2FoldChange>fcdown & res2$log2FoldChange<fcup) ), "group"] <- "Only Padj"
# res2[which( res2$padj<=padj & res2$log2FoldChange>fcup), "group"] <- "FC Up & Padj"
# res2[which( res2$padj<=padj & res2$log2FoldChange<fcdown), "group"] <- "FC Down & Padj"
# res2[which( res2$padj>padj & ( res2$log2FoldChange<fcdown | res2$log2FoldChange>fcup ) ), "group"] <- "Only FC"
# 
# if(!is.null(genes)){
#   topTotal <- res2[which(res2$GeneName_Symbol %in% genes), ]
#   topTotal$GeneName_Symbol <- as.character(topTotal$GeneName_Symbol)
#   a <- list()
#   for (i in seq_len(nrow(topTotal))) {
#     m <- topTotal[i,]
#     a[[i]] <- list(
#       x = m[["log2FoldChange"]],
#       y = -log10(m[["padj"]]),
#       text = m[["GeneName_Symbol"]],
#       xref = "x",
#       yref = "y",
#       xanchor = "left",
#       yanchor = "bottom",
#       showarrow = FALSE,
#       arrowhead = 4,
#       arrowsize = 0.5,
#       ax = 20,
#       ay = -40
#     )
#   }
# }
# line <- list(
#   type = "line",
#   line = list(color = "black", dash = "dash", width=0.7),
#   xref = "paper"
# )
# lines <- list()
# for (i in c(0.05)) {
#   line[["x0"]] <- 0
#   line[["x1"]] <- 1
#   line[c("y0", "y1")] <- -log10(i)
#   lines <- c(lines, list(line))
# }
# linev <- list(
#   type = "line",
#   line = list(color = "black", dash = "dash",width=0.7),
#   yref = "paper"
# )
# linesv <- list()
# for(j in c(-0.5,0.5)){
#   linev[["y0"]] <- 0
#   linev[["y1"]] <- 1
#   linev[c("x0","x1")] <- j
#   linesv <- c(linesv, list(linev))
# }
# linesT <- c(lines, linesv)
# 
# 
# pal <- c(NS="gray", "Only Padj"="#7cccc3", "Only FC"="#d99c01", "FC Up & Padj"=col[1],
#          "FC Down & Padj"=col[2] )
# p <- res2 %>% plot_ly(x = ~log2FoldChange, y = ~(-log10(padj)), text = ~GeneName_Symbol,
#                       mode = "markers", color = ~group, type = "scatter", size=I(5), colors=pal )%>% 
#   layout(shapes = linesT ) %>% 
#   layout(xaxis = list(zeroline=FALSE), yaxis=list(zeroline=FALSE))
# if(!is.null(genes)){ p <- p %>% layout(annotations = a) }
# return(p)
# }
# 
# 
#     genes <- res$GeneName_Symbol[!(res$padj>padj & 
#                                       res$log2FoldChange>logfc[1] & 
#                                       res$log2FoldChange<logfc[2]) ]
#     logfc<-c(-0.5, 0.5);padj=0.05
# as.character(res$GeneName_Symbol[ which( (res$padj<padj |
#                   (res$log2FoldChange<logfc[1] |
#                   res$log2FoldChange>logfc[2])) )])
#  
#   

#######################################################################################
# res <- as.data.frame(lfcShrink(readRDS("deseq.Rds"), coef=2, type="apeglm"))
#     res <- res[!is.na(res$padj),]
#     conversion <- geneIdConverter(rownames(res), "Mm" )
#     res$baseMean <- round(res$baseMean,4)
#     res$lfcSE <- round(res$lfcSE,4)
#     res$log2FoldChange <- round(res$log2FoldChange,4)
#     res <- cbind(`Description`=conversion$description, res)
#     res <- cbind(`GeneName_Symbol`=conversion$consensus, res)
#     res <-  res %>% dplyr::select(-c(pvalue))
#     spc = "Mus_musculus"
#     links = paste0("<a href='http://www.ensembl.org/",spc,"/Gene/Summary?db=core;g=",
#                     rownames(res),"' target='_blank'>",rownames(res),"</a>")
#     res$`-log10padj` <- (-log10(res$padj))
#     res <- cbind(`GeneName_Ensembl`= links, res)
res <- readRDS("res.Rds")
CustomV <- function (toptable, lab, x, y, selectLab = NULL, xlim = c(min(toptable[[x]], 
                           na.rm = TRUE), max(toptable[[x]], na.rm = TRUE)), 
                           ylim = c(0, max(-log10(toptable[[y]]), na.rm = TRUE) + 5), xlab = bquote(~Log[2] ~ "fold change"), 
                           ylab = bquote(~-Log[10] ~ italic(P)), axisLabSize = 18, 
                           title = "Volcano plot highlighting the different groups of signification", subtitle = "", caption = paste0("Total = ", 
                           nrow(toptable), " variables"), titleLabSize = 18, subtitleLabSize = 14, 
                           captionLabSize = 14, pCutoff = 1e-05, pLabellingCutoff = pCutoff, 
                           FCcutoffDOWN = -1, FCcutoffUP = 1 , cutoffLineType = "longdash", cutoffLineCol = "black", 
                           cutoffLineWidth = 0.4, transcriptPointSize = 0.8, transcriptLabSize = 3, 
                           transcriptLabCol = "black", transcriptLabFace = "plain", 
                           transcriptLabhjust = 0, transcriptLabvjust = 1.5, pointSize = 2, 
                           labSize = 3, labCol = "black", labFace = "plain", labhjust = 0, 
                           labvjust = 1.5, boxedlabels = FALSE, boxedLabels = FALSE, 
                           shape = 19, shapeCustom = NULL, col = c("grey30", "forestgreen", 
                           "royalblue", "red2"), colCustom = NULL, colAlpha = 1/2, 
                           legend = c("NS", "Log2 FC", "P", "P & Log2 FC"), legendLabels = c("NS", 
                           expression(Only ~ log[2]~FC), "Only p-adjusted", expression(p-adjusted ~ and ~ log[2]~FC)), 
                           legendPosition = "top", legendLabSize = 14, 
                           legendIconSize = 4, legendVisible = TRUE, shade = NULL, 
                           shadeLabel = NULL, shadeAlpha = 1/2, shadeFill = "grey", 
                           shadeSize = 0.01, shadeBins = 2, drawconnectors = FALSE, 
                           drawConnectors = FALSE, widthConnectors = 0.5, typeConnectors = "closed", 
                           endsConnectors = "first", lengthConnectors = unit(0.01, "npc"), colConnectors = "grey10", 
                           hline = NULL, hlineType = "longdash", 
                           hlineCol = "black", hlineWidth = 0.4, vline = NULL, vlineType = "longdash", 
                           vlineCol = "black", vlineWidth = 0.4, gridlines.major = TRUE, 
                           gridlines.minor = TRUE, border = "partial", borderWidth = 0.8, 
                           borderColour = "black") 
{
  
  if (!is.numeric(toptable[[x]])) {
    stop(paste(x, " is not numeric!", sep = ""))
  }
  if (!is.numeric(toptable[[y]])) {
    stop(paste(y, " is not numeric!", sep = ""))
  }
  i <- xvals <- yvals <- Sig <- NULL
  if (!missing("transcriptPointSize")) {
    warning(paste0("transcriptPointSize argument deprecated in v1.4", 
                   " - please use pointSize"))
    pointSize <- transcriptPointSize
  }
  if (!missing("transcriptLabSize")) {
    warning(paste0("transcriptLabSize argument deprecated in v1.4", 
                   " - please use labSize"))
    labSize <- transcriptLabSize
  }
  if (!missing("transcriptLabCol")) {
    warning(paste0("transcriptLabCol argument deprecated in v1.4", 
                   " - please use labCol"))
    labCol <- transcriptLabCol
  }
  if (!missing("transcriptLabFace")) {
    warning(paste0("transcriptLabFace argument deprecated in v1.4", 
                   " - please use labFace"))
    labFace <- transcriptLabFace
  }
  if (!missing("transcriptLabhjust")) {
    warning(paste0("transcriptLabhjust argument deprecated in v1.4", 
                   " - please use labhjust"))
    labhjust <- transcriptLabhjust
  }
  if (!missing("transcriptLabvjust")) {
    warning(paste0("transcriptLabvjust argument deprecated in v1.4", 
                   " - please use labvjust"))
    labvjust <- transcriptLabvjust
  }
  if (!missing("boxedlabels")) {
    warning(paste0("boxedlabels argument deprecated in v1.4", 
                   " - please use boxedLabels"))
    boxedLabels <- boxedlabels
  }
  if (!missing("drawconnectors")) {
    warning(paste0("drawconnectors argument deprecated since v1.2", 
                   " - please use drawConnectors"))
    drawConnectors <- drawconnectors
  }
  toptable <- as.data.frame(toptable)
  toptable$Sig <- "NS"
  toptable$Sig[toptable[[x]] >= FCcutoffUP] <- "FC"
  toptable$Sig[toptable[[x]] <= FCcutoffDOWN] <- "FC"
  toptable$Sig[(toptable[[y]] < pCutoff)] <- "P"
  toptable$Sig[(toptable[[y]] < pCutoff) & (toptable[[x]] >= FCcutoffUP)] <- "FC_Pup"
  toptable$Sig[(toptable[[y]] < pCutoff) & (toptable[[x]] <= FCcutoffDOWN)] <- "FC_Pdown"
  toptable$Sig <- factor(toptable$Sig, levels = c("NS", "FC", "P", "FC_Pup","FC_Pdown"))
  
  if (min(toptable[[y]], na.rm = TRUE) == 0) {
    warning(paste("One or more p-values is 0.", "Converting to 10^-1 * current", 
                  "lowest non-zero p-value..."), call. = FALSE)
    toptable[which(toptable[[y]] == 0), y] <- min(toptable[which(toptable[[y]] != 0), y], na.rm = TRUE) * 10^-1
  }
  toptable$lab <- lab
  toptable$xvals <- toptable[[x]]
  toptable$yvals <- toptable[[y]]
  if (!is.null(selectLab)) {
    names.new <- rep(NA, length(toptable$lab))
    indices <- which(toptable$lab %in% selectLab)
    names.new[indices] <- toptable$lab[indices]
    toptable$lab <- names.new
  }
  th <- theme_bw(base_size = 24) + theme(legend.background = element_rect(), 
          plot.title = element_text(angle = 0, size = titleLabSize, 
          face = "bold", vjust = 1), plot.subtitle = element_text(angle = 0, 
          size = subtitleLabSize, face = "plain", vjust = 1), 
          plot.caption = element_text(angle = 0, size = captionLabSize, 
          face = "plain", vjust = 1), axis.text.x = element_text(angle = 0, size = axisLabSize, vjust = 1), 
          axis.text.y = element_text(angle = 0, 
          size = axisLabSize, vjust = 1), axis.title = element_text(size = axisLabSize), 
          legend.position = legendPosition, legend.key = element_blank(), 
          legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = legendLabSize), 
          title = element_text(size = legendLabSize), legend.title = element_blank())
  if (!is.null(colCustom) & !is.null(shapeCustom)) {
    plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + 
      th + guides(colour = guide_legend(order = 1, override.aes = list(size = legendIconSize)), 
                  shape = guide_legend(order = 2, override.aes = list(size = legendIconSize))) + 
      geom_point(aes(color = factor(names(colCustom)), 
                     shape = factor(names(shapeCustom))), alpha = colAlpha, 
                 size = pointSize, na.rm = TRUE) + scale_color_manual(values = colCustom) + 
      scale_shape_manual(values = shapeCustom)
  }
  else if (!is.null(colCustom) & is.null(shapeCustom) & length(shape) == 
           1) {
    plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + 
      th + guides(colour = guide_legend(order = 1, override.aes = list(size = legendIconSize)), 
                  shape = guide_legend(order = 2, override.aes = list(size = legendIconSize))) + 
      geom_point(aes(color = factor(names(colCustom))), 
                 alpha = colAlpha, shape = shape, size = pointSize, 
                 na.rm = TRUE) + scale_color_manual(values = colCustom) + 
      scale_shape_manual(guide = TRUE)
  }
  else if (!is.null(colCustom) & is.null(shapeCustom) & length(shape) == 
           4) {
    plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + 
      th + guides(colour = guide_legend(order = 1, override.aes = list(size = legendIconSize)), 
                  shape = guide_legend(order = 2, override.aes = list(size = legendIconSize))) + 
      geom_point(aes(color = factor(names(colCustom)), 
                     shape = factor(Sig)), alpha = colAlpha, size = pointSize, 
                 na.rm = TRUE) + scale_color_manual(values = colCustom) + 
      scale_shape_manual(values = c(NS = shape[1], FC = shape[2], 
                                    P = shape[3], FC_Pup = shape[4], FC_Pdown = shape[4] ),
                         labels = c(NS = legendLabels[1], 
                                    FC = legendLabels[2], P = legendLabels[3], FC_Pup = legendLabels[4],
                                    FC_Pdown = legendLabels[4]), 
                                    guide = TRUE)
  }
  else if (is.null(colCustom) & !is.null(shapeCustom)) {
    plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + 
      th + guides(colour = guide_legend(order = 1, override.aes = list(size = legendIconSize)), 
                  shape = guide_legend(order = 2, override.aes = list(size = legendIconSize))) + 
      geom_point(aes(color = factor(Sig), shape = factor(names(shapeCustom))), 
                 alpha = colAlpha, size = pointSize, na.rm = TRUE) + 
      scale_color_manual(values = c(NS = col[1], FC = col[2], 
                                    P = col[3], FC_Pup = col[4], FC_Pdown = col[5] ),
                         labels = c(NS = legendLabels[1],
                                    FC = legendLabels[2], P = legendLabels[3],
                                    FC_Pup = legendLabels[4],
                                    FC_Pdown = legendLabels[4])) + 
      scale_shape_manual(values = shapeCustom)
  }
  else if (is.null(colCustom) & is.null(shapeCustom) & length(shape) == 
           1) {
    plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + 
      th + guides(colour = guide_legend(order = 1, override.aes = list(shape = shape, 
               size = legendIconSize))) + geom_point(aes(color = factor(Sig)), 
               alpha = colAlpha, shape = shape, size = pointSize, 
               na.rm = TRUE, show.legend = legendVisible) + scale_color_manual(values = c(NS = col[1], FC = col[2], 
                                    P = col[3], FC_Pup = col[4], FC_Pdown = col[5] ),
                         labels = c(NS = legendLabels[1],
                                    FC = legendLabels[2], P = legendLabels[3],
                                    FC_Pup = legendLabels[4],
                                    FC_Pdown = legendLabels[4]))
  }
  else if (is.null(colCustom) & is.null(shapeCustom) & length(shape) == 4) {
    plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + 
      th + guides(colour = guide_legend(order = 1, override.aes = list(shape = c(NS = shape[1], 
      FC = shape[2], P = shape[3], FC_Pup = shape[4],FC_Pdown = shape[4]), size = legendIconSize))) + 
      geom_point(aes(color = factor(Sig), shape = factor(Sig)), 
                 alpha = colAlpha, size = pointSize, na.rm = TRUE, 
                 show.legend = legendVisible) + scale_color_manual(values = c(NS = col[1], FC = col[2], 
                                    P = col[3], FC_Pup = col[4], FC_Pdown = col[5] ),
                         labels = c(NS = legendLabels[1],
                                    FC = legendLabels[2], P = legendLabels[3],
                                    FC_Pup = legendLabels[4],
                                    FC_Pdown = legendLabels[4])) + 
      scale_shape_manual(values = c(NS = shape[1], FC = shape[2], 
                                    P = shape[3], FC_Pup = shape[4], FC_Pdown = shape[4]), guide = FALSE)
  }
  plot <- plot + xlab(xlab) + ylab(ylab) + xlim(xlim[1], xlim[2]) + 
    ylim(ylim[1], ylim[2]) + geom_vline(xintercept = c(FCcutoffDOWN,FCcutoffUP), linetype = cutoffLineType, colour = cutoffLineCol, 
                                        size = cutoffLineWidth) + geom_hline(yintercept = -log10(as.numeric(pCutoff)), 
                                        linetype = cutoffLineType, colour = cutoffLineCol, size = cutoffLineWidth)
  plot <- plot + labs(title = title, subtitle = subtitle, caption = caption)
  if (!is.null(vline)) {
    plot <- plot + geom_vline(xintercept = vline, linetype = vlineType, 
                              colour = vlineCol, size = vlineWidth)
  }
  if (!is.null(hline)) {
    plot <- plot + geom_hline(yintercept = -log10(hline), 
                              linetype = hlineType, colour = hlineCol, size = hlineWidth)
  }
  if (border == "full") {
    plot <- plot + theme(panel.border = element_rect(colour = borderColour, 
                                                     fill = NA, size = borderWidth))
  }
  else if (border == "partial") {
    plot <- plot + theme(axis.line = element_line(size = borderWidth, 
                                                  colour = borderColour), panel.border = element_blank(), 
                         panel.background = element_blank())
  }
  else {
    stop("Unrecognised value passed to 'border'. Must be 'full' or 'partial'")
  }
  if (gridlines.major == TRUE) {
    plot <- plot + theme(panel.grid.major = element_line())
  }
  else {
    plot <- plot + theme(panel.grid.major = element_blank())
  }
  if (gridlines.minor == TRUE) {
    plot <- plot + theme(panel.grid.minor = element_line())
  }
  else {
    plot <- plot + theme(panel.grid.minor = element_blank())
  }
  if (boxedLabels == FALSE) {
    if (drawConnectors == TRUE && is.null(selectLab)) {
      plot <- plot + geom_text_repel(data = subset(toptable, 
                  toptable[[y]] < pLabellingCutoff & ((toptable[[x]] > FCcutoffUP) | (toptable[[x]] < FCcutoffDOWN))),
                  aes(label = subset(toptable, toptable[[y]] < pLabellingCutoff & ((toptable[[x]] > FCcutoffUP) | (toptable[[x]] < FCcutoffDOWN)))[["lab"]]), 
                  size = labSize, segment.color = colConnectors, 
                  segment.size = widthConnectors, arrow = arrow(length = lengthConnectors, 
                                     type = typeConnectors, ends = endsConnectors), 
                                     hjust = labhjust, vjust = labvjust, colour = labCol, 
                                     fontface = labFace, na.rm = TRUE)
    }
    else if (drawConnectors == TRUE && !is.null(selectLab)) {
      plot <- plot + geom_text_repel(data = subset(toptable, 
                                     !is.na(toptable[["lab"]])), aes(label = subset(toptable, 
                                     !is.na(toptable[["lab"]]))[["lab"]]), size = labSize, 
                                     segment.color = colConnectors, segment.size = widthConnectors, 
                                     arrow = arrow(length = lengthConnectors, type = typeConnectors, 
                                                   ends = endsConnectors), hjust = labhjust, 
                                     vjust = labvjust, colour = labCol, fontface = labFace, 
                                     na.rm = TRUE)
    }
    else if (drawConnectors == FALSE && !is.null(selectLab)) {
      plot <- plot + geom_text(data = subset(toptable, 
                               !is.na(toptable[["lab"]])), aes(label = subset(toptable, 
                               !is.na(toptable[["lab"]]))[["lab"]]), size = labSize, 
                               check_overlap = TRUE, hjust = labhjust, vjust = labvjust, 
                               colour = labCol, fontface = labFace, na.rm = TRUE)
    }
    else if (drawConnectors == FALSE && is.null(selectLab)) {
      plot <- plot + geom_text(data = subset(toptable, 
                                             toptable[[y]] < pLabellingCutoff & ((toptable[[x]] > FCcutoffUP) | (toptable[[x]] < FCcutoffDOWN))), 
                                             aes(label = subset(toptable, toptable[[y]] < 
                                             pLabellingCutoff & ((toptable[[x]] > FCcutoffUP) | (toptable[[x]] < FCcutoffDOWN)))[["lab"]]), 
                               size = labSize, check_overlap = TRUE, hjust = labhjust, 
                               vjust = labvjust, colour = labCol, fontface = labFace, 
                               na.rm = TRUE)
    }
  }
  else {
    if (drawConnectors == TRUE && is.null(selectLab)) {
      plot <- plot + geom_label_repel(data = subset(toptable, 
                                                    toptable[[y]] < pLabellingCutoff & (toptable[[x]] > FCcutoffUP) | (toptable[[x]] < FCcutoffDOWN)), 
                                                    aes(label = subset(toptable, 
                                                    toptable[[y]] < pLabellingCutoff & ((toptable[[x]] > FCcutoffUP) | (toptable[[x]] < FCcutoffDOWN)))[["lab"]]), 
                                      size = labSize, segment.color = colConnectors, 
                                      segment.size = widthConnectors, arrow = arrow(length = lengthConnectors, 
                                                                                    type = typeConnectors, ends = endsConnectors), 
                                      hjust = labhjust, vjust = labvjust, colour = labCol, 
                                      fontface = labFace, na.rm = TRUE)
    }
    else if (drawConnectors == TRUE && !is.null(selectLab)) {
      plot <- plot + geom_label_repel(data = subset(toptable, 
                                      !is.na(toptable[["lab"]])), aes(label = subset(toptable, 
                                      !is.na(toptable[["lab"]]))[["lab"]]), size = labSize, 
                                      segment.color = colConnectors, segment.size = widthConnectors, 
                                      arrow = arrow(length = lengthConnectors, type = typeConnectors, 
                                                    ends = endsConnectors), hjust = labhjust, 
                                      vjust = labvjust, colour = labCol, fontface = labFace, 
                                      na.rm = TRUE)
    }
    else if (drawConnectors == FALSE && !is.null(selectLab)) {
      plot <- plot + geom_label(data = subset(toptable, 
                                !is.na(toptable[["lab"]])), aes(label = subset(toptable, 
                                !is.na(toptable[["lab"]]))[["lab"]]), size = labSize, 
                                hjust = labhjust, vjust = labvjust, colour = labCol, 
                                fontface = labFace, na.rm = TRUE)
    }
    else if (drawConnectors == FALSE && is.null(selectLab)) {
      plot <- plot + geom_label(data = subset(toptable, 
                                toptable[[y]] < pLabellingCutoff & ((toptable[[x]] > FCcutoffUP) | (toptable[[x]] < FCcutoffDOWN))),
                                aes(label = subset(toptable, toptable[[y]] < pLabellingCutoff & ((toptable[[x]] > FCcutoffUP) | (toptable[[x]] < FCcutoffDOWN)))[["lab"]]), 
                                size = labSize, hjust = labhjust, vjust = labvjust, 
                                colour = labCol, fontface = labFace, na.rm = TRUE)
    }
  }
  if (!is.null(shade)) {
    plot <- plot + stat_density2d(data = subset(toptable, 
                                                rownames(toptable) %in% shade), fill = shadeFill, 
                                  alpha = shadeAlpha, geom = "polygon", contour = TRUE, 
                                  size = shadeSize, bins = shadeBins, show.legend = FALSE, 
                                  na.rm = TRUE) + scale_fill_identity(name = shadeLabel, 
                                                                      labels = shadeLabel, guide = "legend")
  }
  return(plot)
}

  CustomV(res, lab = as.character(res$GeneName_Symbol), selectLab = "Socs3",
                    x = 'log2FoldChange',
                    y = 'padj',
                    pCutoff = 0.05,
                    FCcutoffUP = 0.5,
                    FCcutoffDOWN = -0.5,
                    xlim = c(-8, 8),
                    col = c("gray", "#7cccc3", "#d99c01", "red", "blue")
          )

