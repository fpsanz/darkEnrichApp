library(DESeq2)
library(limma)
library(tidyverse)

dds
vds <- vst(dds)
rv <- rowVars(assay(vds))
select <- order(rv, decreasing = TRUE)[seq_len(min(500, length(rv)))]


##########
keep <- rowSums(counts(dds) >= 1000) >= (round(ncol(dds)*0.8))
dds <- dds[keep,]
dfx <- as.data.frame(assay(dds))

kk <- function(x, dds){
    df2aov <- data.frame(datos = x)
    df2aov$group <- colData(dds)[["AAV"]]
    names(df2aov) <- c("datos", "group")
    dfaov <- aov(datos ~ group + Error(group), data = df2aov)
    within <- summary(dfaov)$"Error: Within"[[1]]$"Mean Sq"
    dfWithin <- summary(dfaov)$"Error: Within"[[1]]$"Df"
    group <- summary(dfaov)$"Error: group"[[1]]$"Mean Sq"
    dfgroup <- summary(dfaov)$"Error: group"[[1]]$"Df"
    between <- (group - within) / ((dfWithin / (dfgroup + 1)) + 1)
    r <- sqrt(between^2 / within^2)
    return(between)
}
r <- apply(dfx, 1, function(x){ kk(x,dds) } )
df2 <- dfx
df2$var <- r
select2 <- order(df2$var, decreasing = TRUE)
############


#seleccionar los 500 con más varianza
df <- as.data.frame(assay(vds)[select,])
#seleccionar 500 al azar
df <- sample_n(as.data.frame(assay(vds)), 500)
# seleccionar 500 con ratio between/within más grande
df <- as.data.frame(assay(vds)[select2,])[1:500,]

df <- as.data.frame(t(df))
df$group <- colData(vds)$AAV
df$sample <- colData(vds)$code
df$type <- as.character(colData(vds)$type)
df$type[1:5]<- "KO"
df2 <- df %>% pivot_longer(c(-group,-sample, -type), names_to = "samples", values_to = "counts")
yarrr::pirateplot(formula = counts~group, data=df2,
            pal = "southpark", # southpark color palette
           bean.f.o = .6, # Bean fill
           point.o = .3, # Points
           inf.f.o = .7, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 1, # Average line
           bar.f.o = .5, # Bar
           inf.f.col = "white", # Inf fill col
           inf.b.col = "black", # Inf border col
           avg.line.col = "black", # avg line col
           bar.f.col = gray(.8), # bar filling color
           point.pch = 21,
           point.bg = "white",
           point.col = "black",
           theme=3,
            quant = c(.25, .75),
           quant.col = "black")




df <- as.data.frame(assay(dds))
kk <- function(x, dds){
    df2aov <- data.frame(datos = x)
    df2aov$group <- colData(dds)[["AAV"]]
    names(df2aov) <- c("datos", "group")
    dfaov <- aov(datos ~ group + Error(group), data = df2aov)
    within <- summary(dfaov)$"Error: Within"[[1]]$"Mean Sq"
    dfWithin <- summary(dfaov)$"Error: Within"[[1]]$"Df"
    group <- summary(dfaov)$"Error: group"[[1]]$"Mean Sq"
    dfgroup <- summary(dfaov)$"Error: group"[[1]]$"Df"
    between <- (group - within) / ((dfWithin / (dfgroup + 1)) + 1)
    r <- sqrt(between^2 / within^2)
    return(between)
}

r <- apply(df, 1, function(x){ kk(x,dds) } )
b <- apply(df, 1, function(x){ kk(x,dds) } )

df2 <- df
df2$var <- b
df2 <- df2[order(df2$var, decreasing = TRUE),-c(13)]
df3 <- as.data.frame(t(df2[1:10,]))
df3$group <- colData(dds)[["AAV"]]
df4 <- df3 %>% pivot_longer(-c(group), names_to = "genes", values_to = "counts" )
df4 %>% ggplot(aes(x=genes, y=counts, colour = group))+ geom_jitter(width = 0.1) +
    theme(axis.text.x = element_text(angle=90))


