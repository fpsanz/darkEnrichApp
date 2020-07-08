require(GO.db)
getAllBPParents <- function(goids){
    ans <- unique(unlist(mget(goids, GOBPPARENTS), use.names=FALSE))
    ans <- ans[!is.na(ans)]
    return(ans)
}
getAllMFParents <- function(goids){
    ans <- unique(unlist(mget(goids, GOMFPARENTS), use.names=FALSE))
    ans <- ans[!is.na(ans)]
    return(ans)
}
getAllCCParents <- function(goids){
    ans <- unique(unlist(mget(goids, GOCCPARENTS), use.names=FALSE))
    ans <- ans[!is.na(ans)]
    return(ans)
}
getAllBPChildren <- function(goids){
    ans <- unique(unlist(mget(goids, GOBPCHILDREN), use.names=FALSE))
    ans <- ans[!is.na(ans)]
    return(ans)
}
getAllCCChildren <- function(goids){
    ans <- unique(unlist(mget(goids, GOCCCHILDREN), use.names=FALSE))
    ans <- ans[!is.na(ans)]
    return(ans)
}
getAllMFChildren <- function(goids){
    ans <- unique(unlist(mget(goids, GOMFCHILDREN), use.names=FALSE))
    ans <- ans[!is.na(ans)]
    return(ans)
}


##Para BP terms
goIds <- readRDS("resources/Mm//GO/GOlinks.Rds")
goIdsBP <- goIds[ goIds$Ontology == "BP", ]
goIdsBP2 <- unique(goIdsBP$go_id)
bpList <- lapply(goIdsBP2, function(x){getAllBPChildren(x)} )
bpList2 <- lapply(bpList, function(x){isEmpty(x)})
lastGOBP <- goIdsBP2[which(unlist(bpList2))]
kk <- lapply(lastGOBP, function(x){getAllBPParents(x)})
kk <- unlist2(kk)
allBP <- c(bpList2,kk)
allBPgenes <- goIdsBP[which(goIdsBP$go_id %in% allBP), ]
saveRDS(allBPgenes, "BPlastTerm&Parents.RDS")
###Para MF terms
goIdsMF <- goIds[ goIds$Ontology == "MF", ]
goIdsMF2 <- unique(goIdsMF$go_id)
mfList <- lapply(goIdsMF2, function(x){getAllMFChildren(x)} )
mfList2 <- lapply(mfList, function(x){isEmpty(x)})
lastGOMF <- goIdsMF2[which(unlist(mfList2))]
kk <- lapply(lastGOMF, function(x){getAllMFParents(x)})
kk <- unlist2(kk)
allMF <- c(mfList2,kk)
allMFgenes <- goIdsMF[which(goIdsMF$go_id %in% allMF), ]
saveRDS(allMFgenes, "MFlastTerm&Parents.RDS")
###Para CC terms
goIdsCC <- goIds[ goIds$Ontology == "CC", ]
goIdsCC2 <- unique(goIdsCC$go_id)
ccList <- lapply(goIdsCC2, function(x){getAllCCChildren(x)} )
ccList2 <- lapply(ccList, function(x){isEmpty(x)})
lastGOCC <- goIdsCC2[which(unlist(ccList2))]
kk <- lapply(lastGOCC, function(x){getAllCCParents(x)})
kk <- unlist2(kk)
allCC <- c(ccList2,kk)
allCCgenes <- goIdsCC[which(goIdsCC$go_id %in% allCC), ]
saveRDS(allCCgenes, "CClastTerm&Parents.RDS")


allOnt <- rbind(allBPgenes, allMFgenes, allCCgenes)
links <- readRDS("./resources/Mm/GO/GOlinks.Rds")

saveRDS(allOnt, "AllOntLastTerm&Parents.RDS")



##convert symbol 2 entrez
library("MAGeCKFlute")
library(tidyverse)
genes <- read.csv("~/Escritorio/NAs.csv", row.names = 1)
kk <- TransGeneID(genes$x, fromType = "Symbol", toType = "Entrez", "mmu")
kkk <- as.data.frame(kk)
kkk$symbol <- genes$x

annot <- read_delim("~/Escritorio/A-GEOD-11017.adf.txt", delim = "\t", col_names = FALSE)
annot$X1 <- tolower(annot$X1)
kk2 <- left_join( kkk, annot, by = c("symbol"="X1") )
write.table(kk2, "~/Escritorio/kk2.csv", quote = F)
kk2$X4 <- gsub(".[0-9]$", "", kk2$X4)

<<<<<<< HEAD:getLastTerm&Parents.R
### ### ### grafos #### ####
goIds <- readRDS("resources/Hs/GO/GOlinks.Rds")
goIdsCC <- goIds[ goIds$Ontology == "CC", ]
goIdsCC2 <- unique(goIdsCC$go_id)
ccList <- lapply(goIdsCC2, function(x){getAllCCChildren(x)} )
=======
kk4 <- read.table("~/Escritorio/kk4", header = F)
cualos <- which(kk4$V1 %in% kk2$X4)
kk4 <- kk4[cualos,]

kk_kk <- TransGeneID(kk4$V2, fromType = "Symbol", toType = "Entrez", "mmu")
kk_kk <- as.data.frame(kk_kk)
kk_kk$symbol <- rownames(kk_kk)
>>>>>>> 3100aae8fdb6feeae926174e75585586b1b018ef:kk/getLastTermParents.R

kk4$entrez <- kk_kk$kk_kk
kksemifinal <- left_join(kk2, kk4, by = c("X4"="V1"))
kksemifinal$nuevoEntrez <- NA
kksemifinal$kk <- as.character(kksemifinal$kk)
kksemifinal$entrez <- as.character(kksemifinal$entrez)

kkFinal <- kksemifinal %>% mutate(nuevoEntrez = if_else(is.na(kk), entrez, kk) )
write.table(kkFinal[,c(2,8)], "~/Escritorio/menosNAs.csv", sep = ",", col.names = F, row.names = F)
