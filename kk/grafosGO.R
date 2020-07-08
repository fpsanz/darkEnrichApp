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
getAllCCAncestors <- function(goids){
    ans <- unique(unlist(mget(goids, GOCCANCESTOR), use.names=FALSE))
    ans <- ans[!is.na(ans)]
    return(ans)
}

#cargar todos los GO
goIds <- readRDS("resources/Mm//GO/GOlinks.Rds") 
# Filtar por ontología
goIdsCC <- goIds[ goIds$Ontology == "CC", ]
# Eliminar repeticiones
goIdsCC2 <- unique(goIdsCC$go_id)
# Obtener los padres de cada término
ccAscen <- lapply(goIdsCC2, function(x){getAllCCParents(x)} )
names(ccAscen)<- goIdsCC2

####
prepararLista <- function(listaGO){
    ll <- listaGO
    cc <- names(ll)
    pp <- data.frame(a = character(0), b = numeric(0))
    for(i in cc){
      pp <- rbind(pp, data.frame(a = i, b = ll[[i]]))
    }
    pp <- pp %>% mutate_all(as.character)
    return(pp)
}

pintarFlujograma <- function(listaPreparada, init){
    pp <- listaPreparada
    a <- b <- vector()
    a <- which(pp$a==init)
    repeat{
        b <- append(b,a)
        a <- unlist(lapply(a, function(x) { which(pp$a==pp$b[x] ) }))
        if(is.null(a)) break
    }
    data <- pp[unique(b),]
    data <- data[data$b!="all", ]
    k <- paste0( "digraph nice_graph {",
                 paste0("'",data$b,"'"," -> ","'",data$a,"'", collapse = "\n"),
                 "}" )
    return(k)
}

init <- "GO:0005783"
pp <- prepararLista(ccAscen)
k <- pintarFlujograma(pp, init)
DiagrammeR::grViz(k, engine = "dot")


## Para flujograma GO
# 

