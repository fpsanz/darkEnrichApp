library(shinydashboard)
library(AnnotationDbi)
library(org.Mm.eg.db) #Mus musculus
library(org.Hs.eg.db) #Homo sapiens
#library(org.Dr.eg.db) #Danio rerio (zebra fish)
library(org.Rn.eg.db) #Ratus norvegicus
#library(org.Mmu.eg.db) #Macaca mulata
library(chorddiag)
library(EnsDb.Mmusculus.v79)
library(EnsDb.Hsapiens.v86)
library(EnsDb.Rnorvegicus.v79)
library(limma)
library(tidyverse)
library(DT)
library(RColorBrewer)
library(purrr)
library(plotly)
library(ggpubr)
library(DESeq2)
library(fgsea)
library(shinyalert)
library(shinyBS)
library(shinyWidgets)
library(shinydashboardPlus)
library(pheatmap)
library(heatmaply)
library(shinyjs)
library(shinythemes)
library(rgl)
library(rglwidget)
library(scales)
library(stringr)
library(shinybusy)
source("../utils.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    plotOutput("volcano", click = "plot_click" ),
    tableOutput("texto")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    res <- reactive({
    res <- as.data.frame(lfcShrink(readRDS("../deseq.Rds"), coef=2, type="apeglm"))
    res <- res[!is.na(res$padj),]
    conversion <- geneIdConverter(rownames(res), "Mm" )
    res$baseMean <- round(res$baseMean,4)
    res$lfcSE <- round(res$lfcSE,4)
    res$log2FoldChange <- round(res$log2FoldChange,4)
    res <- cbind(`Description`=conversion$description, res)
    res <- cbind(`GeneName_Symbol`=conversion$consensus, res)
    res <-  res %>% dplyr::select(-c(pvalue))
    spc = "Mus_musculus"
    links = paste0("<a href='http://www.ensembl.org/",spc,"/Gene/Summary?db=core;g=",
                    rownames(res),"' target='_blank'>",rownames(res),"</a>")
    res$`-log10padj` <- (-log10(res$padj))
    res <- cbind(`GeneName_Ensembl`= links, res)
    
    })
    
    
    xy <- reactive({
        nearPoints(res(), input$plot_click, xvar = "log2FoldChange", yvar = "-log10padj")
        })
    

    
    output$volcano <- renderPlot({
        validate(need(res(),""))
        res <- res()
        CustomVolcano(res, lab = res$GeneName_Symbol, 
                  x = 'log2FoldChange',
                  y = 'padj',
                  pCutoff = 0.05,
                  FCcutoffUP = -0.5,
                  FCcutoffDOWN = 0.5,
                  xlim = c(-8, 8),
                  col = c("gray", "#7cccc3", "#d99c01", "red", "blue")
                  )
    })
    
    output$texto <- renderTable({
        xy <- xy()
        xy
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
