I have found a (not so pretty) solution (but it works), so I share it :

1. First, in the UI part I create an invisible download link (I set label ="" to get it invisible => it allows to declare a downloadLink but not to see it):
downloadLink("downloadData",label="")

2. I also declare (in the UI part) this javascript code (inspired by this) :
tags$head(tags$script(HTML('
                           Shiny.addCustomMessageHandler("jsCode",
                           function(message) {
                           eval(message.value);
                           });'
)))
3. In the server part I create thise observer (partially inspired by this)
observeEvent(input$my_own_trigger, {
  output$downloadData <<- downloadHandler(filename = x$Nom, content = function(file){ file.copy(file0,file) } )
  jsinject <- "setTimeout(function(){window.open($('#downloadData').attr('href'))}, 100);"
  session$sendCustomMessage(type = 'jsCode', list(value = jsinject))    
})

So now I can produce a download in the client side with any trigger (input$my_own_trigger)


      # nrall <- rowsAll()
      # nrup <- rowsUp()
      # nrdown <- rowsdown()
      # bpnrup <- bprowsup()
      # mfnrup <- mfrowsup()
      # ccnrup <- ccrowsup()
      # bpnrdown <- bprowsdown()
      # mfnrdown <- mfrowsdown()
      # ccnrdown <- ccrowsdown()
      # variablepca <- variables()
      # samplecluster <- samplename()
      # gseanr <- gsearow()
      # bpnrall <- bprowsall()
      # mfnrall <- mfrowsall()
      # ccnrall <- ccrowsall()
      # if(is.null(gseanr)){gseanr <- c(1)}
      # if(is.null(nrup)){ nrup <- ( if( dim(kggDT$up)[1]<10) 1:dim(kggDT$up)[1] else c(1:10) ) }
      # if(is.null(nrall)){ nrall <-  ( if( dim(kggDT$all)[1]<10) 1:dim(kggDT$all)[1] else c(1:10) ) }
      # if(is.null(nrdown)){ nrdown <- ( if( dim(kggDT$down)[1]<10) 1:dim(kggDT$down)[1] else c(1:10) ) }
      # 
      # if(is.null(ccnrup)){ ccnrup <- ( if (dim(goDT$up[goDT$up$Ont=="CC", ])[1]<10)
      #                                        1:dim(goDT$up[goDT$up$Ont=="CC", ])[1] else c(1:10) ) }
      # if(is.null(mfnrup)){ mfnrup <- ( if (dim(goDT$up[goDT$up$Ont=="MF", ])[1]<10)
      #                                        1:dim(goDT$up[goDT$up$Ont=="MF", ])[1] else c(1:10) ) }
      # if(is.null(bpnrup)){ bpnrup <- ( if (dim(goDT$up[goDT$up$Ont=="BP", ])[1]<10)
      #                                        1:dim(goDT$up[goDT$up$Ont=="BP", ])[1] else c(1:10) )}
      # 
      # if(is.null(ccnrdown)){ccnrdown <- ( if (dim(goDT$down[goDT$down$Ont=="CC", ])[1]<10)
      #                                         1:dim(goDT$down[goDT$down$Ont=="CC", ])[1] else c(1:10) ) }
      # if(is.null(mfnrdown)){mfnrdown <- ( if (dim(goDT$down[goDT$down$Ont=="MF", ])[1]<10)
      #                                         1:dim(goDT$down[goDT$down$Ont=="MF", ])[1] else c(1:10) ) }
      # if(is.null(bpnrdown)){bpnrdown <- ( if (dim(goDT$down[goDT$down$Ont=="BP", ])[1]<10)
      #                                         1:dim(goDT$down[goDT$down$Ont=="BP", ])[1] else c(1:10) )}
      # 
      # if(is.null(bpnrall)){bpnrall <- ( if (dim(goDT$all[goDT$all$Ont=="BP", ])[1]<10)
      #                                       1:dim(goDT$all[goDT$all$Ont=="BP", ])[1] else c(1:10)) }
      # if(is.null(mfnrall)){mfnrall <- ( if (dim(goDT$all[goDT$all$Ont=="MF", ])[1]<10)
      #                                       1:dim(goDT$all[goDT$all$Ont=="MF", ])[1] else c(1:10)) }
      # if(is.null(ccnrall)){ccnrall <- ( if (dim(goDT$all[goDT$all$Ont=="CC", ])[1]<10)
      #                                       1:dim(goDT$all[goDT$all$Ont=="CC", ])[1] else c(1:10))}
      # 
      # if(is.null(variablepca)){variablepca=NULL}
      # if(is.null(samplecluster)){samplecluster=NULL}
      # params <- list(nrup=nrup, nrdown=nrdown, bpnrup=bpnrup, bpnrdown=bpnrdown,
      #                mfnrup=mfnrup, mfnrdown=mfnrdown, ccnrup=ccnrup, ccnrdown=ccnrdown,
      #                variablepca=variablepca, samplecluster=samplecluster, tempdir =tempdir(),
      #                gseanr=gseanr, gene = gene(), numheatmap=numheatmap(), nrall = nrall,
      #                bpnrall=bpnrall, mfnrall=mfnrall, ccnrall=ccnrall,
      #                explainPreview=explainPreview(), biologicalText=biologicalText(),
      #                keggAllText = keggAllText(), GSEAText = GSEAText(), deseq = datos$dds,
      #                kggAll = kgg$all, kggUp = kgg$up, kggDown = kgg$down,
      #                kggDTall = kggDT$all, kggDTup = kggDT$up, kggDTdown = kggDT$down,
      #                goAll = go$all, goDTall = goDT$all, goUp=go$up, goDTup = goDT$up,
      #                goDown = go$down, goDTdown = goDT$down, gsea = gsea$gsea)
      # 
      # 


library(shiny)
library(shinyWidgets)

shinyApp(
  ui = basicPage(
    actionButton("show", "Show modal dialog"),
    verbatimTextOutput("print")
  ),
  
  server = function(input, output) {
    
    vals <- reactiveValues()
    
    # Create modal
    popupModal <- function() {
      modalDialog(
          title = "Report configuration",
          size = "l",
          fluidRow(column(width=11,
          tabsetPanel(
              tabPanel("Preview",
                       checkboxGroupButtons(
                           size="sm",
                           individual = TRUE,
                           inputId = "modalPreview",
                           label = "Select preview elements to report",
                           choices = c("PCA", "BoxPlot", "Heatmap", "Cluster","Top6",
                                       "Top1", "Volcano","MA"),
                           status = "primary",
                           checkIcon = list(
                               yes = icon("ok",
                                          lib = "glyphicon"),
                               no = icon("remove",
                                         lib = "glyphicon")
                           )
                       )
                    ),
              tabPanel("Kegg",
                       checkboxGroupButtons(
                           size = "sm",
                           individual = TRUE,
                           inputId = "modalkeggAll",
                           label = "Select elements to report Kegg All",
                           choices = c("Table", "Barplot", "Chorplot", "Dotplot", "Heatmap", "Netplot"),
                           status = "primary",
                           checkIcon = list(
                               yes = icon("ok",
                                          lib = "glyphicon"),
                               no = icon("remove",
                                         lib = "glyphicon")
                           )
                       ),
                       checkboxGroupButtons(
                           size = "sm",
                           individual = TRUE,
                           inputId = "modalkeggUp",
                           label = "Select elements to report Kegg Up",
                           choices = c("Table", "Barplot", "Chorplot", "Dotplot", "Heatmap", "Netplot"),
                           status = "primary",
                           checkIcon = list(
                               yes = icon("ok",
                                          lib = "glyphicon"),
                               no = icon("remove",
                                         lib = "glyphicon")
                           )
                       ),
                       checkboxGroupButtons(
                           size = "sm",
                           individual = TRUE,
                           inputId = "modalkeggDown",
                           label = "Select elements to report Kegg Down",
                           choices = c("Table", "Barplot", "Chorplot", "Dotplot", "Heatmap", "Netplot"),
                           status = "primary",
                           checkIcon = list(
                               yes = icon("ok",
                                          lib = "glyphicon"),
                               no = icon("remove",
                                         lib = "glyphicon")
                           )
                       )
                       ), # fin tabpanel KEGG
              tabPanel("GO",
                    checkboxGroupButtons(
                           size = "sm",
                           individual = TRUE,
                           inputId = "modalGOAll",
                           label = "Select elements to report GO All",
                           choices = c("Table", "Barplot", "Dotplot", "GObarplot", "GOcircleplot"),
                           status = "primary",
                           checkIcon = list(
                               yes = icon("ok",
                                          lib = "glyphicon"),
                               no = icon("remove",
                                         lib = "glyphicon")
                           )
                       ),
                    checkboxGroupButtons(
                           size = "sm",
                           individual = TRUE,
                           inputId = "modalGOUp",
                           label = "Select elements to report GO Up",
                           choices = c("Table", "Barplot", "Dotplot", "GObarplot", "GOcircleplot"),
                           status = "primary",
                           checkIcon = list(
                               yes = icon("ok",
                                          lib = "glyphicon"),
                               no = icon("remove",
                                         lib = "glyphicon")
                           )
                       ),
                    checkboxGroupButtons(
                           size = "sm",
                           individual = TRUE,
                           inputId = "modalGODown",
                           label = "Select elements to report GO Down",
                           choices = c("Table", "Barplot", "Dotplot", "GObarplot", "GOcircleplot"),
                           status = "primary",
                           checkIcon = list(
                               yes = icon("ok",
                                          lib = "glyphicon"),
                               no = icon("remove",
                                         lib = "glyphicon")
                           )
                       )
              ), #fin tabpanel GO
              tabPanel("GSEA",
                  checkboxGroupButtons(
                           size = "sm",
                           individual = TRUE,
                           inputId = "modalGSEA",
                           label = "Select elements to report GSEA",
                           choices = c("Table", "GSEA plot"),
                           status = "primary",
                           checkIcon = list(
                               yes = icon("ok",
                                          lib = "glyphicon"),
                               no = icon("remove",
                                         lib = "glyphicon")
                           )
                       )
              ) #fin de tabpanel GSEA
                       ) # fin tabsetpanel
          )
      ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      )
    }
    
    # Show modal when button is clicked.
    observeEvent(input$show, {
      showModal(popupModal())
    })
    
    observeEvent(input$ok, {
        vals$preview <- input$modalPreview
        vals$keggAll <- input$modalKeggAll
        vals$keggUp <- input$modalKeggUp
        vals$keggDown <- input$modalKeggDown
        vals$GOAll <- input$modalGOAll
        vals$GOUp <- input$modalGOUp
        vals$GODown <- input$modalGODown
        vals$GSEA <- input$modalGSEA
        removeModal()
    })
    
    # Print inputted text
    output$print <- renderPrint({
        kk <- list()
        kk$gsea <- vals$GSEA
        kk$preview <- vals$preview
        print(kk)
    })
  }
)