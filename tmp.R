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
        validate(need(vals$GSEA,""))
        kk <- list()
        kk$gsea <- vals$GSEA
        kk$preview <- vals$preview
        print(kk)
    })
  }
)