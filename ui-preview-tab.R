fluidPage(
    fluidRow(infoBoxOutput("allbox"),
             infoBoxOutput("upbox"),
             infoBoxOutput("downbox")
             ),
    # fin fluidrow boxinfos
    fluidRow(column(
        width = 3,
        offset = 1,
        box(
            title = "Cutoff values",
            solidHeader = FALSE,
            status = "primary",
            width = NULL,
            uiOutput("logfc"),
            tags$br(),
            uiOutput("padj"),
            tags$br(),
            strong("Click to compute enrichment"),
            tags$br(),
            actionButton("runEnrich", "Apply values")
        )
    ),
    column(
        width = 6,
        offset = 1,
        tabBox(
            width = 12,
            # title = "Biological context",
            # solidHeader = FALSE,
            # status = "primary",
            # width = 12,
            tabPanel(
                title = "Biological Context",
                circleButton(
                    inputId = "info1",
                    icon = icon("info"),
                    size = "xs",
                    status = "primary"
                ),
                bsTooltip(
                    "info1",
                    "Enter free text explaining biological experiment",
                    trigger = "hover",
                    placement = "right"
                ),
                textAreaInput(
                    "biologicalText",
                    label = NULL,
                    resize = NULL
                    #width = "100%",
                    #height = "258px"
                )
            ),
            # tabpanel
            tabPanel(
                title = "Background experiment",
                textAreaInput(
                    "explainPreview",
                    label = NULL,
                    resize = NULL
                    #width = "100%",
                    #height = "258px"
                ) #texarea
            ) #tabpanel
        ) #tabbox
    ) #column
    ), 
             fluidRow( column(width = 9, offset = 2,
                 tabBox(width = 12,
                     tabPanel(
                         title = "Samples info Coldata",
                         DTOutput("samples")
                     ),
                     tabPanel(
                         title = "Expression preview",
                         DTOutput("preview")
                     )
                 )
             ) ), # fin column y tabbox
    
             fluidRow(
                 column(width = 3,
                box(
                    title = "Conditions and Variables",
                    width = NULL,
                    uiOutput("sampleGroup"),
                    uiOutput("samplesName"))
             ),
             column(width = 8,
                             tabBox( width = 12,
                                 tabPanel(
                                     title = "PCA plot",
                                     plotOutput("pca", width = "100%", height = "800px")),
                                 tabPanel(
                                     title = "Top genes",
                                     plotOutput("top6", width = "100%", height = "800px")
                                     )
                                 )
                             )
                      ), 
  fluidRow(
      column(
     width = 8,
     offset = 3,
     tabBox(
         width = 12,
         title = "",
         tabPanel(title = "Heatmap",
                  tagList(fluidRow(
                      column(
                          width = 3,
                          sliderInput(
                              "numheatmap",
                              label = "Select number of genes",
                              min = 5,
                              max = 40,
                              value = 20,
                              step = 1
                          )
                      ),
                      column(width = 9,
                             plotOutput("heat", height = "725px"))
                  ))),
         tabPanel(title = "Cluster",
                  tagList(fluidRow(column(width = 9, offset = 3,
                  plotOutput("cluster", height = "725px")
                  )))
                  ),
         tabPanel(title = "Volcano plot",
                  tagList(fluidRow(column(width = 9, offset = 3,
                  plotOutput("volcano", height = "725px")
                  )))
                  ),
         tabPanel(title = "MA plot",
                  tagList(fluidRow(column(width = 9, offset = 3,
                  plotOutput("MA", height = "725px")
                  )))
                  )
 )))
) # fin page