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
                    resize = NULL,
                    width = "710px",
                    height = "258px"
                )
            ),
            # tabpanel
            tabPanel(
                title = "Background experiment",
                textAreaInput(
                    "explainPreview",
                    label = NULL,
                    resize = NULL,
                    width = "710px",
                    height = "258px"
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
    
             fluidRow(column(width = 3,
                box(
                    title = "Conditions and Variables",
                    width = NULL,
                    uiOutput("sampleGroup"),
                    uiOutput("samplesName"))
             ),
             column(width = 9,
                             tabBox( width = 12,
                                 tabPanel(
                                     title = "PCA plot",
                                     plotOutput("pca")),
                                 tabPanel(
                                     title = "Top genes",
                                     plotOutput("top6")
                                     )
                                 )
                             )
                      ), 


    column(
        width = 4,
        offset = 6,
        sliderInput(
            "numheatmap",
            label = "Select number of genes",
            min = 5,
            max = 40,
            value = 20,
            step = 1
        )
    ),
    column(width = 6,
           plotOutput("cluster", height = "600px")),
    column(width = 6,
           plotOutput("heat", height = "600px")),
    hr(),
    column(width = 6,
           plotOutput("volcano", height = "600px")),
    column(width = 6,
           plotOutput("MA", height = "600px"))
) # fin page