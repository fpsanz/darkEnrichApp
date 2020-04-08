fluidPage(
    fluidRow(infoBoxOutput("allbox"),
             infoBoxOutput("upbox"),
             infoBoxOutput("downbox")
             ),
    # fin fluidrow boxinfos
    fluidRow(column(
      circleButton(
        inputId = "information1",
        icon = icon("info"),
        size = "xs",
        status = "primary"
      ),
      bsTooltip(
        "information1",
        "Customize here the statistical values that you 
        intend to apply to your experiment as a cutoff
        to consider a gene differentially expressed. The
        numbers per category can be check above. 
        Click on 'Apply values' once you have finished
        exploring this tab and before moving to the next one.",
        trigger = "hover",
        placement = "right"
      ),
        width = 4,
        offset = 1,
        box(
            align = 'center',
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
        width = 5,
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
                    "Enter free text explaining biological experiment.",
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
                title = "Experiment results / Statistics",
                circleButton(
                  inputId = "info2",
                  icon = icon("info"),
                  size = "xs",
                  status = "primary"
                ),
                bsTooltip(
                  "info2",
                  "Enter free text explaining the results obtained here
                  or the statistical value selected.",
                  trigger = "hover",
                  placement = "right"
                ),
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
             fluidRow( column(width = 12,
                 tabBox(width = 12,
                     tabPanel(
                         title = "Samples information (Coldata)",
                         DTOutput("samples")
                     ),
                     tabPanel(
                         title = "Statistical expression values",
                         DTOutput("preview")
                     )
                 )
             ) ), # fin column y tabbox
    
             fluidRow(column(
               circleButton(
               inputId = "information2",
               icon = icon("info"),
               size = "xs",
               status = "primary"
             ),
             bsTooltip(
               "information2",
               "Choose here the variables from your 'Coldata' that will 
               help to represent the data graphically 
               and explore all the conditions properly.",
               trigger = "hover",
               placement = "left"
             ),
                width = 3,
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
                                     circleButton(
                                       inputId = "information3",
                                       icon = icon("info"),
                                       size = "xs",
                                       status = "primary"
                                     ),
                                     bsTooltip(
                                       "information3",
                                       "Up to two conditions may be accepted for PCA 2D. 
                                       The first variable
                                       will be represented with different colours 
                                       and the second will correspond to the dot shape.
                                       For PCA 3D only the first one is taken.",
                                       trigger = "hover",
                                       placement = "right"
                                     ),
                                     tagList(fluidRow(
                                     column(width=3,
                                        materialSwitch(inputId = "pca3d", label = "PCA 3D",
                                                       status = "primary")
                                            ),
                                     column(width=9,
                                        uiOutput("pca3", height = "800px")
                                        #plotOutput("pca", width = "100%", height = "800px"),
                                        #rglwidgetOutput("pca3d", width = "100%", height = "800px")
                                     )
                                     ))),
                                 tabPanel(title = "Heatmap",
                                          circleButton(
                                            inputId = "information4",
                                            icon = icon("info"),
                                            size = "xs",
                                            status = "primary"
                                          ),
                                          bsTooltip(
                                            "information4",
                                            "Up to two conditions may be accepted for the heatmap. 
                                            Every variable will be represented as a line on top 
                                            of the plot with different colors showing the
                                            distincts condition per sample.",
                                            trigger = "hover",
                                            placement = "right"
                                          ),
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
                                                   plotOutput("heat", height = "800px"))
                                          ))),
                                 tabPanel(
                                      title = "Cluster",
                                      plotOutput("cluster", width = "100%", height = "800px")
                                 ),
                                 tabPanel(
                                     title = "Top genes",
                                     plotOutput("top6", width = "100%", height = "800px")
                                     )
                                 )
                             )
                      ), 
  fluidRow(
      column(
     width = 9,
     offset = 3,
     tabBox(
         width = 12,
         title = "",
         tabPanel(
                title = "Volcano plot",
                plotOutput("volcano", width = "100%", height = "800px")
            ),
         tabPanel(
                title = "MA plot",
                  plotOutput("MA", width = "100%", height = "800px")
            )
 )))
) # fin page
