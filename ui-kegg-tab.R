fluidPage(
    tabsetPanel(
        tabPanel( "All DE genes",          # pestaña All
            tags$br(),
            fluidRow(  # primera fila
                column( width = 3,
                    box(title = "Kegg description",
                        circleButton(
                            inputId = "information8",
                            icon = icon("info"),
                            size = "xs",
                            status = "primary"
                        ),
                        bsTooltip(
                            "information8",
                            "Enter free text explaining the results obtained here
                            or the data selected",
                            trigger = "hover",
                            placement = "right"
                        ),
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL, 
                        textAreaInput(
                            "keggAllText",
                            label = "Kegg all genes",
                            resize = NULL
                        )
                    ) # fin caja para el texto
                ),
                column( width = 9,
                    box(title = "Table of pathways",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableAll")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( 
                    circleButton(
                        inputId = "information5",
                        icon = icon("info"),
                        size = "xs",
                        status = "primary"
                    ),
                    bsTooltip(
                        "information5",
                        "Please, select rows with the pathways of interest 
                                         in the table to activate all the plots.",
                        trigger = "hover",
                        placement = "left"
                    ),
                    width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                            tabPanel(title = "Barplot",
                                    fluidRow(column(
                                         width = 3,
                                         radioGroupButtons(
                                             inputId = "selectkeggall",
                                             label = "Select bar plot type",
                                             choices = c("Dodge", "Stack", "Opposite"),
                                             selected = "Dodge",
                                             size = "sm",
                                             status = "primary",
                                             checkIcon = list(
                                                 yes = icon("ok",
                                                            lib = "glyphicon"),
                                                 no = icon("remove",
                                                           lib = "glyphicon")
                                             )
                                         )
                                     )), # fin fluidRow, column & radioGroupButtons
                                     fluidRow(class = "text-center",
                                              column(
                                                  align = "center",
                                                  plotlyOutput("keggPlotAll"),
                                                  width = 9
                                              ))),  #barplot
                        tabPanel(title = "Chordplot",
                                 chorddiagOutput("keggChordAll",
                                                 width = "600px", 
                                                 height = "600px") ), #cordplot
                        tabPanel(title = "Dotplot",
                                 plotOutput("keggDotAll")
                                 ), # dotplot
                        tabPanel(title = "Heatmap",
                                 plotOutput("heatmapKeggAll")
                                 ), # heatmap
                        tabPanel(title = "Netplot",
                                 plotOutput("cnetKeggAll")
                                 ) # cnetplot
                        )
                    )
            )
        ), #fin tab all genes
        tabPanel( "Upregulated genes",          # pestaña upregulates
            tags$br(),
            fluidRow(  # primera fila
                column( width = 3,
                    box(title = "Kegg description",
                        circleButton(
                            inputId = "information9",
                            icon = icon("info"),
                            size = "xs",
                            status = "primary"
                        ),
                        bsTooltip(
                            "information9",
                            "Enter free text explaining the results obtained here
                            or the data selected",
                            trigger = "hover",
                            placement = "right"
                        ),
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL, 
                        textAreaInput(
                            "keggUpText",
                            label = "Kegg upregulated genes",
                            resize = NULL
                        )
                    ) # fin caja para el texto
                ),
                column( width = 9,
                    box(title = "Table of pathways",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("table")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( 
                    circleButton(
                        inputId = "information6",
                        icon = icon("info"),
                        size = "xs",
                        status = "primary"
                    ),
                    bsTooltip(
                        "information6",
                        "Please, select rows with the pathways of interest 
                                         in the table to activate all the plots.",
                        trigger = "hover",
                        placement = "left"
                    ),
                    width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                 plotlyOutput("keggPlot")
                                              ),  #barplot
                        tabPanel(title = "Chordplot",
                                 chorddiagOutput("keggChord",
                                                 width = "600px", 
                                                 height = "600px") ), #cordplot
                        tabPanel(title = "Dotplot",
                                 plotOutput("keggDotUp")
                                 ), # dotplot
                        tabPanel(title = "Heatmap",
                                 plotOutput("heatmapKeggUp")
                                 ), # heatmap
                        tabPanel(title = "Netplot",
                                 plotOutput("cnetKeggUp")
                                 ) # cnetplot
                        )
                    )
            )
        ), #fin tab uprergulated genes
        tabPanel( "Downregulated genes",          # pestaña downregulates
            tags$br(),
            fluidRow(  # primera fila
                column( width = 3,
                    box(title = "Kegg description",
                        circleButton(
                            inputId = "information10",
                            icon = icon("info"),
                            size = "xs",
                            status = "primary"
                        ),
                        bsTooltip(
                            "information10",
                            "Enter free text explaining the results obtained here
                            or the data selected",
                            trigger = "hover",
                            placement = "right"
                        ),
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL, 
                        textAreaInput(
                            "keggDownText",
                            label = "Kegg downregulated genes",
                            resize = NULL
                        )
                    ) # fin caja para el texto
                ),
                column( width = 9,
                    box(title = "Table of pathways",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableDown")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( 
                    circleButton(
                        inputId = "information7",
                        icon = icon("info"),
                        size = "xs",
                        status = "primary"
                    ),
                    bsTooltip(
                        "information7",
                        "Please, select rows with the pathways of interest 
                                         in the table to activate all the plots.",
                        trigger = "hover",
                        placement = "left"
                    ),
                    width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                 plotlyOutput("keggPlotDown")
                                              ),  #barplot
                        tabPanel(title = "Chordplot",
                                 chorddiagOutput("keggChordDown",
                                                 width = "600px", 
                                                 height = "600px") ), #cordplot
                        tabPanel(title = "Dotplot",
                                 plotOutput("keggDotDown")
                                 ), # dotplot
                        tabPanel(title = "Heatmap",
                                 plotOutput("heatmapKeggDown")
                                 ), # heatmap
                        tabPanel(title = "Netplot",
                                 plotOutput("cnetKeggDown")
                                 ) # cnetplot
                        )
                    )
            )
        ) #fin tab downRegulated genes
    ) # fin tabsetpanel
) #fin fluidpage    


