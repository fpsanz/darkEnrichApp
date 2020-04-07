fluidPage(
    tabsetPanel(
        tabPanel( "All DE genes",          # pestaña All
            tags$br(),
            fluidRow(  # primera fila
                column( width = 3,
                    box(title = "Kegg description",
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
                column( width = 9, offset = 3,
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
                                                 width = "500px", 
                                                 height = "500px") ), #cordplot
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
                column( width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                  plotlyOutput("keggPlot")
                                              ),  #barplot
                        tabPanel(title = "Chordplot",
                                 chorddiagOutput("keggChord",
                                                 width = "500px", 
                                                 height = "500px") ), #cordplot
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
                column( width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                  plotlyOutput("keggPlotDown")
                                              ),  #barplot
                        tabPanel(title = "Chordplot",
                                 chorddiagOutput("keggChordDown",
                                                 width = "500px", 
                                                 height = "500px") ), #cordplot
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


