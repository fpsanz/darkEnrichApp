fluidPage(
      tabsetPanel(
## All genes ##########################################
## BP all #######################
        tabPanel( "All DE genes",          # pestaña All
            #tags$br(),
            tabsetPanel(  # tabsetpanel all
                tabPanel("Biological Proccess",
                         fluidRow(# primera fila all
                             column(
                                 width = 3,
                                 box(
                                     title = "GO BP description",
                                     solidHeader = FALSE,
                                     status = "primary",
                                     width = NULL,
                                     textAreaInput("GObpText",
                                                   label = "",
                                                   resize = NULL)
                                 ) # fin caja para el texto
                             ),
                             column(
                                 width = 9,
                                 box(
                                     title = "Biological proccess terms",
                                     solidHeader = FALSE,
                                     status = "primary",
                                     width = NULL,
                                     DTOutput("tableBPall")
                                 ) # caja para la tabla
                             )), 
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                            tabPanel(title = "Barplot",
                                     fluidRow(column(
                                         width = 3,
                                         radioGroupButtons(
                                             inputId = "selectbpall",
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
                                                  plotlyOutput("plotBPall"),
                                                  width = 9
                                              ))),  #barplot
                            tabPanel(title = "Dotplot",
                                     plotOutput("BPDotall")
                                     ) # dotplot
                        )
                    )
            )),
                ### MF all #############################
            #tags$br(),
            tabPanel("Molecular function",  # MF all
             fluidRow(  # primera fila mf all 
                column( width = 3,
                    box(title = "GO MF description",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL, 
                        textAreaInput(
                            "GOmfText",
                            label = "",
                            resize = NULL
                        )
                    ) # fin caja para el texto
                ),
                column( width = 9,
                    box(title = "Molecular function terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableMFall")
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
                                             inputId = "selectmfall",
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
                                                  plotlyOutput("plotMFall"),
                                                  width = 9
                                              ))),  #barplot
                            tabPanel(title = "Dotplot",
                                     plotOutput("MFDotall")
                                     ) # dotplot
                        )
                    )
            )),
                ### CC all #############################
                tabPanel("Cellular component",   ##tab CC all
                fluidRow(  # primera fila all CC
                    column( width = 3,
                        box(title = "GO CC description",
                            solidHeader = FALSE,
                            status = "primary",
                            width = NULL, 
                            textAreaInput(
                                "GOccText",
                                label = "",
                                resize = NULL
                            )
                        ) # fin caja para el texto
                    ),
                column( width = 9,
                    box(title = "Cellular component terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableCCall")
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
                                             inputId = "selectccall",
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
                                                  plotlyOutput("plotCCall"),
                                                  width = 9
                                              ))),  #barplot
                            tabPanel(title = "Dotplot",
                                     plotOutput("CCDotall")
                                     ) # dotplot
                        ) #tabbox
                    ) #column 
            ) #fluidrow
            ) # tabpanel CC 
            ) # tabsetpanel all
        ), #fin tab all genes
###### Upregulated genes #####################################
## BP up #############################
        tabPanel( "Upregulated genes",          # pestaña upregulates
            tabsetPanel(  # tabsetpanel up
             tabPanel("Biological Proccess",
            fluidRow(  # primera fila
                column( width = 3,
                    box(title = "GO BP description",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL, 
                        textAreaInput(
                            "GOBpUpText",
                            label = "",
                            resize = NULL
                        )
                    ) # fin caja para el texto
                ),
                column( width = 9,
                    box(title = "Biological proccess terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableBP")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                  plotlyOutput("plotBP")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 plotOutput("BPDotUp")
                                 ) # dotplot
                        )
                    )
            )),
## MF up #############################
tabPanel("Molecular function",
fluidRow(  # primera fila
                column( width = 3,
                    box(title = "GO MF description",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL, 
                        textAreaInput(
                            "GOMfUpText",
                            label = "",
                            resize = NULL
                        )
                    ) # fin caja para el texto
                ),
                column( width = 9,
                    box(title = "Molecular function terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableMF")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                  plotlyOutput("plotMF")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 plotOutput("MFDotUp")
                                 ) # dotplot
                        )
                    )
            )),
## CC up ###################################
        tabPanel("Cellular component",
            fluidRow(  # primera fila
                        column( width = 3,
                            box(title = "GO CC description",
                                solidHeader = FALSE,
                                status = "primary",
                                width = NULL, 
                                textAreaInput(
                                    "GOCcUpText",
                                    label = "",
                                    resize = NULL
                                )
                            ) # fin caja para el texto
                        ),
                        column( width = 9,
                            box(title = "Cellular component terms",
                                solidHeader = FALSE,
                                status = "primary",
                                width = NULL,
                                DTOutput("tableCC")
                            ) # caja para la tabla
                            )
                        ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                  plotlyOutput("plotCC")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 plotOutput("CCDotUp")
                                 ) # dotplot
                            )
                        )
                    )
        ))), #fin tab upregulated genes
## Down regulated genes ############################################
## BP down ##############################
        tabPanel( "Downregulated genes",          # pestaña downregulates
            tabsetPanel(
                tabPanel( "Biological proccess",
            fluidRow(  # primera fila
                column( width = 3,
                    box(title = "GO BP description",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL, 
                        textAreaInput(
                            "GOBpDownText",
                            label = "",
                            resize = NULL
                        )
                    ) # fin caja para el texto
                ),
                column( width = 9,
                    box(title = "Biological proccess terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableBPdown")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                  plotlyOutput("plotBPdown")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 plotOutput("BPDotDown")
                                 ) # dotplot
                        )
                    )
            )),
## MF down ###################################
        tabPanel( "Molecular function",
            fluidRow(  # primera fila
                column( width = 3,
                    box(title = "GO MF description",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL, 
                        textAreaInput(
                            "GOMfDownText",
                            label = "",
                            resize = NULL
                        )
                    ) # fin caja para el texto
                ),
                column( width = 9,
                    box(title = "Molecular functions terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableMFdown")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                  plotlyOutput("plotMFdown")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 plotOutput("MFDotDown")
                                 ) # dotplot
                        )
                    )
            )),
## CC down ###################################
                tabPanel("Cellular component",
                fluidRow(  # primera fila
                column( width = 3,
                    box(title = "GO CC description",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL, 
                        textAreaInput(
                            "GOCcDownText",
                            label = "",
                            resize = NULL
                        )
                    ) # fin caja para el texto
                ),
                column( width = 9,
                    box(title = "Cellular component terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableCCdown")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                  plotlyOutput("plotCCdown")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 plotOutput("CCDotDown")
                                 ) # dotplot
                        )
                    )
            ))
) # fin tabsetpanel downregulated
        ) #fin tab downrergulated genes
    ) # fin tabsetpanel
)#fin fluidpage