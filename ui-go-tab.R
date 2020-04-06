fluidPage(
             tabsetPanel(
               tabPanel("All DE genes",
                        h3("Biological proccess"),
                        fluidRow(column(
                          # table BP
                          width = 8,
                          offset = 2,
                          dataTableOutput("tableBPall")
                        )),
                        hr(),
                        fluidRow(column(
                            width = 4,
                            offset = 2,
                            radioGroupButtons(
                                inputId = "selectbpall",
                                label = "Select bar plot type",
                                choices = c("Dodge", "Stack", "Opposite"),
                                selected = "Dodge",
                                status = "primary",
                                checkIcon = list(
                                    yes = icon("ok",
                                               lib = "glyphicon"),
                                    no = icon("remove",
                                              lib = "glyphicon")
                                )
                            )
                        )), # fin fluidRow, column & radioGroupButtons
                        fluidRow(
                          #plot BP
                          class = "text-center",
                          column(
                            align = "center",
                            offset = 2,
                            plotlyOutput("plotBPall"),
                            width = 8)),
                        hr(),
                        fluidRow(
                          class="text-center",
                          column(
                            align = "center",
                            offset= 2,
                            plotOutput("BPDotall"),
                            width = 8)),
                        h3("Molecular Functions"),
                        fluidRow(column(
                          # table MF
                          width = 8,
                          offset = 2,
                          dataTableOutput("tableMFall")
                        )),
                        hr(),
                        fluidRow(column(
                            width = 4,
                            offset = 2,
                            radioGroupButtons(
                                inputId = "selectmfall",
                                label = "Select bar plot type",
                                choices = c("Dodge", "Stack", "Opposite"),
                                selected = "Dodge",
                                status = "primary",
                                checkIcon = list(
                                    yes = icon("ok",
                                               lib = "glyphicon"),
                                    no = icon("remove",
                                              lib = "glyphicon")
                                )
                            )
                        )), # fin fluidRow, column & radioGroupButtons
                        fluidRow(# plot MF
                          class = "text-center",
                          column( align = "center",offset = 2,
                                  plotlyOutput("plotMFall"),width = 8)
                        ),
                        hr(),
                        fluidRow(
                          class="text-center",
                          column(
                            align = "center",
                            offset= 2,
                            plotOutput("MFDotall"),
                            width = 8
                          )
                        ),
                        h3("Cellular components"),
                        fluidRow( # table CC
                          column(width = 8,offset = 2,dataTableOutput("tableCCall"))
                        ),
                        hr(),
                        fluidRow(column(
                            width = 4,
                            offset = 2,
                            radioGroupButtons(
                                inputId = "selectccall",
                                label = "Select bar plot type",
                                choices = c("Dodge", "Stack", "Opposite"),
                                selected = "Dodge",
                                status = "primary",
                                checkIcon = list(
                                    yes = icon("ok",
                                               lib = "glyphicon"),
                                    no = icon("remove",
                                              lib = "glyphicon")
                                )
                            )
                        )), # fin fluidRow, column & radioGroupButtons
                        fluidRow(# plot CC
                          class = "text-center",
                          column(align = "center",offset = 2,
                                 plotlyOutput("plotCCall"),width = 8)
                        ),
                        hr(),
                        fluidRow(
                          class="text-center",
                          column(
                            align = "center",
                            offset= 2,
                            plotOutput("CCDotall"),
                            width = 8
                          )
                        ) #fin fluidrow
               ),
               tabPanel("Upregulated",
                        h3("Biological proccess"),
                        fluidRow(column(
                          # table BP
                          width = 8,
                          offset = 2,
                          dataTableOutput("tableBP")
                        )),
                        hr(),
                        fluidRow(
                          #plot BP
                          class = "text-center",
                          column(
                            align = "center",
                            offset = 2,
                            plotlyOutput("plotBP"),
                            width = 8)),
                        hr(),
                        fluidRow(
                          class="text-center",
                          column(
                            align = "center",
                            offset= 2,
                            plotOutput("BPDotUp"),
                            width = 8)),
                        h3("Molecular Functions"),
                        fluidRow(column(
                          # table MF
                          width = 8,
                          offset = 2,
                          dataTableOutput("tableMF")
                        )),
                        hr(),
                        fluidRow(# plot MF
                          class = "text-center",
                          column( align = "center",offset = 2,plotlyOutput("plotMF"),width = 8)
                        ),
                        hr(),
                        fluidRow(
                          class="text-center",
                          column(
                            align = "center",
                            offset= 2,
                            plotOutput("MFDotUp"),
                            width = 8
                          )
                        ),
                        h3("Cellular components"),
                        fluidRow( # table CC
                          column(width = 8,offset = 2,dataTableOutput("tableCC"))
                        ),
                        hr(),
                        fluidRow(# plot CC
                          class = "text-center",
                          column(align = "center",offset = 2,plotlyOutput("plotCC"),width = 8)
                        ),
                        hr(),
                        fluidRow(
                          class="text-center",
                          column(
                            align = "center",
                            offset= 2,
                            plotOutput("CCDotUp"),
                            width = 8
                          )
                        ) #fin fluidrow
               ),
               tabPanel("Downregulated",
                        h3("Biological proccess"),
                        fluidRow(column(# table BP
                          width = 8,
                          offset = 2,
                          dataTableOutput("tableBPdown")
                        )),
                        hr(),
                        fluidRow(#plot BP
                          class = "text-center",
                          column(
                            align = "center",
                            offset = 2,
                            plotlyOutput("plotBPdown"),
                            width = 8
                          )
                        ),
                        hr(),
                        fluidRow(
                          class="text-center",
                          column(
                            align = "center",
                            offset= 2,
                            plotOutput("BPDotDown"),
                            width = 8)),
                        h3("Molecular Functions"),
                        fluidRow(column(# table MF
                          width = 8,
                          offset = 2,
                          dataTableOutput("tableMFdown")
                        )),
                        hr(),
                        fluidRow(# plot MF
                          class = "text-center",
                          column( align = "center",offset = 2,plotlyOutput("plotMFdown"),width = 8)
                        ),
                        hr(),
                        fluidRow(
                          class="text-center",
                          column(
                            align = "center",
                            offset= 2,
                            plotOutput("MFDotDown"),
                            width = 8)),
                        h3("Cellular components"),
                        fluidRow( # table CC
                          column(width = 8,offset = 2,dataTableOutput("tableCCdown"))
                        ),
                        hr(),
                        fluidRow(# plot CC
                          class = "text-center",
                          column(align = "center",offset = 2,plotlyOutput("plotCCdown"),width = 8)
                        ),
                        hr(),
                        fluidRow(
                          class="text-center",
                          column(
                            align = "center",
                            offset= 2,
                            plotOutput("CCDotDown"),
                            width = 8)) #fin fluidrow
               )) # tab GO
)#fin fluidpage