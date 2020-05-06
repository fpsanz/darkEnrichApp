fluidPage(
    fluidRow(infoBoxOutput("allbox", width = 4),
             infoBoxOutput("downbox", width = 4),
             infoBoxOutput("upbox", width = 4)),
    fluidRow(column(width = 2, offset = 3,
                 uiOutput("fcdown")),
             column(width=2,
                 uiOutput("fcup")),
             column(width = 2,
                 uiOutput("pval"))
             ),
    # fin fluidrow boxinfos
    tags$br(),
    fluidRow(
        column(
            width = 4,
            offset = 1, 
            circleButton(
                inputId = "information1",
                icon = icon("info"),
                size = "xs",
                status = "primary"
            ),
            bsTooltip(
                    "information1",
                    paste0("Customize here the statistical values that you intend",
                           " to apply to your experiment as a cutoff to consider a",
                           " gene differentially expressed. The numbers per",
                           " category can be checked above. Click on Apply values",
                           " once you have finished exploring this tab and",
                           " before moving to the next one."),
                    trigger = "hover",
                    placement = "right"
                ),
        box(
            align = 'center',
            title = "Cutoff values",
            solidHeader = FALSE,
            status = "primary",
            width = NULL,
            uiOutput("logfc"),
            tags$br(),
            uiOutput("padj")
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
                  paste0("Enter free text explaining the results obtained here",
                  " or the statistical value selected."),
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
    
             fluidRow(column(width = 3,
               circleButton(
               inputId = "information2",
               icon = icon("info"),
               size = "xs",
               status = "primary"
             ),
             bsTooltip(
               "information2",
               paste0("Choose here the conditions from your Coldata that will ", 
               "help to represent the data graphically ", 
               "and explore all the variables properly."),
               trigger = "hover",
               placement = "right"
             ),
                box(
                    title = "Conditions and Variables",
                    width = NULL,
                    uiOutput("sampleGroup"),
                    uiOutput("colorPalettes"),
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
                                       paste0("Up to two conditions may be accepted for PCA 2D. ", 
                                       "The first one of them ",
                                       "will be represented with different colours ",
                                       "and the second will correspond to the dot shape. ",
                                       "For PCA 3D only the first one is taken."),
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
                                                                  tabPanel(
                                     title = "Box/violin plot",
                                     circleButton(
                                            inputId = "infobox",
                                            icon = icon("info"),
                                            size = "xs",
                                            status = "primary"),
                                          bsTooltip(
                                            "infobox",
                                            paste0("Change here the way you want to represent ",
                                            "the profile of the counts per gene for the normalized samples. ",
                                            "Only the first condition selected will be taken into ",
                                            "account to group the samples."),
                                            trigger = "hover",
                                            placement = "right"
                                          ),
                                     tagList(fluidRow(
                                     column(width=3,
                                        materialSwitch(inputId = "boxplotswitch", label = "Violin plot",
                                                       status = "primary")
                                            ),
                                     column(width=9,
                                        plotlyOutput("boxviolin", width="100%", height = "800px")
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
                                            paste0("Up to two conditions may be accepted for the heatmap. ",
                                            "Every condition will be represented on top ",
                                            "of the plot with different colors showing the ",
                                            "distincts group per sample."),
                                            trigger = "hover",
                                            placement = "right"
                                          ),
                                          #tagList(
                                            fluidRow(
                                            column(
                                              width = 3,
                                              sliderInput(
                                                "numheatmap",
                                                label = "Select number of genes",
                                                min = 5,
                                                max = 120,
                                                value = 20,
                                                step = 1
                                              )
                                            ),
                                            column(width = 9,
                                                   plotlyOutput("heat", height = "800px"))
                                          )#)
                                          ),
                                 tabPanel(
                                      title = "Cluster",
                                      plotlyOutput("cluster", width = "100%", height = "800px")
                                 ),
                                 tabPanel(title = "Top 6 genes",
                                          circleButton(
                                            inputId = "information200",
                                            icon = icon("info"),
                                            size = "xs",
                                            status = "primary"
                                          ),
                                          bsTooltip(
                                            "information200",
                                            paste0("Only the first condition selected will be ",
                                                   "taken into account to group the samples."),
                                            trigger = "hover",
                                            placement = "right"
                                          ),
                                          plotlyOutput("top6", width="100%", height = "800px")),
                                 tabPanel(title = "Top unique gene",
                                          circleButton(
                                            inputId = "information100",
                                            icon = icon("info"),
                                            size = "xs",
                                            status = "primary"
                                          ),
                                          bsTooltip(
                                            "information100",
                                            paste0("Select the individual gene in which you are interested ",
                                                   "in exploring the expression. Only the first condition selected will be ",
                                                   "taken into account to group the samples."),
                                            trigger = "hover",
                                            placement = "right"
                                          ),
                                          tagList(fluidRow(
                                            column(
                                              width = 3,
                                              textInput("gene", value="", label = "Select the ensembl gene of interest"),
                                              htmlOutput("top1text")
                                            ),
                                            column(width = 9,
                                                   plotlyOutput("top1", height = "800px"))
                                          ))),
                                 tabPanel(title = "KaryoPlot",
                                          circleButton(
                                            inputId = "karyoInfo",
                                            icon = icon("info"),
                                            size = "xs",
                                            status = "primary"
                                          ),
                                          bsTooltip(
                                            "karyoInfo",
                                            paste0("Pon lo que te salga del ornotorrinco "),
                                            trigger = "hover",
                                            placement = "right"
                                          ),
                                          tagList(
                                            fluidRow(
                                              column(width=10, offset=1,
                                                     plotOutput("karyoPlot", height = "800px") )))
                                          )
                                 )
                             )
                      ), 
  fluidRow(
      column(width = 3,
             box( title = "Customize plots",
                  width = NULL,
                tagList(
                    tags$p("Volcano & MA plots color scheme"),
             #    spectrumInput(
             #     inputId = "logfcColor",
             #     label = "Pick log FC color:",
             #     selected = '#2ca25f',
             #     width = "60%",
             #     choices = list(
             #         list("#b30000","#e34a33","#fc8d59","#fdbb84","#fdd49e","#fef0d9"),
             #         list('#045a8d','#2b8cbe','#74a9cf','#a6bddb','#d0d1e6','#f1eef6'),
             #         list('#006d2c', '#2ca25f', '#66c2a4', '#99d8c9', '#ccece6','#edf8fb'),
             #         list('#252525', '#636363', '#969696', '#bdbdbd', '#d9d9d9', '#f7f7f7')
             #     ),
             #     options = list(`toggle-palette-more-text` = "Show more")
             # ),
             #              spectrumInput(
             #     inputId = "padjColor",
             #     label = "Pick p-val color:",
             #     selected = '#2b8cbe',
             #     width = "60%",
             #     choices = list(
             #         list("#b30000","#e34a33","#fc8d59","#fdbb84","#fdd49e","#fef0d9"),
             #         list('#045a8d','#2b8cbe','#74a9cf','#a6bddb','#d0d1e6','#f1eef6'),
             #         list('#006d2c', '#2ca25f', '#66c2a4', '#99d8c9', '#ccece6','#edf8fb'),
             #         list('#252525', '#636363', '#969696', '#bdbdbd', '#d9d9d9', '#f7f7f7')
             #     ),
             #     options = list(`toggle-palette-more-text` = "Show more")
             # ),
             #              spectrumInput(
             #     inputId = "bothColor",
             #     label = "Pick logFC & p-pval color:",
             #     selected = "#e34a33",
             #     width = "60%",
             #     choices = list(
             #         list("#b30000","#e34a33","#fc8d59","#fdbb84","#fdd49e","#fef0d9"),
             #         list('#045a8d','#2b8cbe','#74a9cf','#a6bddb','#d0d1e6','#f1eef6'),
             #         list('#006d2c', '#2ca25f', '#66c2a4', '#99d8c9', '#ccece6','#edf8fb'),
             #         list('#252525', '#636363', '#969696', '#bdbdbd', '#d9d9d9', '#f7f7f7')
             #     ),
             #     options = list(`toggle-palette-more-text` = "Show more")
             # ),
             #              spectrumInput(
             #     inputId = "nsColor",
             #     label = "Pick no significant color:",
             #     selected = '#969696',
             #     width = "60%",
             #     choices = list(
             #         list("#b30000","#e34a33","#fc8d59","#fdbb84","#fdd49e","#fef0d9"),
             #         list('#045a8d','#2b8cbe','#74a9cf','#a6bddb','#d0d1e6','#f1eef6'),
             #         list('#006d2c', '#2ca25f', '#66c2a4', '#99d8c9', '#ccece6','#edf8fb'),
             #         list('#252525', '#636363', '#969696', '#bdbdbd', '#d9d9d9', '#f7f7f7')
             #     ),
             #     options = list(`toggle-palette-more-text` = "Show more")
             # ),
             spectrumInput(
                 inputId = "upColor",
                 label = "Pick upregulated color:",
                 selected = "#b30000",
                 width = "60%",
                 choices = list(
                     list("#b30000","#e34a33","#fc8d59","#fdbb84","#fdd49e","#fef0d9"),
                     list('#045a8d','#2b8cbe','#74a9cf','#a6bddb','#d0d1e6','#f1eef6'),
                     list('#006d2c', '#2ca25f', '#66c2a4', '#99d8c9', '#ccece6','#edf8fb'),
                     list('#252525', '#636363', '#969696', '#bdbdbd', '#d9d9d9', '#f7f7f7')
                 ),
                 options = list(`toggle-palette-more-text` = "Show more")
             ),
             spectrumInput(
                 inputId = "downColor",
                 label = "Pick downregulated color:",
                 selected = '#045a8d',
                 width = "60%",
                 choices = list(
                     list("#b30000","#e34a33","#fc8d59","#fdbb84","#fdd49e","#fef0d9"),
                     list('#045a8d','#2b8cbe','#74a9cf','#a6bddb','#d0d1e6','#f1eef6'),
                     list('#006d2c', '#2ca25f', '#66c2a4', '#99d8c9', '#ccece6','#edf8fb'),
                     list('#252525', '#636363', '#969696', '#bdbdbd', '#d9d9d9', '#f7f7f7')
                 ),
                 options = list(`toggle-palette-more-text` = "Show more")
             ),
             uiOutput("geneSelector")
             )
             )
  ),
      column(
     width = 9,
     tabBox(
         width = 12,
         title = "",
         tabPanel(
                title = "Volcano plot",
                plotlyOutput("volcano", width = "100%", height = "800px")
            ),
         tabPanel(
                title = "MA plot",
                  plotOutput("MA", width = "100%", height = "800px")
            )
 ))),
 fluidRow(column(width = 4, offset = 4,
            strong("Click to compute enrichment"),
            tags$br(),
            actionButton("runEnrich", "Apply values", width = "100%")
             ))
) # fin page
