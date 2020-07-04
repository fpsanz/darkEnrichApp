fluidPage(
    fluidRow(
        column(width = 4,
               box(width = 12,
                uiOutput("testVariable"),
                htmlOutput("textTestAlgorithm"),
                uiOutput("testAlgorithm"),
                uiOutput("testButton")
               )
               ),
        column(width = 8,
               box(width = 12, 
                   title = "Sample table",
                   DTOutput("coldataTable"))
               )),
    fluidRow(
        column(width = 12,
               box(width = NULL,
                   title = "Expression matrix (first 10 rows)",
                   div(style = 'overflow-x: scroll', DT::dataTableOutput("expressionTable")) )
               )
    )
)