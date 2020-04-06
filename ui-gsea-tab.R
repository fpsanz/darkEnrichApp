fluidPage(
            h3("Gene Set Enrichment Analysis"),
            fluidRow(column(
              width = 8,
              offset = 2,
              dataTableOutput("gseaTable")
            )),
            hr(),
            fluidRow(column(
              width = 8,
              offset = 2,
              plotOutput("gseaPlot")
            ))
) #fin fluidpage