fluidPage(
    h3("Gene Set Enrichment Analysis"),
    fluidRow(
      column(width = 3,
             box(title = "GSEA description",
                 circleButton(
                   inputId = "information20",
                   icon = icon("info"),
                   size = "xs",
                   status = "primary"
                 ),
                 bsTooltip(
                   "information20",
                   "Enter free text explaining the results obtained here
                            or the data selected",
                   trigger = "hover",
                   placement = "right"
                 ),
                 solidHeader = FALSE,
                 status = "primary",
                 width = NULL, 
                 textAreaInput(
                   "GSEAText",
                   label = "",
                   resize = NULL )
                 )
             ),
      column(width = 9,
             box(title = "Table of GSEA pathways",
                 solidHeader = FALSE,
                 status = "primary",
                 width = NULL,
                 DTOutput("gseaTable")
                 )
        )
      ),
    fluidRow(column( 
        circleButton(
          inputId = "information21",
          icon = icon("info"),
          size = "xs",
          status = "primary"
        ),
        bsTooltip(
          "information21",
          "Select up to 3 pathways at the same time.",
          trigger = "hover",
          placement = "left"
        ),
        width = 9, offset = 3,
        box(title = "GSEA plot",
            
                  solidHeader = FALSE,
                  status = "primary",
                  width = NULL,
                  plotOutput("gseaPlot")
              )
      )
    )
)

