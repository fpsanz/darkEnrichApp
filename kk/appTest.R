library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(scales)

choices_brewer2 <- list(
  as.list(rev(brewer_pal(palette = "Blues")(9))),
  as.list(rev(brewer_pal(palette = "Greens")(9))),
  as.list(rev(brewer_pal(palette = "Reds")(9))),
  as.list(rev(brewer_pal(palette = "Oranges")(9))),
  as.list(rev(brewer_pal(palette = "Purples")(9))),
  as.list(rev(brewer_pal(palette = "Greys")(9)))
)

ui <- dashboardPage(
  dashboardHeader(title = "Value boxes"),
  dashboardSidebar(),
  dashboardBody(
 useShinydashboard(),
      spectrumInput(
            inputId = "paste0(niveles[x])",
            label = 'paste0("Select ",niveles[x]," color")',
            selected = "red",
            choices = list("black","red"),
            options = list(`toggle-palette-more-text` = "Show more")
            )

  )
)

server <- function(input, output) {
  output$colores <- renderUI({
    # niveles=2
    # l1 <- rep(1:6, times = niveles / 6 , length.out = niveles)
    #   l2 <- rep(1:9, each = 6, length.out = niveles)
      # selectores <- lapply(seq_len(niveles), function(x){
          spectrumInput(
            inputId = "paste0(niveles[x])",
            label = 'paste0("Select ",niveles[x]," color")',
            selected = "red",
            choices = list("black","red"),
            options = list(`toggle-palette-more-text` = "Show more")
            )
      # })
  })
 


}

shinyApp(ui, server)