library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(scales)


spectrumInp <- function (inputId, label, choices = NULL, selected = NULL, flat = FALSE, 
                          options = list(), update_on = c("move", "dragstop", "change"), 
                          width = NULL) 
{
    selected <- shiny::restoreInput(id = inputId, default = selected)
    update_on <- match.arg(update_on)
    selected <- if (is.null(selected)) {
        unlist(choices, recursive = TRUE)[1]
    }
    else {
        as.character(selected)
    }
    if (length(options) > 0) {
        if (any(names(options) %in% "")) {
            stop("All 'options' must be named", call. = FALSE)
        }
        names(options) <- paste0("data-", names(options))
    }
    spectrumProps <- shinyWidgets:::dropNulls(list(type = "text", id = inputId, 
                                    class = "form-control sw-spectrum", `data-flat` = flat, 
                                    `data-color` = selected, `data-palette` = if (!is.null(choices)) jsonlite::toJSON(choices, 
                                                                                                                      auto_unbox = TRUE), `data-toggle-palette-only` = !is.null(choices), 
                                    `data-show-palette-only` = !is.null(choices), `data-show-palette` = !is.null(choices), 
                                    `data-replacer-class-name` = "sw-spectrum btn-spectrum", 
                                    `data-container-class-name` = "sw-spectrum", `data-update-on` = update_on))
    spectrumProps <- utils::modifyList(x = spectrumProps, val = options)
    spectrumProps <- lapply(spectrumProps, function(x) {
        if (identical(x, TRUE)) 
            "true"
        else if (identical(x, FALSE)) 
            "false"
        else x
    })
    spectrumTag <- htmltools::tags$div(class = "form-group shiny-input-container", 
                                       class = if (flat) 
                                           "shiny-input-container-inline", style = if (!is.null(width)) 
                                               paste0("width: ", htmltools::validateCssUnit(width), 
                                                      ";"), htmltools::tags$label(label, `for` = inputId), 
                                       if (flat) 
                                           htmltools::tags$br(), do.call(htmltools::tags$input, 
                                                                         spectrumProps))
    shinyWidgets:::attachShinyWidgetsDep(spectrumTag, "spectrum")
}


hoices_brewer2 <- list(
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
    htmlOutput("kk"),
    spectrumInp(
            inputId = "paste0(niveles[x])",
            label = 'paste0("Select ",niveles[x]," color")',
            selected = NULL,
            choices = c("black","red"),
            options = list(`toggle-palette-more-text` = "Show more")
            )
  )
)

server <- function(input, output) {
  output$kk <- renderUI({
      spectrumInp(
          inputId = "qq",
          label = '',
          selected = NULL,
          choices = list("black","red")
      )
  })
 


}

shinyApp(ui, server)