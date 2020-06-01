    ui <- fluidPage(
            numericInput("obs_numeric", "Number of observations", min = 0, max = 500, value = 500),
            sliderInput("obs", "Number of observations:",
                        min = 0, max = 1000, value = 500
            ),
            plotOutput("distPlot")
    )

    # Server logic
    server <- function(input, output, session) {
            output$distPlot <- renderPlot({
                    hist(rnorm(input$obs))
            })
            observeEvent(input$obs, {
                    updateNumericInput(session, "obs_numeric", value = input$obs)
            })
            observeEvent(input$obs_numeric, {
                    updateSliderInput(session, "obs",
                                      value = input$obs_numeric)
            })
    }

    # Complete app with UI and server components
    shinyApp(ui, server)
