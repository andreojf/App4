library(shiny)
library(magrittr)
shinyApp(
  ui = fluidPage(
    sliderInput("x", "x", 1, 10, 5),
    sliderInput("y", "y", 1, 10, 5),
    div("x * y: "),
    verbatimTextOutput("txt")
  ),
  server = function(input, output) {
    r <- reactive({
      # The value expression is an _expensive_ computation
      message("Doing expensive computation...")
      Sys.sleep(2)
      input$x * input$y
    }) %>%
      bindCache(input$x, input$y)
    
    output$txt <- renderText(r())
  }
)