selectionUI <- function(id){
  
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, 
             actionButton(ns("btnselectvar"), "Lancer le processus de selection"),
             helpText("NB: Le processus de selection repose essentiellement sur une analyse de correlation. 
                                            Toutefois, l'utilisateur peut se passer de cette etape en l'absence de variables explicatives."),
             helpText("Aide: lorsque deux variables sont fortement correlees, 
                      prioriser celle qui est la plus correlée avec la variable target.")
      )
    ),
    hr(),
    h4("Contribution de chaque variable par rapport a la variable cible"),
    fluidRow(
      column(3, selectInput(ns("y"), "Target:",
                            choices = c(""))),
      column(3, selectInput(ns("x"), "Explicatives",
                            choices = c(""), multiple = T)),
      column(3, selectInput(ns("method"),"Methode",
                            choices = c("pearson","kendall","spearman")))
    ),
    br(),
    fluidRow(
      column(6, plotlyOutput(ns("plot1"))),
      column(6, tableOutput(ns("data1")))
    ),
    hr(),
    h4("Correlation entre les variables explicatives"),
    br(),
    fluidRow(
      column(12, tableOutput(ns("data2")))
    )
  )
}

selectionServer <- function(id, data){
  moduleServer(
    id,
    function(input, output, session){
      
      # update des champs de selection
      observe({
        updateSelectInput(session, "y", 
                          choices = names(data()))
        updateSelectInput(session, "x", 
                          choices = names(data()))
        
      })
      
      # 
      output$data1 <- renderTable({
        data() %>% 
          corrY(c(input$x), input$y, method = input$method)
      }, rownames = T)
      
      
      output$plot1 <- renderPlotly({
        data() %>% 
          corrYplot(c(input$x), input$y, method = input$method)
      })
      
      output$data2 <- renderTable({
        data() %>% 
          corrX(c(input$x), input$y, method = input$method)
      }, rownames = T)
      
    }
  )
}