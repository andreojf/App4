EDAUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, wellPanel(selectInput(ns("plottype"), "Choix graphique",
                                      choices = c("Nuage de points"="scatter",
                                                  "Courbe"="line",
                                                  "Histogram"="hist",
                                                  "Boxplot"="box")))),
      column(3,
             wellPanel(selectInput(ns("xaxis"), "X:",
                                   choices = c(""), selected = ""))),
      column(3,
             conditionalPanel("input.plottype == 'scatter' | input.plottype == 'line'",
                              ns = ns,
                              wellPanel(selectInput(ns("yaxis"), "Y:",
                                                    choices = c(""), selected = ""))))
    ),
    hr(),
    fluidRow(
      column(8, offset = 2, plotlyOutput(ns("graphique"), height = "500px")),
    )
  )
}

EDAServer <- function(id, data){
  moduleServer(
    id,
    function(input, output, session){
      observe({
        req(data())
        updateSelectInput(session, "xaxis",
                          choices = names(data()), selected = "")
        updateSelectInput(session, "yaxis",
                          choices = names(data()), selected = "")
      })
      
      # realiser les graphiques
      output$graphique <- renderPlotly({
        
        
        if (input$plottype == "line"){
          validate(
            need(input$xaxis, "Definir les coordonnees: X"),
            need(input$yaxis, "Definir les coordonnees: Y")
          )
          data() %>%
            EDALine(
              x = input$xaxis,
              y = input$yaxis
            )
        } else if (input$plottype == "scatter"){
          validate(
            need(input$xaxis, "Definir les coordonnees: X"),
            need(input$yaxis, "Definir les coordonnees: Y")
          )
          data() %>%
            EDAScatter(
              x = input$xaxis,
              y = input$yaxis
            )
        } else if (input$plottype == "box"){
          validate(
            need(input$xaxis, "Definir les coordonnees: X"),
          )
          data() %>%
            EDABoxplot(
              y = input$xaxis
            )
        } else {
          validate(
            need(input$xaxis, "Definir les coordonnees: X"),
          )
          data() %>%
            EDAHistogram(
              x = input$xaxis
            )
        }
      })
    }
  )
}