DQUI <- function(id){
  
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             h4("Nombre d'observations"),
                textOutput(ns("nrows"))),
      column(3,
             h4("Nombre de variable"),
                textOutput(ns("ncols"))),
      column(3,
             h4("Valeurs manquantes"),
                textOutput(ns("nmiss"))),
      column(3,
             h4("Fenetre d'etude"),
                textOutput(ns("window")))
    ),
    hr(),
    fluidRow(
      column(9,
             actionButton(ns("saveDQ"), "Sauvegarder mes travaux"))
    ),
    hr(),
    fluidRow(
      column(12, dataTableOutput(ns("resume")))
    )
  )
}

DQServer <- function(id, data){
  moduleServer(
    id,
    function(input, output, session){
      
      output$nrows <- renderText({
        paste(nrow(data()))
      })
      
      output$ncols <- renderText({
        paste(ncol(data()))
      })
      
      output$nmiss <- renderText({
        nmiss <- sum(is.na(data()))
        txmiss <- nmiss / (nrow(data())*ncol(data()))
        paste(nmiss, "(",100*round(txmiss,4),"%)", sep = "")
      })
      
      output$window <- renderText({
        paste(c(min(data()$Date), max(data()$Date)), collapse = " to ")
      })
    }
  )
}