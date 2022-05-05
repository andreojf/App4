# NORMALITE DES RESIDUS ----
resNORMALITYUI <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(6, plotlyOutput(ns("plot"))),
      column(6,
             fluidRow(
               column(12, verbatimTextOutput(ns("test")))
             ),
             fluidRow(
               column(12, h3(textOutput(ns("status"))))
             ))
    )
  )
}

resNORMALITYServer <- function(id, model){
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      residus <- reactive({
        model()$residuals
      })
      
      test <- reactive({
        tseries::jarque.bera.test(residus())
      })
      
      # test de jarque bera
      output$test <- renderPrint({
        test()
      })
      
      # plot de la distribution des résidus
      output$plot <- renderPlotly({
        distplot(residus())
      })
      
      # interpretation
      # H0: les donnees suivent une loi normale
      # H1: les donnees ne suivent pas une loi normale
      # DECISION: si p_value <= 5%, on rejette H0 alors les donnees ne sont pas normale
      output$status <- renderText({
        
        if (test()$p.value > 0.05){
          paste("SUCCES !!!")
        } else {
          paste("ECHEC")
        }
      })
      
      
    }
  )
}


# AUTOCORRELATION DES RESIDUS ----
resAUTOCORRELATIONUI <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(6, plotOutput(ns("plot"))),
      column(6,
             fluidRow(
               column(12, verbatimTextOutput(ns("test")))
             ),
             fluidRow(
               column(12, h3(textOutput(ns("status"))))
             ))
    )
  )
}

resAUTOCORRELATIONServer <- function(id, model){
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      residus <- reactive({
        model()$residuals
      })
      
      # TEST DE BOX-PIERCE
      test <- reactive({
        Box.test(residus(), type = "Box-Pierce")
      })
      
      # plot de l'autocorrelation
      output$plot <- renderPlot({
        acf(residus(), main="Correlogramme des résidus")
      })
      
      # AFFICHAGE DU TEXTE
      output$test <- renderPrint({
        test()
      })
      
      
      # interpretation
      # H0: les residus sont indépendants : p0 = p1 = ... = pn = 0
      # H1: les residus ne sont pas indépendemment distribuées, il existe une correlation. 
      # Il existe pi non significativement égal à 0.
      # DECISION: si p_value <= 5%, on rejette H0 alors les donnees ne sont pas normale
      output$status <- renderText({
        
        if (test()$p.value > 0.05){
          paste("SUCCES !!!")
        } else {
          paste("ECHEC")
        }
      })
      
    }
  )
}


# HOMOSCEDASTICITY DES RESIDUS ----
resHOMOSCEDASTICITYUI <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(6, plotlyOutput(ns("plot")),
             helpText("S'il existe des pics qui sortent de l'intervalle à l'exception du premier pic, 
                      alors cela laisse traduire une hétéroscédasticité des résidus autrement dit la variance 
                      des résidus n'est pas constante.")),
      column(6,
             fluidRow(
               column(12, verbatimTextOutput(ns("test")))
             ),
             fluidRow(
               column(12, h3(textOutput(ns("status"))))
             ))
    )
  )
}

resHOMOSCEDASTICITYServer <- function(id, model){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      residus <- reactive({
        model()$residuals
      })
      
      test <- reactive({
        Box.test(residus()^2, type = "Box-Pierce")
      })
      
      # plot de l'autocorrelation DES RESIDUS AU CARRE
      output$plot <- renderPlot({
        acf(residus()^2, main="Correlogramme des résidus au carré")
      })
      
      # AFFICHAGE DU TEST
      output$test <- renderPrint({
        test()
      })
      
      
      # interpretation
      # H0: la variance des résidus est constante (homoscédasticité)
      # H1: la variance des résidus n'est pas constante (hétéroscédasticité)
      # DECISION: si p_value <= 5%, on rejette H0, il y a hétéroscédasticité
      output$status <- renderText({
        
        if (test()$p.value > 0.05){
          paste("SUCCES !!!")
        } else {
          paste("ECHEC")
        }
      })
    }
  )
}