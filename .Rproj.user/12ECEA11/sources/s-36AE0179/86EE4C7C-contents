modelUI <- function(id){
  ns <- NS(id)
  tagList(
    br(), br(),
    fluidRow(
      column(3, selectInput(ns("modelapproche"),"Choix du modèle",
                            choices = c("L’approche structurelle"="structurel",
                                        "L’approche semi structurelle"="semistructurel",
                                        "L’approche en équilibre partiel"="partiel"
                            ))),
      column(3, selectInput(ns("targetvar"), "Choix de la variable cible",
                            choices = c("")), offset = 3)
    ),
    fluidRow(
      column(6,
             conditionalPanel(
               condition="input.modelapproche == 'structurel'", 
               ns = ns,
               helpText("Les modèles structurels désignent des modèles dont la fonction d’écoulement résulte 
                                        d’un raisonnement microéconomique selon lequel le client a un comportement rationnel 
                                        visant à maximiser son intérêt."),
             ),
             
             conditionalPanel(
               condition = "input.modelapproche == 'semistructurel'",
               ns = ns,
               helpText("Les modèles semi structurelle qui est basée sur l’étude des propriétés stochastiques des séries par la méthode de BOX et JENKINS"),
             ),
             
             conditionalPanel(
               condition = "input.modelapproche == 'partiel'",
               ns = ns,
               helpText("L’approche en équilibre partiel est basée sur 
                                                 l’utilisation d’un modèle économétrique avec des variables dépendantes qui reflètent l’activité économique 
                                                 et le comportement individuel des agents économiques en termes de consommation d’investissement, etc.")
             ),
             
             conditionalPanel("input.modelapproche == 'partiel'",
                              ns = ns,
                              selectizeInput("modelchoice",NULL,
                                             choices = c("Modèle de Selvaggio"="selvaggio",
                                                         "Modèle de Denis Dupré"="dupre",
                                                         "Modèle de Jarrow et Van Deventer [1998]"="jarvanter"
                                             ))),
             
             actionButton(ns("help"), "Aide technique", icon = icon("question-sign", lib = "glyphicon"))
      ),
      column(6, 
             selectInput(ns("Xvar"), "Choix des variables explicatives",
                         choices = c(""), multiple = T))
    ),
    hr(),
    
    fluidRow(
       conditionalPanel(
         condition = "input.modelapproche == 'semistructurel'",
         ns = ns,
         fluidRow(
           column(12,
           tabsetPanel(id = "tabsmodels",
             tabPanel("Identification du modèle", 
                      fluidRow(
                        column(6, plotOutput(ns("plotacf"))),
                        column(6, plotOutput(ns("plotpacf")))
                      )),
             tabPanel("Estimation",
                      br(), br(),
                      fluidRow(
                        column(1, numericInput(ns("p"), "AR(p):", value = 0, min = 0, max = 5)),
                        column(1, numericInput(ns("d"),"I(d):", value = 0, min = 0, max = 5)),
                        column(1, numericInput(ns("q"),"MA(q):", value = 0, min = 0, max = 5))
                      ),
                      fluidRow(
                        column(6, 
                               fluidRow(column(12, verbatimTextOutput(ns("resume1")))),
                               fluidRow(column(12, verbatimTextOutput(ns("resume2"))))
                               ),
                        column(6, h4("LOe modèle s'écrit sous la forme:"),
                               uiOutput(ns("expression")))
                      )
                    )
                )
           )
         )
       ),
       
       conditionalPanel(
         condition = "input.modelapproche == 'partiel'",
         tabsetPanel(
           tabPanel("Estimation")
         )
       )
    )
  )
}

arimaModel <- function(id, data){
  moduleServer(
    id,
    function(input, output, session){
      
      # mise a jour des differents champs
      observe({
        updateSelectInput(session, "targetvar", choices = names(data()))
        updateSelectInput(session, "Xvar", choices = names(data()))
      })
      
      # calcul de l'acf
      output$plotacf <- renderPlot({
        data() %>% 
          select(input$targetvar) %>% 
          acf(main="Autocorrelation")
          
      })
      
      # calcul du pacf
      output$plotpacf <- renderPlot({
        data() %>% 
          select(input$targetvar) %>% 
          pacf(main="Partial autocorrelation")
      })
      
      # create model
      arimamodel <- reactive({
        if (input$targetvar == "Date"){
          validate("Selectionner une autre variable")
        }
        arima(data()[, input$targetvar], 
              order = c(input$p, input$d, input$q))
      })
      
      # estimation du modèle ARIMA
      output$resume1 <- renderPrint({
        arimamodel()
      })
      
      output$resume2 <- renderPrint({
        lmtest::coeftest(arimamodel())
      })
      
      # ecriture de l'expression
      output$expression <- renderUI({
        withMathJax(writeExpr(arimamodel()$coef, p = input$p, q = input$q))
      })
      
      return(arimamodel)
    }
  )
}