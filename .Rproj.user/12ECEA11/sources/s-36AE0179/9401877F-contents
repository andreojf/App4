statnSaisonUI <- function(id){
  
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(
      column(3,
             wellPanel(
               selectizeInput(ns("var"), NULL,
                              choices = c(""), 
                              options = list(placeholder = 'Choisis ta variable')))),
      column(3,
             wellPanel(
               radioButtons(ns("trans"), "Transformation logarithmique",
                            choices = c("Oui" = T,
                                        "Non" = F), selected = F))),
      column(3,
             wellPanel(
               numericInput(ns("order"),"Ordre de differenciation",
                            value = 0, min = 0, max = 3)
             )),
      column(3, actionButton(ns("inserer"),"Rajouter la variable"))
      
    ),
    hr(),
    fluidRow(
      column(8, offset = 2, plotOutput(ns("plotdav")))
    ),
    br(),
    h4("ANALYSE DE LA STATIONNARITE"),
    fluidRow(
      column(6, plotOutput(ns("correlogram"))),
      column(6, 
             br(),
             fluidRow(
               selectInput(ns("typetest"), "Method:",
                           choices = c("Augmented Dickey-Fuller Test"="adf",
                                       "KPSS test"="kpss"))
             ),
             fluidRow(
               verbatimTextOutput(ns("testresult")),
               textOutput(ns("interpretation"))
             )
        )
    ),
    br(),
    h4("ANALYSE DE LA SAISONNALITE"),
    fluidRow(
      column(6, plotlyOutput(ns("seasonality")))
    )
  )
}

statnSaisonServer <- function(id, dataset){
  moduleServer(
    id,
    function(input, output, session){
      
      # ns <- session$ns
      
      # Inserer le bouton "Inserer"
      # observeEvent(list(input$trans, input$order), {
      #   output$btninserer <- renderUI({
      #     wellPanel(actionButton(ns("inserer"),"Rajouter la variable"))
      #   })
      # })
      
      X <- reactiveVal(0)

      observe({
        req(dataset())
        X(dataset())
        # print(X())


        # METTRE A JOUR LA TABLE AVEC LA NOUVELLE VARIABLE

      })
      
      newdata <- reactive({
        # input$inserer
        cat("------\n")
        df <- dataset()
        df$Date <- NULL
        df <- xts::as.xts(df, order.by = dataset()$Date)
        cat("Variables :", names(df), "\n")
        df
      })
      
      # observe({
      #   print(newdata())
      # })
      
      observe({
        updateSelectizeInput(session, "var",
                             choices = names(newdata()))
      })
      
      
      # output$btninserer <- renderUI({
      #   wellPanel(actionButton(ns("inserer"),"Rajouter la variable"))
      # })
      
      # nos graphes
      output$plotdav <- renderPlot({
        checkStationnarityBYPLOT(newdata(), input$var, 
                                 trans = input$trans,
                                 order = input$order)
      })
      
      #
      output$correlogram <- renderPlot({
        checkacf(newdata(), input$var, 
                 trans = input$trans,
                 order = input$order)
      })
      
      output$testresult <- renderPrint({
        
        testStationarite(newdata(), input$var, 
                         trans = input$trans,
                         order = input$order, 
                         test=input$typetest) %>% print
      })
      
      
      # seasonalite
      output$seasonality <- renderPlotly({
        saisonnalitePlot(newdata(), input$var, 
                         trans = input$trans,
                         order = input$order)
      })
      
      # 
      # observe({
      #   if (!exists(newX)){
      #     dataX(newX())
      #   } else {
      #     dataX(dataset())
      #   }
      # })
      
      # Mettre la table sous format zoo
      
      
      
      
      
      # mettre à jour les champs
      # observe({
      #   updateSelectizeInput(session, "var",
      #                     choices = names(newdata()))
      # })
      
      
      
      # # nos graphes
      # output$plotdav <- renderPlot({
      #   checkStationnarityBYPLOT(newdata(), input$var, 
      #                            trans = input$trans,
      #                            order = input$order)
      # })
      # 
      # #
      # output$correlogram <- renderPlot({
      #   checkacf(newdata(), input$var, 
      #            trans = input$trans,
      #            order = input$order)
      # })
      # 
      # output$testresult <- renderPrint({
      #   
      #   testStationarite(newdata(), input$var, 
      #                    trans = input$trans,
      #                    order = input$order, 
      #                    test=input$typetest) %>% print
      # })
      # 
      # 
      # # seasonalite
      # output$seasonality <- renderPlotly({
      #   saisonnalitePlot(newdata(), input$var, 
      #                    trans = input$trans,
      #                    order = input$order)
      # })
      
      # # calcul de la variable
      newX <- eventReactive(input$inserer, {
        # req(input$var)
        # calcul de la variable
        d_log_x <- newVar(newdata(), input$var, 
                          trans = input$trans,
                          order = input$order)

        logflag <- ifelse(input$trans, "log_","")

        #cat(d_log_x)
        diffflag <- ifelse(input$order>0, paste("diff_",input$order,"_", sep = ""), "")
        newlabel = paste(diffflag, logflag, input$var, sep = "")
        # 
        cat("\nINTO newX", newlabel, sep = "\n")
        showNotification(paste("Ajout de la variable:", newlabel, sep = ""), type = "message")
        Sys.sleep(1)
        X(X() %>% mutate("{newlabel}" := d_log_x))
        X()
      })
      
      
      # # 
      # observe({
      #   req(input$var)
      #   logflag <- ifelse(input$trans, "log_","")
      # 
      #   #cat(d_log_x)
      #   diffflag <- ifelse(input$order>0, paste("diff_",input$order,"_", sep = ""), "")
      #   newlabel = paste(diffflag, logflag, input$var, sep = "")
      #   cat(newlabel, "\n")
      # })
      # # 
      # # # # inserer la variable
      # # observeEvent(input$inserer, {
      # # 
      # #   # tmpX <-
      # #   # dataset
      # #   X(newX())
      # #   cat("Variables :", names(X()), "\n")
      # # })
      # 
      observe({
        print(names(newX()))
      })
      
        
      return(newX)
    }
  )
}