function(input, output, session){

  # si l'utilisateur clique sur "Nouveau modèle"
  
  
  # IMPORTATION ----
  rawdata <- uploadFileServer("importmydata")
  # afficher les données
  output$data <- renderDataTable({
    datatable(
      rawdata(),
      filter = 'top', 
    )
  })
  
  # EDA ----
  # Module ----
  EDAServer("myeda", rawdata)
  # % ----
  # observe({
  #   req(rawdata())
  #   updateSelectInput(session, "xaxis",
  #                     choices = names(rawdata()), selected = "")
  #   updateSelectInput(session, "yaxis",
  #                     choices = names(rawdata()), selected = "")
  # })
  # 
  # # realiser les graphiques
  # output$graphique <- renderPlotly({
  #   
  #   
  #   if (input$plottype == "line"){
  #     validate(
  #       need(input$xaxis, "Definir les coordonnées: X"),
  #       need(input$yaxis, "Definir les coordonnées: Y")
  #     )
  #      rawdata() %>%
  #       EDALine(
  #         x = input$xaxis,
  #         y = input$yaxis
  #       )
  #   } else if (input$plottype == "scatter"){
  #     validate(
  #       need(input$xaxis, "Definir les coordonnées: X"),
  #       need(input$yaxis, "Definir les coordonnées: Y")
  #     )
  #     rawdata() %>%
  #       EDAScatter(
  #         x = input$xaxis,
  #         y = input$yaxis
  #       )
  #   } else if (input$plottype == "box"){
  #     validate(
  #       need(input$xaxis, "Definir les coordonnées: X"),
  #     )
  #     rawdata() %>%
  #       EDABoxplot(
  #         y = input$xaxis
  #       )
  #   } else {
  #     validate(
  #       need(input$xaxis, "Definir les coordonnées: X"),
  #     )
  #     rawdata() %>%
  #       EDAHistogram(
  #         x = input$xaxis
  #       )
  #   }
  #   
  # })
  # fin ----
  
  # TRAITEMENT DES DONNEES ----
  # DATA QUALITY ----
  DQServer("mydq", rawdata)
  
  # PREPROCESSING ----
  cleanedData <- preprocessingServer("mypreprocessing", rawdata)
  data2 <- statnSaisonServer("mystationnarite", rawdata)
  
  # SELECTION DES VARIABLES ----
  selectionServer("myselection", data2)
  
  # ESTIMATION DU MODELE ----
  if (!is.null(data2)){
    model <- arimaModel("mymodels", data2)
  } else {
    model <- arimaModel("mymodels", rawdata)
  }
  
  # VALIDITE DES RESULTATS ----
  # % NORMALITE DES RESIDUS ---
  resNORMALITYServer("normalite", model)
  resHOMOSCEDASTICITYServer("homoscedasticite", model)
  resAUTOCORRELATIONServer("autocorrelation", model)
  
  # générer le rapport
  rapportServer("report", params=list(intro = input$intro,
                                      description = input$description))
  
}