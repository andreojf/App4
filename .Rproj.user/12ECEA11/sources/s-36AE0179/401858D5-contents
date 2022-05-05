# uploader un fichier csv ----
uploadFileUI <- function(id, label){
  
  ns <- NS(id)
  fileInput(ns("upload"),label = label, buttonLabel = "Importer",
            accept = c(".csv", ".xlsx"))
}


uploadFileServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      data <- reactive({
        
        # s'assurer que le fichier existe
        req(input$upload)
        
        # extension du fichier
        ext <- tools::file_ext(input$upload$name)
        
        # creation de la table sous format df
        switch(ext,
               csv = read.csv2(input$upload$datapath),
               xlsx = read_excel(input$upload$datapath),
               validate("Invalid file; Please upload a .csv or .xlsx file")
        )
        
        
      })
      
      # Return the reactive that yields the data frame
      return(data)
    }
  )    
}

