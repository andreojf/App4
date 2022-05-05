rapportUI <- function(id){
  
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, downloadButton(ns("exporter"), "Generer le rapport"))
    )
  )
}

rapportServer <- function(id, params){
  moduleServer(
    id,
    function(input, output, session){
      
      
      output$exporter <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "rapport.html",
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("reports/report.Rmd", tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          # params <- list(intro = input$intro,
          #                description = input$description)
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )
      
    }
  )
}