preprocessingUI <- function(id){
  
  ns <- NS(id)
  tagList(
    fluidRow(
        column(3,
               selectizeInput(ns("vartobecleaned"), NULL,
                           choices = c(""),
                           options = list(placeholder = 'Choisis ta variable'))),
        column(2, align="center",
               checkboxInput(ns("imputevar"), "Imputer", value = FALSE),
                   conditionalPanel(condition = "input.imputevar == true",
                                    ns = ns,
                                    selectInput(ns("imputeby"), NULL,
                                                choices = c("Moyenne","Mediane")))),
        column(1, checkboxInput(ns("removevar"), "Supprimer", value = FALSE)),
        column(3, checkboxInput(ns("tronqvar"), "Tronquer", value = FALSE),
               conditionalPanel("input.tronqvar == true",
                                ns = ns,
                                hr(),
                                fluidRow(
                                  column(6,selectInput(ns("start"),"De:",
                                                       choices = c(""))),
                                  column(6, selectInput(ns("end"),"A:",
                                                        choices = c("")))
                                )
                                )),
        column(3, uiOutput(ns("btn")))
        ),

    hr(),
    fluidRow(
      column(12, dataTableOutput(ns("datacleaned")))
    )
  )
}

preprocessingServer <- function(id, data){
  moduleServer(
    id, 
    function(input, output, session){
      
      ns <- session$ns
      
      observeEvent(input$vartobecleaned, {
        output$btn <- renderUI({
          actionButton(ns("btnclean"), "Appliquer le traitement",
                       icon = icon("cog", lib = "glyphicon"))
        })
      })
      
      # mettre à jour la liste des variables disponibles
      observe({
        updateSelectizeInput(session, "vartobecleaned",
                             choices = names(data()))
      })
      
      # mettre à jour le tronquage
      observe({
        req(input$vartobecleaned)
        updateSelectInput(session, "start",
                          choices = data() %>% 
                            select(input$vartobecleaned) %>% 
                            unique %>% 
                            arrange)
        updateSelectInput(session, "end",
                          choices = data() %>% 
                            select(input$vartobecleaned) %>% 
                            unique %>% 
                            arrange)
      })
      
      # Par ordre de priorité c'est la suppression
      # ensuite l'imputation
      
      
      cleanedData <- reactive({
        req(input$vartobecleaned)
        req(input$btnclean)
        
        if (input$removevar){
          df <- data() %>% 
            select(-input$vartobecleaned)
          showNotification("Suppression de la variable", type = "warning")
          Sys.sleep(1)
        } else {
          
          if (input$imputevar){
            if (input$imputeby=="Moyenne"){
              
              df <- data() #%>% 
                # mutate(
                #   .data[[input$vartobecleaned]] := Hmisc::impute(.data[[input$vartobecleaned]], mean))
              showNotification("Imputation par la moyenne", type = "warning")
              Sys.sleep(1)
            } else {
              df <- data() #%>% 
                # mutate(
                #   .data[[input$vartobecleaned]] := Hmisc::impute(.data[[input$vartobecleaned]], median))
              showNotification("Imputation par la mediane", type = "warning")
              Sys.sleep(1)
            }
          } else {
            df <- data()
          }
          
          if (input$tronqvar){
            df <- df %>% 
              filter(.data[[input$vartobecleaned]] >= input$start,
                     .data[[input$vartobecleaned]] < input$end)
            showNotification("Donnees filtrees", type = "warning")
            Sys.sleep(1)
          } 
          
          
        }
        
        df
      })
      
      
      # rendre la table 
      output$datacleaned <- renderDataTable({
        req(input$btnclean)
        req(cleanedData())
        
        datatable(
          cleanedData(),
          filter = 'top', 
        )
      })
      
      ## return ---
      return(cleanedData)
      
    }
  )
}