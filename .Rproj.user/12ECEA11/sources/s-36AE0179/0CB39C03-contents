library(shiny)
library(shinyjs)

ui <- navbarPage("My Application",
                 
                 useShinyjs(),
                 
                 # IMPORTATION DES DONNEES ----
                 tabPanel("Importation",
                          sidebarPanel(
                            actionButton("reset", "Nouveau modele", width = "150px"),
                            actionButton("loadwork", "Charger mon travail", width = "150px"),
                            helpText("En cliquant sur nouveau modèle, vous partirez sur page vierge. 
                                     Tous vos travaux précédents seront supprimés."),
                            helpText("En cliquant sur charger mon travail, vous ."),
                            hr(),
                            uploadFileUI("importmydata","Importer"), width = 3),
                            mainPanel(dataTableOutput("data"))
                          ),
                 
                 # EXPLORATORY DATA ANALYSIS ----
                 tabPanel("Analyse exploratoire",
                          # module ----
                          EDAUI("myeda")
                          # %% ----
                          # fluidRow(
                          #   column(3, wellPanel(selectInput("plottype", "Choix graphique",
                          #                                   choices = c("Nuage de points"="scatter",
                          #                                               "Courbe"="line",
                          #                                               "Histogram"="hist",
                          #                                               "Boxplot"="box")))),
                          #   column(3,
                          #          wellPanel(selectInput("xaxis", "X:",
                          #                      choices = c(""), selected = ""))),
                          #   column(3,
                          #          conditionalPanel("input.plottype == 'scatter' | input.plottype == 'line'",
                          #                           wellPanel(selectInput("yaxis", "Y:",
                          #                                       choices = c(""), selected = ""))))
                          #          # conditionalPanel("input.plottype == 'line'",
                          #          #                  wellPanel(selectInput("yaxis", "Y:",
                          #          #                              choices = c(""), selected = ""))))
                          #   #column(9, uiOutput("aes")),
                          # ),
                          # hr(),
                          # fluidRow(
                          #   column(8, offset = 2, plotlyOutput("graphique", height = "500px")),
                          #   # column(6, textAreaInput("comment1", "Commentaires", 
                          #   #                         placeholder = "Commente ton résultat",
                          #   #                         width = "100%", height = "300px"))
                          # )
                          # fin ----
                          ,
                          hr(),
                          fluidRow(
                            column(12, actionButton("saveEDA", "Sauvegarder mes travaux"))
                          )
                          ),
                 
                 # TRAITEMENT DES DONNEES ----
                 navbarMenu("Traitement des données",
                            
                            # DATA QUALITY ----
                            tabPanel("Qualité de la donnée",
                                     
                                     # MODULE ----
                                     DQUI("mydq")
                                     # % ----
                                     # fluidRow(
                                     #   column(3, 
                                     #          h4("Nombre d'observations",
                                     #             textOutput("nrows"))),
                                     #   column(3,
                                     #          h4("Nombre de variable",
                                     #             textOutput("ncols"))),
                                     #   column(3,
                                     #          h4("Taux de valeurs manquantes",
                                     #             textOutput("nmiss"))),
                                     #   column(3,
                                     #          h4("Fenêtre d'étude",
                                     #             textOutput("window")))
                                     # ),
                                     # hr(),
                                     # fluidRow(
                                     #   column(9, 
                                     #          # textAreaInput("comment2", "Commentaires",
                                     #          #              placeholder = "Commente ton résultat",
                                     #          #              width = "100%", height = "200px"),
                                     #          actionButton("saveDQ", "Sauvegarder mes travaux"))
                                     # ),
                                     # hr(),
                                     # fluidRow(
                                     #   column(12, dataTableOutput("resume"))
                                     # )  
                                     # ----
                                     ),
                            
                            # TRAITEMENT DATA ----
                            tabPanel("Nettoyage des données",
                                     
                                     # MODULE ----
                                     preprocessingUI("mypreprocessing")
                                     # % ----
                                     # fluidRow(
                                     #     column(3, 
                                     #            selectizeInput("vartobecleaned", NULL, 
                                     #                        choices = c(""), 
                                     #                        options = list(placeholder = 'Choisis ta variable'))),
                                     #     column(2, align="center",
                                     #            checkboxInput("imputevar", "Imputer", value = FALSE),
                                     #                conditionalPanel(condition = "input.imputevar == true",
                                     #                                 selectInput("imputeby", "Par:", 
                                     #                                             choices = c("Moyenne","Mediane")))),
                                     #     column(2, checkboxInput("removevar", "Supprimer", value = FALSE)),
                                     #     column(3, actionButton("btnclean", "Appliquer le traitement", 
                                     #                            icon = icon("cog", lib = "glyphicon")))
                                     #     ),
                                     # 
                                     # hr(),
                                     # fluidRow(
                                     #   column(12, dataTableOutput("datacleaned"))
                                     # )
                                     # FIN ----
                                     ),
                            
                            # STATIONNARITE ET SAISONNALITE
                            tabPanel("Stationarité et saisonnalité",
                                     
                                     # MODULE ----
                                     statnSaisonUI("mystationnarite")
                                     # % ----
                                     # tabsetPanel(id = "tabs1",
                                     #             tabPanel("Stationnarité",
                                     #                      statnSaisonUI("mystationnarite")),
                                     #             tabPanel("Saisonnalité"))
                                     
                                     # fluidRow(
                                     #   column(3,
                                     #          wellPanel(
                                     #            selectizeInput("var", NULL,
                                     #                      choices = c(""), 
                                     #                      options = list(placeholder = 'Choisis ta variable')))),
                                     #   column(3,
                                     #          wellPanel(
                                     #            radioButtons("trans", "Transformation logarithmique",
                                     #                       choices = c("Oui" = T,
                                     #                                   "Non" = F)))),
                                     #   column(3,
                                     #          wellPanel(
                                     #            numericInput("order","Ordre de différenciation",
                                     #                         value = 0, min = 0, max = 3)
                                     #          ))
                                     #   
                                     # ),
                                     # hr(),
                                     # fluidRow(
                                     #   column(6, plotOutput("plotdav")),
                                     #   column(6, plotOutput("correlogram"))
                                     # ),
                                     # h4("RESULTATS"),
                                     # br(),
                                     # fluidRow(
                                     #   column(6,
                                     #          selectInput("typetest", "Method:",
                                     #                      choices = c("Augmented Dickey-Fuller Test"="adf",
                                     #                                  "KPSS test"="kpss")),
                                     #          conditionalPanel("input.typetest == 'adf'"),
                                     #          conditionalPanel("input.typetest == 'kpss'"),
                                     #          textOutput("res1")),
                                     #   column(6, textOutput("res2"))
                                     # )
                                     # FIN ----
                                     )
                            
                            ),
                 
                 # SELECTION DES VARIABLES ----
                 tabPanel("Selection des variables",
                          
                          # MODULE ----
                          selectionUI("myselection")
                          # % ----
                          # fluidRow(
                          #   column(12, 
                          #          actionButton("btnselectvar", "Lancer le processus de selection"),
                          #          helpText("NB: Le processus de sélection repose essentiellement sur une analyse de corrélation. 
                          #                   Toutefois, l'utilisateur peut se passer de cette étape en l'absence de variables explicatives.")
                          #          )
                          # ),
                          # hr(),
                          # h4("Contribution de chaque variable par rapport à la variable cible"),
                          # fluidRow(
                          #   column(6, plotlyOutput("plot1")),
                          #   column(6, tableOutput("data1"))
                          # ),
                          # hr(),
                          # h4("Correlation entre les variables explicatives"),
                          # fluidRow(
                          #   column(6, plotlyOutput("plot2")),
                          #   column(6, tableOutput("data2"))
                          # )
                          
                          # FIN ----
                          ),
                 # UI/ESTIMATION DU MODELE ----
                 tabPanel("Estimation du modèle",
                          # MODULE ----
                          modelUI("mymodels")
                          # % ----
                          # fluidRow(
                          #   column(3, selectInput("modelapproche","Choix du modèle",
                          #                         choices = c("L’approche structurelle"="structurel",
                          #                                     "L’approche semi structurelle"="semistructurel",
                          #                                     "L’approche en équilibre partiel"="partiel"
                          #                         ))),
                          #   column(3, selectInput("targetvar", "Choix de la variable cible",
                          #                         choices = c("")), offset = 3)
                          # ),
                          # fluidRow(
                          #   column(6,
                          #          conditionalPanel(
                          #            condition="input.modelapproche == 'structurel'", 
                          #            helpText("Les modèles structurels désignent des modèles dont la fonction d’écoulement résulte 
                          #               d’un raisonnement microéconomique selon lequel le client a un comportement rationnel 
                          #               visant à maximiser son intérêt."),
                          #          ),
                          #          
                          #          conditionalPanel(
                          #            condition = "input.modelapproche == 'semistructurel'",
                          #            helpText("Les modèles semi structurelle qui est basée sur l’étude des propriétés stochastiques des séries par la méthode de BOX et JENKINS"),
                          #          ),
                          #          
                          #          conditionalPanel(
                          #            condition = "input.modelapproche == 'partiel'",
                          #            helpText("L’approche en équilibre partiel est basée sur 
                          #                        l’utilisation d’un modèle économétrique avec des variables dépendantes qui reflètent l’activité économique 
                          #                        et le comportement individuel des agents économiques en termes de consommation d’investissement, etc.")
                          #          ),
                          #          
                          #          conditionalPanel("input.modelapproche == 'partiel'",
                          #                           selectizeInput("modelchoice",NULL,
                          #                                          choices = c("Modèle de Selvaggio"="selvaggio",
                          #                                                      "Modèle de Denis Dupré"="dupre",
                          #                                                      "Modèle de Jarrow et Van Deventer [1998]"="jarvanter"
                          #                                          ))),
                          #          
                          #          actionButton("help", "Aide technique", icon = icon("question-sign", lib = "glyphicon"))
                          #   ),
                          #   column(6, 
                          #          selectInput("Xvar", "Choix des variables explicatives",
                          #                      choices = c(""), multiple = T))
                          # ),
                          # hr(),
                          # 
                          # fluidRow(
                          #   column(6,
                          #          conditionalPanel(
                          #            condition = "input.modelapproche == 'semistructurel'",
                          #            tabsetPanel(
                          #              tabPanel("Identification du modèle", 
                          #                       fluidRow(
                          #                         column(6, plotOutput("acf")),
                          #                         column(6, plotOutput("pacf"))
                          #                       )),
                          #              tabPanel("Estimation")
                          #            )
                          #          ),
                          #          
                          #          conditionalPanel(
                          #            condition = "input.modelapproche == 'partiel'",
                          #            tabsetPanel(
                          #              tabPanel("Estimation")
                          #            )
                          #          ))
                          # ),
                          # 
                          # 
                          # # # si le choix a été fait sur l"approche structurelle
                          # # conditionalPanel(
                          # #   condition = "",
                          # #   tagList(
                          # #     tabsetPanel(id = "tabsesti",
                          # #                 tabPanel("Identification du modèle"),
                          # #                 tabPanel("Estimation")
                          # #                 )
                          # #   )
                          # # ),
                          # # 
                          # # conditionalPanel(
                          # #   condition = "input.modelapproche == 'semistructurel'",
                          # #   tagList(
                          # #     tabsetPanel(id = "tabsesti",
                          # #                 tabPanel("Identification du modèle"),
                          # #                 tabPanel("Estimation")
                          # #     )
                          # #   )
                          # # )
                          # 
                          # FIN ----
                          
                          ),
                 tabPanel("Validité des résultats",
                          h4("VALIDITE DES HYPOTHESES"),
                          br(),
                          # fluidRow(
                          #   column(12,
                          #          actionButton("zeromean","Nullité de la moyenne"),
                          #          actionButton("autocorrelation","Autocorrelation des résidus"),
                          #          actionButton("stabilite","Stabilité des résidus"),
                          #          actionButton("homoscedasticite","Homoscédasticité des résidus"),
                          #          actionButton("normalite", "Normalité des résidus"),
                          #          actionButton("senseco","Sens économique"))
                          # ),
                          
                          tabsetPanel(id = "tabsvad",
                            tabPanel("Nullité de la moyenne"),
                            tabPanel("Autocorrelation des résidus", br(),
                                     resAUTOCORRELATIONUI("autocorrelation")),
                            tabPanel("Stabilité des résidus"),
                            tabPanel("Homoscédasticité des résidus", br(),
                                     resHOMOSCEDASTICITYUI("homoscedasticite")),
                            tabPanel("Normalité des résidus", br(),
                                       resNORMALITYUI("normalite")),
                            tabPanel("Sens économique")
                            
                            
                          )
                          # shinyjs::hidden(
                          #   div(id = "nullite-moyenne",
                          #       fluidRow(
                          #         column(12, p("Nullité de la moyenne"))
                          #       ))
                          # ),
                          # 
                          # shinyjs::hidden(
                          #   div(id = "autocorrelation-residus",
                          #       fluidRow(
                          #         column(12, p("Autocorrelation"))
                          #       ))
                          # ),
                          # 
                          # shinyjs::hidden(
                          #   div(id = "stabilite-residus",
                          #       fluidRow(
                          #         column(12, p("Nullité de la moyenne"))
                          #       ))
                          # )
                          # fluidRow(
                          #   column(6, h5("1. Normalité des résidus")),
                          #   column(6, h5("2. Homoscedasticité"))
                          # ),
                          # fluidRow(
                          #   column(6, h5("3. Autocorrelation des résidus")),
                          #   column(6, h5("4. Sens économique"))
                          # ),
                          # hr(),
                          # h4("BACKTESTING")
                          ),
                 tabPanel("Ecoulement"),
                 tabPanel("Application"),
                 tabPanel("Reporting")
  
  
  # fluidRow(
  #   column(12,
  #          # introduction
  #          textAreaInput("intro", "1. INTRODUCTION", width = "700px", height = "200px",
  #                        placeholder = "Rédiger votre introduction"),
  #          
  #          hr(),
  #          
  #          # introduction
  #          textAreaInput("description", "2. DESCRIPTION DES DONNEES", width = "700px", height = "200px",
  #                        placeholder = "Rédiger votre Descriptif des données"))
  # ),
  # 
  # # bouton pour générer le rapport
  # rapportUI("report")
)