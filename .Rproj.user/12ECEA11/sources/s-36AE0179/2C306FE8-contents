# plot data
library(plotly)


# 1. Exploration des donnees ----
# filterData <- function(dataset, x, y, by1=NULL, by2=NULL){
#   if (is.null(by1) & is.null(by2)){
#      dataset <- dataset
#     
#   } else if (!is.null(by1) & is.null(by2)){
#     cat("cas 2 ----\n")
#     dataset <- dataset %>% 
#       filter(.data[[by1$Var]] %in% by1$modalites)
# 
#   } else if (is.null(by2) & !is.null(by1)){
#     dataset <- dataset %>% 
#       filter(.data[[by2$Var]] %in% by2$modalites)
#     
#   } else {
#     dataset <- dataset %>% 
#       filter(.data[[by1$Var]] %in% by1$modalites,
#              .data[[by2$Var]] %in% by2$modalites)
#   }
#   
#   # on recalcule les statistiques en considérant les aggrégations
#   if (x == "Date"){
#     dataset %>% 
#       group_by(Date) %>% 
#       summarise(
#         Montant = sum(.data[[y]], na.rm = T)
#       )
#   } else {
#     dataset %>% 
#       group_by(Date) %>% 
#       summarise(
#         Montant1 = sum(.data[[x]], na.rm = T),
#         Montant2 = sum(.data[[y]], na.rm = T)
#       ) %>% 
#       dplyr::rename(x = Montant1, y = Montant2)
#   }
#   
# }
# 
# # d %>% 
# #   filterData(
# #     x = "Date",
# #     y = "Montant",
# #     by1 = list(Var = "Type1", modalites = "Gros"),
# #     by2 = list(Var = "Type2", modalites = c("courant","epargne"))
# #   )
# 
# # en cas de line plot
# EDALine <- function(dataset, x, y, by1, by2, title=NULL){
#   
#   
#   dataset %>% 
#     plot_ly(x = ~.data[[x]], y = ~.data[[y]]) %>% 
#     add_lines() %>% 
#     layout(title = sprintf("Evolution des dépôts: clientèle %s sur %s", 
#                            paste(by1$modalites, collapse = "-"), 
#                            paste(by2$modalites, collapse = "-")),
#            xaxis = list(title = x),
#            yaxis = list(title = y))
# }
# 
# # d %>% 
# #   filterData(
# #     x = "Date",
# #     y = "Montant",
# #     by1 = list(Var = "Type1", modalites = "Gros"),
# #     by2 = list(Var = "Type2", modalites = c("courant","epargne"))
# #   ) %>% 
# #   EDALine(
# #     x = "Date",
# #     y = "Montant",
# #     by1 = list(Var = "Type1", modalites = "Gros"),
# #     by2 = list(Var = "Type2", modalites = c("courant","epargne"))
# #     
# #   )
# 
# 
# EDAScatter <- function(dataset, x, y, by1, by2, title=NULL){
#   
#   dataset %>% 
#     plot_ly(x = ~.data[[x]], y = ~.data[[y]]) %>% 
#     add_markers() %>% 
#     layout(title = sprintf("Evolution des depots: clientele %s sur %s", 
#                            paste(by1$modalites, collapse = "-"), 
#                            paste(by2$modalites, collapse = "-")),
#            xaxis = list(title = x),
#            yaxis = list(title = y))
# }
# 
# # EDALine <- function(d, x, y, by, filt, title){
# #   
# #   d %>% 
# #     filter(.data[[by]] %in% filt) %>% 
# #     plot_ly(x = ~.data[[x]], y = ~.data[[y]]) %>% 
# #     add_lines() %>% 
# #     layout(title = title,
# #            xaxis = list(title = x),
# #            yaxis = list(title = y))
# # }
# # 
# # 
# # 
# # 
# # d %>% 
# #   group_by()
# #   plot_ly(x = ~Date, y = ~Montant) %>% 
# #   add_lines() %>% 
# #   layout(title = "Evolution du montant des DAV")
# # 
# # # en cas de scatter plot
# # d %>% 
# #   plot_ly(x = ~Date, y = ~Montant) %>% 
# #   add_markers() %>% 
# #   layout(title = "Evolution du montant des DAV")


EDALine <- function(dataset, x, y){


  dataset %>%
    plot_ly(x = ~.data[[x]], y = ~.data[[y]]) %>%
    add_lines() %>%
    layout(title = sprintf("Evolution des depots sur %s", y),
           xaxis = list(title = x),
           yaxis = list(title = "Montant"))
}

EDAScatter <- function(dataset, x, y){

  dataset %>%
    plot_ly(x = ~.data[[x]], y = ~.data[[y]]) %>%
    add_markers() %>%
    layout(title = sprintf("Evolution des depots sur %s", y),
           xaxis = list(title = x),
           yaxis = list(title = "Montant"))
}

# d %>% EDAScatter(x = "Date", y = "DETAIL_COURANTS")

EDABoxplot <- function(dataset, y){
  
  dataset %>%
    plot_ly(y = ~.data[[y]],
            alpha = 0.1, boxpoints = "suspectedoutliers") %>%
    add_boxplot() %>%
    layout(title = sprintf("Distribution des depots sur %s", y),
           xaxis = list(title = y),
           yaxis = list(title = "Montant"))
  
}

EDAHistogram <- function(dataset, x){
  
  dataset %>%
    plot_ly(x = ~.data[[x]]) %>%
    add_histogram(name=x) %>%
    layout(title = sprintf("Distribution des depots sur %s", x),
           xaxis = list(title = x),
           yaxis = list(title = "Montant"))
}


# QUALITE DE LA DONNEE ----
DQ <- function(dataset){
  
  # nombre d'observations
  N <- nrow(dataset)
  
  # nombre de variables
  K <- ncol(dataset)
  
  # taux de valeurs manquantes
  nmiss <- sum(is.na(dataset))
  txmiss <- nmiss / (N*K)
  
  # fenetre d'etude
  window <- paste(c(min(dataset$Date), max(dataset$Date)), collapse = " to ")
  
  return(list(N=N, K=K, nmiss=nmiss, txmiss=txmiss, window=window))
}

# d %>% DQ

# SELECTION DES VARIABLES ----
corrX <- function(dataset, X, y, method="pearson"){
  
  dataset %>% 
    select(X) %>% 
    cor(use = "complete.obs", method = method)
}

d %>% 
  corrX(c("DETAIL_COURANTS", "DETAIL_EPARGNE","DETAIL"))

corrY <- function(dataset, X, y, method="pearson"){
  dataset %>% 
    select(c(X,y)) %>% 
    cor(use = "complete.obs", method = method) %>%
    as.data.frame %>% 
    dplyr::select(y) %>% 
    dplyr::arrange(desc(.data[[y]]))
}

corrYplot <- function(dataset, X, y, method="pearson"){
  
  dataset %>% 
    corrY(X=X, y=y, method=method) %>% 
    plot_ly(x = ~rownames(.data), y = ~.data[[y]]) %>% 
    add_bars() %>% 
    layout(title = sprintf("Correlation avec la variable cible: %s", y),
           xaxis = list(title = "Explicatives"),
           yaxis = list(title = " "))
  
}

d %>% 
  corrYplot(c("DETAIL_COURANTS", "DETAIL_EPARGNE","DETAIL"), "GROS")


# STATIONNARITE ET SAISONNALITE ----
newVar <- function(dataset, y, trans, order){
  if (trans){
    d_log_x = log(dataset[, y])
  } else {
    d_log_x = dataset[, y]
  }
  
  for (i in 0:order){
    if (i==0){
      d_log_x = d_log_x
    } else {
      d_log_x = diff(d_log_x)
    }
  }
  d_log_x %>% as.vector
}

checkStationnarityBYPLOT <- function(dataset, x, trans=T, order=1){
  
  # Obtain first log-differences of disposable income
  if (trans){
    d_log_x = log(dataset[, x])
  } else {
    d_log_x = dataset[, x]
  }
  
  for (i in 0:order){
    if (i==0){
      d_log_x = d_log_x
    } else {
      d_log_x = diff(d_log_x)
    }
  }

  # # Plot and add horizontal line at 0
  plot(d_log_x, main = sprintf("%s",x),
       ylab = sprintf("Series differenciees d'ordre %s", order))
  
}

checkStationnarityBYPLOT(dd, "DETAIL", trans = F)
checkacf <- function(dataset, x, trans=T, order=1){
  
  if (trans){
    d_log_x = log(dataset[, x])
  } else {
    d_log_x = dataset[, x]
  }
  
  for (i in 0:order){
    if (i==0){
      d_log_x = d_log_x
    } else {
      d_log_x = diff(d_log_x)
    }
  }
  
  # # Plot and add horizontal line at 0
  acf(d_log_x, main = sprintf("Correlogramme sur %s",x), lag.max = 20,
       ylab = sprintf("Series differenciees d'ordre %s", order), na.action = na.pass)
}
checkacf(dd, "DETAIL", trans = F)


testStationarite <- function(dataset, x, trans=T, order=1, test="adf"){
  
  if (trans){
    d_log_x = log(dataset[, x])
  } else {
    d_log_x = dataset[, x]
  }
  
  for (i in 0:order){
    if (i==0){
      d_log_x = d_log_x
    } else {
      d_log_x = diff(d_log_x)
    }
  }
  
  # test de staionarite
  if (test == "adf"){
    na.omit(d_log_x) %>% 
      adf.test(alternative = "stationary")
  } else {
    
  }
  
  
}

# SAISONNALITE ----
saisonnalitePlot <- function(dataset, y, trans=T, order=1){
  
  if (trans){
    d_log_x = log(dataset[, y])
  } else {
    d_log_x = dataset[, y]
  }
  
  for (i in 0:order){
    if (i==0){
      d_log_x = d_log_x
    } else {
      d_log_x = diff(d_log_x)
    }
  }
  
  # 
  d_log_x %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Index") %>% 
    mutate(mois = lubridate::month(Index, label = T),
           annee = factor(lubridate::year(Index))) %>%
    plot_ly(x = ~mois, y = ~.data[[y]], color = ~annee) %>%
    add_lines() %>%
    layout(title = sprintf("Saisonnalite sur %s", y),
           xaxis = list(title = y),
           yaxis = list(title = "Montant"))
}

#saisonnalitePlot(dd, "DETAIL", order = 0)
# MODELLING ----
writeExpr <- function(coefs, p, q){
  
  expr <- ""
  if ("intercept" %in% names(coefs)){
    # gestion de l'intercept 
    intercept = tail(coefs, 1)
    cste = intercept * (1 - sum(coefs[1:p]))
    c = paste(round(cste,4), " +", sep = "")
    
    # partie AR
    
    ARp <- ""
    if (p > 0){
      ARcoefs <- coefs[1:p]
      cat("AR coefs:", ARcoefs, "\n")
      for (i in 1:p){
        
        cat("ar",i, "\n")
        if (i == 1){
          ARp <- paste(ARp, " (", round(ARcoefs[i],4), ")\\y_{t-", i, "}", sep = "")
        } else {
          ARp <- paste(ARp, " + (", round(ARcoefs[i],4), ")\\y_{t-", i, "}", sep = "")
        }
      }
    }
    
    # Partie MA
    
    MAp <- ""
    if (q > 0){
      MAcoefs <- coefs[(p+1):(p+q)]
      cat("MA coefs:", MAcoefs, "\n")
      
      for (i in 1:q){
        
        cat("ma",i, "\n")
        
        if (i == 1 && p == 0){
          MAp <- paste(MAp, " (", round(MAcoefs[i],4), ")\\epsilon_{t-", i, "}", sep = "")
        } else {
          MAp <- paste(MAp, " + (", round(MAcoefs[i],4), ")\\epsilon_{t-", i, "}", sep = "")
        }
      }
      
      cat("MA part:", MAp, "\n")
      
    }
    
    
  } else {
    c = ""
    
    # partie AR
    ARp <- ""
    if (p > 0){
      ARcoefs <- coefs[1:p]
      for (i in 1:p){
        if (i == 1){
          ARp <- paste(ARp, " (", round(ARcoefs[i],4), ")\\y_{t-", i, "}", sep = "")
        } else {
          ARp <- paste(ARp, " + (", round(ARcoefs[i],4), ")\\y_{t-", i, "}", sep = "")
        }
      }
    }
    
    # Partie MA
    
    MAp <- ""
    if (q > 0){
      MAcoefs <- coefs[p+1:(p+q)]
      for (i in 1:q){
        if (i == 1 && p == 0){
          MAp <- paste(MAp, " (", round(MAcoefs[i],4), ")\\epsilon_{t-", i, "}", sep = "")
        } else {
          MAp <- paste(MAp, " + (", round(MAcoefs[i],4), ")\\epsilon_{t-", i, "}", sep = "")
        }
      }
    }
  }
  
  
  express <- paste("$$\\y_t = ", c, ARp, MAp, "$$", sep = "")
  return(express)
  
}


#writeExpr(coefs, 1, 1)
# plot_ly(x = ~m$residuals, type = "histogram") 
# plot_ly(x = ~density(m$residuals), type = "line") 

distplot <- function(x){
  
  fit <- density(x)
  
  plot_ly(x = x, type = "histogram", name = "Histogram") %>% 
    add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density", alpha = 0) %>% 
    layout(yaxis2 = list(overlaying = "y", side = "right", showgrid = F))
  
}

#distplot(m$residuals)
