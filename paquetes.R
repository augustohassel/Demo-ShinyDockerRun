# Librerias -------------------------------------------------------------------

paquetes <- list(
  "Shiny Core" = list("bs4Dash"),
  "Shiny Extras" = list("shinyjs", "shinyWidgets"),
  "Plotting" = list("plotly"),
  "Tables" = list("DT"),
  "Generales" = list("rRofex", "quantmod", "log4r")
)

lapply(as.list(c(paquetes, recursive = T, use.names = F)),
       function(x) {
         install.packages(x, verbose = F)
       })
rm(list = c("paquetes"))
