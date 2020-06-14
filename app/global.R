# Librerias -------------------------------------------------------------------

paquetes <- list(
  "Shiny Core" = list("shiny", "bs4Dash"),
  "Shiny Extras" = list("shinyjs", "shinyWidgets"),
  "Plotting" = list("plotly"),
  "Tables" = list("DT"),
  "Tidyverse" = list("tidyverse", "lubridate", "glue"),
  "Generales" = list("rRofex", "quantmod", "log4r")
)
lapply(as.list(c(paquetes, recursive = T, use.names = F)),
       function(x) {
         if (x %in% rownames(installed.packages()) == FALSE) {
           install.packages(x, verbose = F)
         }
         library(x, character.only = T, verbose = F)
       })
rm(list = c("paquetes"))


# Funciones ---------------------------------------------------------------

ccl <- function(connection, data) {
  message(glue("Busco la market data de los {n} productos locales y extranjeros...", n = nrow(data)))
  precio_local <- map_df(.x = as.list(data$Local), .f = ~ trading_md(connection = connection, symbol = .x, entries = list("LA", "BI", "OF")))
  precio_extranjero <- rownames_to_column(getQuote(Symbols = data$Extranjero), var = "Symbol") %>% rename(TradeTime = `Trade Time`)

  message(glue("Uno las tablas y calculo el CCL.."))
  data <- data %>%
    left_join(precio_local %>% select(Symbol, LA_date, LA_price, BI_price, OF_price), by = c("Local" = "Symbol")) %>%
    left_join(precio_extranjero %>% select(Symbol, TradeTime, Last), by = c("Extranjero" = "Symbol")) %>%
    mutate(
      CCL_Last = (LA_price / Last) * Factor,
      CCL_Bid = (BI_price / Last) * Factor,
      CCL_Offer = (OF_price / Last) * Factor
    ) %>%
    mutate(across(.cols = starts_with("CCL_"), .fns = ~ ifelse(. == 0, NA_real_, .))) %>%
    mutate(
      CCL_Last = case_when(
        CCL_Last %in% boxplot.stats(x = .$CCL_Last, coef = 3)$out ~ NA_real_,
        TRUE ~ CCL_Last
      )
    )

  return(data)
}
