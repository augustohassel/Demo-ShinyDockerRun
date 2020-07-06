# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {

  # 0. Global -----

  global_connection <- reactiveValues(conn = NULL)

  # 1. Configuración -----

  observeEvent(input$login, {
    if (input$username == "" || input$password == "" || input$base_url == "") {
      sendSweetAlert(session = session, title = HTML("Mmm..."), text = HTML("Tenes que completar todos las campos para avanzar..."),  type = "warning", html = TRUE)
    } else {

      withCallingHandlers({
        global_connection$conn <- trading_login(username = input$username, password = input$password, base_url = input$base_url)
      },
      message = function(m) {output$console_login <- renderPrint({m$message})},
      warning = function(m) {output$console_login <- renderPrint({m$message})}
      )

      if (!is.null(global_connection$conn)) {
        updateTextInput(session = session, inputId = "details_login_date_time", value = login_date_time(global_connection$conn))
        updateTextInput(session = session, inputId = "details_agent", value = agent(global_connection$conn))
      }

      showModal(
        modalDialog(
          title = tagList(icon("info"), "Informacion"),
          easyClose = T,
          size = "l",
          footer = tagList(
            modalButton("Cerrar")
          ),
          verbatimTextOutput("console_login", placeholder = TRUE)
        )
      )

    }
  })

  global_productos <- reactive({
    progressSweetAlert(session = session, id = "progress_global_productos", title = tagList("Buscando productos...", bs4Loading()), display_pct = T, value = 0, striped = T, status = "primary")

    data <- if (is.null(global_connection$conn)) {
      NULL
    } else {
      trading_instruments(connection = global_connection$conn, request = "securities", sec_detailed = TRUE) %>%
        arrange(Symbol)
    }

    updateProgressBar(session = session, id = "progress_global_productos", value = 100, status = "success")
    Sys.sleep(0.5)
    closeSweetAlert(session = session)

    return(data)
  })

  # 2. Gráficos -----

  global_graficos <- reactiveValues(data = NULL)
  environment_graficos_iniciar <- rlang::env()

  output$ui_graficos_producto <- renderUI({
    progressSweetAlert(session = session, id = "progress_ui_graficos_producto", title = tagList("Buscando productos...", bs4Loading()), display_pct = T, value = 0, striped = T, status = "primary")

    ui <- if (is.null(global_connection$conn)) {
      selectizeInput(inputId = "graficos_producto", label = "Producto:", choices = NULL, selected = "", options = list(placeholder = "Elegir producto..."), width = "100%")
    } else {
      prod <- setNames(object = as.list(global_productos()$Symbol), nm = global_productos()$Symbol)
      selectizeInput(inputId = "graficos_producto", label = "Producto:", choices = prod, selected = "", options = list(placeholder = "Elegir producto..."), width = "100%")
    }

    updateProgressBar(session = session, id = "progress_ui_graficos_producto", value = 100, status = "success")
    Sys.sleep(0.5)
    closeSweetAlert(session = session)

    return(ui)

  })

  observeEvent(input$graficos_iniciar, {
    if (is.null(global_connection$conn)) {
      sendSweetAlert(session = session, title = HTML("Mmm..."), text = HTML("Tenes que conectarte primero..."),  type = "warning", html = TRUE)
    } else {

      trading_ws_md(connection = global_connection$conn,
                    destination = "data",
                    symbol = input$graficos_producto,
                    entries = list("LA"),
                    listen_to = list("LA_price"),
                    where_is_env = environment_graficos_iniciar)

      global_graficos$data <- reactivePoll(intervalMillis = 1000,
                                           session = session,
                                           checkFunc = function() {
                                             if (!is.null(environment_graficos_iniciar$data)) max(environment_graficos_iniciar$data$timestamp)
                                           },
                                           valueFunc = function() {
                                             return(environment_graficos_iniciar$data)
                                           })
    }
  })

  output$graficos_grafico <- renderPlotly({
    shiny::validate(
      need(is.reactive(global_graficos$data) && !is.null(global_graficos$data()) && nrow(global_graficos$data()) > 0, message = "Aún no hay data.")
    )
    plot_ly(data = global_graficos$data(), x = ~LA_date, y = ~LA_price, mode = 'line') %>% layout(title = input$graficos_producto, xaxis = list(title = "Timestamp"), yaxis = list(title = "Precio"))
  })

  observeEvent(input$graficos_parar, {
    if (is.null(global_graficos$data)) {
      sendSweetAlert(session = session, title = HTML("Mmm..."), text = HTML("No hay conexi&oacute;n activa..."),  type = "warning", html = TRUE)
    } else {
      try(trading_ws_close(close_all = FALSE, selection = list("data"), where_is_env = environment_graficos_iniciar))
      environment_graficos_iniciar$data <- NULL
      sendSweetAlert(session = session, title = HTML("Ok!"), text = HTML("Se ha eliminado la informaci&oacute;n!"),  type = "success", html = TRUE)
    }
  })

  # 3. Tablas -----

  # * 3.1 CCL -----

  # * 3.1.1 Parámetros -----

  ccl_productos <- reactive({

    progressSweetAlert(session = session, id = "progress_ccl_productos", title = tagList("Buscando productos para CCL", bs4Loading()), display_pct = T, value = 0, striped = T, status = "primary")

    productos <- global_productos()

    updateProgressBar(session = session, id = "progress_ccl_productos", value = 30, status = "success")

    adrs <- adrs %>%
      left_join(productos %>% filter(Settlement == '48hs'), by = c("Local" = "Ticker")) %>%
      select(Extranjero, Symbol, Factor) %>%
      rename(Local = Symbol) %>%
      mutate(Tipo = "ADR") %>%
      filter(complete.cases(.))

    updateProgressBar(session = session, id = "progress_ccl_productos", value = 60, status = "success")

    cedears <- productos %>%
      filter(Settlement == '48hs' & Cficode == 'EMXXXX') %>%
      rename(
        Extranjero = Ticker,
        Local = Symbol,
        Factor = TickSize) %>%
      select(Extranjero, Local, Factor) %>%
      mutate(Tipo = "CEDEAR") %>%
      filter(complete.cases(.))

    updateProgressBar(session = session, id = "progress_ccl_productos", value = 80, status = "success")

    data <- bind_rows(adrs, cedears) %>%
      mutate(
        ToShow = str_c(Local , " - ", Tipo)
      )

    rm(list = c("adrs", "cedears", "productos"))

    updateProgressBar(session = session, id = "progress_ccl_productos", value = 100, status = "success")
    Sys.sleep(0.5)
    closeSweetAlert(session = session)

    return(data)
  })

  output$ui_table_ccl <- renderUI({
    if (is.null(global_connection$conn)) {
      selectizeInput(inputId = "table_ccl_productos", label = "Producto/s:", choices = NULL, selected = "", options = list(placeholder = "Elegir producto/s..."), width = "100%")
    } else {
      prod <- setNames(object = as.list(ccl_productos()$Local), nm = ccl_productos()$ToShow)
      selectizeInput(inputId = "table_ccl_productos", label = "Producto/s:", choices = prod, selected = "", multiple = TRUE, options = list(placeholder = "Elegir producto/s..."), width = "100%")
    }
  })

  # * 3.1.2 Data + Botones -----

  observe({
    invalidateLater(millis = input$table_ccl_timer * 1000, session = session)

    if (input$table_ccl_status == TRUE) {
      shinyjs::click("table_ccl_iniciar")
    }
  })

  data_ccl <- eventReactive(list(input$table_ccl_iniciar), {

    data <- if (is.null(global_connection$conn)) {
      sendSweetAlert(session = session, title = HTML("Mmm..."), text = HTML("Tenes que conectarte primero..."),  type = "warning", html = TRUE)
      updateSwitchInput(session = session, inputId = "table_ccl_status", value = FALSE)
      NULL
    } else if (!is.null(global_connection$conn)) {
      if (is.null(input$table_ccl_productos)) {
        sendSweetAlert(session = session, title = HTML("Mmm..."), text = HTML("Tenes que seleccionar un producto primero..."),  type = "warning", html = TRUE)
        updateSwitchInput(session = session, inputId = "table_ccl_status", value = FALSE)
        NULL
      } else {
        updateSwitchInput(session = session, inputId = "table_ccl_status", value = TRUE)
        shinyjs::hide("table_ccl_iniciar")
        tryCatch(ccl(connection = global_connection$conn,
                     data = ccl_productos() %>% filter(Local %in% input$table_ccl_productos)),
                 error = function(cnd) NULL)
      }
    }

    return(data)
  }, ignoreInit = TRUE)

  observeEvent(list(input$table_ccl_parar), {
    updateSwitchInput(session = session, inputId = "table_ccl_status", value = FALSE)
    global_ccl_grafico$Precios <- NULL
    global_ccl_grafico$Timestamp <- NULL
    shinyjs::show("table_ccl_iniciar")
  })

  # * 3.1.3 Tabla -----

  output$table_ccl_tabla <- renderDT({
    shiny::validate(
      shiny::need(!is_null(data_ccl()) && nrow(data_ccl()) > 0, 'No existen datos...')
    )

    datatable(data_ccl() %>%
                select(Local, LA_price, Last, CCL_Last, CCL_Bid, CCL_Offer) %>%
                arrange(desc(CCL_Last)),
              selection = "none",
              rownames = F,
              filter = 'top',
              extensions = c('Buttons'),
              options = list(searchHighlight = TRUE,
                             dom = 'Btipr',
                             buttons = list(
                               list(
                                 extend = "excel",
                                 filename = 'ccl'
                               )
                             ),
                             pageLength = 20
              )) %>%
      formatRound(columns = c("LA_price", "Last", "CCL_Last", "CCL_Bid", "CCL_Offer"), digits = 2) %>%
      formatStyle(columns = "CCL_Last", backgroundColor = "lightblue")
  }, server = F)

  # * 3.1.4 Info Box + Grafico Promedio-----

  global_ccl_grafico <- reactiveValues(Precio = NULL, Timestamp = NULL)

  observeEvent(list(data_ccl()), {

    output$table_ccl_value <- renderText({
      sprintf(mean(data_ccl()$CCL_Last, na.rm = TRUE), fmt = "%.2f")
    })

    global_ccl_grafico$Precios <- append(x = global_ccl_grafico$Precios, values = mean(data_ccl()$CCL_Last, na.rm = TRUE))
    global_ccl_grafico$Timestamp <- append(x = global_ccl_grafico$Timestamp, values = Sys.time())

  }, ignoreNULL = FALSE)

  output$table_ccl_grafico <- renderPlotly({
    shiny::validate(
      need(!is.null(global_ccl_grafico$Precios), message = "Aún no hay data.")
    )
    plot_ly(x = global_ccl_grafico$Timestamp, y = global_ccl_grafico$Precios, mode = 'line') %>% layout(title = NULL, xaxis = list(title = "Timestamp"), yaxis = list(title = "Precio"))
  })

  # 4. Algoritmos -----

  # 4.1 The Molesto -----

  environment_the_molesto <- rlang::env()

  global_algoritmos_1 <- reactiveValues(data = NULL)

  output$ui_algoritmos_1_producto <- renderUI({

    ui <- if (is.null(global_connection$conn)) {
      selectizeInput(inputId = "algoritmos_1_producto", label = "Producto:", choices = NULL, selected = "", options = list(placeholder = "Elegir producto..."), width = "30%")
    } else {
      prod <- setNames(object = as.list(global_productos()$Symbol), nm = global_productos()$Symbol)
      selectizeInput(inputId = "algoritmos_1_producto", label = "Producto:", choices = prod, selected = "", options = list(placeholder = "Elegir producto..."), width = "30%")
    }

    return(ui)

  })

  observeEvent(input$algoritmos_1_iniciar, {

    if (is.null(global_connection$conn)) {
      updateSwitchInput(session = session, inputId = "algoritmos_1_status", value = FALSE)
      sendSweetAlert(session = session, title = HTML("Mmm..."), text = HTML("Tenes que conectarte primero..."),  type = "warning", html = TRUE)
    } else if (!is.null(global_connection$conn) & !grepl(pattern = "remarkets", x = agent(global_connection$conn))) {
      updateSwitchInput(session = session, inputId = "algoritmos_1_status", value = FALSE)
      sendSweetAlert(session = session, title = HTML("Peligro!"), text = HTML("Solo esta permitido conectarse a reMarkets..."),  type = "error", html = TRUE)
    } else {
      if (input$algoritmos_1_producto == "" || input$algoritmos_1_cuenta == "") {
        updateSwitchInput(session = session, inputId = "algoritmos_1_status", value = FALSE)
        sendSweetAlert(session = session, title = HTML("Mmm..."), text = HTML("Tenes que elegir un producto y una cuenta..."),  type = "warning", html = TRUE)
      } else {
        shinyjs::disable("algoritmos_1_iniciar")

        trading_ws_md(connection = global_connection$conn,
                      destination = "data_algoritmos_1",
                      symbol = input$algoritmos_1_producto,
                      entries = list("BI", "OF"),
                      where_is_env = environment_the_molesto)

        if (!dir.exists("logs")) {
          dir.create("logs")
        }

        logger_algoritmos_1 <- create.logger(logfile = glue("logs/the_molesto_{input$algoritmos_1_producto}_{Sys.Date()}.log"), level = "INFO")

        updateSwitchInput(session = session, inputId = "algoritmos_1_status", value = TRUE)

        info(logger_algoritmos_1, str_c("- [INICIO]"))

        global_algoritmos_1$data <- reactivePoll(intervalMillis = 2000,
                                                 session = session,
                                                 checkFunc = function() {
                                                   if (!is.null(environment_the_molesto$data_algoritmos_1)) max(environment_the_molesto$data_algoritmos_1$timestamp)
                                                 },
                                                 valueFunc = function() {
                                                   return(last(environment_the_molesto$data_algoritmos_1))
                                                 })

        output$algoritmos_1_logs <- reactiveFileReader(intervalMillis = 2000,
                                                       session = session,
                                                       filePath = glue("logs/the_molesto_{input$algoritmos_1_producto}_{Sys.Date()}.log"),
                                                       readFunc = function(x) read_file(file = x))

        shinyjs::show("algoritmos_1_logs_download_ui")

        output$algoritmos_1_logs_download <- downloadHandler(
          filename = glue("the_molesto_{input$algoritmos_1_producto}_{Sys.Date()}.log"),
          content = function(file) {
            write_file(x = read_file(file = glue("logs/the_molesto_{input$algoritmos_1_producto}_{Sys.Date()}.log")), path = file)
          }
        )

      }
    }
  })

  observeEvent(input$algoritmos_1_parar, {
    if (file.exists(glue("logs/the_molesto_{input$algoritmos_1_producto}_{Sys.Date()}.log")) & input$algoritmos_1_status == TRUE) {
      try(trading_ws_close(close_all = FALSE, selection = list("data_algoritmos_1"), where_is_env = environment_the_molesto)) # cierro conexion
      environment_the_molesto$data_algoritmos_1 <- NULL # elimino data
      logger_algoritmos_1 <- create.logger(logfile = glue("logs/the_molesto_{input$algoritmos_1_producto}_{Sys.Date()}.log"), level = "INFO") # creo nuevamente el objecto (podria haber creado un reactiveVal...)
      info(logger_algoritmos_1, str_c("- [FIN]")) # doy fin en los logs
      shinyjs::click("algoritmos_1_logs_download") # descargo los logs
      shinyjs::hide("algoritmos_1_logs_download_ui") # esoconder boton de descarga
      updateSwitchInput(session = session, inputId = "algoritmos_1_status", value = FALSE) # update estado
      sendSweetAlert(session = session, title = HTML("Ok!"), text = HTML("Se ha eliminado la informaci&oacute;n!"),  type = "success", html = TRUE) # notificacion
      shinyjs::enable("algoritmos_1_iniciar")
    } else {
      updateSwitchInput(session = session, inputId = "algoritmos_1_status", value = FALSE)
      sendSweetAlert(session = session, title = HTML("Mmm..."), text = HTML("No hay ning&uacute;n algoritmo corriendo..."),  type = "warning", html = TRUE)
    }
  })

  global_algoritmos_1_ordenes <- reactiveValues(Bid = NULL, BidPrice = NULL, Offer = NULL, OfferPrice = NULL, MinTradeVol = NULL, MinPriceIncrement = NULL)

  observe({

    req(!is.null(global_algoritmos_1$data))
    req(global_algoritmos_1$data())

    logger_algoritmos_1 <- create.logger(logfile = glue("logs/the_molesto_{input$algoritmos_1_producto}_{Sys.Date()}.log"), level = "INFO")

    # Objectivo: colocar una punta molesta, en el bid o en el ask cada vez que cambia el size en cualquiera de los casos

    isolate({

      market_data <- global_algoritmos_1$data()
      productos <- global_productos()

      global_algoritmos_1_ordenes$MinTradeVol <- productos %>% filter(Symbol == input$algoritmos_1_producto) %>% pull(MinTradeVol)
      global_algoritmos_1_ordenes$MinPriceIncrement <- productos %>% filter(Symbol == input$algoritmos_1_producto) %>% pull(MinPriceIncrement)

      if (some(.x = list("BI_price", "BI_size"), .p = ~ . %in% unlist(strsplit(market_data$Changes, ","))) &&
          every(.x = list("BI_price", "BI_size"), .p = ~ . %in% colnames(market_data)) &&
          !is.na(market_data$BI_price)) {

        info(logger_algoritmos_1, str_c("- [BID] Cambia la punta compradora"))

        if (is.null(global_algoritmos_1_ordenes$Bid)) {

          bid <- trading_new_order(connection = global_connection$conn,
                                   account  = input$algoritmos_1_cuenta,
                                   symbol   = input$algoritmos_1_producto,
                                   side     = "Buy",
                                   quantity = global_algoritmos_1_ordenes$MinTradeVol,
                                   price    = market_data$BI_price + global_algoritmos_1_ordenes$MinPriceIncrement)

          global_algoritmos_1_ordenes$Bid <- append(x = global_algoritmos_1_ordenes$Bid, values = bid$clOrdId)
          global_algoritmos_1_ordenes$BidPrice <- append(x = global_algoritmos_1_ordenes$BidPrice, values = bid$price)

          info(logger_algoritmos_1, glue("- [BID] Primer compra - {price} - {status}",
                                         price = bid$price,
                                         status = bid$status))
        } else {

          if (market_data$BI_price == last(global_algoritmos_1_ordenes$BidPrice) &&
              market_data$BI_size == global_algoritmos_1_ordenes$MinTradeVol) {

            info(logger_algoritmos_1, glue("- [BID] No juego (soy yo)"))

          } else {

            bid <- trading_new_order(connection = global_connection$conn,
                                     account  = input$algoritmos_1_cuenta,
                                     symbol   = input$algoritmos_1_producto,
                                     side     = "Buy",
                                     quantity = global_algoritmos_1_ordenes$MinTradeVol,
                                     price    = market_data$BI_price + global_algoritmos_1_ordenes$MinPriceIncrement)

            global_algoritmos_1_ordenes$Bid <- append(x = global_algoritmos_1_ordenes$Bid, values = bid$clOrdId)
            global_algoritmos_1_ordenes$BidPrice <- append(x = global_algoritmos_1_ordenes$BidPrice, values = bid$price)

            trading_cancel_order(connection = global_connection$conn, id = nth(global_algoritmos_1_ordenes$Bid, -2), proprietary = "PBCP")

            info(logger_algoritmos_1, glue("- [BID] Molesto - {price} - {status}",
                                           price = bid$price,
                                           status = bid$status))
          }
        }
      }

      # Punta vendedora
      if (some(.x = list("OF_size", "OF_size"), .p = ~ . %in% unlist(strsplit(market_data$Changes, ","))) &&
          every(.x = list("OF_price", "OF_size"), .p = ~ . %in% colnames(market_data)) &&
          !is.na(market_data$OF_price)) {

        info(logger_algoritmos_1, str_c("- [OFFER] Cambia la punta vendedora"))

        if (is.null(global_algoritmos_1_ordenes$Offer)) {

          offer <- trading_new_order(connection = global_connection$conn,
                                     account  = input$algoritmos_1_cuenta,
                                     symbol   = input$algoritmos_1_producto,
                                     side     = "Sell",
                                     quantity = global_algoritmos_1_ordenes$MinTradeVol,
                                     price    = market_data$OF_price - global_algoritmos_1_ordenes$MinPriceIncrement)

          global_algoritmos_1_ordenes$Offer <- append(x = global_algoritmos_1_ordenes$Offer, values = offer$clOrdId)
          global_algoritmos_1_ordenes$OfferPrice <- append(x = global_algoritmos_1_ordenes$OfferPrice, values = offer$price)

          info(logger_algoritmos_1, glue("- [OFFER] Primer venta - {price} - {status}",
                                         price = offer$price,
                                         status = offer$status))
        } else {

          if (market_data$OF_price == last(global_algoritmos_1_ordenes$OfferPrice) &&
              market_data$OF_size == global_algoritmos_1_ordenes$MinTradeVol) {

            info(logger_algoritmos_1, glue("- [OFFER] No juego (soy yo)"))

          } else {

            offer <- trading_new_order(connection = global_connection$conn,
                                       account  = input$algoritmos_1_cuenta,
                                       symbol   = input$algoritmos_1_producto,
                                       side     = "Sell",
                                       quantity = global_algoritmos_1_ordenes$MinTradeVol,
                                       price    = market_data$OF_price - global_algoritmos_1_ordenes$MinPriceIncrement)

            global_algoritmos_1_ordenes$Offer <- append(x = global_algoritmos_1_ordenes$Offer, values = offer$clOrdId)
            global_algoritmos_1_ordenes$OfferPrice <- append(x = global_algoritmos_1_ordenes$OfferPrice, values = offer$price)

            trading_cancel_order(connection = global_connection$conn, id = nth(global_algoritmos_1_ordenes$Offer, -2), proprietary = "PBCP")

            info(logger_algoritmos_1, glue("- [OFFER] Molesto - {price} - {status}",
                                           price = offer$price,
                                           status = offer$status))
          }
        }
      }
    })
  })

}
