# UI ----------------------------------------------------------------------

ui <- bs4DashPage(sidebar = bs4DashSidebar(title = "rRofex",
                                           skin = "light",
                                           status = "primary",
                                           brandColor = NULL,
                                           src = "https://matbarofex.github.io/rRofex/reference/figures/logo.png",
                                           bs4SidebarUserPanel(img = "https://www.pinpng.com/pngs/m/26-264278_derp-face-emoticon-funny-icons-hd-png-download.png", text = "Bienvenido!"),
                                           bs4SidebarMenu(
                                             bs4SidebarHeader("Menú"),
                                             bs4SidebarMenuItem(tabName = "configuracion", icon = "cogs", text = "Configuración"),
                                             bs4SidebarMenuItem(tabName = "graficos", icon = "chart-line", text = "Gráficos"),
                                             bs4SidebarMenuItem(icon = "table", text = "Tablas", startExpanded = FALSE,
                                                                bs4SidebarMenuSubItem(text = "CCL", tabName = "table_ccl", icon = "skull")),
                                             bs4SidebarMenuItem(tabName = "algoritmos", icon = "robot", text = "Algoritmos"))),
                  body = bs4DashBody(
                    shinyjs::useShinyjs(),
                    includeCSS("www/styles.css"),
                    bs4TabItems(
                      bs4TabItem(tabName = "configuracion",
                                 column(width = 9, offset = 3,
                                        fluidRow(
                                          bs4Box(title = "Conexión",
                                                 width = 4,
                                                 textInput("username", label = tagList(icon("user"), "Usuario"), width = "100%"),
                                                 passwordInput("password", label = tagList(icon("lock"), HTML("Contrase&ntilde;a")), width = "100%"),
                                                 textInput("base_url", label = tagList(icon("plug"), "Base URL"), width = "100%", value = "https://api.remarkets.primary.com.ar"),
                                                 div(actionButton("login", "Log-in", icon = icon("sign-in-alt"), style = "background-color:forestgreen; color:white"), align = "right")
                                          ),
                                          bs4Box(title = "Detalles",
                                                 width = 4,
                                                 textInput("details_login_date_time", label = tagList(icon("clock"), "Log-in Timestamp"), width = "100%"),
                                                 textInput("details_agent", label = tagList(icon("user-secret"), HTML("Agente")), width = "100%")
                                          )
                                        )
                                 )
                      ),
                      bs4TabItem(tabName = "graficos",
                                 fluidRow(
                                   bs4Box(title = "Parámetros", width = 3,
                                          uiOutput("ui_graficos_producto"),
                                          div(actionButton("graficos_iniciar", "Iniciar", icon = icon("play"), style = "background-color:forestgreen; color:white"),
                                              actionButton("graficos_parar", "Parar", icon = icon("stop"), style = "background-color:indianred; color:white"),
                                              align = "right")
                                   ),
                                   bs4Box(title = "Gráfico en Real Time", width = 9,
                                          plotlyOutput(outputId = "graficos_grafico")
                                   )
                                 )
                      ),
                      bs4TabItem(tabName = "table_ccl",
                                 fluidRow(
                                   bs4Box(title = "Parámetros", width = 3,
                                          uiOutput("ui_table_ccl"),
                                          sliderInput(inputId = "table_ccl_timer", label = "Actualización (segundos):", min = 2, max = 20, value = 5, step = 1,ticks = TRUE, width = "100%"),
                                          div(actionButton("table_ccl_iniciar", "Iniciar", icon = icon("play"), style = "background-color:forestgreen; color:white"),
                                              actionButton("table_ccl_parar", "Parar", icon = icon("stop"), style = "background-color:indianred; color:white"),
                                              align = "right")
                                   ),
                                   bs4TabCard(id = "table_ccl_center",
                                              title = tagList(switchInput(inputId = "table_ccl_status", onStatus = "success", offStatus = "danger", label = "Live", value = FALSE, inline = TRUE, size = "large", disabled = TRUE)),
                                              width = 6, collapsible = FALSE, closable = FALSE, maximizable = TRUE, tabStatus = "light",
                                              bs4TabPanel(tabName = "CCL",
                                                          div(style = 'overflow-x: scroll;font-size:90%', DTOutput('table_ccl_tabla'))
                                              )
                                   ),
                                   column(width = 3,
                                          bs4InfoBox(title = "Promedio CCL",
                                                     value = textOutput("table_ccl_value"),
                                                     icon = "money",
                                                     status = "success",
                                                     width = 12),
                                          bs4Box(title = "Gráfico Promedio", width = 12,
                                                 plotlyOutput(outputId = "table_ccl_grafico")
                                          )
                                   )

                                 )
                      ),
                      bs4TabItem(tabName = "algoritmos",
                                 fluidRow(
                                   bs4TabCard(id = "algoritmos_left",
                                              title = "",
                                              width = 5,
                                              collapsible = FALSE,
                                              closable = FALSE,
                                              maximizable = FALSE,
                                              tabStatus = "light",
                                              bs4TabPanel(tabName = "The Molesto",
                                                          fluidRow(
                                                            column(width = 12,
                                                                   uiOutput("ui_algoritmos_1_producto"),
                                                                   textInput(inputId = "algoritmos_1_cuenta", label = "Cuenta Comitente", width = "30%")
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(width = 12,
                                                                   verbatimTextOutput("algoritmos_1_logs"),
                                                                   shinyjs::hidden(div(id = "algoritmos_1_logs_download_ui", downloadBttn(outputId = "algoritmos_1_logs_download", label = "Descargar Logs", style = "minimal", color = "primary", block = TRUE)))
                                                            )
                                                          )
                                              )
                                   ),
                                   bs4Table(cardWrap = TRUE,
                                            headTitles = c("", "ALGORITMO", "STATUS", "ACCIÓN"),
                                            bordered = TRUE,
                                            striped = TRUE,
                                            width = 7,
                                            bs4TableItems(
                                              bs4TableItem(icon("angry"), dataCell = TRUE),
                                              bs4TableItem("The Molesto", dataCell = TRUE),
                                              bs4TableItem(switchInput(inputId = "algoritmos_1_status", onStatus = "success", offStatus = "danger", label = "Live", value = FALSE, inline = FALSE, size = "small", disabled = TRUE), dataCell = TRUE),
                                              bs4TableItem(div(actionButton("algoritmos_1_iniciar", "Iniciar", icon = icon("play"), style = "background-color:forestgreen; color:white"),
                                                               actionButton("algoritmos_1_parar", "Parar", icon = icon("stop"), style = "background-color:indianred; color:white"),
                                                               align = "right"), dataCell = TRUE)
                                            )
                                   )
                                 )
                      )
                    ),
                  ),
                  footer = bs4DashFooter(copyrights = a(
                    href = "https://matbarofex.github.io/rRofex/",
                    target = "_blank", "rRofex"
                  ),
                  right_text = "HasselPunk"),
                  title = "rRofex - Shiny App",
                  enable_preloader = TRUE,
                  loading_background = "#8B008B",
                  loading_duration = 1,
                  sidebar_collapsed = TRUE)
