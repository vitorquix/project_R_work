############################################### CABECALHO ########################
fluidPage(
  theme = shinytheme("united"),
  #theme = shinytheme("spacelab"),
  
  includeCSS("www/css/shiny.css"),

  #includeScript("www/js/scripts.js"),

  ############################################### MAIN PAGE - SHERLOCK ##########################################################
  navbarPage("Indicadores!",
             ############################################### NAVBARPAGE - HOME ####################################################################           
             tabPanel("Home",
                      sidebarLayout(
                        sidebarPanel(id="sidebar",
                                     h2("Disponibilidade"),
                                     br(),
                                     p("Este portal tem como objetivo disponibilizar informações geradas a partir da análise de dados de infraestrutura e negócios."),
                                     br(),
                                     br(),
                                     img(src = 'bsss.png', height = 45, width = 115),
                                     img(src = 'r.png', height = 35, width = 45),
                                     br(),
                                     "desenvolvido pelo", 
                                     span("GDC", style = "color:red")
                        ),
                        mainPanel(
                          h2("O que é Data Science?"),
                          br(),
                          p("É o estudo disciplinado dos dados e informações inerentes ao negócio e todas as visões que podem cercar um determinado assunto."),
                          p("E uma ciência que estuda as informações, seu processo de captura, transformação, geração e, posteriormente, análise de dados."),
                          img(src = 'background.png', width = 750, height = 313),
                          br(),
                          br(),
                          h2("Características do Portal:"),
                          br(),
                          p("* Página responsiva - Funciona em qualquer navegador ou dispositivo mobile;"),
                          p("* Por ser responsiva, os gráficos são auto ajustaveis, o zoom é ajustavel de acordo com o tamanho da tela;", 
                            p("* Possibilidade de utilizar diversas fontes de dados, bancos relacionais, dados não estruturados, estruturados;", 
                              p("* Consolidar todas as informações geradas pelas diversas ferramentas utilizadas na Bradesco Seguros."
                                
                              ))),
                          
                          br(),
                          
                          p("Para acesso a documentação do GDC, acesse",
                            a("Portal GDC.", 
                              href = "http://gdc.bradseg.com.br"))
                          
                        ), fluid = TRUE
                      )),
             
             ############################################### NAVBARPAGE - DADOS ###########################################################  
             navbarMenu("Dados",
                        ############################################### DADOS - SUMARIO ###################################################
                        tabPanel("Sumario",
                                 tags$div(HTML("<h3><center>Data Warehouse</center></h3>")),
                                 br(),
                                 div(dataTableOutput('dtsumario', width = "100%"), style = "font-size:95%, width: 75%")
                        ),
                        ############################################### DADOS - APM REAL ###################################################
                        tabPanel("Monitoramento Real",
                                 dateRangeInput('drperfapli',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(Aplicacoes_APM_Geral$Data),
                                                max = max(Aplicacoes_APM_Geral$Data),
                                                start = max(Aplicacoes_APM_Geral$Data) - 30, 
                                                end = max(Aplicacoes_APM_Geral$Data)),
                                 tags$div(HTML("<h3><center>Desempenho das aplicacoes monitoradas pelo APM</center></h3>")),
                                 div(dataTableOutput('dtperfapli', width = "100%"), style = "font-size:85%")
                        ),
                        ############################################### DADOS - APM SINTETICO ###################################################
                        tabPanel("Monitoramento Sintetico",
                                 dateRangeInput('drsintapli',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(Sintetico_APM_Geral$Data),
                                                max = max(Sintetico_APM_Geral$Data),
                                                start = max(Sintetico_APM_Geral$Data) - 30, 
                                                end = max(Sintetico_APM_Geral$Data)),
                                 tags$div(HTML("<h3><center>Disponibilidade das aplicacoes monitoradas pelo APM</center></h3>")),
                                 div(dataTableOutput('dtsintapli', width = "100%"), style = "font-size:85%")
                        ),
                        ############################################### DADOS - INCIDENTES COTI ###################################################
                        tabPanel("Incidentes COTI",
                                 dateRangeInput('drconsolidadoCOTI',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(baseFullCOTI$'Data de Abertura'),
                                                max = max(baseFullCOTI$'Data de Abertura'),
                                                start = max(baseFullCOTI$'Data de Abertura') - 30, 
                                                end = max(baseFullCOTI$'Data de Abertura')),
                                 tags$div(HTML("<h3><center>Incidentes abertos pelo Monitoramento Operacional</center></h3>")),
                                 textOutput("CCText"),
                                 div(dataTableOutput('dtDisp2', width = "100%"), style = "font-size:85%")
                        ),
      
                        ############################################### DADOS - INCIDENTES  #####################################################
                        tabPanel("Incidentes Bradesco Seguros",
                                 dateRangeInput('drconsolidadoIN',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(as.Date(baseFullIN$Hora.de.Abertura, "%d/%m/%Y")),
                                                max = max(as.Date(baseFullIN$Hora.de.Abertura, "%d/%m/%Y")),
                                                start = max(as.Date(baseFullIN$Hora.de.Abertura, "%d/%m/%Y")) - 30, 
                                                end = max(as.Date(baseFullIN$Hora.de.Abertura, "%d/%m/%Y"))),
                                 tags$div(HTML("<h3><center>Todos os incidentes registrados na BS</center></h3>")),
                                 textOutput("TextIN"),
                                 div(dataTableOutput('dtIN', width = "100%"), style = "font-size:85%")
                                 
                        ),
                        ############################################### DADOS - EVENTOS DE MONITORACAO #####################################################
                        tabPanel("Eventos de Monitoracao",
                                 dateRangeInput('drconsolidadoEventos',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(as.Date(baseEventosFull$ABERTURA)),
                                                max = max(as.Date(baseEventosFull$ABERTURA)),
                                                start = max(as.Date(baseEventosFull$ABERTURA)) - 30, 
                                                end = max(as.Date(baseEventosFull$ABERTURA))),
                                 tags$div(HTML("<h3><center>Eventos de monitoracao relacionados a infraestrutura da BS</center></h3>")),
                                 textOutput("CGEText"),
                                 div(dataTableOutput('dtDisp5', width = "100%"), style = "font-size:85%")
                                 
                        ),
                        ############################################### DADOS - TRANSACOES MAINFRAME #####################################################
                        tabPanel("Transacoes Mainframe",
                                 dateRangeInput('drconsolidadoMainframe',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(Mainframe_Geral$'Data'),
                                                max = max(Mainframe_Geral$'Data'),
                                                start = max(Mainframe_Geral$'Data') - 30, 
                                                end = max(Mainframe_Geral$'Data')),
                                 tags$div(HTML("<h3><center>Transacoes executadas na Alta Plataforma</center></h3>")),
                                 div(dataTableOutput('dtDisp6', width = "100%"), style = "font-size:90%")
                        ),
                        ############################################### DADOS - PROGRAMAS MAINFRAME #####################################################
                        tabPanel("Programas Mainframe ",
                                 dateRangeInput('drconsomainapp',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(Mainframe_Dados_Geral$'Data'),
                                                max = max(Mainframe_Dados_Geral$'Data'),
                                                start = max(Mainframe_Dados_Geral$'Data') - 30, 
                                                end = max(Mainframe_Dados_Geral$'Data')),
                                 tags$div(HTML("<h3><center>Programas executados na Alta Plataforma</center></h3>")),
                                 div(dataTableOutput('dtmainapp', width = "100%"), style = "font-size:90%")
                        ),
                        ############################################### DADOS - BIOL #####################################################
                        tabPanel("Cotacao BIOL",
                                 dateRangeInput('drbiol',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(baseBIOL$'Data'),
                                                max = max(baseBIOL$'Data'),
                                                start = max(baseBIOL$'Data') - 30, 
                                                end = max(baseBIOL$'Data')),
                                 tags$div(HTML("<h3><center>Dados de cotacao e efetivacao extraidos da base BIOL</center></h3>")),
                                 div(dataTableOutput('dtbiol', width = "100%"), style = "font-size:90%")
                        ),
                        ############################################## DADOS - REQUISICOES JVM DIARIO #####################################################
                        tabPanel("Requisicoes JVMs",
                                 dateRangeInput('drjvm',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(as.Date(baseJVMGeral$Date)),
                                                max = max(as.Date(baseJVMGeral$Date)),
                                                start = max(as.Date(baseJVMGeral$Date)) - 30,
                                                end = max(as.Date(baseJVMGeral$Date))),
                                 tags$div(HTML("<h3><center>Requisicoes das JVMs</center></h3>")),
                                 div(dataTableOutput('dtjvm', width = "100%"), style = "font-size:90%")
                        )
                        # ############################################## DADOS - REQUISICOES JVM HORA #####################################################
                        # tabPanel("Requisicoes JVMs por Hora",
                        #          dateRangeInput('drjvmh',
                        #                         label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                        #                                         Date Range")),
                        #                         min = min(as.Date(baseJVMGeralHora$Data)),
                        #                         max = max(as.Date(baseJVMGeralHora$Data)),
                        #                         start = max(as.Date(baseJVMGeralHora$Data)) - 30,
                        #                         end = max(as.Date(baseJVMGeralHora$Data))),
                        #          tags$div(HTML("<h3><center>Requisicoes das JVMs</center></h3>")),
                        #          div(dataTableOutput('dtjvmh', width = "100%"), style = "font-size:90%")
                        # )
                        
             ),
             
             ############################################### NAVBARPAGE - DESEMPENHO #################################################################
             navbarMenu("Desempenho",
                        ############################################### DESEM. - APLICACAO ###################################################
                        tabPanel("Aplicacao",             
                                 sidebarPanel(
                                   selectInput('namesApp',
                                               label = h4(HTML("<i class='glyphicon glyphicon-signal'></i>
                                            Aplicacao")),
                                               choices = unique(Aplicacoes_APM_Geral$Application)),
                                   dateRangeInput('dateRange',
                                                  label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                            Date Range")),
                                                  min = min(Aplicacoes_APM_Geral$Data),
                                                  max = max(Aplicacoes_APM_Geral$Data),
                                                  start = max(Aplicacoes_APM_Geral$Data) - 30, 
                                                  end = max(Aplicacoes_APM_Geral$Data)
                                                  
                                   ), width = 3,
                                   tags$div(HTML(paste(tags$span(style="color:blue", "* Linha Azul"), " - Media dias uteis", sep = ""))),
                                   tags$div(HTML(paste(tags$span(style="color:red", "* Linha Vermelha"), " - Media semana cheia", sep = "")))
                                   
                                 ),
                                 mainPanel(
                                   plotOutput('plot1', width = "110%", height = "400px"),
                                   plotOutput('plot2', width = "110%", height = "400px"),
                                   plotOutput('plot3', width = "110%", height = "400px"),
                                   plotOutput('plot4', width = "110%", height = "400px")
                                   
                                 ))
             ),
             
             
             ############################################### NAVBARPAGE - DISPONIBILIDADE ###########################################################
             navbarMenu("Disponibilidade",
                        tabPanel("Sistemas pela Monitoracao",
                                 sidebarPanel(
                                   selectInput('namesSystem',
                                               label = h4(HTML("<i class='glyphicon glyphicon-signal'></i>
                                         Sistema")),
                                               choices = unique(APMUniquesNames$Application)),
                                   dateRangeInput('dateRangeES',
                                                  label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                            Date Range")),
                                                  min = min(Sintetico_APM_Geral$Data),
                                                  max = max(Sintetico_APM_Geral$Data),
                                                  start = max(Sintetico_APM_Geral$Data) - 7, 
                                                  end = max(Sintetico_APM_Geral$Data)), 
                                   width = 3),
                                 mainPanel(
                                   plotOutput('esDisp1', width = "110%", height = "350px"),
                                   dataTableOutput('view')
                                   #div(dataTableOutput('view', width = '100%'), style = "font-size:90%")
                                   
                                 )
                        ),
                    
                        ############################################### DISP. - POR INCIDENTES BS ####################################################
                        tabPanel("Produtos por Incidentes BS",
                                 dateRangeInput('dateRangeIncidentesIN',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Periodo")),
                                                min = min(as.Date(baseININdisp$Hora.de.Abertura)),
                                                max = Sys.Date(),
                                                start = Sys.Date() - 30, 
                                                end = Sys.Date()),
                                 actionButton("dispinbsbutton", "Pesquisar", style = "background-color:#e95420"),
                                 textOutput("DTinText"),
                                 tags$div(HTML("<h3><center>Indisponibilidades Registradas</center></h3>")),
                                 div(dataTableOutput('dtDispIN', width = "100%"), style = "font-size:80%")
                                                )
             ),
             ############################################### INDISPONIBILIDADE ####################################################
             # navbarMenu("Indisponibilidade",
             #            ############################################### INDISP. - CICS #####################################################
             #            tabPanel("CICS",
             #                     sidebarPanel(
             #                       selectInput('namesRECURSO',
             #                                   label = h4(HTML("<i class='glyphicon glyphicon-signal'></i>
             #                                                   Recurso")),
             #                                   choices = sort(unique(baseCICSFull$RECURSO)),
             #                                   selected = "bs1a"),
             #                       
             #                       dateRangeInput('dateRangeIndisMainframe',
             #                                      label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
             #                                                      Date Range")),
             #                                      min = min(as.Date(baseEventosFull$ABERTURA)),
             #                                      max = max(as.Date(baseEventosFull$ABERTURA)),
             #                                      start = max(as.Date(baseEventosFull$ABERTURA)) - 30,
             #                                      end = max(as.Date(baseEventosFull$ABERTURA))
             #                       ), width = 3
             #                     ),
             #                     mainPanel(
             #                       plotOutput('indisCICS', width = "110%", height = "450px"),
             #                       dataTableOutput('indisCICSDT')
             #                     )
             #                     
             #            )
             # ),
             
             
             ############################################### NAVBARPAGE - DADOS TRATADOS ########################################################
             navbarMenu("Dados tratados",
                        ############################################### MAINFRAME CONSOLIDADO ##############################################
                        tabPanel("Volumetria do Mainframe Consolidado",
                                 
                                 fluidPage(
                                   tags$style(type='text/css', ".selectize-input { font-size: 13px; line-height: 13px;} 
                                                                .selectize-dropdown { font-size: 13px; line-height: 13px; }"),
                                   div(style="display: inline-block;vertical-align:top; width: 130px;",
                                       selectInput("filtroMATipo",
                                                   h4(HTML("<i class='glyphicon glyphicon-hdd'></i>
                                                       Recurso:")),
                                                   c("CICS","TRANSACOES"),
                                                   selected = "TRANSACOES")),
                                   div(style="display: inline-block;vertical-align:top; width: 230px;",
                                       selectInput("filtroVolumetriaMainframe",
                                                   h4(HTML("<i class='glyphicon glyphicon-filter'></i>
                                                       Tipo:")),
                                                   c("TOTAL DE TRANSACOES (QTD)",
                                                     "TEMP.RESP. ENTRE 0-1 SEG (%)",
                                                     "TEMP.RESP. ENTRE 0-2 SEG (%)",
                                                     "TEMP.RESP. ENTRE 0-3 SEG (%)",
                                                     "TEMP.RESP. ENTRE 0-4 SEG (%)",
                                                     "TEMP.RESP. ENTRE 0-5 SEG (%)",
                                                     "TEMP.RESP. ACIMA DE 5 SEG (%)"),
                                                   selected = "TEMP.RESP. ACIMA DE 5 SEG (%)")),
                                   dateRangeInput('dtconsolidadoMA',
                                                  label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                  min = min(Mainframe_Geral$Data),
                                                  max = max(Mainframe_Geral$Data),
                                                  start = max(Mainframe_Geral$Data) - 13,
                                                  end = max(Mainframe_Geral$Data), width = 355)),
                                 textOutput("DMText"),
                                 div(dataTableOutput('dtProj02', width = "100%"), style = "font-size:90%")
                        ),
                        
                        ############################################### MAINFRAME GERAL##############################################
                        tabPanel("Volumetria do Mainframe DB2",
                                 fluidPage(
                                   #sidebarPanel(
                                   tags$style(type='text/css', ".selectize-input { font-size: 13px; line-height: 13px;} 
                                                                .selectize-dropdown { font-size: 13px; line-height: 13px; }"),
                                   div(style="display: inline-block;vertical-align:top; width: 120px;",
                                       selectInput("filtroMAGParticao",
                                                   h4(HTML("<i class='glyphicon glyphicon-hdd'></i>
                                                       CICS:")),
                                                   unique(Mainframe_Dados_Geral$CICS),
                                                   selected = "BS1CICPA")             
                                   ),
                                   div(style="display: inline-block;vertical-align:top; width: 240px;",
                                       selectInput("filtroMAG",
                                                   h4(HTML("<i class='glyphicon glyphicon-filter'></i>
                                                       Tipo:")),
                                                   c("QUANTIDADE DE EXECUCOES",
                                                     "VOLUMETRIA",
                                                     "CONSUMO DE CPU (SEGUNDOS)"),
                                                   selected = "CONSUMO DE CPU (SEGUNDOS)")),
                                   dateRangeInput('dtconsolidadoMAG',
                                                  label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                  min = min(Mainframe_Dados_Geral$Data),
                                                  max = max(Mainframe_Dados_Geral$Data),
                                                  start = max(Mainframe_Dados_Geral$Data) - 15,
                                                  end = max(Mainframe_Dados_Geral$Data), width = 355),
                                   textOutput("DM2Text"),
                                   div(dataTableOutput('dtProj03', width = "100%"), style = "font-size:90%"),
                                   mainPanel()
                                 )
                        ),
                        
                        ############################################### INCIDENTES COTI ##############################################
                        
                        tabPanel("Incidentes COTI",
                                 dateRangeInput('dtconsolidadoCOTI',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(baseFullCOTI$`Data de Abertura`),
                                                max = max(baseFullCOTI$`Data de Abertura`),
                                                start = max(baseFullCOTI$`Data de Abertura`) - 30, 
                                                end = max(baseFullCOTI$`Data de Abertura`)),
                                 textOutput("DCIText"),
                                 tags$div(HTML("<h3><center>Incidentes abertos pelo Monitoramento Operacional</center></h3>")),
                                 br(),
                                 tags$div(HTML("<h4><center>TOP - ICs Impactados</center></h4>")),
                                 div(dataTableOutput('dtcoti4', width = "100%"), style = "font-size:90%"),
                                 tags$div(HTML("<h4><center>TOP - TIPOs Recorrentes</center></h4>")),
                                 div(dataTableOutput('dtcoti5', width = "100%"), style = "font-size:80%")
                        ),
                        
                        ############################################### ALERTAS DE MONITORACAO##############################################
                        
                        tabPanel("Eventos de Monitoração",
                                 dateRangeInput('dtconsolidadoAlertas',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(as.Date(baseEventosFull$ABERTURA)),
                                                max = max(as.Date(baseEventosFull$ABERTURA)),
                                                start = max(as.Date(baseEventosFull$ABERTURA)) - 15, 
                                                end = max(as.Date(baseEventosFull$ABERTURA))),
                                 textOutput("DAMText"),
                                 tags$div(HTML("<h3><center>Eventos de Monitoração</center></h3>")),
                                 br(),
                                 tags$div(HTML("<h4><center>TOP - Recursos Impactados</center></h4>")),
                                 div(dataTableOutput('dtaler1', width = "100%"), style = "font-size:90%"),
                                 tags$div(HTML("<h4><center>TOP - Componentes Recorrentes</center></h4>")),
                                 div(dataTableOutput('dtaler2', width = "100%"), style = "font-size:90%"),
                                 tags$div(HTML("<h4><center>TOP - Metricas Recorrentes</center></h4>")),
                                 div(dataTableOutput('dtaler3', width = "100%"), style = "font-size:90%"),
                                 tags$div(HTML("<h4><center>TOP - Mensagens Recorrentes</center></h4>")),
                                 div(dataTableOutput('dtaler4', width = "100%"), style = "font-size:90%")
                         ),
                        ############################################### REQUISICOES JVM ##############################################
                        
                        tabPanel("Requisicoes JVM",
                                 dateRangeInput('dtdadosjvm',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(as.Date(baseJVMGeral$Date)),
                                                max = max(as.Date(baseJVMGeral$Date)),
                                                start = max(as.Date(baseJVMGeral$Date)) - 10, 
                                                end = max(as.Date(baseJVMGeral$Date))),
                                 tags$div(HTML("<h3><center>Requisicoes JVM</center></h3>")),
                                 br(),
                                 div(dataTableOutput('dtjvm1', width = "100%"), style = "font-size:90%")
                        )
                        
                        
                        ),
             
             
             
             ############################################### NAVBARPAGE - PESQUISAR ELEMENTOS ########################################################
             ############################################### PESQUISA INCIDENTES ##############################################
             navbarMenu("Pesquisar elementos",
                        tabPanel("Pesquisa COTI",
                                 dateRangeInput('dtpesquisaCOTI',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = min(baseFullCOTI$`Data de Abertura`),
                                                max = max(baseFullCOTI$`Data de Abertura`),
                                                start = max(baseFullCOTI$`Data de Abertura`) - 30, 
                                                end = max(baseFullCOTI$`Data de Abertura`), width = 240),
                                 #fluidPage(
                                 # Copy the line below to make a text input box
                                 textInput("text", label = h4("Pesquisa"), value = ""),
                                 tags$style(type="text/css", "#text { height: 30px; width: 80%; text-align:left; font-size: 14px; display: block;}"),
                                 tags$div(HTML("<h6>Obs.: Servidor, Ferramenta, Aplicacao, Alerta.</h6>")),
                                 actionButton("findCOTIbutton", "Submit", style = "background-color:#e95420"),
                                 br(),
                                 br(),
                                 hr(),
                                 verbatimTextOutput("txtdtfind1"),
                                 tags$style(type="text/css", "#txtdtfind1 { font-size: 16px; text-align:center; color:black}"),
                                 div(dataTableOutput('dtfind1', width = "100%"), style = "font-size:90%"),
                                 verbatimTextOutput("txtdtfind2"),
                                 tags$style(type="text/css", "#txtdtfind2 { font-size: 16px; text-align:center; color:black}"),
                                 div(dataTableOutput('dtfind2', width = "100%"), style = "font-size:90%"),
                                 verbatimTextOutput("txtdtfind3"),
                                 tags$style(type="text/css", "#txtdtfind3 { font-size: 16px; text-align:center; color:black}"),
                                 div(dataTableOutput('dtfind3', width = "100%"), style = "font-size:90%")
                                                ),
                        ############################################### PESQUISA EVENTOS ##############################################
                        tabPanel("Pesquisa Eventos",
                                 dateRangeInput('dtpesquisaeven',
                                                label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                Date Range")),
                                                min = as.Date(min(baseEventosFull$ABERTURA)),
                                                max = as.Date(max(baseEventosFull$ABERTURA)),
                                                start = as.Date(max(baseEventosFull$ABERTURA)) - 10, 
                                                end = as.Date(max(baseEventosFull$ABERTURA)), width = 240),
                                 div(style="display:inline-block",
                                   textInput("recurso", label = h4("Pesquisa Recurso"), value = ""),
                                   textInput(inputId="metrica", label=h4("Metrica"), value = "")),
                                 br(),
                                 actionButton("findevenbutton", "Submit", style = "background-color:#e95420"),
                                 br(),
                                 br(),
                                 verbatimTextOutput("txtdtfindeven1"),
                                 tags$style(type="text/css", "#txtdtfindeven1 { font-size: 16px; text-align:center; color:black}"),
                                 div(dataTableOutput('dtfindeven1', width = "100%"), style = "font-size:90%"),
                                 verbatimTextOutput("txtdtfindeven2"),
                                 tags$style(type="text/css", "#txtdtfindeven2 { font-size: 16px; text-align:center; color:black}"),
                                 div(dataTableOutput('dtfindeven2', width = "100%"), style = "font-size:90%"),
                                 verbatimTextOutput("txtdtfindeven3"),
                                 tags$style(type="text/css", "#txtdtfindeven3 { font-size: 16px; text-align:center; color:black}"),
                                 div(dataTableOutput('dtfindeven3', width = "100%"), style = "font-size:90%")
                                 
                                 )
                        
             ),
             
             ############################################### NAVBARPAGE - GRAFICOS #################################################################
             navbarMenu("Graficos",
                        ############################################### EVENTOS POR FAIXA DE HORARIO###################################################
                        tabPanel("Eventos por Faixa de Horario",             
                                 sidebarPanel(
                                   selectInput('namesRecursosefggplot',
                                               label = h4(HTML("<i class='glyphicon glyphicon-signal'></i>
                                                               Recurso")),
                                               choices = sort(unique(baseEventosFull$RECURSO)),
                                               selected = "d7156ws0037"),
                                   dateRangeInput('dateRangeRecursos',
                                                  label = h4(HTML("<i class='glyphicon glyphicon-time'></i>
                                                                  Date Range")),
                                                  min = as.Date(min(baseEventosFull$ABERTURA)),
                                                  max = as.Date(max(baseEventosFull$ABERTURA)),
                                                  start = as.Date(max(baseEventosFull$ABERTURA)) - 30, 
                                                  end = as.Date(max(baseEventosFull$ABERTURA))
                                                  ), width = 3
                                               ),
                                 mainPanel(
                                   plotOutput('plotef1', width = "900px", height = "450px"),
                                   br(),
                                   plotOutput('plotef2', width = "960px", height = "450px")
                                 ))
                        )
             
             
  ))