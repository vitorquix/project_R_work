################################## CABECALHO ##################################
shinyServer(function(input, output, session) {
  
  ####### @@@ DESEMPENHO POR APLICACAO @@@ #######
  ####### GGPLOT - VOLUMETRIA  ##############
  
  output$plot1 <- renderPlot({
    
    withProgress(message = 'Loading...',
                 detail = ' ', value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.15)
                   }
                 })
    
    newdata <- subset(Aplicacoes_APM_Geral, Aplicacoes_APM_Geral$Application == input$namesApp &
                        Aplicacoes_APM_Geral$Data >= input$dateRange[1] & Aplicacoes_APM_Geral$Data <= input$dateRange[2]);
    
    newdata$Date_Formatada <- format(newdata$Data,"%a %b %d");
    newdata_date <- subset(newdata, (!grepl("dom\\s*", newdata$Date_Formatada) & (!grepl("s[[:lower:]]b\\s*", newdata$Date_Formatada))));
    
    ggplot(newdata, aes(x = Data , y = Requests_Breakdown, fill = Tier)) + 
      theme(legend.title = element_text(size = 13)) +
      theme(legend.text = element_text(size = 13)) +
      theme(axis.title = element_text(hjust = 0.55, size = 13 )) +
      ggtitle(paste("Volumetria - ", input$namesApp)) +
      theme(plot.title = element_text(hjust = 0.55, size = 15))  +
      ylab("") +
      xlab("") +
      scale_y_continuous(breaks = c(0,(max(newdata$Requests_Breakdown) * 0.15),
                                    (max(newdata$Requests_Breakdown) * 0.35),
                                    (max(newdata$Requests_Breakdown) * 0.55),
                                    (max(newdata$Requests_Breakdown) * 0.75),
                                    (max(newdata$Requests_Breakdown)))) + 
      theme(axis.text.y = element_text(size = 13, hjust = 1.0)) +
      scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d-%b") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12, hjust = 1.0)) +
      geom_bar(position="dodge", stat="identity", alpha=1) +
      geom_hline(yintercept = mean(newdata$Requests_Breakdown), color="red") +
      geom_hline(yintercept = mean(newdata_date$Requests_Breakdown), color="blue")
    
    
  })
  
  ######################## GGPLOT - PERFORMANCE ##################
  
  output$plot2 <- renderPlot({
    newdata <- subset(Aplicacoes_APM_Geral, Aplicacoes_APM_Geral$Application == input$namesApp &
                        Aplicacoes_APM_Geral$Data >= input$dateRange[1] & Aplicacoes_APM_Geral$Data <= input$dateRange[2]);
    
    newdata$Date_Formatada <- format(newdata$Data,"%a %b %d");
    newdata_date <- subset(newdata, (!grepl("dom\\s*", newdata$Date_Formatada) & (!grepl("s[[:lower:]]b\\s*", newdata$Date_Formatada))));
    
    ggplot(newdata, aes(x = Data , y = Performance, fill = Tier)) + 
      theme(legend.title = element_text(size = 13)) +
      theme(legend.text = element_text(size = 13)) +
      ggtitle(paste("Performance - ", input$namesApp)) +
      theme(plot.title = element_text(hjust = 0.55, size = 15))  +
      ylab("") +
      xlab("") + 
      scale_y_continuous(breaks = c(0,30,50,60,70,80,90,100)) + 
      theme(axis.text.y = element_text(size = 13, hjust = 1.0)) +
      scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d-%b") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12, hjust = 1.0)) +
      geom_area(position="dodge", size=.2, alpha=0.4) +
      geom_hline(yintercept = mean(newdata$Performance), color="red") +
      geom_hline(yintercept = mean(newdata_date$Performance), color="blue")
    
  })
  
  ######################## GGPLOT - TEMPO DE RESPOSTA ##################
  
  output$plot3 <- renderPlot({
    newdata <- subset(Aplicacoes_APM_Geral, Aplicacoes_APM_Geral$Application == input$namesApp &
                        Aplicacoes_APM_Geral$Data >= input$dateRange[1] & Aplicacoes_APM_Geral$Data <= input$dateRange[2]);
    
    newdata$Date_Formatada <- format(newdata$Data,"%a %b %d");
    
    newdata_date <- subset(newdata, (!grepl("dom\\s*", newdata$Date_Formatada) & (!grepl("s[[:lower:]]b\\s*", newdata$Date_Formatada))));
    
    ggplot(newdata, aes(x = Data , y = Operation_transaction_time_with_breakdown, fill = Tier)) + 
      theme(legend.title = element_text(size = 13)) +
      theme(legend.text = element_text(size = 13)) +
      theme(axis.title = element_text(hjust = 0.55, size = 13 )) +
      ggtitle(paste("Tempo de Resposta - ", input$namesApp)) +
      theme(plot.title = element_text(hjust = 0.55, size = 15))  +
      ylab("") +
      xlab("") +
      scale_y_continuous(breaks = c(0,(max(newdata$Operation_transaction_time_with_breakdown) * 0.15),
                                    (max(newdata$Operation_transaction_time_with_breakdown) * 0.35),
                                    (max(newdata$Operation_transaction_time_with_breakdown) * 0.55),
                                    (max(newdata$Operation_transaction_time_with_breakdown) * 0.75),
                                    (max(newdata$Operation_transaction_time_with_breakdown)))) + 
      theme(axis.text.y = element_text(size = 13, hjust = 1.0)) +
      scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d-%b") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12, hjust = 1.0)) +
      geom_bar(position="dodge", stat="identity", alpha=1) +
      geom_hline(yintercept = mean(newdata$Operation_transaction_time_with_breakdown), color="red") +
      geom_hline(yintercept = mean(newdata_date$Operation_transaction_time_with_breakdown), color="blue") 
    
  })
  
  ######################## GGPLOT - MENSAGENS DE ERRO ##################
  
  output$plot4 <- renderPlot({
    newdata <- subset(Aplicacoes_APM_Geral, Aplicacoes_APM_Geral$Application == input$namesApp &
                        Aplicacoes_APM_Geral$Data >= input$dateRange[1] & Aplicacoes_APM_Geral$Data <= input$dateRange[2]);
    
    ggplot(newdata, aes(x = Data , y = Response_Messages, fill = Tier)) + 
      theme(legend.title = element_text(size = 13)) +
      theme(legend.text = element_text(size = 13)) +
      theme(plot.title = element_text(hjust = 0.55, size = 15))  +
      theme(axis.title = element_text(hjust = 0.55, size = 13 )) +
      theme(axis.text.y = element_text(size = 13, hjust = 1.0)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12, hjust = 1.0)) +
      ggtitle(paste("Mensagens de Erros - ", input$namesApp)) +
      ylab("") +
      xlab("") +
      scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d-%b") +
      geom_bar(position="dodge", stat="identity", alpha=1)
    
  })
  
  ######################## @@@ DISPONIBILIDADE @@@##################
  ######################## GGPLOT - DISPONIBILIDADE PELO MONITORAMENTO ##################
  output$esDisp1 <- renderPlot({
    newdata <- subset(Sintetico_APM_Geral, Sintetico_APM_Geral$Application == input$namesSystem &
                        Sintetico_APM_Geral$Data > input$dateRangeES[1] & Sintetico_APM_Geral$Data < input$dateRangeES[2]);
    
    withProgress(message = 'Loading...',
                 detail = ' ', value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.15)
                   }
                 })
    
    ggplot(newdata, aes(x = Data , y = Disponibilidade, fill = Transaction)) + 
      theme(legend.title = element_text(size = 13)) +
      theme(legend.text = element_text(size = 13)) +
      ggtitle(paste("Disponibilidade - ", input$namesSystem)) +      
      theme(plot.title = element_text(hjust = 0.55, size = 15))  +
      ylab("") +
      xlab("") + 
      scale_y_continuous(breaks = c(0,30,50,60,70,80,90,100)) + 
      theme(axis.text.y = element_text(size = 13, hjust = 1.0)) +
      scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d-%b") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12, hjust = 1.0)) +
      #geom_bar(position = "dodge", stat = 'identity', size=.4, alpha=0.9) +
      geom_bar(position = "dodge", stat = 'identity', alpha=0.9) +
      geom_hline(yintercept = mean(newdata$Disponibilidade), color="red")
    
  })
  
  ######################## DATATABLE - DISPONIBILIDADE PELO MONITORAMENTO  ##################
  
  output$view <- renderDataTable({
    
    newdata <- subset(Sintetico_APM_Geral, Sintetico_APM_Geral$Application == input$namesSystem &
                        Sintetico_APM_Geral$Data >= input$dateRangeES[1] & Sintetico_APM_Geral$Data <= input$dateRangeES[2]);
    
    newdata$Application <- NULL;
    newdata$Data <- format(newdata$Data, format="%m-%d");
    newdata$Disponibilidade <- sprintf("%.2f", as.double(newdata$Disponibilidade));
    #newdata$Disponibilidade <- as.numeric(newdata$Disponibilidade)
    newdata <- reshape::melt(newdata,id.vars=c("Transaction","Data"));
    dfDisp <- as.data.frame(reshape::cast(newdata, Transaction~Data));
    
    brks <- seq(from = 0, to = 85, by = 5)
    clrs <- round(seq(100, 255, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
    
    datatable(dfDisp, options = list(
      columnDefs = list(list(className = 'dt-center', targets = 1:ncol(dfDisp))), 
      searching = FALSE),
      class = 'display cell-border compact') %>% 
      formatStyle(names(dfDisp), 
                  backgroundColor = styleInterval(brks, clrs))
    
  })
  
  ######################## DATATABLE - DISPONIBILIDADE POR INCIDENTES BS ##################
  
  output$dtDispIN <- renderDataTable({
    
    input$dispincibutton
    
    if (input$dispinbsbutton == 0)
      return()              
    isolate ({ 

      newdata <- subset(baseININdisp, as.Date(baseININdisp$Hora.de.Abertura) >= input$dateRangeIncidentesIN[1] & as.Date(baseININdisp$Hora.de.Abertura) <= input$dateRangeIncidentesIN[2]);
      
      minuts <- as.numeric(as.numeric(as.Date(input$dateRangeIncidentesIN[2]) - as.Date(input$dateRangeIncidentesIN[1])) * 24) * 60
      
      newdata <- dplyr::mutate(newdata, Valor = 1);
      
      newdata$TotalMinutosOff <- as.numeric((newdata$Dias * 24) * 60) + as.numeric(newdata$Horas * 60) + as.numeric(newdata$Minutos)
      
      # Soma os minutos off em cada sistema
      baseMinutosOff <- aggregate(TotalMinutosOff~Tipo.de.Produto,newdata,sum)
      
      baseMinutosOff <- baseMinutosOff %>% 
                            mutate(ID1 = '',
                                   ID2 = '',
                                   ID3 = '',
                                   ID4 = '')

      # Formata Coluna
      newdata$Hora.de.Abertura <- format(newdata$Hora.de.Abertura, format="%m-%d");
      
      #Popula os campos com os numeros de incidentes e a duracao
      for(y in c(0:nrow(newdata))){
        for(a in c(3:6)){
          baseMinutosOff[baseMinutosOff$Tipo.de.Produto %in% unique(newdata$Tipo.de.Produto)[y],a] <- 
            paste(
              as.character(newdata$ID.do.Incidente[which(newdata$Tipo.de.Produto == unique(newdata$Tipo.de.Produto)[y])[a-2]]),"-",
              as.character(newdata$Duracao[which(newdata$Tipo.de.Produto == unique(newdata$Tipo.de.Produto)[y])[a-2]]))
        }
      }
      
      baseMinutosOff$ID2 <- gsub("^NA - NA$", "", baseMinutosOff$ID2)
      baseMinutosOff$ID3 <- gsub("^NA - NA$", "", baseMinutosOff$ID3)
      baseMinutosOff$ID4 <- gsub("^NA - NA$", "", baseMinutosOff$ID4)

      # BASE AGREGADA POR TIPO
      baseINDISP <- dcast(setDT(newdata), Tipo.de.Produto ~ newdata$Hora.de.Abertura, value.var = "Valor", drop = TRUE)
      
      # Left join na coluna total minutos off
      baseINDISP <- left_join(baseINDISP, baseMinutosOff %>% select(Tipo.de.Produto, TotalMinutosOff))
      
      # converte em percentual
      baseINDISP$Disponibilidade <- ((minuts - as.numeric(baseINDISP$TotalMinutosOff)) / minuts) * 100
      
      baseINDISP$SLA <- ifelse(baseINDISP$Tipo.de.Produto == "SCAM", 97, 99)

      # Diminui para 2 casas decimais
      baseINDISP$Disponibilidade <- as.numeric(format(round(baseINDISP$Disponibilidade, 2), nsmall = 2));
      
      baseINDISP$Situacao <- ifelse(as.numeric(format(round(baseINDISP$SLA - baseINDISP$Disponibilidade, 2), nsmall = 2)) < 0, "Favoravel", "Desfavoravel")
      
      baseINDISP$SLA <- sprintf("%.2f", round(baseINDISP$SLA,2))  
      
      colnames(baseINDISP)[1] <- "Sistemas"
      colnames(baseINDISP)[ncol(baseINDISP)-3] <- "Indisp.(Minutos)"
      
      baseINDISP[baseINDISP == 0] <- NA
      
      baseINDISP <- baseINDISP %>% 
        mutate(ID1 = baseMinutosOff$ID1,
               ID2 = baseMinutosOff$ID2,
               ID3 = baseMinutosOff$ID3,
               ID4 = baseMinutosOff$ID4)
      
      baseINDISP <- baseINDISP[moveme(names(baseINDISP), "ID4 first")]
      baseINDISP <- baseINDISP[moveme(names(baseINDISP), "ID3 first")]
      baseINDISP <- baseINDISP[moveme(names(baseINDISP), "ID2 first")]
      baseINDISP <- baseINDISP[moveme(names(baseINDISP), "ID1 first")]
      
      brks <- seq(from = 0, to = 3, by = 1)
      clrs <- round(seq(255, 50, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}

       datatable(cbind(' ' = '&oplus;', baseINDISP), escape = -2,
          options = list(
          columnDefs = list(
          list(visible = FALSE, targets = c(2,3,4,5)),
          list(orderable = FALSE, className = 'details-control', targets = 1),
          list(className = 'dt-center', targets = 7:ncol(baseINDISP))
          ),
        pageLength = 25),
        callback = JS("
                 table.column(1).nodes().to$().css({cursor: 'pointer'});
                     var format = function(d) {
                        return '<div style=\"background-color:#eeeee; padding: .5em;\"> <b>IN/Duracao:</b><br/> ' +
                        d[2] + '<br/>' + d[3] + '<br/>' + d[4] + '<br/>' + d[5] +'</div>';
                        };

                 table.on('click', 'td.details-control', function() {
                     var td = $(this);
                     row = table.row(td.closest('tr'));

                            if (row.child.isShown()) {
                              row.child.hide();
                              td.html('&oplus;');
                            } else {
                              row.child(format(row.data())).show();
                               td.html('&CircleMinus;');
                              }
                            });"
       ),class = 'display cell-border compact') %>%
         formatStyle(names(baseINDISP)[6:(ncol(baseINDISP)-4)], backgroundColor = styleInterval(brks, clrs)) %>%
         formatStyle('Situacao', color = 'black', backgroundColor = styleEqual("Desfavoravel", 'red'))
        
    })
  })
  
  # ######################## @@@ INDISPONIBILIDADES @@@##################
  # 
  # ######################## GGPLOT - CICS  ##################
  # output$indisCICS <- renderPlot({
  #   
  #   newdataCICS <- subset(baseCICSFull, baseCICSFull$RECURSO == input$namesRECURSO &
  #                           baseCICSFull$`Data de Abertura` >= input$dateRangeIndisMainframe[1] & 
  #                           baseCICSFull$`Data de Abertura` <= input$dateRangeIndisMainframe[2]);
  #   
  #   newdataCICS$`Data de Abertura` <- as.Date(newdataCICS$`Data de Abertura`);
  #   newdataCICS$`Data de Abertura` <- as.Date(newdataCICS$`Data de Abertura`);
  #   newdataCICS$`Hora de Abertura` <- as.POSIXct(newdataCICS$`Hora de Abertura`,format="%H:%M:%S")
  #   
  #   ggplot(newdataCICS, aes(x = newdataCICS$`Data de Abertura`, y = newdataCICS$`Hora de Abertura`, color = newdataCICS$COMPONENTE)) +
  #     ggtitle("Indisponibilidades - CICS") +
  #     theme(plot.title = element_text(hjust = 0.5))  +
  #     labs(color = "Componente") +
  #     ylab("Faixa de Abertura do Alerta") +
  #     scale_y_datetime(date_breaks = "1 hour", date_labels = "%H:00") +
  #     xlab("Periodo") +
  #     scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d-%b") +
  #     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 14)) +
  #     theme(plot.title = element_text(size = 20)) +
  #     theme(text = element_text(size = 16)) +
  #     geom_point(size = 3)
  #   
  #   
  # })
  # 
  # ######################## DATATABLE - CICS   ##################
  # output$indisCICSDT <- renderDataTable({
  #   
  #   newdataCICS <- subset(baseCICSFull, baseCICSFull$RECURSO == input$namesRECURSO &
  #                           baseCICSFull$`Data de Abertura` >= input$dateRangeIndisMainframe[1] & baseCICSFull$`Data de Abertura` <= input$dateRangeIndisMainframe[2]);
  #   
  #   days <- as.numeric(as.numeric(as.Date(input$dateRangeIndisMainframe[2]) - as.Date(input$dateRangeIndisMainframe[1])) * 24) * 60
  #   
  #   newdataCICS <- dplyr::mutate(newdataCICS, Valor = 1);
  #   newdataCICS$TotalMinutosOff <- as.numeric((newdataCICS$Dias * 24) * 60) + as.numeric(newdataCICS$Horas * 60) + as.numeric(newdataCICS$Minutos)
  #   newdataCICS$Disponibilidade = ((days - as.numeric(newdataCICS$TotalMinutosOff)) / days) * 100
  #   baseIndispFiltradaCICS = aggregate(Disponibilidade~COMPONENTE,newdataCICS,mean)
  #   baseIndispFiltradaTotalCICS = aggregate(TotalMinutosOff~COMPONENTE,newdataCICS,sum)
  #   baseIndispFiltradaValorCICS = aggregate(Valor~COMPONENTE,newdataCICS,sum)
  #   baseIndispFiltradaCICS$Indisponibilidade = baseIndispFiltradaTotalCICS$TotalMinutosOff
  #   baseIndispFiltradaCICS$Incidentes = baseIndispFiltradaValorCICS$Valor
  #   colnames(baseIndispFiltradaCICS)[2] <- "Disponibilidade"
  #   colnames(baseIndispFiltradaCICS)[3] <- "Indisponibilidade (Minutos)"
  #   colnames(baseIndispFiltradaCICS)[4] <- "Alertas (Indisponibilidade)"
  #   baseIndispFiltradaCICS$Disponibilidade <- as.numeric(format(round(baseIndispFiltradaCICS$Disponibilidade, 2), nsmall = 2));
  #   baseIndispFiltradaCICS$Disponibilidade <- paste(baseIndispFiltradaCICS$Disponibilidade,"%", sep = "")
  #   
  #   datatable(baseIndispFiltradaCICS, options = list(
  #     columnDefs = list(list(className = 'dt-center', targets = 1:4)),
  #     pageLength = 50),
  #     class = 'display cell-border compact')
  #   
  #   
  # })
  # 
  # ######################## GGPLOT - DB2  ##################
  # output$indisDB2 <- renderPlot({
  #   
  #   newdataDBS <- subset(baseDB2Full, baseDB2Full$RECURSO == input$namesRECURSODB2 &
  #                          baseDB2Full$`Data de Abertura` >= input$dateRangeIndisDB2[1] & baseDB2Full$`Data de Abertura` <= input$dateRangeIndisDB2[2]);
  #   
  #   newdataDBS$`Data de Abertura` <- as.Date(newdataDBS$`Data de Abertura`);
  #   newdataDBS$`Data de Abertura` <- as.Date(newdataDBS$`Data de Abertura`);
  #   newdataDBS$`Hora de Abertura` <- as.POSIXct(newdataDBS$`Hora de Abertura`,format="%H:%M:%S")
  #   
  #   ggplot(newdataDBS, aes(x = newdataDBS$`Data de Abertura`, y = newdataDBS$`Hora de Abertura`, color = newdataDBS$COMPONENTE)) +
  #     ggtitle("Indisponibilidades - DB2") +
  #     theme(plot.title = element_text(hjust = 0.5))  +
  #     labs(color = "Componente") +
  #     ylab("Faixa de Abertura do Alerta") +
  #     scale_y_datetime(date_breaks = "1 hour", date_labels = "%H:00") +
  #     xlab("Periodo") +
  #     scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d-%b") +
  #     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 14)) +
  #     theme(plot.title = element_text(size = 20)) +
  #     theme(text = element_text(size = 16)) +
  #     geom_point(size = 3)
  #   
  #   
  # })
  # 
  # ######################## DATATABLE - DB2   ##################
  # output$indisDB2DT <- renderDataTable({
  #   
  #   newdataDBS <- subset(baseDB2Full, baseDB2Full$RECURSO == input$namesRECURSODB2 &
  #                          baseDB2Full$`Data de Abertura` >= input$dateRangeIndisDB2[1] & baseDB2Full$`Data de Abertura` <= input$dateRangeIndisDB2[2]);
  #   
  #   days <- as.numeric(as.numeric(as.Date(input$dateRangeIndisDB2[2]) - as.Date(input$dateRangeIndisDB2[1])) * 24) * 60
  #   
  #   newdataDBS <- dplyr::mutate(newdataDBS, Valor = 1);
  #   newdataDBS$TotalMinutosOff <- as.numeric((newdataDBS$Dias * 24) * 60) + as.numeric(newdataDBS$Horas * 60) + as.numeric(newdataDBS$Minutos)
  #   newdataDBS$Disponibilidade = ((days - as.numeric(newdataDBS$TotalMinutosOff)) / days) * 100
  #   baseIndispFiltradaDB2 = aggregate(Disponibilidade~COMPONENTE,newdataDBS,mean)
  #   baseIndispFiltradaTotalDB2 = aggregate(TotalMinutosOff~COMPONENTE,newdataDBS,sum)
  #   baseIndispFiltradaValorDB2 = aggregate(Valor~COMPONENTE,newdataDBS,sum)
  #   baseIndispFiltradaDB2$Indisponibilidade = baseIndispFiltradaTotalDB2$TotalMinutosOff
  #   baseIndispFiltradaDB2$Incidentes = baseIndispFiltradaValorDB2$Valor
  #   colnames(baseIndispFiltradaDB2)[2] <- "Disponibilidade"
  #   colnames(baseIndispFiltradaDB2)[3] <- "Indisponibilidade (Minutos)"
  #   colnames(baseIndispFiltradaDB2)[4] <- "Alertas (Indisponibilidade)"
  #   baseIndispFiltradaDB2$Disponibilidade <- as.numeric(format(round(baseIndispFiltradaDB2$Disponibilidade, 2), nsmall = 2));
  #   baseIndispFiltradaDB2$Disponibilidade <- paste(baseIndispFiltradaDB2$Disponibilidade,"%", sep = "")
  #   
  #   datatable(baseIndispFiltradaDB2, options = list(
  #     columnDefs = list(list(className = 'dt-center', targets = 1:4)),
  #     pageLength = 50),
  #     class = 'display cell-border compact')
  #   
  #   
  # })
  
  ######################## @@@ IMPACTO @@@##################
  
  ######################## GGPLOT - GRAFICOS (COTI) ##################
  
  output$grDisp1 <- renderPlot({
    
    baseTempIncidentes <- subset(baseFullCOTI, baseFullCOTI$`Data de Abertura` > input$dateRangeIncidentes[1] & baseFullCOTI$`Data de Abertura` < input$dateRangeIncidentes[2]);
    
    # Cria funcao necessaria para classificar os incidentes
    procura <- as.data.frame(sapply(as.character(baseICs$IC), function(x) grepl(x, baseTempIncidentes$Solicitacao)));
    procura$Missing[rowSums(procura) == 0] <- T
    for(i in 1:nrow(baseTempIncidentes)) {
      baseTempIncidentes$IC[i] <- colnames(procura)[which(procura[i,] == T)]
    }
    
    baseFiltrada <- subset(baseTempIncidentes, IC != "N/A");
    baseFiltrada <- left_join(baseTempIncidentes, baseICs %>% select(IC, Cluster))
    baseFiltrada <- left_join(baseFiltrada, baseICs %>% select(IC, Vertical))
    
    # Cria coluna Valor
    baseFiltrada <- dplyr::mutate(baseFiltrada, Valor = 1);
    baseFiltrada[is.na(baseFiltrada)] <- "N/A"
    
    baseVertical <- aggregate(Valor~Vertical,baseFiltrada,sum);
    
    baseVertical$fraction = baseVertical$Valor / sum(baseVertical$Valor);
    baseVertical = baseVertical[order(baseVertical$fraction), ];
    baseVertical$ymax = cumsum(baseVertical$fraction);
    baseVertical$ymin = c(0, head(baseVertical$ymax, n=-1));
    baseVertical$fraction <- format(round(baseVertical$fraction, 2), nsmall = 2);
    baseVertical$fraction <- as.double(baseVertical$fraction);
    
    withProgress(message = 'Loading...',
                 detail = ' ', value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.15)
                   }
                 })
    
    ggplot(baseVertical, aes(fill=Vertical, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect() +
      coord_polar(theta="y") +
      xlim(c(0, 4)) +
      xlab("") +
      ylab("") +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      annotate("text", x = 0, y = 0, label = "Incidentes por Vertical", size = 5) +
      labs(title="") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      theme(legend.title = element_text(size = 13)) +
      theme(legend.text = element_text(size = 13)) +
      geom_label(aes(label=paste(fraction*100,"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE, size = 5)
    
  })
  
  ######################## @@@ DADOS  @@@ ##################
  
  ######################## DATATABLE - SUMARIO ############
  output$dtsumario <- renderDataTable({
    
    newdata <- data.frame(matrix(NA,10,6))
    names(newdata) <- c("NOME","FERRAMENTA","DESCRICAO","QTD_REGISTROS","DT_INICIO","DT_FIM")
    
    newdata[1,] <- c("Monitoramento Real", "APM","Desempenho das Aplicacoes",nrow(Aplicacoes_APM_Geral),
                     format(min(Aplicacoes_APM_Geral$Data), "%Y-%m-%d"), format(max(Aplicacoes_APM_Geral$Data), "%Y-%m-%d"))
    
    newdata[2,] <- c("Monitoramento Sintetico", "APM","Disponibilidade das Aplicacoes",nrow(Sintetico_APM_Geral),
                     format(min(Sintetico_APM_Geral$Data), "%Y-%m-%d"), format(max(Sintetico_APM_Geral$Data), "%Y-%m-%d"))
    
    newdata[3,] <- c("Incidentes COTI", "HPSM","Incidentes abertos pelo COTI",nrow(baseFullCOTI),
                     format(min(baseFullCOTI$`Data de Abertura`), "%Y-%m-%d"), format(max(baseFullCOTI$`Data de Abertura`), "%Y-%m-%d"))    
    
    newdata[4,] <- c("Eventos de Monitoracao", "Tivoli", "Alertas de Infraestrutura",nrow(baseEventosFull),
                     format(min(baseEventosFull$ABERTURA), "%Y-%m-%d"), format(max(baseEventosFull$ABERTURA), "%Y-%m-%d"))
    
    newdata[5,] <- c("Transacoes Mainframe", "Mainframe", "Transacoes executadas no Mainframe",nrow(Mainframe_Geral),
                     format(min(Mainframe_Geral$Data), "%Y-%m-%d"), format(max(Mainframe_Geral$Data), "%Y-%m-%d"))
    
    newdata[6,] <- c("Programas Mainframe", "Mainframe","Programas executados no Mainframe", nrow(Mainframe_Dados_Geral),
                     format(min(Mainframe_Dados_Geral$Data), "%Y-%m-%d"), format(max(Mainframe_Dados_Geral$Data), "%Y-%m-%d"))
    
    newdata[7,] <- c("Volumetria de Negocios", "BIOL","Cotacao e efetivacao de seguros auto", nrow(baseBIOL),
                     format(min(baseBIOL$Data), "%Y-%m-%d"), format(max(baseBIOL$Data), "%Y-%m-%d")) 
    
    newdata[8,] <- c("Incidentes BS", "HPSM","Todos os incidentes registrados na BS", nrow(baseFullIN),
                     format(min(as.Date(baseFullIN$Hora.de.Abertura,"%d/%m/%Y"))), format(max(as.Date(baseFullIN$Hora.de.Abertura,"%d/%m/%Y")))) 
    
    newdata[9,] <- c("Requisicoes por JVM Diario", "Dynatrace","Quantidade de requisicoes por JVM por Dia", nrow(baseJVMGeral),
                      format(min(as.Date(baseJVMGeral$Date)), "%Y-%m-%d"), format(max(as.Date(baseJVMGeral$Date)), "%Y-%m-%d")) 
    
    newdata[10,] <- c("Requisicoes por JVM Hora", "Dynatrace","Quantidade de requisicoes por JVM por Hora", nrow(baseJVMGeralHora),
                      format(min(as.Date(baseJVMGeral$Date)), "%Y-%m-%d"), format(max(as.Date(baseJVMGeral$Date)), "%Y-%m-%d")) 
    
    newdata$`QTD_REGISTROS` <- as.numeric(newdata$`QTD_REGISTROS`)
    newdata$DT_INICIO <- as.Date(newdata$DT_INICIO)
    newdata$DT_FIM <- as.Date(newdata$DT_FIM)
    
    
    # datatable(newdata, options = list(
    #   colReorder = TRUE,
    #   columnDefs = list(list(className = 'dt-center', targets = 0:4)),
    #   scrollX = TRUE
    # ), filter = 'top',
    # class = 'display cell-border compact',
    # rownames = TRUE)
    
    datatable(newdata, options = list(dom = 't',
                                      columnDefs = list(list(className = 'dt-center', targets = 0:6)),
                                      pageLength = 15),
              class = 'display cell-border compact')
    
    
  })
  
  ######################## DATATABLE - MONITORAMENTO REAL ############
  output$dtperfapli <- renderDataTable({
    
    newdata <- subset(Aplicacoes_APM_Geral, as.Date(Aplicacoes_APM_Geral$Data) >= input$drperfapli[1] & as.Date(Aplicacoes_APM_Geral$Data) <= input$drperfapli[2]);
    
    newdata$Performance <- format(newdata$Performance, digits = 2)
    
    datatable(newdata, extensions = c('Buttons','ColReorder'), options = list(
      colReorder = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'excel',
          buttons = 'excel',
          text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Excel</h4>'),             
        list(
          extend = 'colvis',
          buttons = list('colvis','colvisRestore'),
          text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Filtrar Colunas</h4>')),
      searchHighlight = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:7)),
      scrollX = TRUE,
      pageLength = 100
    ), filter = 'top',
    class = 'display cell-border compact',
    rownames = FALSE)
    
  })
  
  ######################## DATATABLE - MONITORAMENTO SINTETICO ############
  output$dtsintapli <- renderDataTable({
    
    newdata <- subset(Sintetico_APM_Geral, as.Date(Sintetico_APM_Geral$Data) >= input$drsintapli[1] & as.Date(Sintetico_APM_Geral$Data) <= input$drsintapli[2]);
    
    newdata$Disponibilidade <- format(newdata$Disponibilidade, digits = 2)
    
    datatable(newdata, extensions = c('Buttons','ColReorder'), options = list(
      colReorder = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'excel',
          buttons = 'excel',
          text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Excel</h4>'),             
        list(
          extend = 'colvis',
          buttons = list('colvis','colvisRestore'),
          text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Filtrar Colunas</h4>')),
      searchHighlight = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:3)),
      scrollX = TRUE,
      pageLength = 100
    ), filter = 'top',
    class = 'display cell-border compact',
    rownames = FALSE)
  })
  
  
  ######################## DATATABLE - INCIDENTES COTI ##################
  
  output$dtDisp2 <- renderDataTable({
    
    d <- function(a) { a ;
      warning("Consolidado COTI - Digite data Valida") ;
      message("This is a message not a warning")}
    
    newdata <- subset(baseFullCOTI, as.Date(baseFullCOTI$`Data de Abertura`) >= input$drconsolidadoCOTI[1] & as.Date(baseFullCOTI$`Data de Abertura`) <= input$drconsolidadoCOTI[2]);
    
    if(as.numeric(as.Date(input$drconsolidadoCOTI[2]) - as.Date(input$drconsolidadoCOTI[1])) < 0){
      
      output$CCText <- renderText("DIGITE PERIODO VALIDO")
      suppressMessages(b(1))
      
    }else if(as.numeric(as.Date(input$drconsolidadoCOTI[2]) - as.Date(input$drconsolidadoCOTI[1])) > 0){
      
      output$CCText <- renderText(" ");
      
      newdata$Valor <- NULL
      newdata$ICS <- NULL
      newdata$TIPO <- NULL
      
      datatable(newdata, extensions = c('Scroller','Buttons','ColReorder'), options = list(
        colReorder = TRUE,
        dom = 'Bfrtip',
        buttons = 
          list(
            list(
              extend = 'excel',
              buttons = 'excel',
              text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Excel</h4>'),
            list(
              extend = 'colvis',
              buttons = list('colvis','colvisRestore'),
              text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Filtrar Colunas</h4>')),
        searchHighlight = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = 0:6)), 
        scrollX = TRUE,
        pageLength = 100
      ), filter = 'top',
      class = 'display cell-border compact',
      rownames = FALSE)
      
    }      
  })
  
  ######################## DATATABLE - INCIDENTES BS ##################
  
  output$dtIN <- renderDataTable({
    
    d <- function(a) { a ;
      warning("Consolidado IN - Digite data Valida") ;
      message("This is a message not a warning")}
    
    newdata <- subset(baseFullIN, as.Date(baseFullIN$Hora.de.Abertura,"%d/%m/%Y") >= input$drconsolidadoIN[1] & as.Date(baseFullIN$Hora.de.Abertura, "%d/%m/%Y") <= input$drconsolidadoIN[2]);
    
    if(as.numeric(as.Date(input$drconsolidadoIN[2]) - as.Date(input$drconsolidadoIN[1])) < 0){
      
      output$TextIN <- renderText("DIGITE PERIODO VALIDO")
      suppressMessages(c(1))
      
    }else if(as.numeric(as.Date(input$drconsolidadoIN[2]) - as.Date(input$drconsolidadoIN[1])) > 0){
      
      output$TextIN <- renderText(" ");

      datatable(newdata, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'excel',
            buttons = 'excel',
            text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Excel</h4>'),             
          list(
            extend = 'colvis',
            buttons = list('colvis','colvisRestore'),
            text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Filtrar Colunas</h4>')),
        searchHighlight = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = 0:11)),
        scrollX = TRUE,
        pageLength = 125
      ), filter = 'top',
      class = 'display cell-border compact',
      rownames = FALSE)
      
    }
    
  })
  
  ######################## DATATABLE - ALERTAS DE MONITORACAO ##################
  
  output$dtDisp5 <- renderDataTable({
    
    withProgress(message = 'Loading...',
                 detail = ' ', value = 0, {
                   
                   baseEventosFull <- tidyr::separate(baseEventosFull, col = ABERTURA, into = c("Data de Abertura", "Hora de Abertura"), sep = "\\ ");
                   baseEventosFull$`Data de Abertura` <- as.Date(baseEventosFull$`Data de Abertura`);
                   baseEventosFull <- tidyr::separate(baseEventosFull, col = FECHAMENTO, into = c("Data de Fechamento", "Hora de Fechamento"), sep = "\\ ");
                   baseEventosFull$`Data de Fechamento` <- as.Date(baseEventosFull$`Data de Fechamento`);
                   baseEventosFull <- baseEventosFull[order(as.Date(baseEventosFull$`Data de Abertura`, format="%d-%m-%Y"), decreasing = TRUE),]
                   
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.15)
                   }
                 })
    
    d <- function(a) { a ;
      warning("Consolidado Eventos de Monitoracao - Digite data Valida") ;
      message("This is a message not a warning")}
    
    newdata <- subset(baseEventosFull, as.Date(baseEventosFull$`Data de Abertura`) >= input$drconsolidadoEventos[1] & as.Date(baseEventosFull$`Data de Abertura`) <= input$drconsolidadoEventos[2]);
    
    if(as.numeric(as.Date(input$drconsolidadoEventos[2]) - as.Date(input$drconsolidadoEventos[1])) < 0){
      
      output$CGEText <- renderText("DIGITE PERIODO VALIDO")
      suppressMessages(d(1))
      
    }else if(as.numeric(as.Date(input$drconsolidadoEventos[2]) - as.Date(input$drconsolidadoEventos[1])) > 0){
      
      output$CGEText <- renderText(" ");
      
      baseEventosFull$Valor <- NULL;
      
      datatable(newdata, extensions = c('Buttons','ColReorder'), options = list(
        colReorder = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'excel',
            buttons = 'excel',
            text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Excel</h4>'),             
          list(
            extend = 'colvis',
            buttons = list('colvis','colvisRestore'),
            text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Filtrar Colunas</h4>')),
        searchHighlight = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = 0:9)),
        scrollX = TRUE,
        pageLength = 100
      ), filter = 'top',
      class = 'display cell-border compact',
      rownames = FALSE)
      
    }
    
  })
  
  ######################## DATATABLE - MAINFRAME TRANSACOES############
  output$dtDisp6 <- renderDataTable({
    
    newdata <- subset(Mainframe_Geral, as.Date(Mainframe_Geral$`Data`) >= input$drconsolidadoMainframe[1] & as.Date(Mainframe_Geral$`Data`) <= input$drconsolidadoMainframe[2]);
    
    newdata <- newdata[-c(7:13)]
    
    newdata$SEG. <- NULL
    
    datatable(newdata, extensions = c('Buttons','ColReorder'), options = list(
      colReorder = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'excel',
          buttons = 'excel',
          text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Excel</h4>'),             
        list(
          extend = 'colvis',
          buttons = list('colvis','colvisRestore'),
          text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Filtrar Colunas</h4>')),
      searchHighlight = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:7)),
      scrollX = TRUE,
      pageLength = 100
    ), filter = 'top',
    class = 'display cell-border compact',
    rownames = FALSE)
    
  })
  
  ######################## DATATABLE - MAINFRAME PROGRAMAS############
  output$dtmainapp <- renderDataTable({
    
    newdata <- subset(Mainframe_Dados_Geral, as.Date(Mainframe_Dados_Geral$`Data`) >= input$drconsomainapp[1] & as.Date(Mainframe_Dados_Geral$`Data`) <= input$drconsomainapp[2]);
    
    datatable(newdata, extensions = c('Buttons','ColReorder'), options = list(
      colReorder = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'excel',
          buttons = 'excel',
          text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Excel</h4>'),             
        list(
          extend = 'colvis',
          buttons = list('colvis','colvisRestore'),
          text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Filtrar Colunas</h4>')),
      searchHighlight = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:5)),
      scrollX = TRUE,
      pageLength = 100
    ), filter = 'top',
    class = 'display cell-border compact',
    rownames = FALSE)
    
  })
  
  ####################### DATATABLE - BIOL############
  output$dtbiol <- renderDataTable({

    newdata <- subset(baseBIOL, as.Date(baseBIOL$`Data`) >= input$drbiol[1] & as.Date(baseBIOL$`Data`) <= input$drbiol[2]);
  
    colnames(newdata)[5] <- "Proporcao (Efe/Cot)"
    
    datatable(newdata, extensions = c('Buttons','ColReorder'), options = list(
      colReorder = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'excel',
          buttons = 'excel',
          text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Excel</h4>'),
        list(
          extend = 'colvis',
          buttons = list('colvis','colvisRestore'),
          text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Filtrar Colunas</h4>')),
      searchHighlight = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:4)),
      scrollX = TRUE,
      pageLength = 100
    ), filter = 'top',
    class = 'display cell-border compact',
    rownames = FALSE)

  })
  
  ####################### DATATABLE - REQ JVMS DIARIO ############
  output$dtjvm <- renderDataTable({
    
    newdata <- subset(baseJVMGeral, as.Date(baseJVMGeral$Date) >= input$drjvm[1] & as.Date(baseJVMGeral$Date) <= input$drjvm[2]);
    
    datatable(newdata, extensions = c('Buttons','ColReorder'), options = list(
      colReorder = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'excel',
          buttons = 'excel',
          text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Excel</h4>'),
        list(
          extend = 'colvis',
          buttons = list('colvis','colvisRestore'),
          text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Filtrar Colunas</h4>')),
      searchHighlight = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:2)),
      scrollX = TRUE,
      pageLength = 100
    ), filter = 'top',
    class = 'display cell-border compact',
    rownames = FALSE)
    
  })
  
  ####################### DATATABLE - REQ JVMS HORA############
  # output$dtjvmh <- renderDataTable({
  #   
  #   newdata <- subset(baseJVMGeralHora, as.Date(baseJVMGeralHora$Data) >= input$drjvmh[1] & as.Date(baseJVMGeralHora$Data) <= input$drjvmh[2]);
  #   
  #   datatable(newdata, extensions = c('Buttons','ColReorder'), options = list(
  #     colReorder = TRUE,
  #     dom = 'Bfrtip',
  #     buttons = list(
  #       list(
  #         extend = 'excel',
  #         buttons = 'excel',
  #         text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Excel</h4>'),
  #       list(
  #         extend = 'colvis',
  #         buttons = list('colvis','colvisRestore'),
  #         text = '<h4 style="font-size: 12px;"><i class="fa fa-trash fa-x5"></i> Filtrar Colunas</h4>')),
  #     searchHighlight = TRUE,
  #     columnDefs = list(list(className = 'dt-center', targets = 0:3)),
  #     scrollX = TRUE,
  #     pageLength = 100
  #   ), filter = 'top',
  #   class = 'display cell-border compact',
  #   rownames = FALSE)
  #   
  # })
  
  ######################## @@@ DADOS TRATADOS @@@############
  ######################## DADOS MAINFRAME CONSOLIDADO############
  output$dtProj02 <- renderDataTable({
    
    d <- function(a) { a ;
      warning("Dados Mainframe - Digite data Valida") ;
      message("This is a message not a warning")}
    
    newdata <- subset(Mainframe_Geral, as.Date(Mainframe_Geral$Data) >= input$dtconsolidadoMA[1] & as.Date(Mainframe_Geral$Data) <= input$dtconsolidadoMA[2]);
    
    if(input$filtroMATipo == "CICS"){
      newdata <- subset(newdata, TRANSACTION %in% c("A01CICBS",
                                                    "BS1CICAR",
                                                    "BS1CICBC",
                                                    "BS1CICCX",
                                                    "BS1CICPA",
                                                    "BS1CICQM",
                                                    "BS1CICSA",
                                                    "BS1CICUC",
                                                    "BS1CICVD",
                                                    "SAUDE"))
    }else if(input$filtroMATipo == "TRANSACOES"){
      newdata <- subset(newdata, !TRANSACTION %in% c("A01CICBS",
                                                     "BS1CICAR",
                                                     "BS1CICBC",
                                                     "BS1CICCX",
                                                     "BS1CICPA",
                                                     "BS1CICQM",
                                                     "BS1CICSA",
                                                     "BS1CICUC",
                                                     "BS1CICVD",
                                                     "SAUDE"))
    }
    
    if(as.numeric(as.Date(input$dtconsolidadoMA[2]) - as.Date(input$dtconsolidadoMA[1])) < 0){
      
      output$DMText <- renderText("DIGITE PERIODO VALIDO")
      suppressMessages(d(1))
      
    }else if(as.numeric(as.Date(input$dtconsolidadoMA[2]) - as.Date(input$dtconsolidadoMA[1])) > 0){
      
      output$DMText <- renderText(" ");
      
      brks <- seq(from = 0, to = 100, by = 5)
      clrs <- round(seq(255, 255, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      if(input$filtroVolumetriaMainframe == "TOTAL DE TRANSACOES (QTD)"){
        columnTarget <- "TOTAL"
      }else if(input$filtroVolumetriaMainframe == "TEMP.RESP. ENTRE 0-1 SEG (%)"){
        columnTarget <- "NTRANS01PER"
      }else if(input$filtroVolumetriaMainframe == "TEMP.RESP. ENTRE 0-2 SEG (%)"){
        columnTarget <- "NTRANS02PER"
      }else if(input$filtroVolumetriaMainframe == "TEMP.RESP. ENTRE 0-3 SEG (%)"){
        columnTarget <- "NTRANS03PER"
      }else if(input$filtroVolumetriaMainframe == "TEMP.RESP. ENTRE 0-4 SEG (%)"){
        columnTarget <- "NTRANS04PER"
      }else if(input$filtroVolumetriaMainframe == "TEMP.RESP. ENTRE 0-5 SEG (%)"){
        columnTarget <- "NTRANS05PER"
      }else if(input$filtroVolumetriaMainframe == "TEMP.RESP. ACIMA DE 5 SEG (%)"){
        columnTarget <- "NTRANS5MPER"
        
        brks <- seq(from = 5, to = 80, by = 5)
        clrs <- round(seq(255, 20, length.out = length(brks) + 1), 0) %>%
        {paste0("rgb(255,", ., ",", ., ")")}
        
      }
      
      newdata <- newdata[,c("TRANSACTION",columnTarget,"Data")]

      newdata$Data <- format(as.POSIXct(as.Date(newdata$Data)), "%m-%d", tz="UTC")
      
      newdata <- reshape::melt(newdata,id.vars=c("TRANSACTION","Data"));
      
      newdata_target <- as.data.frame(reshape::cast(newdata, TRANSACTION~Data, fun.aggregate = mean));
      
      newdata_target$Media <- 0
      
      for(b in c(1:nrow(newdata_target))){
        newdata_target[b,"Media"] <- sum(newdata_target[b,2:ncol(newdata_target)], na.rm = TRUE) / sum(!is.na(newdata_target[b,2:(ncol(newdata_target)-1)]))
      }
      
      newdata_target$Media <- as.double(sprintf("%.2f", newdata_target$Media));
      
      newdata_target <- newdata_target[order(newdata_target$Media, decreasing = TRUE),];
      
      datatable(newdata_target, options = list(
        columnDefs = list(list(className = 'dt-center', 
                               targets = 1:ncol(newdata_target))), 
        searching = TRUE,
        pageLength = 100),
        class = 'display cell-border compact') %>% 
        formatStyle(names(newdata_target)[3:ncol(newdata_target)-1], 
                    backgroundColor = styleInterval(brks, clrs)
        )}
  })
  
  ######################## DADOS MAINFRAME GERAL############
  output$dtProj03 <- renderDataTable({
    
    d <- function(a) { a ;
      warning("Dados Mainframe - Digite data Valida") ;
      message("This is a message not a warning")}
    
    newdata <- subset(Mainframe_Dados_Geral, Mainframe_Dados_Geral$CICS == input$filtroMAGParticao & as.Date(Mainframe_Dados_Geral$Data) >= input$dtconsolidadoMAG[1] & as.Date(Mainframe_Dados_Geral$Data) <= input$dtconsolidadoMAG[2]);
    
    if(as.numeric(as.Date(input$dtconsolidadoMAG[2]) - as.Date(input$dtconsolidadoMAG[1])) < 0){
      
      output$DM2Text <- renderText("DIGITE PERIODO VALIDO")
      suppressMessages(d(1))
      
    }else if(as.numeric(as.Date(input$dtconsolidadoMAG[2]) - as.Date(input$dtconsolidadoMAG[1])) > 0){
      
      output$DM2Text <- renderText(" ");
      
      brks <- seq(from = 0, to = 100, by = 5)
      clrs <- round(seq(255, 255, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      if(input$filtroMAG == "QUANTIDADE DE EXECUCOES"){
        columnTarget <- "Qtd_Exec"
      }else if(input$filtroMAG == "VOLUMETRIA"){
        columnTarget <- "Volumetria"
      }else if(input$filtroMAG == "CONSUMO DE CPU (SEGUNDOS)"){
        columnTarget <- "Consumo_CPU"
        
        brks <- seq(from = 0, to = 15000, by = 1000)
        clrs <- round(seq(255, 20, length.out = length(brks) + 1), 0) %>%
        {paste0("rgb(255,", ., ",", ., ")")}
        
      }
      
      newdata <- newdata[,c("CICS","PROGRAMA",columnTarget,"Data")]
      
      newdata$Data <- format(as.POSIXct(newdata$Data), "%m-%d", tz="UTC")
      
      newdata <- reshape::melt(newdata,id.vars=c("CICS","PROGRAMA","Data"));
      
      newdata_target <- as.data.frame(reshape::cast(newdata, PROGRAMA~Data, fun.aggregate = mean));
      
      for(v in c(2:ncol(newdata_target))){
        newdata_target[ ,v] <- as.double(sprintf("%.2f", newdata_target[,v]));  
      }
      
      newdata_target$Media <- 0
      
      for(b in c(1:nrow(newdata_target))){
        newdata_target[b,"Media"] <- sum(newdata_target[b,2:ncol(newdata_target)], na.rm = TRUE) / sum(!is.na(newdata_target[b,2:(ncol(newdata_target)-1)]))
      }
      
      newdata_target$Media <- as.double(sprintf("%.2f", newdata_target$Media));
      
      newdata_target <- newdata_target[order(newdata_target$Media, decreasing = TRUE),];
      
      datatable(newdata_target, options = list(
        columnDefs = list(list(className = 'dt-center', 
                               targets = 1:ncol(newdata_target))), 
        searching = TRUE,
        pageLength = 100),
        class = 'display cell-border compact') %>% 
        formatStyle(names(newdata_target)[3:ncol(newdata_target)-1], 
                    backgroundColor = styleInterval(brks, clrs)
        )}
  })
  
  ######################## INCIDENTES TRATADOS - ICS ##################
  
  output$dtcoti4 <- renderDataTable({
    
    newdata <- subset(baseFullCOTI, as.Date(baseFullCOTI$`Data de Abertura`) >= input$dtconsolidadoCOTI[1] & as.Date(baseFullCOTI$`Data de Abertura`) <= input$dtconsolidadoCOTI[2]);
    
    brks <- seq(from = 0, to = 7, by = 1)
    clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
    
    newdata$`Data de Abertura` <- format(newdata$`Data de Abertura`, format="%m-%d");
    
    # Filtra somente incidentes manuais
    #newdata <- subset(newdata, newdata$`Nome do Solicitante` != "OPER_BS_OMNIBUS")
    
    # BASE AGREGADA POR ICS
    baseCOTIICS <- dcast(setDT(newdata), ICS ~ newdata$`Data de Abertura`, value.var = "Valor", drop = FALSE)
    
    baseCOTIICS$Total <- apply(baseCOTIICS[ ,2:ncol(baseCOTIICS)], 1, sum)
    
    baseCOTIICS <- baseCOTIICS[order(baseCOTIICS$Total, decreasing = TRUE),]
    
    datatable(baseCOTIICS, options = list(
      columnDefs = list(list(className = 'dt-center', 
                             targets = 1:ncol(baseCOTIICS))), 
      searching = TRUE,
      pageLength = 25),
      class = 'display cell-border compact') %>% 
      formatStyle(names(baseCOTIICS)[2:(ncol(baseCOTIICS)-1)], 
                  backgroundColor = styleInterval(brks, clrs)
      )
  })
  
  ######################## INCIDENTES TRATADOS - TIPO ##################
  
  output$dtcoti5 <- renderDataTable({
    
    newdata <- subset(baseFullCOTI, as.Date(baseFullCOTI$`Data de Abertura`) >= input$dtconsolidadoCOTI[1] & as.Date(baseFullCOTI$`Data de Abertura`) <= input$dtconsolidadoCOTI[2]);
    
    brks <- seq(from = 0, to = 7, by = 1)
    clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
    
    newdata$`Data de Abertura` <- format(newdata$`Data de Abertura`, format="%m-%d");
    
    # Filtra somente incidentes manuais
    #newdata <- subset(newdata, newdata$`Nome do Solicitante` != "OPER_BS_OMNIBUS")
    
    # BASE AGREGADA POR TIPO
    baseCOTITIPO <- dcast(setDT(newdata), TIPO ~ newdata$`Data de Abertura`, value.var = "Valor", drop = FALSE)
    
    baseCOTITIPO$Total <- apply(baseCOTITIPO[ ,2:ncol(baseCOTITIPO)], 1, sum)
    
    baseCOTITIPO <- baseCOTITIPO[order(baseCOTITIPO$Total, decreasing = TRUE),]
    
    datatable(baseCOTITIPO, options = list(
      columnDefs = list(list(className = 'dt-center', 
                             targets = 1:ncol(baseCOTITIPO))), 
      searching = TRUE,
      pageLength = 50),
      class = 'display cell-border compact') %>% 
      formatStyle(names(baseCOTITIPO)[2:(ncol(baseCOTITIPO)-1)], 
                  backgroundColor = styleInterval(brks, clrs)
      )
  })
  
  ######################## ALERTAS TRATADOS - RECURSO ##################
  
  output$dtaler1 <- renderDataTable({
    
    newdata <- subset(baseEventosFull, as.Date(baseEventosFull$ABERTURA) >= input$dtconsolidadoAlertas[1] & as.Date(baseEventosFull$ABERTURA) <= input$dtconsolidadoAlertas[2]);
    
    brks <- seq(from = 0, to = 500, by = 10)
    clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
    
    withProgress(message = 'Loading...',
                 detail = ' ', value = 0, {
                   for (i in 1:3) {

    newdata$ABERTURA <- format(newdata$ABERTURA, format="%m-%d");
    
    # BASE AGREGADA POR RECURSO
    baseALERTAREC <- dcast(setDT(newdata), RECURSO ~ newdata$ABERTURA, value.var = "Valor", drop = FALSE)
    
    baseALERTAREC$Total <- apply(baseALERTAREC[ ,2:ncol(baseALERTAREC)],1,sum)
    
    baseALERTAREC <- baseALERTAREC[order(baseALERTAREC$Total, decreasing = TRUE),]
    
    incProgress(1/3)
    Sys.sleep(0.10)
                   }
                 })
    
    datatable(baseALERTAREC, options = list(
      columnDefs = list(list(className = 'dt-center', 
                             targets = 1:ncol(baseALERTAREC))), 
      searching = TRUE,
      pageLength = 15),
      class = 'display cell-border compact') %>% 
      formatStyle(names(baseALERTAREC)[2:(ncol(baseALERTAREC)-1)], 
                  backgroundColor = styleInterval(brks, clrs)
      )
  })
  
  ######################## ALERTAS TRATADOS - COMPONENTE ##################
  
  output$dtaler2 <- renderDataTable({
    
    newdata <- subset(baseEventosFull, as.Date(baseEventosFull$ABERTURA) >= input$dtconsolidadoAlertas[1] & as.Date(baseEventosFull$ABERTURA) <= input$dtconsolidadoAlertas[2]);
    
    brks <- seq(from = 0, to = 500, by = 20)
    clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
    
    newdata$ABERTURA <- format(newdata$ABERTURA, format="%m-%d");
    
    # BASE AGREGADA POR COMPONENTE
    baseALERTACOMP <- dcast(setDT(newdata), COMPONENTE ~ newdata$ABERTURA, value.var = "Valor", drop = FALSE)
    
    baseALERTACOMP$Total <- apply(baseALERTACOMP[ ,2:ncol(baseALERTACOMP)],1,sum)
    
    baseALERTACOMP <- baseALERTACOMP[order(baseALERTACOMP$Total, decreasing = TRUE),]
    
    datatable(baseALERTACOMP, options = list(
      columnDefs = list(list(className = 'dt-center', 
                             targets = 1:ncol(baseALERTACOMP))), 
      searching = TRUE,
      pageLength = 15),
      class = 'display cell-border compact') %>% 
      formatStyle(names(baseALERTACOMP)[2:(ncol(baseALERTACOMP)-1)], 
                  backgroundColor = styleInterval(brks, clrs)
      )
  })
  
  ######################## ALERTAS TRATADOS - METRICA ##################
  
  output$dtaler3 <- renderDataTable({
    
    newdata <- subset(baseEventosFull, as.Date(baseEventosFull$ABERTURA) >= input$dtconsolidadoAlertas[1] & as.Date(baseEventosFull$ABERTURA) <= input$dtconsolidadoAlertas[2]);
    
    brks <- seq(from = 0, to = 500, by = 25)
    clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
    
    newdata$ABERTURA <- format(newdata$ABERTURA, format="%m-%d");
    
    # BASE AGREGADA POR METRICA
    baseALERTAMETR <- dcast(setDT(newdata), METRICA ~ newdata$ABERTURA, value.var = "Valor", drop = FALSE)
    
    baseALERTAMETR$Total <- apply(baseALERTAMETR[ ,2:ncol(baseALERTAMETR)],1,sum)
    
    baseALERTAMETR <- baseALERTAMETR[order(baseALERTAMETR$Total, decreasing = TRUE),]
    
    datatable(baseALERTAMETR, options = list(
      columnDefs = list(list(className = 'dt-center', 
                             targets = 1:ncol(baseALERTAMETR))), 
      searching = TRUE,
      pageLength = 15),
      class = 'display cell-border compact') %>% 
      formatStyle(names(baseALERTAMETR)[2:(ncol(baseALERTAMETR)-1)], 
                  backgroundColor = styleInterval(brks, clrs)
      )
  })
  
  ######################## ALERTAS TRATADOS - MENSAGEM ##################
  
  output$dtaler4 <- renderDataTable({
    
    newdata <- subset(baseEventosFull, as.Date(baseEventosFull$ABERTURA) >= input$dtconsolidadoAlertas[1] & as.Date(baseEventosFull$ABERTURA) <= input$dtconsolidadoAlertas[2]);
    
    brks <- seq(from = 0, to = 200, by = 25)
    clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
    
    newdata$ABERTURA <- format(newdata$ABERTURA, format="%m-%d");
    
    # BASE AGREGADA POR MENSAGEM
    baseALERTAMENS <- dcast(setDT(newdata), MENSAGEM ~ newdata$ABERTURA, value.var = "Valor", drop = FALSE)
    
    baseALERTAMENS$Total <- apply(baseALERTAMENS[ ,2:ncol(baseALERTAMENS)],1,sum)
    
    baseALERTAMENS <- baseALERTAMENS[order(baseALERTAMENS$Total, decreasing = TRUE),]
    
    datatable(baseALERTAMENS, options = list(
      columnDefs = list(list(className = 'dt-center', 
                             targets = 1:ncol(baseALERTAMENS))), 
      searching = TRUE,
      pageLength = 15),
      class = 'display cell-border compact') %>% 
      formatStyle(names(baseALERTAMENS)[2:(ncol(baseALERTAMENS)-1)], 
                  backgroundColor = styleInterval(brks, clrs)
      )
  })
  
  ######################## TRANSACOES JVM ##################
  
  output$dtjvm1 <- renderDataTable({
    
    newdata <- subset(baseJVMGeral, as.Date(baseJVMGeral$Date) >= input$dtdadosjvm[1] & as.Date(baseJVMGeral$Date) <= input$dtdadosjvm[2]);
    
    brks <- seq(from = 0, to = 3279075, by = 250000)
    clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
    
    newdata$Date <- format(as.Date(newdata$Date), format="%m-%d");
    
    # BASE AGREGADA POR TIPO
    baseJVM <- dcast(setDT(newdata), Measure ~ newdata$Date, value.var = "Count", drop = FALSE)
    
    baseJVM$Measure <- gsub("@","[", baseJVM$Measure)
    
    baseJVM <- separate(baseJVM, col = Measure, into = c("Measure", "Lixo"), sep = "\\[")
    
    baseJVM$Lixo <- NULL
    
    baseJVM <- baseJVM[!(baseJVM$Measure == "Number of Requests (split by Agent)"), ]
    
    baseJVM$Total <- apply(baseJVM[ ,2:ncol(baseJVM)], 1, sum)
    
    baseJVM <- baseJVM[rowSums(is.na(baseJVM))!= length(baseJVM[1,])-1, ]
    
    baseJVM <- baseJVM[order(baseJVM$Total, decreasing = TRUE),]
    
    datatable(baseJVM, options = list(
      columnDefs = list(list(className = 'dt-center', 
                             targets = 2:ncol(baseJVM))), 
      searching = TRUE,
      pageLength = 50),
      class = 'display cell-border compact') %>% 
      formatStyle(names(baseJVM)[2:(ncol(baseJVM)-1)], 
                  backgroundColor = styleInterval(brks, clrs)
      )
  })
  
  
  ######################## @@@ PESQUISA DOS DADOS @@@############
  ######################## PESQUISA SOLICITACAO - INCI ##################
  
  output$dtfind1 <- renderDataTable({
    
    input$findCOTIbutton
    
    if (input$findCOTIbutton == 0)
      return()              
    isolate ({ 
      
      newdata <- subset(baseFullCOTI, as.Date(baseFullCOTI$`Data de Abertura`) >= input$dtpesquisaCOTI[1] & as.Date(baseFullCOTI$`Data de Abertura`) <= input$dtpesquisaCOTI[2]);
      
      brks <- seq(from = 0, to = 7, by = 1)
      clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      newdata <- filter(newdata, grepl(input$text, newdata$Solicitacao));
      
      newdata$`Data de Abertura` <- format(newdata$`Data de Abertura`, format="%m-%d");
      
      # BASE AGREGADA POR solicitacao
      baseCOTISOLI <- dcast(setDT(newdata), Solicitacao ~ newdata$`Data de Abertura`, value.var = "Valor", drop = FALSE)
      
      baseCOTISOLI[,2:ncol(baseCOTISOLI)][is.na(baseCOTISOLI[,2:ncol(baseCOTISOLI)])] <- 0;
      
      baseCOTISOLI$Total <- apply(baseCOTISOLI[ ,2:ncol(baseCOTISOLI)],1,sum)
      
      baseCOTISOLI <- baseCOTISOLI[order(baseCOTISOLI$Total, decreasing = TRUE),]
      
      output$txtdtfind1 <- renderText("Incidentes classificados pela Solicitacao")
      
      datatable(baseCOTISOLI, options = list(
        columnDefs = list(list(className = 'dt-center', 
                               targets = 2:ncol(baseCOTISOLI))), 
        searching = TRUE,
        pageLength = 25),
        class = 'display cell-border compact') %>% 
        formatStyle(names(baseCOTISOLI)[2:ncol(baseCOTISOLI)], 
                    backgroundColor = styleInterval(brks, clrs)
        )
      
    })
    
  })
  
  ######################## PESQUISA SOLICITACAO IC - INCI ##################
  
  output$dtfind2 <- renderDataTable({
    
    input$findCOTIbutton
    
    if (input$findCOTIbutton == 0)
      return()              
    isolate ({ 
      
      newdata <- subset(baseFullCOTI, as.Date(baseFullCOTI$`Data de Abertura`) >= input$dtpesquisaCOTI[1] & as.Date(baseFullCOTI$`Data de Abertura`) <= input$dtpesquisaCOTI[2]);
      
      brks <- seq(from = 0, to = 7, by = 1)
      clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      newdata <- filter(newdata, grepl(input$text, newdata$Solicitacao));
      
      newdata$`Data de Abertura` <- format(newdata$`Data de Abertura`, format="%m-%d");
      
      # BASE AGREGADA POR IC
      baseCOTIIC <- dcast(setDT(newdata), ICS ~ newdata$`Data de Abertura`, value.var = "Valor", drop = FALSE)
      
      baseCOTIIC[,2:ncol(baseCOTIIC)][is.na(baseCOTIIC[,2:ncol(baseCOTIIC)])] <- 0;
      
      baseCOTIIC$Total <- apply(baseCOTIIC[ ,2:ncol(baseCOTIIC)],1,sum)
      
      baseCOTIIC <- baseCOTIIC[order(baseCOTIIC$Total, decreasing = TRUE),]
      
      output$txtdtfind2 <- renderText("Classificados pelo IC")
      
      datatable(baseCOTIIC, options = list(
        columnDefs = list(list(className = 'dt-center',
                               targets = 2:ncol(baseCOTIIC))),
        searching = TRUE,
        pageLength = 50),
        class = 'display cell-border compact') %>%
        formatStyle(names(baseCOTIIC)[2:ncol(baseCOTIIC)],
                    backgroundColor = styleInterval(brks, clrs)
        )
    })
    
  })
  
  ######################## PESQUISA SOLICITACAO TIPO - INCI ##################
  
  output$dtfind3 <- renderDataTable({
    
    input$findCOTIbutton
    
    if (input$findCOTIbutton == 0)
      return()              
    isolate ({ 
      
      newdata <- subset(baseFullCOTI, as.Date(baseFullCOTI$`Data de Abertura`) >= input$dtpesquisaCOTI[1] & as.Date(baseFullCOTI$`Data de Abertura`) <= input$dtpesquisaCOTI[2]);
      
      brks <- seq(from = 0, to = 7, by = 1)
      clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      newdata <- filter(newdata, grepl(input$text, newdata$Solicitacao));
      
      newdata$`Data de Abertura` <- format(newdata$`Data de Abertura`, format="%m-%d");
      
      # BASE AGREGADA POR TIPO
      baseCOTITIPO <- dcast(setDT(newdata), TIPO ~ newdata$`Data de Abertura`, value.var = "Valor", drop = FALSE)
      
      baseCOTITIPO[,2:ncol(baseCOTITIPO)][is.na(baseCOTITIPO[,2:ncol(baseCOTITIPO)])] <- 0;
      
      baseCOTITIPO$Total <-apply(baseCOTITIPO[ ,2:ncol(baseCOTITIPO)],1,sum)
      
      baseCOTITIPO <- baseCOTITIPO[order(baseCOTITIPO$Total, decreasing = TRUE),]
      
      output$txtdtfind3 <- renderText("Classificados pelo TIPO")
      
      datatable(baseCOTITIPO, options = list(
        columnDefs = list(list(className = 'dt-center',
                               targets = 2:ncol(baseCOTITIPO))),
        searching = TRUE,
        pageLength = 50),
        class = 'display cell-border compact') %>%
        formatStyle(names(baseCOTITIPO)[2:ncol(baseCOTITIPO)],
                    backgroundColor = styleInterval(brks, clrs)
        )
    })
    
  })
  
  ######################## PESQUISA MENSAGEM- EVENTOS##################
  
  output$dtfindeven1 <- renderDataTable({
    
    input$findevenbutton
    
    if (input$findevenbutton == 0)
      return()              
    isolate ({ 
      
      newdata <- subset(baseEventosFull, as.Date(baseEventosFull$ABERTURA) >= input$dtpesquisaeven[1] & as.Date(baseEventosFull$ABERTURA) <= input$dtpesquisaeven[2]);
      
      brks <- seq(from = 0, to = 7, by = 1)
      clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      if(input$recurso == ""){
        targetA = input$metrica
        targetB = newdata$METRICA
      }else if(input$metrica == ""){
        targetA = input$recurso
        targetB = newdata$RECURSO
      }
      
      newdata <- filter(newdata, grepl(targetA, targetB));
      
      newdata$ABERTURA <- format(as.Date(newdata$ABERTURA), format="%m-%d");
      
      # BASE AGREGADA POR solicitacao
      baseCOTISOLI <- dcast(setDT(newdata), MENSAGEM ~ newdata$ABERTURA, value.var = "Valor", drop = FALSE)
      
      baseCOTISOLI[,2:ncol(baseCOTISOLI)][is.na(baseCOTISOLI[,2:ncol(baseCOTISOLI)])] <- 0;
      
      baseCOTISOLI$Total <- apply(baseCOTISOLI[ ,2:ncol(baseCOTISOLI)],1,sum)
      
      baseCOTISOLI <- baseCOTISOLI[order(baseCOTISOLI$Total, decreasing = TRUE),]

      output$txtdtfindeven1 <- renderText("Eventos classificados pela Mensagem")
      
      datatable(baseCOTISOLI, options = list(
        columnDefs = list(list(className = 'dt-center', 
                               targets = 2:ncol(baseCOTISOLI))), 
        searching = TRUE,
        pageLength = 25),
        class = 'display cell-border compact') %>% 
        formatStyle(names(baseCOTISOLI)[2:ncol(baseCOTISOLI)], 
                    backgroundColor = styleInterval(brks, clrs)
        )
      
    })
    
  })
  
  ######################## PESQUISA COMPONENTE - EVENTOS##################
  
  output$dtfindeven2 <- renderDataTable({
    
    input$findevenbutton
    
    if (input$findevenbutton == 0)
      return()              
    isolate ({ 
      
      newdata <- subset(baseEventosFull, as.Date(baseEventosFull$ABERTURA) >= input$dtpesquisaeven[1] & as.Date(baseEventosFull$ABERTURA) <= input$dtpesquisaeven[2]);
      
      brks <- seq(from = 0, to = 7, by = 1)
      clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      if(input$recurso == ""){
        targetA = input$metrica
        targetB = newdata$METRICA
      }else if(input$metrica == ""){
        targetA = input$recurso
        targetB = newdata$RECURSO
      }
      
      newdata <- filter(newdata, grepl(targetA, targetB));
      
      newdata$ABERTURA <- format(as.Date(newdata$ABERTURA), format="%m-%d");
      
      # BASE AGREGADA POR IC
      baseCOTIIC <- dcast(setDT(newdata), COMPONENTE ~ newdata$ABERTURA, value.var = "Valor", drop = FALSE)
      
      baseCOTIIC[,2:ncol(baseCOTIIC)][is.na(baseCOTIIC[,2:ncol(baseCOTIIC)])] <- 0;
      
      baseCOTIIC$Total <- apply(baseCOTIIC[ ,2:ncol(baseCOTIIC)],1,sum)
      
      baseCOTIIC <- baseCOTIIC[order(baseCOTIIC$Total, decreasing = TRUE),]
      
      output$txtdtfindeven2 <- renderText("Classificados pelo Componente")
      
      datatable(baseCOTIIC, options = list(
        columnDefs = list(list(className = 'dt-center',
                               targets = 2:ncol(baseCOTIIC))),
        searching = TRUE,
        pageLength = 25),
        class = 'display cell-border compact') %>%
        formatStyle(names(baseCOTIIC)[2:ncol(baseCOTIIC)],
                    backgroundColor = styleInterval(brks, clrs)
        )
    })
    
  })
  
  ######################## PESQUISA METRICA - EVENTOS##################
  
  output$dtfindeven3 <- renderDataTable({
    
    input$findevenbutton
    
    if (input$findevenbutton == 0)
      return()              
    isolate ({ 
      
      newdata <- subset(baseEventosFull, as.Date(baseEventosFull$ABERTURA) >= input$dtpesquisaeven[1] & as.Date(baseEventosFull$ABERTURA) <= input$dtpesquisaeven[2]);
      
      brks <- seq(from = 0, to = 7, by = 1)
      clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      if(input$recurso == "" & input$metrica != ""){
        targetA = input$metrica
        targetB = newdata$METRICA
      }else if(input$metrica == "" & input$recurso != ""){
        targetA = input$recurso
        targetB = newdata$RECURSO
      }
      
      newdata <- filter(newdata, grepl(targetA, targetB));
      
      newdata$ABERTURA <- format(as.Date(newdata$ABERTURA), format="%m-%d");
      
      # BASE AGREGADA POR METRICA
      if(input$recurso == "" & input$metrica != ""){
        baseCOTITIPO <- dcast(setDT(newdata), RECURSO ~ newdata$ABERTURA, value.var = "Valor", drop = FALSE)
        tarbetC = "RECURSO"
      }else if(input$metrica == "" & input$recurso != ""){
        baseCOTITIPO <- dcast(setDT(newdata), METRICA ~ newdata$ABERTURA, value.var = "Valor", drop = FALSE)
        tarbetC = "METRICA"
      }
      
      baseCOTITIPO[,2:ncol(baseCOTITIPO)][is.na(baseCOTITIPO[,2:ncol(baseCOTITIPO)])] <- 0;
      
      baseCOTITIPO$Total <- apply(baseCOTITIPO[ ,2:ncol(baseCOTITIPO)],1,sum)
      
      baseCOTITIPO <- baseCOTITIPO[order(baseCOTITIPO$Total, decreasing = TRUE),]
      
      output$txtdtfindeven3 <- renderText(paste("Classificados pela",tarbetC))
      
      datatable(baseCOTITIPO, options = list(
        columnDefs = list(list(className = 'dt-center',
                               targets = 2:ncol(baseCOTITIPO))),
        searching = TRUE,
        pageLength = 50),
        class = 'display cell-border compact') %>%
        formatStyle(names(baseCOTITIPO)[2:ncol(baseCOTITIPO)],
                    backgroundColor = styleInterval(brks, clrs)
        )
    })
    
  })
  
  ######################## MAINFRAME CORRELACAO ##################
  
  # output$gpMACor <- renderggiraph({
  # output$gpMACor <- renderPlot({
  # 
  #   newdata <- subset(Mainframe_Correlacao, Mainframe_Correlacao$PROGRAMA == input$filtroMACorAPP &
  #                          as.Date(Mainframe_Correlacao$Data) >= input$dtMACor[1] &
  #                          as.Date(Mainframe_Correlacao$Data) <= input$dtMACor[2]);
  # 
  #   baseMFConsolidada <- newdata
  # 
  #   # Replace NA values per 0
  #   baseMFConsolidada[,2][is.na(baseMFConsolidada[,2])] <- 0
  # 
  #   # Correlacao
  #   x <- format(cor(baseMFConsolidada$Volumetria, baseMFConsolidada$Consumo_CPU), digits = 1)
  # 
  #   for(v in c(1:nrow(tabelaCorrelacao))){
  #     if(x == "NA"){
  #       result <- "Nao foi possivel calcular o coeficiente"
  #     }else if(tabelaCorrelacao[v,1] <= as.double(x) & as.double(x) < tabelaCorrelacao[v,2]){
  #       result <- tabelaCorrelacao[v,3]
  #     }else if(tabelaCorrelacao[v,1] < abs(as.double(x)) & abs(as.double(x)) < tabelaCorrelacao[v,2]){
  #       result <- tabelaCorrelacao[v,3]
  #     }
  #   }
  # 
  #   output$MACorTXT <- renderText(paste("Coeficiente de Correlacao:",x,"(",result,")"));
  # 
  #   ggplot(baseMFConsolidada, aes(x = baseMFConsolidada$Volumetria, y = baseMFConsolidada$Consumo_CPU, color = Consumo_CPU)) +
  #     xlab("Volumetria") +
  #     ylab("Consumo de CPU em Segundos") +
  #     title("Correlacao - Consumo de CPU vs Volumetria") +
  #     theme(plot.title = element_text(size = 18)) +
  #     theme(text = element_text(size = 14)) +
  #     theme(axis.text.y = element_text(size = 13, hjust = 1.0)) +
  #     theme(axis.text.x = element_text(size = 13, hjust = 1.0)) +
  #     scale_x_continuous(labels = comma) +
  #     geom_smooth(method=lm) +
  #     geom_point(size = 3)
  # 
  # 
  # })
  
  ######################## @@@ GRAFICOS @@@ ##################
  ######################## EVENTOS POR FAIXA DE HORARIO - METRICA ##################
  
  output$plotef1 <- renderPlot({
    
    newdata <- subset(baseEventosFull, baseEventosFull$RECURSO == input$namesRecursosefggplot &
                       as.Date(baseEventosFull$ABERTURA) >= input$dateRangeRecursos[1] & 
                       as.Date(baseEventosFull$ABERTURA) <= input$dateRangeRecursos[2]);

    baseEventosTarget <- newdata;
    
    # Transforma para formato D H M
    for(valor in 1:nrow(baseEventosTarget)){
      baseEventosTarget$Duracao[valor] <- as.character(lubridate::as.period(interval(as.POSIXct(baseEventosTarget$ABERTURA[valor]), as.POSIXct(baseEventosTarget$FECHAMENTO[valor]))))
    }
    
    # Remove os segundos
    for(valor in 1:nrow(baseEventosTarget)){
      baseEventosTarget$Duracao[valor] <- gsub("M.*","M", baseEventosTarget$Duracao[valor])
    }

    baseEventosTarget <- separate(baseEventosTarget, col = ABERTURA, into = c("DATA DE ABERTURA", "HORA DE ABERTURA"), sep = "\\ ");
    baseEventosTarget <- separate(baseEventosTarget, col = FECHAMENTO, into = c("DATA DE FECHAMENTO", "HORA DE FECHAMENTO"), sep = "\\ ");
    
    baseEventosTarget$`DATA DE ABERTURA` <- as.Date(baseEventosTarget$`DATA DE ABERTURA`);
    baseEventosTarget$`DATA DE FECHAMENTO` <- as.Date(baseEventosTarget$`DATA DE FECHAMENTO`);
    baseEventosTarget$`HORA DE ABERTURA` <- as.POSIXct(baseEventosTarget$`HORA DE ABERTURA`,format="%H:%M:%S");
    baseEventosTarget$`HORA DE FECHAMENTO` <- as.POSIXct(baseEventosTarget$`HORA DE FECHAMENTO`,format="%H:%M:%S");

    ggplot(baseEventosTarget, aes(x = baseEventosTarget$`DATA DE ABERTURA`, y = baseEventosTarget$`HORA DE ABERTURA`, colour = METRICA)) + 
      ggtitle("METRICAS") +
      theme(plot.title = element_text(hjust = 0.55, size = 15))  +
      ylab("Faixa de Abertura") +
      scale_y_datetime(date_breaks = "1 hour", date_labels = "%H:00") +
      xlab(paste(input$dateRangeRecursos[1],"a",input$dateRangeRecursos[2])) +
      scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d-%b") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12)) +
      theme(axis.text.y = element_text(size = 12)) +
      theme(axis.title.x = element_text(size = 14)) +
      theme(axis.title.y = element_text(size = 14)) +
      theme(legend.text = element_text(size = 12)) +
      geom_point(size = 3.0)
    
    
  })
  
  ######################## EVENTOS POR FAIXA DE HORARIO - COMPONENTE ##################
  
  output$plotef2 <- renderPlot({
    
    newdata <- subset(baseEventosFull, baseEventosFull$RECURSO == input$namesRecursosefggplot &
                        as.Date(baseEventosFull$ABERTURA) >= input$dateRangeRecursos[1] & 
                        as.Date(baseEventosFull$ABERTURA) <= input$dateRangeRecursos[2]);
    
    baseEventosTarget <- newdata;
    
    # Transforma para formato D H M
    for(valor in 1:nrow(baseEventosTarget)){
      baseEventosTarget$Duracao[valor] <- as.character(lubridate::as.period(interval(as.POSIXct(baseEventosTarget$ABERTURA[valor]), as.POSIXct(baseEventosTarget$FECHAMENTO[valor]))))
    }
    
    # Remove os segundos
    for(valor in 1:nrow(baseEventosTarget)){
      baseEventosTarget$Duracao[valor] <- gsub("M.*","M", baseEventosTarget$Duracao[valor])
    }
    
    baseEventosTarget <- separate(baseEventosTarget, col = ABERTURA, into = c("DATA DE ABERTURA", "HORA DE ABERTURA"), sep = "\\ ");
    baseEventosTarget <- separate(baseEventosTarget, col = FECHAMENTO, into = c("DATA DE FECHAMENTO", "HORA DE FECHAMENTO"), sep = "\\ ");
    
    baseEventosTarget$`DATA DE ABERTURA` <- as.Date(baseEventosTarget$`DATA DE ABERTURA`);
    baseEventosTarget$`DATA DE FECHAMENTO` <- as.Date(baseEventosTarget$`DATA DE FECHAMENTO`);
    baseEventosTarget$`HORA DE ABERTURA` <- as.POSIXct(baseEventosTarget$`HORA DE ABERTURA`,format="%H:%M:%S");
    baseEventosTarget$`HORA DE FECHAMENTO` <- as.POSIXct(baseEventosTarget$`HORA DE FECHAMENTO`,format="%H:%M:%S");
    
    ggplot(baseEventosTarget, aes(x = baseEventosTarget$`DATA DE ABERTURA`, y = baseEventosTarget$`HORA DE ABERTURA`, colour = COMPONENTE)) + 
      ggtitle("COMPONENTES") +
      theme(plot.title = element_text(hjust = 0.55, size = 15))  +
      ylab("Faixa de Abertura") +
      scale_y_datetime(date_breaks = "1 hour", date_labels = "%H:00") +
      xlab(paste(input$dateRangeRecursos[1],"a",input$dateRangeRecursos[2])) +
      scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d-%b") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12)) +
      theme(axis.text.y = element_text(size = 12)) +
      theme(axis.title.x = element_text(size = 14)) +
      theme(axis.title.y = element_text(size = 14)) +
      theme(legend.text = element_text(size = 12)) +
      geom_point(size = 3.0)
    
    
  })
  
  
})