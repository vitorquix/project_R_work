########### Bibliotecas ###########
library(shiny)
library(ggplot2)
library(lubridate)
library(reshape2)
library(tidyr)
library(openxlsx)
library(dplyr)
library(shinythemes)
library(DT)
library(stringr)
library(data.table)
library(formattable)

########### Base de Dados - APM Real ########### 
source("D:/Estudo/R/Linguagem R/Scopus/Backup Script/APM_Aplicacoes/Aplicacoes_APM_Geral.R");
Aplicacoes_APM_Geral <- na.omit(Aplicacoes_APM_Geral);

########### Base de Dados - APM Sintetico ###########  
source("D:/Estudo/R/Linguagem R/Scopus/Backup Script/APM_Sintetico/Sintetico_Geral.R");
Sintetico_APM_Geral$Disponibilidade <- gsub(',', '.', Sintetico_APM_Geral$Disponibilidade);
Sintetico_APM_Geral$Disponibilidade <- as.numeric(Sintetico_APM_Geral$Disponibilidade);
APMUniquesNames <- subset(Sintetico_APM_Geral, Sintetico_APM_Geral$Data >= as.Date(format(Sys.time(), "%Y-%m-%d")) - 10 & Sintetico_APM_Geral$Data <= as.Date(format(Sys.time(), "%Y-%m-%d")));

########### Base de Dados - ICs Mapeados ###########  
baseICs <- read.xlsx("D:/Estudo/R/Linguagem R/Scopus/Data/bases_prod/Base_ICs_Full.xlsx", sheet = 1, cols = 1);
baseTIPO <- read.xlsx("D:/Estudo/R/Linguagem R/Scopus/Data/bases_prod/Base_ICs_Full.xlsx", sheet = 1, cols = 2);

########### Base de Dados - Incidentes COTI ###########  
source("D:/Estudo/R/Linguagem R/Scopus/Backup Script/Monitoramento_Operacional/Ocorrencias_COTI_Geral.R");
colnames(baseFullCOTI)[2] <- "Data.de.Criacao";
colnames(baseFullCOTI)[5] <- "Solicitacao";
baseFullCOTI <- tidyr::separate(baseFullCOTI, col = Data.de.Criacao, into = c("Data de Abertura", "Hora de Abertura"), sep = "\\ ");
baseFullCOTI$`Data de Abertura` <- as.Date(baseFullCOTI$`Data de Abertura`);
baseFullCOTI <- baseFullCOTI[order(as.Date(baseFullCOTI$`Data de Abertura`, format="%d-%m-%Y"), decreasing = TRUE),]
#baseFullCOTI$`Nome do Solicitante` <- NULL;
baseFullCOTI$Valor <- 1

########### Base de Dados - Eventos de Monitoracao ###########  
baseEventosFull <- readRDS("D:/Estudo/R/Linguagem R/Scopus/Data/bases_prod/Eventos_Full.rds")
baseEventosFull$X9 <- NULL;
baseEventosFull$SEVERIDADE <- NULL;
baseEventosFull <- dplyr::mutate(baseEventosFull, Valor = 1);

########### Base de Dados - CICS ###########  
baseCICSFull <- filter(baseEventosFull, grepl("DESATIVADO|DESATIVADA",baseEventosFull$MENSAGEM))
# Transforma para formato D H M
for(valor in 1:nrow(baseCICSFull)){
  baseCICSFull$Duracao[valor] <- as.character(lubridate::as.period(interval(as.POSIXct(baseCICSFull$ABERTURA[valor]), as.POSIXct(baseCICSFull$FECHAMENTO[valor]))))
}

# Remove os segundos
for(valor in 1:nrow(baseCICSFull)){
  baseCICSFull$Duracao[valor] <- gsub("M.*","M", baseCICSFull$Duracao[valor])
}

baseCICSFull$Dias <- as.numeric(stringr::str_match(baseCICSFull$Duracao, "([0-9]{1,2})d")[,-1]);
baseCICSFull$Horas <- as.numeric(stringr::str_match(baseCICSFull$Duracao, "([0-9]{1,2})H")[,-1]);
baseCICSFull$Minutos <- as.numeric(stringr::str_match(baseCICSFull$Duracao, "([0-9]{1,2})M")[,-1]);
baseCICSFull <- tidyr::separate(baseCICSFull, col = ABERTURA, into = c("Data de Abertura", "Hora de Abertura"), sep = "\\ ");
baseCICSFull$`Data de Abertura` <- as.Date(baseCICSFull$`Data de Abertura`);
baseCICSFull <- tidyr::separate(baseCICSFull, col = FECHAMENTO, into = c("Data de Fechamento", "Hora de Fechamento"), sep = "\\ ");
baseCICSFull$`Data de Fechamento` <- as.Date(baseCICSFull$`Data de Fechamento`);
baseCICSFull$Dias[is.na(baseCICSFull$Dias)] <- 0
baseCICSFull$Horas[is.na(baseCICSFull$Horas)] <- 0

########### Base de Dados - DB2 ##########  
baseDB2Full <- filter(baseEventosFull, grepl("STARTED TASK DESATIVADA",baseEventosFull$MENSAGEM))
baseDB2Full <- filter(baseDB2Full, grepl("dsndbm1",baseDB2Full$COMPONENTE))

# Transforma para formato D H M
for(valor in 1:nrow(baseDB2Full)){
  baseDB2Full$Duracao[valor] <- as.character(lubridate::as.period(interval(as.POSIXct(baseDB2Full$ABERTURA[valor]), as.POSIXct(baseDB2Full$FECHAMENTO[valor]))))
}

# Remove os segundos
for(valor in 1:nrow(baseDB2Full)){
  baseDB2Full$Duracao[valor] <- gsub("M.*","M", baseDB2Full$Duracao[valor])
}

baseDB2Full$Dias <- as.numeric(stringr::str_match(baseDB2Full$Duracao, "([0-9]{1,2})d")[,-1]);
baseDB2Full$Horas <- as.numeric(stringr::str_match(baseDB2Full$Duracao, "([0-9]{1,2})H")[,-1]);
baseDB2Full$Minutos <- as.numeric(stringr::str_match(baseDB2Full$Duracao, "([0-9]{1,2})M")[,-1]);
baseDB2Full <- tidyr::separate(baseDB2Full, col = ABERTURA, into = c("Data de Abertura", "Hora de Abertura"), sep = "\\ ");
baseDB2Full$`Data de Abertura` <- as.Date(baseDB2Full$`Data de Abertura`);
baseDB2Full <- tidyr::separate(baseDB2Full, col = FECHAMENTO, into = c("Data de Fechamento", "Hora de Fechamento"), sep = "\\ ");
baseDB2Full$`Data de Fechamento` <- as.Date(baseDB2Full$`Data de Fechamento`);
baseDB2Full$Dias[is.na(baseDB2Full$Dias)] <- 0
baseDB2Full$Horas[is.na(baseDB2Full$Horas)] <- 0

########### Base de Dados - Mainframe Consolidado ###########  
source("D:/Estudo/R/Linguagem R/Scopus/Backup Script/Mainframe_Transacoes/Mainframe_Geral.R");
Mainframe_Geral <- subset(Mainframe_Geral, TRANSACTION != "CSSY");
Mainframe_Geral <- subset(Mainframe_Geral, TRANSACTION != "OMEG");
Mainframe_Geral$Data <- as.Date(Mainframe_Geral$Data)

########### Base de Dados - Mainframe Geral ###########  
source("D:/Estudo/R/Linguagem R/Scopus/Backup Script/Mainframe_Dados/Mainframe_Dados_Geral.R");
Mainframe_Dados_Geral$Data <- as.Date(Mainframe_Dados_Geral$Data)
Mainframe_Correlacao <- subset(Mainframe_Dados_Geral, Mainframe_Dados_Geral$Consumo_CPU > 2);

########### Base de Dados - Incidentes Full ###########  
baseFullIN <- read.csv("D:/Estudo/R/Linguagem R/Scopus/Data/bases_prod/Base_Incidentes_Full.csv", sep = ";")
baseININdisp <- subset(baseFullIN, Tipo.de.Problema == "INDISPONIBILIDADE TOTAL" & 
                         Tipo.de.Produto != "ACESSO" &
                         Tipo.de.Produto != "DIVERSOS" &
                         Tipo.de.Produto != "REDE" &
                         Tipo.de.Produto != "SERVIDORES" &
                         Tipo.de.Produto != "RAMAL" &
                         Tipo.de.Produto != "VPN" &
                         Tipo.de.Produto != "LINK" &
                         Tipo.de.Produto != "METAFRAME" &
                         Tipo.de.Produto != "DB2" &
                         Tipo.de.Produto != "INTERNA WEB" &
                         Tipo.de.Produto != "FORA DO HORARIO DE EXPEDIENTE" &
                         Tipo.de.Produto != "AUTO" & 
                         Brd.Tipo.Ambiente == "PRODUCAO")
colnames(baseININdisp)[24] <- "Hora.de.Resolucao"
baseININdisp <- baseININdisp[,c("ID.do.Incidente","Hora.de.Abertura","Hora.de.Resolucao","Tipo.de.Produto")]
baseININdisp$Hora.de.Abertura <- as.POSIXct(strptime(baseININdisp$Hora.de.Abertura, "%d/%m/%Y %H:%M:%S"))
baseININdisp$Hora.de.Resolucao <- as.POSIXct(strptime(baseININdisp$Hora.de.Resolucao, "%d/%m/%Y %H:%M:%S"))

# Transforma para formato D H M
for(valor in 1:nrow(baseININdisp)){
  baseININdisp$Duracao[valor] <- as.character(lubridate::as.period(interval(as.POSIXct(baseININdisp$Hora.de.Abertura[valor]), as.POSIXct(baseININdisp$Hora.de.Resolucao[valor]))))
}

# Remove os segundos
for(valor in 1:nrow(baseININdisp)){
  baseININdisp$Duracao[valor] <- gsub("M.*","M", baseININdisp$Duracao[valor])
}

baseININdisp$Dias <- as.numeric(stringr::str_match(baseININdisp$Duracao, "([0-9]{1,2})d")[,-1]);
baseININdisp$Horas <- as.numeric(stringr::str_match(baseININdisp$Duracao, "([0-9]{1,2})H")[,-1]);
baseININdisp$Minutos <- as.numeric(stringr::str_match(baseININdisp$Duracao, "([0-9]{1,2})M")[,-1]);
baseININdisp$Dias[is.na(baseININdisp$Dias)] <- 0
baseININdisp$Horas[is.na(baseININdisp$Horas)] <- 0
baseININdisp <- na.omit(baseININdisp)

########### Tabela de Correlacao ########### 
source("D:/Estudo/R/Linguagem R/Scopus/Backup Script/Correlacao/tabela.R")

########### Base de Dados - BIOL ###########  
source("D:/Estudo/R/Linguagem R/Scopus/Backup Script/Volumetria - BIOL/Volumetria_PAOL.R");

########### Base de Dados - JVM ###########  
source("D:/Estudo/R/Linguagem R/Scopus/Backup Script/JVMs_Dados/JVM_Geral.R");

########### Functions ###########
moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

########### Start da Aplicacao ###########  
setwd("D:/Estudo/R/Linguagem R/Scopus/Backup Script/sherlock_web_main/index/");
#runApp(host = "172.16.144.119", port = 80)
runApp(host = "127.0.0.1", port = 80)
#runApp(host = "192.168.0.17", port = 4152)
#runApp(port = 80)