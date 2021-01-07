library(readxl)
library(dplyr)
library(ggplot2)
library(stargazer)
library(XML)
library(WriteXLS)
library(lmtest)
library(car)
library(xlsx)
library(devtools)
library(httr)
library(coefplot)
library(tidyr)

#####################################################################################################################
## LEIA ANTES DE RODAR O CÓDIGO!!!!                                                                                ## 
## O CÓDIGO DEMORA MUITO PARA SER RODADO, CASO NÃO TENHAM SIDO FEITAS ALTERAÇÕES NA BASE DE DADOS,                 ##
## RODE A PARTIR DA LINHA 397 PARA POUPAR TEMPO                                                                    ##
## O CÓDIGO IRÁ CARREGAR UMA BASE DE DADOS SALVA QUE ESTÁ DE ACORDO COM O ARTIGO PRECIFICAÇÃO DO TAQUARI PRODUZIDO ##
#####################################################################################################################

setwd("L:/GEFOR/03. Gestão/Estagiários/Daniel Bemerguy/Nova Saída Norte/Precificação Taquari")
#carregando dados
dados <- read_xls("Planilha Dados Comerciais Gefor.xls", sheet = "Base de Dados")
igp_di <- read_xls("IGP-DI.xls")

#Limpando a base de dados
dados <- dados[!-(dados$`ÁREA CONST. M²`== 0.000),]
colnames(dados)[11]<-'PRECO/M²' #alterando o nome da coluna
dados$index <- seq(1,nrow(dados),1)


#Acrescentando dados
dados[,'DISTANCIA.CENTRO'] <- c(0.0) 
dados[,"IGP-DI"] <- c(0.0)

#Criando a categoria Comercial 
for (i in dados$index){
  if (grepl("COMERCIAL/SERVIÇOS", dados$TIPOLOGIA)[i]==TRUE) {
    dados$TIPOLOGIA[i] <- "COMERCIAL"
  }
  i=0
}

#Criando a categoria Residencial
for (i in dados$index){
  if (grepl("HABITAÇÃO UNIFAMILIAR",dados$TIPOLOGIA)[i]==TRUE || grepl("HABITAÇÃO COLETIVA",dados$TIPOLOGIA)[i]==TRUE) {
    dados$TIPOLOGIA[i] <- "RESIDENCIAL"
  }
  i=0
}

#Criando categoria institucional
for (i in dados$index){
  if (grepl("INSTITUCIONAL/COLETIVO",dados$TIPOLOGIA)[i]==TRUE) {
    dados$TIPOLOGIA[i] <- "INSTITUCIONAL"
  }
  i=0
}

#Criando categora misto
for (i in dados$index){
  if (grepl("MISTO",dados$TIPOLOGIA)[i]==TRUE) {
    dados$TIPOLOGIA[i] <- "MISTO"
  }
  i=0
}

#Cirando categoria de posto
for (i in dados$index){
  if (grepl("POSTO",dados$TIPOLOGIA)[i]==TRUE) {
    dados$TIPOLOGIA[i] <- "POSTO"
  }
  i=0
}

#Criando uma categoria industrial
for (i in dados$index){
  if (grepl("INDUSTRIAL",dados$TIPOLOGIA)[i]==TRUE) {
    dados$TIPOLOGIA[i] <- "INDUSTRIAL"
  }
  i=0
}


dados <- dados[!-(dados$TIPOLOGIA=="INDUSTRIAL"),]
dados <- dados[!-(dados$TIPOLOGIA=="INSTITUCIONAL"),]
dados <- dados[!-(dados$TIPOLOGIA=="POSTO"),]


dados$index <- seq(1,nrow(dados),1)

i = 0
unique(dados$TIPOLOGIA)
####################################################################################################
#Calculando distancia

distance2Points <- function(origin,destination){
  origin=gsub(" ", "+", origin)
  destination=gsub(" ", "+", destination)
  results <- list();
  xml.url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,
                    '&key=YOUR GOOGLE API KEY HERE') 
  xmlfile <- xmlParse(GET(xml.url))
  dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
  distance <- as.numeric(sub(" km","",dist))
  distance <- distance/1000
  results[['dist']] <- distance
  return(results)
}

#Teste da funcao
distance2Points("POLO DE MODAS RUA 18,guara","Estacao rodoviaria do plano piloto,brasilia")
####################################################################################################
#Aguas Claras
#Distancia centro
for (j in dados$index){
  if (dados$CIDADE[j]=="AGUAS CLARAS"){
    endereco <- paste0(dados$ENDEREÇO[j],",",dados$CIDADE[j],",","brasilia")
    dados$`DISTANCIA.CENTRO`[j] <- as.numeric(distance2Points(endereco,"Estacao rodoviaria do plano piloto,brasilia"))
  }
  j = 0
}

for (j in dados$index){
  if (dados$CIDADE[j] == "AGUAS CLARAS"){
    if (dados$DISTANCIA.CENTRO[j] == 6.768){
      dados$DISTANCIA.CENTRO[j] <- 22.8
    }
  }
  j = 0
}
######################################################################################################
#Noroeste
#Distancia centro
for (j in dados$index){
  if (dados$CIDADE[j]=="NOROESTE"){
    if (dados$ENDEREÇO[j]=="AENW 02"){
      endereco <- paste0("Hospital de apoio de brasilia",",","brasilia")
    } else{
      endereco <- paste0(dados$ENDEREÇO[j],",","brasilia noroeste")
      dados$`DISTANCIA.CENTRO`[j]<- as.numeric(distance2Points(endereco,"Estacao rodoviaria do plano piloto,brasilia"))
    }
  }
  j = 0
}

for (j in dados$index){
  if (dados$CIDADE[j] == "NOROESTE"){
    if (dados$DISTANCIA.CENTRO[j] == 6.768){
      dados$DISTANCIA.CENTRO[j] <- 10.7
    }
  }
  j = 0
}

unique(dados$DISTANCIA.CENTRO)
#####################################################################################################
#Lago Norte
#Distancia Centro
for (j in dados$index){
  if (dados$CIDADE[j]=="LAGO NORTE"){
    endereco <- paste0(dados$ENDEREÇO[j],",","brasilia")
    dados$`DISTANCIA.CENTRO`[j] <- as.numeric(distance2Points(endereco,"Estacao rodoviaria do plano piloto,brasilia"))
  }
  j = 0
}

for (j in dados$index){
  if (dados$CIDADE[j] == "LAGO NORTE"){
    if (dados$DISTANCIA.CENTRO[j] == 6.768){
      dados$DISTANCIA.CENTRO[j] <- 9.8
    }
  }
  j = 0
}
#####################################################################################################
#Sobradinho
#Distancia centro
for (j in dados$index){
  if (dados$CIDADE[j]=="SOBRADINHO"){
    endereco <- paste0(dados$ENDEREÇO[j],",","Sobradinho, Brasilia - DF","brasilia")
    dados$`DISTANCIA.CENTRO`[j] <- as.numeric(distance2Points(endereco,"Estacao rodoviaria do plano piloto,brasilia"))
  }
  j = 0
}

for (j in dados$index){
  if (dados$CIDADE[j] == "SOBRADINHO"){
    if (dados$DISTANCIA.CENTRO[j] == 22.886){
      dados$DISTANCIA.CENTRO[j] <- 24.5
    }
  }
  j = 0
}
######################################################################################################
#Guará
#Distancia centro
for (j in dados$index){
  if (dados$CIDADE[j]=="GUARA"){
    endereco <- paste0(dados$ENDEREÇO[j],",","guara")
    try(dados$DISTANCIA.CENTRO[j] <- as.numeric(distance2Points(endereco,"Estacao rodoviaria do plano piloto,brasilia")),silent = T)
    if (grepl("Error",dados$DISTANCIA.CENTRO[j])==TRUE){
      dados$DISTANCIA.CENTRO[j] <- NA
    }
  }
  j = 0
}

for (j in dados$index){
  if (dados$CIDADE[j] == "GUARA" & dados$DISTANCIA.CENTRO[j] == 0){
    dados$DISTANCIA.CENTRO[j] <- 16.5
  }
  j = 0
}


#####################################################################################################
#Renda per capita
renda <- read_xlsx("Renda.xlsx")
dados[,"Renda"] <- 0.0

#Sobradinho
for (i in dados$index){
  if (dados$CIDADE[i] == "SOBRADINHO"){
    dados$Renda[i] <- renda$Renda[5]
  }
  i = 0
}

#Guara
for (i in dados$index){
  if (dados$CIDADE[i] == "GUARA"){
    dados$Renda[i] <- renda$Renda[3]
  }
  i = 0
}

#Lago Norte
for (i in dados$index){
  if (dados$CIDADE[i] == "LAGO NORTE"){
    dados$Renda[i] <- renda$Renda[1]
  }
  i = 0
}

#Noroeste
for (i in dados$index){
  if (dados$CIDADE[i] == "NOROESTE"){
    dados$Renda[i] <- renda$Renda[2]
  }
  i = 0
}

#Aguas Claras
for (i in dados$index){
  if (dados$CIDADE[i] == "AGUAS CLARAS"){
    dados$Renda[i] <- renda$Renda[4]
  }
  i = 0
}
#####################################################################################################
#Arrumando a série do IGP-DI para atualizar os valores de renda per capita
dados[(dados$ANO==1995),"IGP-DI"] <- igp_di$`IGP-DI`[1]
dados[(dados$ANO==1996),"IGP-DI"] <- igp_di$`IGP-DI`[2]
dados[(dados$ANO==1997),"IGP-DI"] <- igp_di$`IGP-DI`[3]
dados[(dados$ANO==1998),"IGP-DI"] <- igp_di$`IGP-DI`[4]
dados[(dados$ANO==1999),"IGP-DI"] <- igp_di$`IGP-DI`[5]
dados[(dados$ANO==2000),"IGP-DI"] <- igp_di$`IGP-DI`[6]
dados[(dados$ANO==2001),"IGP-DI"] <- igp_di$`IGP-DI`[7]
dados[(dados$ANO==2002),"IGP-DI"] <- igp_di$`IGP-DI`[8]
dados[(dados$ANO==2003),"IGP-DI"] <- igp_di$`IGP-DI`[9]
dados[(dados$ANO==2004),"IGP-DI"] <- igp_di$`IGP-DI`[10]
dados[(dados$ANO==2005),"IGP-DI"] <- igp_di$`IGP-DI`[11]
dados[(dados$ANO==2006),"IGP-DI"] <- igp_di$`IGP-DI`[12]
dados[(dados$ANO==2007),"IGP-DI"] <- igp_di$`IGP-DI`[13]
dados[(dados$ANO==2008),"IGP-DI"] <- igp_di$`IGP-DI`[14]
dados[(dados$ANO==2009),"IGP-DI"] <- igp_di$`IGP-DI`[15]
dados[(dados$ANO==2010),"IGP-DI"] <- igp_di$`IGP-DI`[16]
dados[(dados$ANO==2011),"IGP-DI"] <- igp_di$`IGP-DI`[17]
dados[(dados$ANO==2012),"IGP-DI"] <- igp_di$`IGP-DI`[18]
dados[(dados$ANO==2013),"IGP-DI"] <- igp_di$`IGP-DI`[19]
dados[(dados$ANO==2014),"IGP-DI"] <- igp_di$`IGP-DI`[20]
dados[(dados$ANO==2015),"IGP-DI"] <- igp_di$`IGP-DI`[21]
dados[(dados$ANO==2016),"IGP-DI"] <- igp_di$`IGP-DI`[22]
dados[(dados$ANO==2017),"IGP-DI"] <- igp_di$`IGP-DI`[23]

dados[,"preco_real"] <- c(0.0)
dados$preco_real <- dados$`PRECO/M²`*dados$`IGP-DI`

#####################################################################################################
#Criando dummys para destinação do lote
dados[,"comercial"] <- 0
dados[,"residencial"] <- 0
dados[,"misto"] <- 0
dados[,"posto"] <- 0
dados[,"industrial"] <- 0
dados[,"institucional"] <- 0



for (i in dados$index){
  if (dados$TIPOLOGIA[i] == "RESIDENCIAL"){
    dados$residencial[i] <- 1
  }
  i = 0
}


for (i in dados$index){
  if (dados$TIPOLOGIA[i] == "COMERCIAL"){
    dados$comercial[i] <- 1
  }
  i = 0
}

for (i in dados$index){
  if (dados$TIPOLOGIA[i] == "INSTITUCIONAL"){
    dados$institucional[i] <- 1
  }
  i = 0
}

for (i in dados$index){
  if (dados$TIPOLOGIA[i] == "INDUSTRIAL"){
    dados$industrial[i] <- 1
  }
  i = 0
}

for (i in dados$index){
  if (dados$TIPOLOGIA[i] == "POSTO"){
    dados$posto[i] <- 1
  }
  i = 0
}

for (i in dados$index){
  if (dados$TIPOLOGIA[i] == "MISTO"){
    dados$misto[i] <- 1
  }
  i = 0
}

#####################################################################################################
#matriz regional
dados[,"loc_noroeste"] <- 0
dados[,"loc_lago_norte"] <- 0
dados[,"loc_sobradinho"] <- 0
dados[,"loc_aguas_claras"] <- 0
dados[,"loc_guara"] <- 0

for (i in dados$index){
  if (dados$CIDADE[i] == "NOROESTE"){
    dados$loc_noroeste[i] <- 1
  }
  i = 0
}

for (i in dados$index){
  if (dados$CIDADE[i] == "LAGO NORTE"){
    dados$loc_lago_norte[i] <- 1
  }
  i = 0
}

for (i in dados$index){
  if (dados$CIDADE[i] == "SOBRADINHO"){
    dados$loc_sobradinho[i] <- 1
  }
  i = 0
}

for (i in dados$index){
  if (dados$CIDADE[i] == "AGUAS CLARAS"){
    dados$loc_aguas_claras[i]<- 1
  }
  i = 0
}

for (i in dados$index){
  if (dados$CIDADE[i] == "GUARA"){
    dados$loc_guara[i] <- 1
  }
  i = 0
}

#####################################################################################################
#Rodando o modelo
#Aplicando log em alguns dados
dados[,"lpot"] <- log(dados$`ÁREA CONST. M²`)
dados[,"ldist_centro"] <- log(dados$`DISTANCIA.CENTRO`)
dados[,"lpreco_real"] <- log(dados$preco_real)
dados[,"larea"] <- log(dados$`ÁREA LOTE M²`)
dados[,"lrenda"] <- log(dados$`Renda`)

######################RODE AQUI#####################################################################
setwd("L:/GEFOR/03. Gestão/Estagiários/Daniel Bemerguy/Nova Saída Norte/Precificação Taquari")
dados <- read_excel("Dados-de-teste.xls.xlsx")

reg1 <- lm(data = dados, lpreco_real ~ `DISTANCIA.CENTRO` + lpot + larea  + residencial+ comercial+ lrenda)
tabela <- stargazer(reg1, type = "text", title = "Resultados da regressão linear")
plot(reg1)
#coefplot, drop(_cons) xline(0)

#teste de Breusch-Pagan
bptest(reg1)

summary(reg1, robust = TRUE)

summaryR.lm <- function(model, type=c("hc3", "hc0", "hc1", "hc2", "hc4"), ...){
  
  if (!require(car)) stop("Required car package is missing.")
  
  type <- match.arg(type)
  V <- hccm(model, type=type)
  sumry <- summary(model)
  table <- coef(sumry)
  table[,2] <- sqrt(diag(V))
  table[,3] <- table[,1]/table[,2]
  table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail=FALSE)
  sumry$coefficients <- table
  p <- nrow(table)
  hyp <- cbind(0, diag(p - 1))
  sumry$fstatistic[1] <- linearHypothesis(model, hyp,white.adjust=type)[2,"F"]
  
  print(sumry)
  cat("Note: Heteroscedasticity-consistent standard errors using adjustment", type, "\n")
  
}

summaryR.lm(reg1,type="hc0") #Estimação robusta de White


####################################################################################################
#Previsão
# Extraindo coeficientes da regressão
(coef(reg1)[6])

b_interc <- as.numeric(coef(reg1)[1])
b_dist <- as.numeric(coef(reg1)[2])
b_pot <- as.numeric(coef(reg1)[3])
b_area <- as.numeric(coef(reg1)[4])
b_comercial <- as.numeric(coef(reg1)[6])
b_residencial <- as.numeric(coef(reg1)[5])
b_renda <- as.numeric(coef(reg1)[7])

#b_interc <- -6.88703
#b_dist <- -0.015719
#b_pot <- 0.998943
#b_area <- -1.10774
#b_comercial <- 0.342523
#b_residencial <- 0.405367
#b_renda <- 1.79981

########################################################################################################
#Gráficos
dados$b_interc <- c(b_interc)
dados$b_dist <- c(b_dist)
dados$b_pot <- c(b_pot)
dados$b_area <- c(b_area)
dados$b_comercial <- c(b_comercial)
dados$b_residencial <- c(b_residencial)
dados$b_renda <- c(b_renda)

dados$valor_previsto_residencial <- (dados$b_interc+ dados$b_dist*dados$`DISTANCIA.CENTRO`+dados$b_pot*dados$lpot
                                     +dados$b_area*dados$larea+dados$b_residencial*dados$residencial+
                                       +dados$b_renda*dados$lrenda)

dados$valor_previsto_comercial <- (dados$b_interc+ dados$b_dist*dados$`DISTANCIA.CENTRO`+dados$b_pot*dados$lpot
                                   +dados$b_area*dados$larea+dados$b_comercial*dados$comercial+
                                     +dados$b_renda*dados$lrenda)

dados$valor_previsto_misto <- (dados$b_interc+ dados$b_dist*dados$`DISTANCIA.CENTRO`+dados$b_pot*dados$lpot
                               +dados$b_area*dados$larea+dados$b_renda*dados$lrenda)

####Plotando
dados_residencial<-subset(dados,dados$TIPOLOGIA=="RESIDENCIAL")
dados_comercial<-subset(dados,dados$TIPOLOGIA=="COMERCIAL")
dados_misto <- subset(dados, dados$TIPOLOGIA=="MISTO")

dados_comercial$index <- seq(1,nrow(dados_comercial),1)

ggplot(dados_comercial,aes(dados_comercial$index)) + 
  geom_line(aes(y = valor_previsto_comercial, colour = "Previsão"),size = 0.75) + 
  geom_line(aes(y = lpreco_real, colour = "Valor real"),size = 0.75) +theme_bw() + ggtitle("Valor real x Valor previsto (Para imóveis comerciais)") +
  xlab("Observações") + ylab("Preço (em log)") + labs(colour = "") + theme(legend.position = "bottom") + theme(legend.text=element_text(size=12))

dados_residencial$index <- seq(1,nrow(dados_residencial),1)

ggplot(dados_residencial,aes(dados_residencial$index)) + 
  geom_line(aes(y = valor_previsto_residencial, colour = "Previsão"),size = 0.75) + 
  geom_line(aes(y = lpreco_real, colour = "Valor real"),size = 0.75) +theme_bw() + ggtitle("Valor real x Valor previsto (Para imóveis residenciais)") +
  xlab("Observações") + ylab("Preço (em log)") + labs(colour = "") + theme(legend.position = "bottom") + theme(legend.text=element_text(size=12))

summary(as.factor(dados$misto))

dados_misto$index <- seq(1,nrow(dados_misto),1)

ggplot(dados_misto,aes(dados_misto$index)) + 
  geom_line(aes(y = valor_previsto_misto, colour = "Previsão"),size = 0.75) + 
  geom_line(aes(y = lpreco_real, colour = "Valor real"),size = 0.75) +theme_bw() + ggtitle("Valor real x Valor previsto (Para imóveis mistos)") +
  xlab("Observações") + ylab("Preço (em log)") + labs(colour = "") + theme(legend.position = "bottom") + theme(legend.text=element_text(size=12))

#Plotando os resíduos
dados$predicted <- predict(reg1)   # Salvando valores estimados
dados$residuals <- residuals(reg1) # Salvando resíduos

ggplot(dados, aes(x = lpot, y = lpreco_real, colour = as.factor(TIPOLOGIA))) +
  #geom_segment(aes(xend = lpot, yend = predicted), alpha = .2) +  # Lines to connect points
  geom_point() +  # Points of actual values
  #geom_point(aes(y = predicted), shape = 1) 
   geom_smooth(method = "glm", se = FALSE, color = "red") + facet_grid(.~TIPOLOGIA) + guides(color = FALSE) + # Points of predicted values
  theme_bw()

head(fortify(reg1))
residPlot <- ggplot(aes(x=.fitted,y=.resid),data = reg1) + geom_point(aes(color = .resid)) + geom_hline(yintercept = 0) +
  labs(x = "Fitted Values",y = "Residual") + theme_minimal() +  # Color mapped here
  scale_colour_gradient2(mid = "blue", low = 'red', high = 'red') + # Colors to use here
  scale_y_continuous(limits = c(-3.0, 3), breaks = c(-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0,0.5,1,1.5,2,2.5,3)) +
  ylab("Resíduos") + xlab("Preço estimado do m² (em log)") + labs(colour = "Residuos") + ggtitle("Resíduos do modelo")
residPlot

###################################################################################################################################
### Adicionando informações para previsão Taquari
#m² médio para o noroeste
metro_comerc <- (dados$comercial==1)*dados$`ÁREA LOTE M²`*(dados$loc_noroeste==1)
metro_comercial <- metro_comerc[!-(metro_comerc==0)]
area_comerc_taquari <- log(mean(metro_comercial))

metro_resid <- (dados$residencial==1)*dados$`ÁREA LOTE M²`*(dados$loc_noroeste==1)
metro_residencial <- metro_resid[!-(metro_resid==0)]
area_resid_taquari <- log(mean(metro_residencial))

## Distancia do centro
dist_centro_taquari <- 19.7
dist_centro_taquari_ponte <- 12.9

##Potencial construtivo
lpot_resid_taquari <- log(6*mean(metro_residencial))
lpot_comerc_taquari <- log(3*mean(metro_comercial))

#Renda
vetor <- c(1775.79, 3339.91, 2683.23) #2683,23 erro de digitação
mean(vetor)
lrend_taquari <- log(mean(vetor))

a <-b_interc
b <- b_pot*lpot_comerc_taquari
c <- b_pot*lpot_resid_taquari
d <- b_area*area_comerc_taquari
e <- b_area*area_resid_taquari
f <- b_residencial
g <- b_comercial
i <- b_dist*dist_centro_taquari
j <- b_renda*lrend_taquari

i1 <- b_dist*dist_centro_taquari_ponte


#Sem a ponte
taquari_resid <- (a+i+c+e+f+j)
preco_resid <- exp(taquari_resid)
print(preco_resid)

taquari_comerc <- (a+i+d+b+g+j)
preco_comerc <- exp(taquari_comerc)
print(preco_comerc)

#Com a ponte
taquari_resid <- (a+i1+c+e+f+j)
preco_resid <- exp(taquari_resid)
print(preco_resid)

taquari_comerc <- (a+i1+d+b+g+j)
preco_comerc <- exp(taquari_comerc)
print(preco_comerc)

#####################################################################################################
#FINAL
#####################################################################################################

####################################################################################################
#Dados
#Gráfico da distribuição de preços
#Densidade
density_compare <- ggplot(dados,aes(lpreco_real,fill=TIPOLOGIA))+geom_density(alpha = 0.6, adjust = 1/2) +
  theme_minimal() + labs(x="Preço real por m² (em log)", y="Densidade") + ggtitle("Distribuição do preço por tipologia de uso")
density_compare
