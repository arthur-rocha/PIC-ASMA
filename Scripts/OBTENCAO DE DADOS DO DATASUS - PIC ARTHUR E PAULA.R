###########################################################################
########## LIDANDO COM DADOS DO DATASUS REFERENTES A INTERNACAO ###########
###########################################################################
### PROGRAMA DE INICIACAO CIENTIFICA - UNIVERSIDADE ESTADUAL DE MARINGA ###
###########################################################################
#AUTORES:                                                                 #
# ARTHUR C. ROCHA - arthurcesariv@gmail.com                               #
# PAULA M. HELLER -                                                       #
#                                                                         #
#PROF. ORIENTADORA : ENIUCE M. SOUZA                                      #
###########################################################################
##################### DEPARTAMENTO DE ESTATISTICA - UEM ###################
###########################################################################


### ATENCAO!!! abrir esse arquivo em UTF-8 para evitar problemas
### NA IDE R-Studio va em file -> Reopen with Encoding e escolha utf-8

## ASMA

#definicoes gerais
options(stringsAsFactors = FALSE) #Evitar fatores desnecessários
setwd("/home/pbe/Downloads")      #Pasta onde os arquivos serão baixados

#Pacotes
library(read.dbc)   #Leitura de formato .dbc
library(foreign)    #
library(MASS)       #
library(sqldf)      #Manipulação SQL
library(ryouready)  #
library(car)        #

###########################################################################
############################# Download dos dados ##########################
###########################################################################

#Baixando dados 1992-2007 - PR
url <- 'ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/'
(anos=paste0(c(92:99,paste0(paste0(0,0:7))),rep(c(paste0(0,1:9),10:12),each=12)))
for(ano in anos){
  arq <- paste('RDPR',ano,'.dbc',sep='')
  download.file(paste0(url,arq), arq, mode='wb')}


#Baixando dados 2008 - 2015 - PR
url <- 'ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/'
(anos=paste0(c(paste0(paste0(0,8:9)),10:17),rep(c(paste0(0,1:9),10:12),each=12)))
sort(anos)
for(ano in anos){
  arq <- paste('RDPR',ano,'.dbc',sep='')
  download.file(paste0(url,arq), arq, mode='wb')}


#Baixando dados 2016 - 2017 - PR
url <- 'ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/'
(anos=paste0(16:17,rep(c(paste0(0,1:9),10:12),each=12)))
anos<-c(1611,1612)
for(ano in anos){
  arq <- paste('RDPR',ano,'.dbc',sep='')
  download.file(paste0(url,arq), arq, mode='wb')}

############################## Fim Download ################################

###########################################################################
####################### Leitura e manipulacao dos dados ###################
###########################################################################

dados <- NULL
#dados2007<-NULL
#(anos=paste0(c(92:99,paste0(paste0(0,0:7))),rep(c(paste0(0,1:9),10:12),each=12)))
(anos=paste0(c(92:99,paste0(paste0(0,0:9)),10:15),rep(c(paste0(0,1:9),10:12),each=24)))

for(ano in anos){ 
  arq <- paste('RDPR',ano,'.dbc',sep='')
  aux <- read.dbc(arq)
  if (ano == anos[1]) vars = colnames(aux)
  print(colnames(aux))
  print(dim(aux))
  dados <- rbind(dados,aux[,vars])} #problema quando colunas indefinidas selecionadas 



#Criando o banco de dados geral com as colunas já selecionadas
anos<-c(92:99,paste0(0,0:9),10:17)
meses<-c(paste0(0,1:9),10:12)
ano=mes=sexo=diagnostico=inter=idade=cod_idade=NULL
dados_gerais=NULL
for (a in 1:length(anos)) {
  for (m in 1:12) {
    arq <- paste('RDPR',anos[a],meses[m],'.dbc',sep='')
    aux <- read.dbc(arq,as.is=T)
    ano<-c(ano,aux$ANO_CMPT)
    mes<-c(mes,aux$MES_CMPT)
    sexo<-c(sexo,aux$SEXO)
    diagnostico<-c(diagnostico,aux$DIAG_PRINC)
    inter<-c(inter,aux$MUNIC_MOV)
    idade<-c(idade,aux$IDADE)
    cod_idade<-c(cod_idade,aux$COD_IDADE)}
  dt.ano<-data.frame(ano=ano,mes=mes,sexo=sexo,diagnostico=diagnostico,inter=inter,idade=idade,cod_idade=cod_idade)
  dados_gerais<-rbind(dados_gerais,dt.ano)
  ano=mes=sexo=diagnostico=inter=idade=cod_idade=NULL}

##Para 2017 o ano esta incompleto, portanto deve-se usar outro laco ate o mes existente (mais recente)
anos<-c(17)
meses<-c(paste0(0,1:9))
ano=mes=sexo=diagnostico=inter=idade=cod_idade=NULL
for (a in 1:length(anos)) {
  for (m in 1:9) {
    arq <- paste('RDPR',anos[a],meses[m],'.dbc',sep='')
    aux <- read.dbc(arq,as.is=T)
    ano<-c(ano,aux$ANO_CMPT)
    mes<-c(mes,aux$MES_CMPT)
    sexo<-c(sexo,aux$SEXO)
    diagnostico<-c(diagnostico,aux$DIAG_PRINC)
    inter<-c(inter,aux$MUNIC_MOV)
    idade<-c(idade,aux$IDADE)
    cod_idade<-c(cod_idade,aux$COD_IDADE)}
  dt.ano<-data.frame(ano=ano,mes=mes,sexo=sexo,diagnostico=diagnostico,inter=inter,idade=idade,cod_idade=cod_idade)
  dados_gerais<-rbind(dados_gerais,dt.ano)
  ano=mes=sexo=diagnostico=inter=idade=cod_idade=NULL}

str(dados_gerais)

##Setando os codigos do CID (J45...,J46...,493)
##cid_asma<-c('J45','J450','J451','J458','J459')

##Utilizando linguagem SQL pra criar tabela asma
dados_asma<-sqldf("select * from dados_gerais where diagnostico like 'J45%' 
                   or diagnostico like '493%' or diagnostico like 'J46%' ") 
                   

### Mudando anos 
dados_asma$ano<-ifelse(dados_asma$ano=='92',"1992",
                       ifelse(dados_asma$ano=='93',"1993",
                              ifelse(dados_asma$ano=='94',"1994",
                                     ifelse(dados_asma$ano=='95',"1995",
                                            ifelse(dados_asma$ano=='96',"1996",
                                                   ifelse(dados_asma$ano=='97',"1997",dados_asma$ano))))))


#Criando Vari?vel de data
dados_asma$data<-paste0(dados_asma$ano,"/",dados_asma$mes)


#Transformando Idade codificada
ida <- function(idade){
  switch(substr(idade,1,1),
         "0" = { return(as.numeric(substr(idade,2,3))/(60*24*365)) },
         "1" = { return(as.numeric(substr(idade,2,3))/(24*365)) },
         "2" = { return(as.numeric(substr(idade,2,3))/365) },
         "3" = { return(as.numeric(substr(idade,2,3))/12) },
         "4" = { return(as.numeric(substr(idade,2,3))) },
         "5" = { return(as.numeric(substr(idade,2,3))+100) })}

dados_asma$anos<-apply(as.matrix(as.numeric(paste0(dados_asma$cod_idade,dados_asma$idade))),1,ida)

##Categorizando idade
dados_asma$idade_cat <- Recode(dados_asma$anos, as.factor.result=TRUE,
                    intervals(" [lo,1)  = 'Infância'; 
                                [1,5)   = 'Infância'; 
                                [5,10)  = 'Infância'; 
                                [10,15) = 'Jovem'; 
                                [15,20) = 'Jovem'; 
                                [20,25) = 'Jovem'; 
                                [25,30) = 'Adulto'; 
                                [30,40) = 'Adulto'; 
                                [40,50) = 'Adulto'; 
                                [50,60) = 'Adulto'; 
                                [60,70) = 'Idoso'; 
                                [70,hi] = 'Idoso';
                                else = NA"))

##Salvando dados da asma - PR em um arquivo
##write.csv(dados_asma,"intern_asma_pr.csv",quote = T)

##Salvando dados da asma - PR (1997-2017) em um arquivo
dados_asma=subset(dados_asma$ano>=1997)
write.csv(dados_asma,"intern_asma_pr.csv",quote = T)