#app asmatico
options(encoding = "UTF-8") ##opção importante

#Pacotes
library(shiny)
library(xts)
library(dygraphs)
library(shinythemes)
library(ggplot2)
library(ggrepel)
library(leaflet)


#####################################Arquivos (global) #####################################
pr_mun=rgdal::readOGR("/home/pbe/Documents/Pic/Arquivos_Internacao/dados_parana.shp")
pr_mun$codigo_ibg=substr(pr_mun$codigo_ibg,start = 1,6) %>% as.numeric()

cidade_codigo=data.frame(cidade=pr_mun$nome,codigo=pr_mun$codigo_ibg)
#sem_info=

dados_int_pr<-read.csv("/home/pbe/Documents/Pic/Arquivos_Internacao/intern_asma_pr_97_2017.csv",sep = ",",stringsAsFactors = F)
dados_int_pr$X<-rep(1,length(dados_int_pr$X))
#################################### Fim Arquivos ###########################################


####################################### Séries ##############################################
nenhum<-as.vector(tapply(dados_int_pr$X,dados_int_pr$data,sum))
#nenhum<-ts(nenhum,frequency = 12,start = c(1992,01))
nenhum<-xts(dados_int_pr$X,order.by = as.Date(paste0(dados_int_pr$data,"/01"),format = "%Y/%m/%d"))
nenhum.m<-apply.monthly(nenhum,FUN=sum)

masc<-subset(dados_int_pr,dados_int_pr$sexo==1)
masc<-xts(masc$X,order.by = as.Date(paste0(masc$data,"/01"),format = "%Y/%m/%d"))
masc<-apply.monthly(masc,FUN=sum)

fem<-subset(dados_int_pr,dados_int_pr$sexo!=1)
fem<-xts(fem$X,order.by = as.Date(paste0(fem$data,"/01"),format = "%Y/%m/%d"))
fem<-apply.monthly(fem,FUN=sum)

sex_int<-cbind(masc,fem)
names(sex_int)<-c("Masculino","Feminino")


infa<-subset(dados_int_pr,dados_int_pr$idade_cat=="Infância")
infa<-xts(infa$X,order.by = as.Date(paste0(infa$data,"/01"),format = "%Y/%m/%d"))
infa<-apply.monthly(infa,FUN=sum)

jov<-subset(dados_int_pr,dados_int_pr$idade_cat=="Jovem")
jov<-xts(jov$X,order.by = as.Date(paste0(jov$data,"/01"),format = "%Y/%m/%d"))
jov<-apply.monthly(jov,FUN=sum)

adul<-subset(dados_int_pr,dados_int_pr$idade_cat=="Adulto")
adul<-xts(adul$X,order.by = as.Date(paste0(adul$data,"/01"),format = "%Y/%m/%d"))
adul<-apply.monthly(adul,FUN=sum)

ido<-subset(dados_int_pr,dados_int_pr$idade_cat=="Idoso")
ido<-xts(ido$X,order.by = as.Date(paste0(ido$data,"/01"),format = "%Y/%m/%d"))
ido<-apply.monthly(ido,FUN=sum)

idade_int=cbind(infa,jov,adul,ido)
names(idade_int)=c("Infância","Jovem","Adulto","Idoso")
###################################### Fim Séries ########################################## 

###################################### UI ##################################################
ui <- navbarPage("Dados de internação \npor Asma no Paraná - 1997/2017",theme = shinytheme("flatly"),
   # Application title
   tabPanel("Mapa", 
            div(class="outer",
                tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                leafletOutput("mapa",width="100%",height="1000px"),

                # Shiny versions prior to 0.11 should use class = "modal" instead.
                absolutePanel(id = "controles", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 170, left = 30, right ="auto", bottom = "auto",
                              width = 360, height = "auto",

                              

                              sliderInput("escolha_ano","Anos de interesse",min = 1997,max = 2017,
                                          value = c(1997,2017))
                             )
                  )
    ),
   tabPanel("Série Temporal",
      # Show the plots
      mainPanel(
         conditionalPanel(condition = "input.Selec=='Nenhum'",dygraphOutput("nenhum")),
         conditionalPanel(condition = "input.Selec=='Sexo'",dygraphOutput("sexo")),
         conditionalPanel(condition = "input.Selec=='Idade'",dygraphOutput("idade")),hr()
               ),
      column(3,offset = 1,
             #selectInput("UF","Estado",choices = c("PR")),
             selectInput("Selec","Selecione o critério",c("Nenhum","Sexo","Idade")))),
   tabPanel("Análise Descritiva",
            
            selectInput("cidades","Escolha a cidade:",choices = c("Todas",as.character(pr_mun$nome))),
            hr(), 
            conditionalPanel("input.cidades=='Todas'",
                             plotOutput("sexodesc1"), hr(),
                             plotOutput("idadedesc1")) ,
            conditionalPanel("input.cidades!='Todas'",
                             plotOutput("sexodesc"), hr(),
                             plotOutput("idadedesc"))
                      )
            )

###################################### Fim UI ##############################################

##################################### Server ###############################################
server <- function(input, output){
   output$nenhum <- renderDygraph({
                      dygraph(nenhum.m) %>%
                      dyRangeSelector()})
   output$sexo <- renderDygraph({
                      dygraph(sex_int,main="Internações por Sexo") %>%
                      dyRangeSelector()})
   output$idade <- renderDygraph({
     dygraph(idade_int,main="Internações por Categoria de Idade") %>%
       dyRangeSelector()})
   
  output$sexodesc1<-renderPlot({
    sexo.dt<-as.data.frame(table(dados_int_pr$sexo))
    sexo.dt$Var1<-c("Masculino","Feminino")
    sexo.dt$frac = sexo.dt$Freq / sum(sexo.dt$Freq)
    sexo.dt = sexo.dt[order(sexo.dt$frac), ]
    sexo.dt$ymax = cumsum(sexo.dt$frac)
    sexo.dt$ymin = c(0, head(sexo.dt$ymax, n=-1))
    sexo.dt$Sexo<-sexo.dt$Var1
    sexo.dt$prop<-paste0(round(prop.table(table(dados_int_pr$sexo))*100,2),"%")
    
    grafico.sex1<-ggplot(sexo.dt,aes(fill=Sexo,ymax=ymax, ymin=ymin, xmax=100, xmin=80,label=prop))
    grafico.sex1<-grafico.sex1 +geom_rect()+coord_polar(theta = 'y')+xlim(c(0,100))
    grafico.sex1<-grafico.sex1+  theme(axis.text=element_blank())+theme(axis.ticks=element_blank()) 
    grafico.sex1<-grafico.sex1+ annotate("text", x = 0, y = 0, size = 5, label = "Sexo")  
    grafico.sex1<-grafico.sex1+theme(panel.grid=element_blank())+theme(axis.title=element_blank()) 
    grafico.sex1<-grafico.sex1+ scale_fill_manual(values = c('firebrick3','royalblue4'))
    grafico.sex1<-grafico.sex1+theme(panel.background= element_blank())
    grafico.sex1<-grafico.sex1+geom_label_repel(aes(label = prop, x = 100, y = (ymin + ymax)/2),inherit.aes = F, show.legend = F, size = 5)
    grafico.sex1<-grafico.sex1+ggtitle("Proporção do sexo dos internados por asma \nno Paraná de 1997 a 2017.")
    grafico.sex1<-grafico.sex1+labs(caption="Fonte: DATASUS.")
    grafico.sex1
  })
  output$sexodesc <- renderPlot({
    codigo_s=cidade_codigo[cidade_codigo$cidade==input$cidades,2]
    dados_sex1=subset(dados_int_pr,dados_int_pr$inter==codigo_s)
    sexo.dt<-as.data.frame(table(dados_sex1$sexo))
    sexo.dt$Var1<-c("Masculino","Feminino")
    sexo.dt$frac = sexo.dt$Freq / sum(sexo.dt$Freq)
    sexo.dt = sexo.dt[order(sexo.dt$frac), ]
    sexo.dt$ymax = cumsum(sexo.dt$frac)
    sexo.dt$ymin = c(0, head(sexo.dt$ymax, n=-1))
    sexo.dt$Sexo<-sexo.dt$Var1
    sexo.dt$prop<-paste0(round(prop.table(table(dados_sex1$sexo))*100,2),"%")
    
    grafico.sex<-ggplot(sexo.dt,aes(fill=Sexo,ymax=ymax, ymin=ymin, xmax=100, xmin=80,label=prop))
    grafico.sex<-grafico.sex +geom_rect()+coord_polar(theta = 'y')+xlim(c(0,100))
    grafico.sex<-grafico.sex+  theme(axis.text=element_blank())+theme(axis.ticks=element_blank()) 
    grafico.sex<-grafico.sex+ annotate("text", x = 0, y = 0, size = 5, label = "Sexo")  
    grafico.sex<-grafico.sex+theme(panel.grid=element_blank())+theme(axis.title=element_blank()) 
    grafico.sex<-grafico.sex+ scale_fill_manual(values = c('firebrick3','royalblue4'))
    grafico.sex<-grafico.sex+theme(panel.background= element_blank())
    grafico.sex<-grafico.sex+geom_label_repel(aes(label = prop, x = 100, y = (ymin + ymax)/2),inherit.aes = F, show.legend = F, size = 5)
    grafico.sex<-grafico.sex+ggtitle(paste("Proporção do sexo dos internados por asma", "\nem",input$cidades,"-PR de 1997 a 2017."))
    grafico.sex<-grafico.sex+labs(caption="Fonte: DATASUS.")
    grafico.sex
  })

  output$idadedesc1 <- renderPlot({
    
    
    fxet.dt<-as.data.frame(table(dados_int_pr$idade_cat))
    fxet.dt$Var1<-factor(fxet.dt$Var1,levels = c("Infância","Jovem","Adulto","Idoso"))
    fxet.dt$lb<-paste(fxet.dt$Freq,paste0("(",round(prop.table(table(dados_int_pr$idade_cat))*100,2),"%",")"))
    
    graf.id<-ggplot(fxet.dt,aes(x=Var1,y=Freq,label=lb))+geom_bar(stat = "identity")+theme_minimal()
    graf.id<-graf.id+xlab("Categoria de idade")+ylab("Frequência Absoluta")+geom_text(nudge_y = 5000)
    graf.id<-graf.id+ggtitle("Distribuição da frequência de internações por asma no Paraná \nde 1997 a 2017 conforme a categoria de idade.")
    graf.id<-graf.id+labs(caption="Fonte: DATASUS.")
    graf.id
  })
  
  output$idadedesc <- renderPlot({
    codigo_i=cidade_codigo[cidade_codigo$cidade==input$cidades,2]
    dados_id1=subset(dados_int_pr,dados_int_pr$inter==codigo_i)
    
    fxet.dt<-as.data.frame(table(dados_id1$idade_cat))
    fxet.dt$Var1<-factor(fxet.dt$Var1,levels = c("Infância","Jovem","Adulto","Idoso"))
    fxet.dt$lb<-paste(fxet.dt$Freq,paste0("(",round(prop.table(table(dados_id1$idade_cat))*100,2),"%",")"))
    
    graf.id<-ggplot(fxet.dt,aes(x=Var1,y=Freq,label=lb))+geom_bar(stat = "identity")+theme_minimal()
    graf.id<-graf.id+xlab("Categoria de idade")+ylab("Frequência Absoluta")+geom_text(nudge_y = max(fxet.dt$Freq)/4)
    graf.id<-graf.id+ggtitle(paste("Distribuição da frequência de internações por asma","em",input$cidades ,"-PR \nde 1997 a 2017 conforme a categoria de idade."))
    graf.id<-graf.id+labs(caption="Fonte: DATASUS.")
    graf.id
  })
  
  output$mapa <- renderLeaflet({
    dados_filtro=subset(dados_int_pr,dados_int_pr$ano<=input$escolha_ano[2] & dados_int_pr$ano>=input$escolha_ano[1])
    #dados_filtro=subset(dados_int_pr,dados_int_pr$ano<=2017 & dados_int_pr$ano>=2010)
    data1=table(dados_filtro$inter) %>% as.data.frame() 
    names(data1)=c("codigo_ibg","Freq")
    
    
    mapa1=sp::merge(pr_mun,data1,by="codigo_ibg")
    
    mapa1$lon=sp::coordinates(mapa1)[,1]
    mapa1$lat=sp::coordinates(mapa1)[,2]
    mapa1$legenda=paste(mapa1$nome,"<br>",
                        "Casos:",mapa1$Freq)
    
    cores=colorNumeric(palette = "Blues",
                       domain = mapa1$Freq)
    
    
    leaflet(mapa1)%>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("Blues", Freq)(Freq),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup =~legenda) %>%
      
      addLegend(values =~Freq, pal= cores,title = "Número de casos")
  })
}
####################################### Fim Server #########################################
shinyApp(ui = ui, server = server)

