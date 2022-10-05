#setwd("C:/Users/601545/Desktop/projetos/datasus/") # Davi
setwd("C:/Users/601546/Documents/DATASUS/")  # Lucas

pacman::p_load("tidyverse","ggplot2","stringr","read.dbc","lubridate","shinydashboard","plotly", "magrittr")



data <- get(load('data.RData'))

df_taxa_ocup <- get(load('df_taxa_ocup.RData'))







shinyServer(function(input, output) {#Bancos reativos 
    
 
    #Bancos com filtro 1: Todas unidades e todos os meses
    banco1 <- reactive({
        data %>%  
            filter(ANO_CMPT == input$ano)
    })
    
    banco_date1 <- reactive({
        df_taxa_ocup %>%  
            filter( ANO == input$ano)
    })
    
    #Banco com filtro 2: Todas as unidades e mês especifico
    banco2 <- reactive({
        data %>%  
            filter(ANO_CMPT == input$ano) %>% 
            filter(MES_EXTENSO == input$mes) 
    })
    
    banco_date2 <- reactive({
        df_taxa_ocup %>%  
            filter( ANO == input$ano) %>% 
            filter( MES_EXTENSO == input$mes)
    })
    
    #Banco com filtro 3: Todos os meses e unidade especifica
    banco3 <- reactive({
        data %>%  
            filter(ANO_CMPT == input$ano) %>% 
            filter(CIDADE == input$unidade)
    })  
    
    banco_date3 <- reactive({
        df_taxa_ocup %>%  
            filter( ANO == input$ano) %>% 
            filter( cidade == input$unidade)
    })    
    
    #Banco com filtro 4: Mês e unidade especifica
    banco4 <- reactive({
        data %>%  
            filter(ANO_CMPT == input$ano) %>%  
            filter(MES_EXTENSO == input$mes) %>% 
            filter(CIDADE == input$unidade)
      }
    ) 
        
    banco_date4 <- reactive({
        df_taxa_ocup %>%  
            filter( ANO == input$ano) %>% 
            filter( cidade == input$unidade) %>% 
            filter( MES_EXTENSO == input$mes) 
    })
    
  
    
    
    #Outputs:
    
    
    ########UNIDADE SELECIONADA##### 
    
    output$unidade <- renderText(input$unidade)
    
    output$ano <- renderText(input$ano)
    
    output$mes <- renderText(input$mes)
    
    
    ######## Indicadores ######
    
    
    
    
    ##lOGO ####
    output$logo <- renderImage({
        list(src = "logo_header.png",
             alt = "SARAH",
             width = 180,
             height = 26
        )
    }, deleteFile = F) 
    
    
    ##TOTAL DE AIH ####
    output$total<- renderInfoBox({
        
        banco1 <- banco1()
        banco2 <- banco2()
        banco3 <- banco3()
        banco4 <- banco4()
        
        data$ANO_CMPT <- as.numeric(as.character(data$ANO_CMPT))
        
        if (input$unidade == 'Todos'){
            if (input$mes == 'Todos') {
                total<- banco1 %>% 
                    summarise(cont =  n()) %>% 
                    select(cont) %>% as.numeric
            }
            else{
                total<- banco2 %>% 
                    group_by(MES_CMPT) %>% 
                    summarise(cont =  n()) %>% 
                    select(cont) %>% as.numeric
            }
        } 
        else {
            if (input$mes == 'Todos') {
                total<- banco3 %>% 
                    group_by(MES_CMPT) %>%
                    summarise(cont =  n()) %>% 
                    select(cont) %>% sum 
            }
            else {
                total<- banco4 %>% 
                    group_by(MES_CMPT) %>%
                    summarise(cont =  n()) %>% 
                    select(cont) %>% sum 
            }
            }
        
        
        
        
        infoBox(paste0('Valor Total AIH \n',input$mes," de ",input$ano),
                total, 
                icon = icon('plus',lib ='glyphicon'),
                color ='light-blue')
    })
    
    
    
    ##VALOR DAS AIHS ####
    output$valortotal<- renderInfoBox({
        
        banco1 <- banco1()
        banco2 <- banco2()
        banco3 <- banco3()
        banco4 <- banco4()
        
        data$ANO_CMPT <- as.numeric(as.character(data$ANO_CMPT))
        data$VAL_TOT <- as.numeric(as.character(data$VAL_TOT))
        
        if (input$unidade == 'Todos'){
          if (input$mes == 'Todos') {
            valortotal<- banco1 %>% 
              summarise(soma = sum(VAL_TOT)) %>% 
              as.numeric() %>% convert_dindin
          }
          else{
            valortotal<- banco2 %>% 
              summarise(soma = sum(VAL_TOT)) %>% 
              as.numeric() %>% convert_dindin
          }
        } 
        else {
          if (input$mes == 'Todos') {
            valortotal<- banco3 %>% 
              summarise(soma = sum(VAL_TOT)) %>% 
              as.numeric() %>% convert_dindin  
          }
          else {
            valortotal<- banco4 %>% 
              summarise(soma = sum(VAL_TOT)) %>% 
              as.numeric() %>% convert_dindin
          }
        }
        
        
        infoBox(paste0('Valor Total AIH \n',input$mes," de ",input$ano),
                paste0("R$ ",valortotal),
                icon = icon("money"),
                color ='light-blue')
    })
    
    
    
    
    
    
   
    
    
    ##TAXA DE OCUPAÇÃO ####
    output$taxa_ocupacao <- renderInfoBox({
        
        banco1 <- banco_date1()
        banco2 <- banco_date2()
        banco3 <- banco_date3()
        banco4 <- banco_date4()
        
        #df_taxa_ocup$ANO_CMPT <- as.numeric(as.character(df_taxa_ocup$ANO))
        
        if (input$unidade == 'Todos'){
          if (input$mes == 'Todos') {
            taxa_ocupacao <- banco1 %>% 
              group_by(ANO) %>% 
              summarise(soma = sum(n_pessoas)/(682*365)) %>%
              mutate(soma = round(soma,4)*100) %>% 
              mutate(soma = paste0(soma,"%"))
          }
          else{
            taxa_ocupacao <-banco2 %>%
              group_by(MES_EXTENSO) %>% 
              summarise(soma = sum(n_pessoas)/(682*num_dia_mes)) %>%
              mutate(soma = round(soma,4)*100) %>% 
              mutate(soma = paste0(soma,"%")) %>% 
              unique
          }
        } 
        else {
          if (input$mes == 'Todos') {
            taxa_ocupacao <-banco3 %>%
              group_by(ANO) %>% 
              summarise(soma = sum(n_pessoas)/(LEITOS*365)) %>%
              mutate(soma = round(soma,4)*100) %>% 
              mutate(soma = paste0(soma,"%")) %>% unique
          }
          else {
            taxa_ocupacao <- banco4 %>%  
              group_by(MES_EXTENSO) %>% 
              summarise(soma = sum(n_pessoas)/(LEITOS*num_dia_mes)) %>%
              mutate(soma = round(soma,4)*100) %>% 
              mutate(soma = paste0(soma,"%")) %>% select(soma) %>% 
              unique
          }
        }
        
        
        
        infoBox(paste0('Taxa de ocupação dos Leitos'),
                taxa_ocupacao[2], 
                icon = icon('plus',lib ='glyphicon'),
                color ='light-blue')
        
        
       
    })
    
    
    
    
    ######### Gráficos
    
    
    output$linha <- renderPlotly({
        
        banco1 <- banco1()
        banco2 <- banco2()
        
        data$ANO_CMPT <- as.numeric(as.character(data$ANO_CMPT))
        
        
        
        if (input$unidade == 'Todos'){
            teste <- data %>%
                group_by(MES_CMPT, ANO_CMPT ) %>%
                summarise(cont = n())  %>%
                rename(Mês = MES_CMPT,
                       `Número de AIH´s` = cont,
                       Ano = ANO_CMPT) %>% 
                as.data.frame()
        } 
        else {
            teste <- data %>% 
                filter(CIDADE == input$unidade) %>% 
                group_by(MES_CMPT, ANO_CMPT ) %>%
                summarise(cont = n())  %>%
                rename(Mês = MES_CMPT,
                       `Número de AIH´s` = cont,
                       Ano = ANO_CMPT) %>% 
                as.data.frame()}
        
        ordem <- c("01","02","03","04","05","06","07","08", "09", "10", "11","12")
        
        teste$`Mês` <- factor(levels = ordem, teste$`Mês`, ordered = T)
        teste$Ano <- as.factor(teste$Ano)
        
        linha <- ggplot(teste, aes(x = Mês, y = `Número de AIH´s`, colour = Ano, group =Ano)) + 
            geom_line(size = 0.6) + 
            geom_point(size = 0.6) +
            scale_color_manual(values = c("orange", "steelblue","black", "red", "lightgreen") )+
            expand_limits(y=0)+
            padrao
        
        ggplotly(linha,height= 265)
        
        
        
        
        
        
        
        
    })
    
    ##gráfico de setores####
    
    output$sexo <- renderPlotly({
        
        banco1 <- banco1()
        banco2 <- banco2()
        banco3 <- banco3()
        banco4 <- banco4()
        
        cores<- c("orange", "steelblue")
        data$ANO_CMPT <- as.numeric(as.character(data$ANO_CMPT))
        
        
        if (input$unidade == 'Todos'){
          if (input$mes == 'Todos') {
            newdata <- banco1
          }
          else{
            newdata <- banco2
          }
        } 
        else {
          if (input$mes == 'Todos') {
            newdata <- banco3
          }
          else {
            newdata <- banco4
          }
        }
        
        
        
        newdata$SEXO <- as.factor(newdata$SEXO)
        
        sexo <- newdata %>%
            group_by(SEXO) %>% 
            summarise(count = n())
        
        
        fig <- sexo %>% 
            plot_ly(labels = ~SEXO, values = ~count, marker = list(colors = cores),height= 265 ) %>%
            add_pie(hole = 0.6) %>%
            layout(
                showlegend = T,
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        
        fig
        
    })
    
    output$idade <- renderPlotly({
        
      banco1 <- banco1()
      banco2 <- banco2()
      banco3 <- banco3()
      banco4 <- banco4()
      
      cores<- c("orange", "steelblue")
      data$ANO_CMPT <- as.numeric(as.character(data$ANO_CMPT))
      
      
      if (input$unidade == 'Todos'){
        if (input$mes == 'Todos') {
          newdata <- banco1
        }
        else{
          newdata <- banco2
        }
      } 
      else {
        if (input$mes == 'Todos') {
          newdata <- banco3
        }
        else {
          newdata <- banco4
        }
      }
        
        
        
        
        idade <- newdata %>%
            group_by(maior16) %>% 
            summarise(count = n())
        
        
        fig <- idade %>% 
            plot_ly(labels = ~maior16, values = ~count, marker = list(colors = cores),height= 480, width = 300  ) %>%
            add_pie(hole = 0.6) %>%
            layout(
                showlegend = T,
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                legend = list(orientation = 'h'))
        
        
        fig
        
    })
    
    output$nacional <- renderPlotly({
        
      banco1 <- banco1()
      banco2 <- banco2()
      banco3 <- banco3()
      banco4 <- banco4()
      
      cores<- c("orange", "steelblue")
      data$ANO_CMPT <- as.numeric(as.character(data$ANO_CMPT))
      
      
      if (input$unidade == 'Todos'){
        if (input$mes == 'Todos') {
          newdata <- banco1
        }
        else{
          newdata <- banco2
        }
      } 
      else {
        if (input$mes == 'Todos') {
          newdata <- banco3
        }
        else {
          newdata <- banco4
        }
      }
        
        
        
        agrupado <- newdata %>%
            group_by( ANO_CMPT,NACIONAL) %>%
            count() %>% as.data.frame()
        
        
        agrupado %<>% 
            mutate(pais = ifelse(NACIONAL == '010', 'Brasil', 
                                 ifelse(NACIONAL == '055', 'Erro 55', 'Outros')))
        
        
        
        fig <- agrupado %>% 
            plot_ly(labels = ~pais, values = ~n, marker = list(colors = cores),height= 265 ) %>%
            add_pie(hole = 0.6) %>%
            layout(
                showlegend = T,
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        
        fig
        
    })
    
    output$raca <- renderPlotly({
        
      banco1 <- banco1()
      banco2 <- banco2()
      banco3 <- banco3()
      banco4 <- banco4()
      
      cores<- c("orange", "steelblue")
      data$ANO_CMPT <- as.numeric(as.character(data$ANO_CMPT))
      
      
      if (input$unidade == 'Todos'){
        if (input$mes == 'Todos') {
          newdata <- banco1
        }
        else{
          newdata <- banco2
        }
      } 
      else {
        if (input$mes == 'Todos') {
          newdata <- banco3
        }
        else {
          newdata <- banco4
        }
      }
        
        
        
        agrupado <- newdata %>%
            group_by( ANO_CMPT,RACA_COR) %>%
            count() %>% as.data.frame()
        
        agrupado$RACA_COR <- as.factor(agrupado$RACA_COR)
        
        
        
        fig <- agrupado %>% 
            plot_ly(labels = ~RACA_COR, values = ~n, marker = list(colors = cores),height= 480, width = 300 ) %>%
            add_pie(hole = 0.6) %>%
            layout(
                showlegend = T,
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                legend = list(orientation = 'h'))
        
        
        fig
    })    
    
    ####### Tabelas 
    
    
    data$ANO_CMPT <- as.numeric(as.character(data$ANO_CMPT))
    
    
    tableInternacao <- reactive({ 
        
      
      
      if (input$unidade == 'Todos'){
        if (input$mes == 'Todos') {
          tabela <- data %>% 
            filter(ANO_CMPT == input$ano) %>%  
            group_by(Código , Descrição) %>%
            summarise(Total = n(),
                      Valor = sum(VAL_TOT)) %>%
            as.data.frame() 
        }
        else{
          tabela <- data %>% 
            filter(ANO_CMPT == input$ano) %>%  
            filter(MES_EXTENSO == input$mes) %>% 
            group_by(Código , Descrição) %>%
            summarise(Total = n(),
                      Valor = sum(VAL_TOT)) %>%
            as.data.frame() 
        }
      } 
      else {
        if (input$mes == 'Todos') {
          tabela <- data %>% 
            filter(ANO_CMPT == input$ano) %>%  
            filter(CIDADE == input$unidade) %>% 
            group_by(Código , Descrição) %>%
            summarise(Total = n(),
                      Valor = sum(VAL_TOT)) %>%
            as.data.frame() 
        }
        else {
          tabela <- data %>% 
            filter(ANO_CMPT == input$ano) %>%  
            filter(MES_EXTENSO == input$mes) %>% 
            filter(CIDADE == input$unidade) %>%  
            group_by(Código , Descrição) %>%
            summarise(Total = n(),
                      Valor = sum(VAL_TOT)) %>%
            as.data.frame() 
        }
      }
        
        
    }) 
    
    
    output$tableInternacao <- renderDT(tableInternacao(),
                                      filter = 'top', options = list(pageLength = 5))
    
    
    
    
    data$ANO_CMPT <- as.numeric(as.character(data$ANO_CMPT))
    
    
    
    tableCID <- reactive({ 
        
      if (input$unidade == 'Todos'){
        if (input$mes == 'Todos') {
          tabela <- data %>% 
            filter(ANO_CMPT == input$ano) %>%  
            group_by(`CID-10`, `Descrição CID-10`) %>%
            summarise(Total = n()) %>%
            as.data.frame()
        }
        else{
          tabela <- data %>% 
            filter(ANO_CMPT == input$ano) %>%  
            filter(MES_EXTENSO == input$mes) %>% 
            group_by(`CID-10`, `Descrição CID-10`) %>%
            summarise(Total = n()) %>%
            as.data.frame() 
        }
      } 
      else {
        if (input$mes == 'Todos') {
          tabela <- data %>% 
            filter(ANO_CMPT == input$ano) %>%  
            filter(CIDADE == input$unidade) %>% 
            group_by(`CID-10`, `Descrição CID-10`) %>%
            summarise(Total = n()) %>%
            as.data.frame() 
        }
        else {
          tabela <- data %>% 
            filter(ANO_CMPT == input$ano) %>%  
            filter(MES_EXTENSO == input$mes) %>% 
            filter(CIDADE == input$unidade) %>%  
            group_by(`CID-10`, `Descrição CID-10`) %>%
            summarise(Total = n()) %>%
            as.data.frame()
        }
      }
        
        
        
    })
    
    output$tableCID <- renderDT(tableCID(),
                                filter = 'top',options = list(pageLength = 5))
    
    
    
    
    
    
    
    tablesub <- reactive({ 
        
      if (input$unidade == 'Todos'){
        if (input$mes == 'Todos') {
          tabela <- data %>% 
            filter(ANO_CMPT == input$ano) %>%  
            group_by(`Grupo/Subgrupo`, `Descrição Grupo/Subgrupo`) %>%
            summarise(Total = n(),
                      Valor = sum(VAL_TOT)) %>%
            as.data.frame()
        }
        else{
          tabela <- data %>% 
            filter(ANO_CMPT == input$ano) %>%  
            filter(MES_EXTENSO == input$mes) %>% 
            group_by(`Grupo/Subgrupo`, `Descrição Grupo/Subgrupo`) %>%
            summarise(Total = n(),
                      Valor = sum(VAL_TOT)) %>%
            as.data.frame() 
        }
      } 
      else {
        if (input$mes == 'Todos') {
          tabela <- data %>% 
            filter(ANO_CMPT == input$ano) %>%  
            filter(CIDADE == input$unidade) %>% 
            group_by(`Grupo/Subgrupo`, `Descrição Grupo/Subgrupo`) %>%
            summarise(Total = n(),
                      Valor = sum(VAL_TOT)) %>%
            as.data.frame() 
        }
        else {
          tabela <- data %>% 
            filter(ANO_CMPT == input$ano) %>%  
            filter(MES_EXTENSO == input$mes) %>% 
            filter(CIDADE == input$unidade) %>%  
            group_by(`Grupo/Subgrupo`, `Descrição Grupo/Subgrupo`) %>%
            summarise(Total = n(),
                      Valor = sum(VAL_TOT)) %>%
            as.data.frame()
        }
      }
        
        
        
    })  
    
    
    output$tablesub<- renderDT(tablesub(),
                               filter = 'top',options = list(pageLength = 5))
    
    
    
    output$downloadtableInternacao <- downloadHandler(
        filename = function() {
            paste("Autorizações de Internação por Procedimento Principal - Total e Valor",  ".csv", sep="")
        },
        content = function(file) {
            write.csv(tableInternacao(), file)
        }
    )
    
    output$downloadtableCID <- downloadHandler(
        filename = function() {
            paste("Autorizações de Internação por CID-10",  ".csv", sep="")
        },
        content = function(file) {
            write.csv(tableCID(), file)
        }
    )
    
    output$downloadtablesub <- downloadHandler(
        filename = function() {
            paste("Autorizações de Internação por Grupo/Subgrupo - Total e Valor",  ".csv", sep="")
        },
        content = function(file) {
            write.csv(tablesub(), file)
        }
    )
    
})


    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    







