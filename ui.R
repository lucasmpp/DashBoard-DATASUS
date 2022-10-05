#setwd("C:/Users/601545/Desktop/projetos/datasus/") # Davi
setwd("C:/Users/601546/Documents/DATASUS/")  # Lucas


pacman::p_load("tidyverse","ggplot2","stringr","read.dbc","lubridate","shinydashboard","plotly","DT","wesanderson")


source('funcoes.R', encoding = 'UTF-8')



ui<-dashboardPage(
  skin = 'blue',
  dashboardHeader(title = tags$a(href='https://www.sarah.br/',imageOutput('logo'))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        selectInput('unidade','Unidade:', 
                    choices = c('Todos','Belo Horizonte','Brasília','Fortaleza','Salvador','São Luís')
        )
      ),
      
      menuItem(
        selectInput("ano","Ano:",choices = c(2014,2015, 2016, 2017,2018, 2019), selected = 2015
        )
      ),
      
      menuItem(
        selectInput("mes","Mês:", choices = c( "Todos", "Janeiro","Fevereiro","Março","Abril","Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro"),
                    selected = "Todos"
        )
      )
      
    )
  ),
  
  dashboardBody(
    
    fluidRow(
      box(title = "Unidade", solidHeader = TRUE, 
          status = "primary", width = 1,
          textOutput("unidade", container = span)),
      
      box(title = "Ano", solidHeader = TRUE, 
          status = "primary", width = 1,
          textOutput('ano', container = span)),
      
      box(title = "Mês", solidHeader = TRUE, 
          status = "primary", width = 1,
          textOutput('mes', container = span)),
      infoBoxOutput('total',width = 3),
      infoBoxOutput('taxa_ocupacao',width = 3),
      infoBoxOutput('valortotal',width = 3),
    ),
    
    
    
    fluidRow(
      box(title = 'AIH´s por Nacionalidade',solidHeader = TRUE, status = "primary",
          plotlyOutput('nacional'), width = 3, height = 330),
      box(title = 'AIH´s por Sexo',solidHeader = TRUE, status = "primary",
          plotlyOutput('sexo'), width = 3, height = 330),
      box(title = paste0('Total AIH´s por mês '),solidHeader = TRUE, status = "primary",
          plotlyOutput('linha'),width = 6, height = 330),
      
    ),
    
    fluidRow(
      box(title = 'AIH´s por Idade',solidHeader = TRUE, status = "primary",
          plotlyOutput('idade'), width = 3, height = 547),
      box(title = 'AIH´s por Raça',solidHeader = TRUE, status = "primary",
          plotlyOutput('raca'), width = 3, height = 547),
      box(title = 'Autorizações de Internação por Procedimento Principal - Total e Valor',solidHeader = TRUE, status = "primary",
          DTOutput('tableInternacao'),downloadButton("downloadtableInternacao", "Download"), width = 6)
      
    ),         
    
    fluidRow(
      box(title = 'Autorizações de Internação por Grupo/Subgrupo - Total e Valor',solidHeader = TRUE, status = "primary",
          DTOutput('tablesub'),downloadButton("downloadtablesub", width = 6)),
      box(title = 'Autorizações de Internação por CID-10 - Total',solidHeader = TRUE, status = "primary",
          DTOutput('tableCID'),downloadButton("downloadtableCID", "Download"), width = 6))
    
    
      
           
  
  )
) 
