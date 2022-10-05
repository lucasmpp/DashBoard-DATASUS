#setwd("C:/Users/601545/Desktop/projetos/datasus/") # Davi
setwd("C:/Users/601546/Documents/DATASUS/")  # Lucas


pacman::p_load("tidyverse","ggplot2","stringr","read.dbc",'magrittr',"lubridate","shinydashboard","plotly","DT","wesanderson","shiny")


source("funcoes.R",encoding = "UTF-8")

sistema <- 'SIHSUS' #Verificar os sistemas disponíveis em http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1
modalidade <- 'Dados'
tipo_arquivo<- 'RD'#Varia conforme o sistema
inicio <- 15 #Dois Ãºltimos dígitos do ano de inicio
fim <- 19 #Dois Ãºltimos dígitos do ano final
estados <- c('CE','DF','MA','BA','MG','RJ','AP','PA') #,'#lista de Estados em que o SARAH estÃ¡ presente
meses <- c('12','11','10','09','08','07','06','05','04','03','02','01') #Meses desejados

#

228+86+143+125+100

#brasilia = 228
#fortaleza = 86
#salvador = 143
#belo = 125
#sao luiz = 100



## PEGANDO UNS DATAFRAMES DE FORA PARA COMPARAR OS CÓDIGOS

procedimentos <- read_delim('AIH - Tabela - Procedimento_novo.csv', delim = '|') %>% 
  select(PROC_REA = Código, Descrição = Procedimento) 


CID <- read_delim('AIH - Tabela - CID-10.csv' , delim = '|') %>%
  select(DIAG_PRINC = 'CID-10', 'Descrição CID-10' = 'Desc. CID-10')


subgrupo <- read.table(file = 'tb_sub_grupo.txt', sep = ';') %>% 
  select('Grupo/Subgrupo' = 'V1' , 'Descrição Grupo/Subgrupo' = 'V2') 
  

#Carrega em df_datasus os dados relativos à base de dados montada a partir dos parÃ¢metros
DF_datasus<- extract_datasus(sistema, modalidade, tipo_arquivo, inicio, fim, meses, estados) 




data <- as.data.frame(do.call(rbind, DF_datasus))

backup <- data
data<- backup

#LUCAS

subgrupo$`Grupo/Subgrupo`<- str_c('0',subgrupo$`Grupo/Subgrupo` )
procedimentos$PROC_REA<- str_c('0',procedimentos$PROC_REA) 
data$PROC_REA <- as.character(data$PROC_REA)

data %<>% mutate(`Grupo/Subgrupo` = str_extract(PROC_REA ,'[:graph:]{4}'))

data <- merge(data, subgrupo, by = 'Grupo/Subgrupo') 
data <- merge(data,procedimentos, by = 'PROC_REA') 
data <- merge(data,CID, by = 'DIAG_PRINC')
data <- data %>% 
  rename('CID-10' = DIAG_PRINC, Código = PROC_REA ) 






## TRATANDO ALGUMAS COLUNAS ####



data <- data %>% 
  mutate(Estadia = estadia(DT_INTER,DT_SAIDA),
         DT_INTER = ymd(DT_INTER),
         DT_SAIDA = ymd(DT_SAIDA),
         DIA_INTER = dia_semana(DT_INTER),
         DIA_SAIDA = dia_semana(DT_SAIDA),
         maior16 = ifelse(IDADE<=16, 'Menor ou igual a 16 anos', 'Maior que 16 anos'),
         `Código` = as.character(`Código`),
         CIDADE = decod_cnes(CNES),
         MES_EXTENSO = mes_extenso(MES_CMPT))








## Banco de Dados pra pegar o número de leitos #####

taxa_ocup <- data%>% 
  group_by(DT_INTER, QT_DIARIAS, CIDADE) %>% 
  summarise(n = n())

dias <- taxa_ocup$DT_INTER
n_dias <- taxa_ocup$QT_DIARIAS
n_pessoas <- taxa_ocup$n
cidade <- taxa_ocup$CIDADE



df_taxa_ocup <- df_dates(dias, n_dias, n_pessoas, cidade)




df_taxa_ocup$LEITOS = dplyr::recode(df_taxa_ocup$cidade,
                                    'Brasília' = 228,
                                    'Fortaleza' = 86,
                                    'São Luís' = 100,
                                    'Belo Horizonte' = 125,
                                    'Salvador' = 143)

data$SEXO %<>% recode( '1' = 'Masculino',
                       '3' = 'Feminino')

data$RACA_COR %<>% recode('01' = 'Branca',
                          '02' = 'Preta',
                          '03' = 'Parda',
                          '04' = 'Amarela',
                          '05' = 'Indígena') 

df_taxa_ocup <- df_taxa_ocup %>% 
  mutate(num_dia_mes = days_in_month(Data),
         ANO = year(Data),
         MES_EXTENSO = mes_extenso(month(Data)))






save(data, file = "data.RData")
save(df_taxa_ocup,file = "df_taxa_ocup.RData")











