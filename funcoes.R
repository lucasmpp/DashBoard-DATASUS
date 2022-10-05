#setwd("C:/Users/601545/Desktop/projetos/datasus/") # Davi
setwd("C:/Users/601546/Documents/DATASUS/")  # Lucas

if(!require(pacman)) install.packages('pacman')
pacman::p_load("tidyverse","ggplot2","stringr","read.dbc")

## extrair o banco de dados do site ####
extract_datasus <- function(sistema, modalidade, tipo_arquivo, inicio, fim, meses, estados){
  
  
  #FunÃ§Ã£o gera dataframe a partir de ftp feita na pÃ¡gina do datasus
  #FUNC GERA DATAFRAME A PARTIR DE FTP FEITA NA PAG DO DATASUS
  #sistema ex:'SIHSUS' Verificar os sistemas DISPONIVEIS em http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1
  #modalidade  'dados'
  #tipo_arquivo ex: 'RD'#Varia conforme o sistema
  #inicio ex: 17 Dois últimos dígitos do  ano de início
  #final  ex: 19 Dois últimos dígitos do  ano final
  
  
  
  
  dest_file<- as.vector(sapply(estados,function(UF){      # define o nome dos arquivos que serÃ£o salvos
    
    
    
    sapply(fim:inicio, function(ano){
      sapply(meses,function(mes)
        str_c("bancos/",tipo_arquivo,UF,ano,mes,".dbc")
        
        
        
      )})}))
  
  
  
  str_download <- as.vector(sapply(estados,function(UF){  # Cria um vetor com os links dos arquivos para download
    sapply(fim:inicio, function(ano){
      sapply(meses,function(mes){
        
        
        str_c("ftp://ftp.datasus.gov.br/dissemin/publicos/",sistema,"/","200801","_/",modalidade,"/",tipo_arquivo,UF,ano,mes,".dbc")}
      )})}))
  
  
  # Fazer os downloads e salvar com os nomes ja definidos
  sapply(1:length(str_download), function(x) {
    
    #VERIFICA SE O ARQUIVO EXISTE DENTRO DA PASTA
    if(!file.exists(dest_file[x])){
      download.file(str_download[x],destfile = dest_file[x], mode='wb')
    }
  })
  
  
  datasus<-  lapply(dest_file, function(banco){
    read.dbc(banco) %>% 
      as.data.frame %>% 
      filter(CNES %in% c("2673916","237391","3004791","2673916","3787907","5660149","2497751","2307006")) %>% 
      select(-UTI_MES_IN, -UTI_MES_AN,-UTI_MES_AL, -UTI_INT_IN,-UTI_INT_AN,-UTI_INT_AL,
             -VAL_SADT, -VAL_ACOMP, -VAL_RN,-VAL_ACOMP, -VAL_ORTP, -VAL_SANGUE, -VAL_SADTSR, -VAL_SH_FED,-VAL_SP_FED,
             -VAL_SH_GES, -VAL_SP_GES, RUBRICA,
             -VAL_TRANSP, -VAL_OBSANG, -VAL_PED1AC, -TOT_PT_SP, -CPF_AUT)
    
  })
  
  
}




## PERIODO DE ESTADIA ####

estadia <- function(internacao,saida){
  
  
  #TRANSFORMA OS VETORES NO TIPO DATA
  internacao <- ymd(internacao)
  saida <- ymd(saida)
  

  periodo <- saida - internacao
  
  #RETORNA COMO NUMERICO
  return(as.numeric(periodo))
  
  
}


#TRANSFORMA OS DIAS EM EXTENSO

dia_semana <- function(dia){
  
  dia <- wday(ymd(dia),label = T)
  
  dia <- str_replace_all(dia,"dom","Domingo")
  dia <- str_replace_all(dia,"seg","Segunda")
  dia <- str_replace_all(dia,"ter","Terça")
  dia <- str_replace_all(dia,"qua","Quarta")
  dia <- str_replace_all(dia,"qui","Quinta")
  dia <- str_replace_all(dia,"sex","Sexta")
  dia <- str_replace_all(dia,"sáb","Sábado")
  
  dia <- factor(dia, levels = c("Domingo","Segunda","Terça","Quarta","Quinta","Sexta","Sábado"), ordered = T)
  
  return(dia)
  
}




mes_extenso <- function(mes){
  
  
  mes <- month(mes, label = T)
  
  mes <- str_replace_all(mes, "jan","Janeiro")
  mes <- str_replace_all(mes, "fev","Fevereiro")
  mes <- str_replace_all(mes, "mar","Março")
  mes <- str_replace_all(mes, "abr","Abril")
  mes <- str_replace_all(mes, "mai","Maio")
  mes <- str_replace_all(mes, "jun","Junho")
  mes <- str_replace_all(mes, "jul","Julho")
  mes <- str_replace_all(mes, "ago","Agosto")
  mes <- str_replace_all(mes, "set","Setembro")
  mes <- str_replace_all(mes, "out","Outubro")
  mes <- str_replace_all(mes, "nov","Novembro")
  mes <- str_replace_all(mes, "dez","Dezembro")
  
  mes <- factor(mes, levels = c("Janeiro","Fevereiro","Março","Abril","Maio","Junho","Julho","Agosto",
                                "Setembro","Outubro","Novembro","Dezembro"),ordered = T)
  
  return(mes)
  
}



#decodifica os CNES 
decod_cnes <- function(cnes){
  
  sapply(cnes, function(cnes){
    if(cnes == "2673916"){return("Brasília")}
    
    if(cnes == "2373971"){return("Fortaleza")}
    
    if(cnes == "3004791"){return("Belo Horizonte")}
    
    if(cnes == "3787907"){return("Macapá")}
    
    if(cnes == "5660149"){return("Belém")}
    
    if(cnes == "2497751"){return("Salvador")}
    
    if(cnes == "2307006"){return("São Luís")}
    
    else(return(""))
    
  })
}





















