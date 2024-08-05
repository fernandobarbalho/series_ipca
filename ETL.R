library(ipeadatar)
library(tidyverse)
library(readxl)

series_disponiveis<-
  ipeadatar::available_series()


codigos<-
  ((series_disponiveis %>%
      filter(str_detect(code, "^PRECOS12_IPCA[a-zA-Z]" )) %>%
      select(code)))$code

variacao<-
  ipeadatar::ipeadata(codigos)

variacao_trabalho<-
  variacao %>%
  filter(!code %in% c("PRECOS12_IPCASP12"),
         date >= "1995-02-01") %>%
  inner_join(series_disponiveis)

writexl::write_xlsx(variacao_trabalho, "series_ipca.xlsx")

variacao_trabalho$variacao_acumulada <- 100



series_ipca_numero_indice <- read_excel("series_ipca_numero_indice.xlsx")

saveRDS(series_ipca_numero_indice, "series_ipca_numero_indice.rds")


series_ipca_numero_indice<-
purrr::map_dfr(unique(variacao_trabalho$code),function(codigo){
  print(codigo)
  
  IPCA_historico<- 
    variacao_trabalho %>%
    filter(code == codigo)
  
  num_indice_aux<-
  map_dbl(1:NROW(IPCA_historico), function(a_i){
    
    print(a_i)
    prod(1+IPCA_historico$value[1:a_i]/100)
    
  })
  
  
  #IPCA_historico$num_indice_aux <- num_indice_aux
  
  IPCA_historico$numero_indice =  100 * num_indice_aux#IPCA_historico$num_indice_aux
    
  IPCA_historico
})

saveRDS(series_ipca_numero_indice, "series_ipca_numero_indice.rds")
