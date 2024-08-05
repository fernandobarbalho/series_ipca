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

fab<-
  variacao_trabalho %>%
  group_by(code) %>%
  mutate(lag_value = lag(value, n=1)) %>%
  mutate(variacao_acumulada =  ifelse(is.na(lag_value), 100 * (1+value/100), lag(variacao_acumulada, n=1) * (1+value/100)))



series_ipca_numero_indice <- read_excel("series_ipca_numero_indice.xlsx")

saveRDS(series_ipca_numero_indice, "series_ipca_numero_indice.rds")
