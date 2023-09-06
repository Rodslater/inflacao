library(sidrar)
library(dplyr)
library(lubridate)

ipca <- get_sidra(x = 1737,
                  variable = 2266,
                  period = 'all')

ipca <- ipca |> 
  select(c(8,5)) |> 
  rename(indice_ipca = Valor ,
         Mês = `Mês (Código)`) |> 
  mutate(Mês = ymd(paste(substr(Mês, 1, 4), substr(Mês, 5, 6), "01", sep = "-")))


saveRDS(ipca, 'data/ipca.rds')
write.csv(ipca, "data/ipca.csv", row.names = FALSE)
