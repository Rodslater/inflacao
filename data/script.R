library(sidrar)
library(dplyr)
library(lubridate)
library(jsonlite)

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



#############################################
data_atual <- format(Sys.Date(), "%Y%m")

ipca_full <- sidrar::get_sidra(
  api = paste0("/t/1737/n1/all/v/63,2265/p/all")
) %>%
  mutate(Data = as.Date(paste0(`Mês (Código)`, "01"), format = "%Y%m%d"),
         Valor = as.numeric(Valor),
         Tipo_Variacao = case_when(
           `Variável (Código)` == "63" ~ "Mensal",
           `Variável (Código)` == "2265" ~ "Acumulado 12 meses"
         )) %>%
  select(Data, Variacao = Valor, Tipo_Variacao) %>%
  filter(!is.na(Variacao))


## Série grupos
# Definir os períodos de interesse
periodos <- list(
  c(202001, 202312), # 2020 a 2023
  c(202401, 202412), # 2024
  c(202501, data_atual) #2025 - atual
  
)

# Criar uma lista para armazenar os dados
lista_dados <- list()

# Loop para coletar os dados de cada período
for (p in periodos) {
  inicio <- p[1]
  fim <- p[2]
  dados <- sidrar::get_sidra(
    api = paste0("/t/7060/n1/all/v/63,2265/p/", inicio, "-", fim, "/c315/all")
  ) %>%
    mutate(Data = as.Date(paste0(`Mês (Código)`, "01"), format = "%Y%m%d"),
           Valor = as.numeric(Valor),
           Tipo_Variacao = case_when(
             `Variável (Código)` == "63" ~ "Mensal",
             `Variável (Código)` == "2265" ~ "Acumulado 12 meses"
           )) %>%
    select(Data, Grupo = `Geral, grupo, subgrupo, item e subitem`, Variacao = Valor, Tipo_Variacao)
  lista_dados[[paste0(inicio, "-", fim)]] <- dados
}

# Combinar todos os dados em um único dataframe
ipca_grupos <- do.call(rbind, lista_dados)
rownames(ipca_grupos) <- NULL

ipca_grupos <- ipca_grupos |> 
  filter(!is.na(Variacao))


ipca_indice <- ipca |> 
  rename(Data = Mês)

write_json(ipca_full, "data/ipca_full.json", pretty = TRUE, auto_unbox = TRUE)
write_json(ipca_grupos, "data/ipca_grupos.json", pretty = TRUE, auto_unbox = TRUE)
write_json(ipca_indice, "data/ipca_indice.json", pretty = TRUE, auto_unbox = TRUE)
#############################################
