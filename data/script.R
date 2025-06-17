library(sidrar)
library(dplyr)
library(lubridate)
library(jsonlite)
library(httr)

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

periodos <- list(
  c(202001, 202312),
  c(202401, 202412),
  c(202501, data_atual)
)

lista_dados <- list()

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

ipca_grupos <- do.call(rbind, lista_dados)
rownames(ipca_grupos) <- NULL
ipca_grupos <- ipca_grupos |> 
  filter(!is.na(Variacao))

ipca_indice <- ipca |> 
  rename(Data = Mês)

write_json(ipca_full, "data/ipca_full.json", pretty = TRUE, auto_unbox = TRUE)
write_json(ipca_grupos, "data/ipca_grupos.json", pretty = TRUE, auto_unbox = TRUE)
write_json(ipca_indice, "data/ipca_indice.json", pretty = TRUE, auto_unbox = TRUE)

webhook_url <- "https://n8n.rodslater.com/webhook/inflacao"

payload <- list(
  status = "success",
  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  message = "Dados IPCA atualizados com sucesso",
  files_updated = c("ipca_full.json", "ipca_grupos.json", "ipca_indice.json", "ipca.csv", "ipca.rds"),
  last_data_date = max(ipca_full$Data, na.rm = TRUE)
)

tryCatch({
  cat("Ativando webhook n8n...\n")
  
  response <- POST(
    url = webhook_url,
    body = payload,
    encode = "json",
    add_headers("Content-Type" = "application/json"),
    timeout(30)
  )
  
  if (status_code(response) == 200) {
    cat("✅ Webhook n8n ativado com sucesso!\n")
    cat("Response:", content(response, "text"), "\n")
  } else {
    cat("❌ Erro ao ativar webhook. Status code:", status_code(response), "\n")
    cat("Response:", content(response, "text"), "\n")
  }
  
}, error = function(e) {
  cat("❌ Erro ao chamar webhook:", e$message, "\n")
  
  tryCatch({
    cat("Tentando webhook simples (GET)...\n")
    response_get <- GET(webhook_url, timeout(30))
    if (status_code(response_get) == 200) {
      cat("✅ Webhook ativado com GET!\n")
    }
  }, error = function(e2) {
    cat("❌ Falha total ao ativar webhook:", e2$message, "\n")
  })
})

cat("Processamento concluído!\n")
