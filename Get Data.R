# Setup
library(httr2)
library(readr)

setwd("PATH")


# IFDATA -----------------------------------------------------------------------

base_url <- "https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/odata/IfDataValores(AnoMes=@AnoMes,TipoInstituicao=@TipoInstituicao,Relatorio=@Relatorio)?"

dates <- seq(as.Date("2012-03-01"), as.Date("2024-12-01"), by = "3 months")

dates <- format(dates, "%Y%m")


for (t in dates) {
  url <- paste(
    base_url,
    "@AnoMes=",
    t,
    "&@TipoInstituicao=1&@Relatorio='T'&$format=text/csv&$select=TipoInstituicao,CodInst,AnoMes,NomeRelatorio,NumeroRelatorio,Grupo,Conta,NomeColuna,DescricaoColuna,Saldo",
    sep = ""
  )

  req <- request(url) %>%
    req_timeout(120) %>%
    req_retry(
      max_tries = 5,
      retry_on_failure = TRUE,
      is_transient = function(resp) {
        resp_status(resp) %in% c(429, 503, 504)
      }
    )

  resp <- req_perform(req, path = paste("IFData", t, ".csv", sep = ""))
}


### Financial Institutions from IFData and their classifications --------------
base_url <- "https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/odata/IfDataCadastro(AnoMes=@AnoMes)?@AnoMes="


dates <- seq(as.Date("2012-03-01"), as.Date("2024-12-01"), by = "3 months")
dates <- format(dates, "%Y%m")

for (t in dates) {
  url <- paste0(
    base_url,
    t,
    "&$format=text/csv&$select=CodInst,Data,NomeInstituicao,Tcb,Td,Tc,SegmentoTb,Atividade,Sr,CnpjInstituicaoLider"
  )

  req <- request(url) %>%
    req_timeout(120) %>%
    req_retry(
      max_tries = 5,
      retry_on_failure = TRUE,
      is_transient = function(resp) {
        resp_status(resp) %in% c(429, 503, 504)
      }
    )

  resp <- req_perform(
    req,
    path = paste("Info_InstFins", t, ".csv", sep = "")
  )
}


### IFDATA (Conglomerados Financeiros e Inst. Ind. para puxar dados de crédito)
base_url <- "https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/odata/IfDataValores(AnoMes=@AnoMes,TipoInstituicao=@TipoInstituicao,Relatorio=@Relatorio)?@AnoMes="


dates <- seq(as.Date("2014-06-01"), as.Date("2024-12-01"), by = "3 months")
dates <- format(dates, "%Y%m")

for (t in dates) {
  url <- paste0(
    base_url,
    t,
    "&@TipoInstituicao=2&@Relatorio='T'&$format=text/csv&$select=TipoInstituicao,CodInst,AnoMes,NomeRelatorio,NumeroRelatorio,Grupo,Conta,NomeColuna,DescricaoColuna,Saldo"
  )

  req <- request(url) %>%
    req_timeout(120) %>%
    req_retry(
      max_tries = 5,
      retry_on_failure = TRUE,
      is_transient = function(resp) {
        resp_status(resp) %in% c(429, 503, 504)
      }
    )

  resp <- req_perform(
    req,
    path = paste("IF_", t, ".csv", sep = "")
  )
}


# Juros (MA 5 dias) ----------------------------------------------------------
url <- "https://olinda.bcb.gov.br/olinda/servico/taxaJuros/versao/v2/odata/TaxasJurosDiariaPorInicioPeriodo?$format=text/csv&$select=InicioPeriodo,FimPeriodo,Segmento,Modalidade,InstituicaoFinanceira,TaxaJurosAoMes,TaxaJurosAoAno,cnpj8"

req <- req <- request(url) %>%
  req_timeout(120) %>%
  req_retry(
    max_tries = 5,
    retry_on_failure = TRUE,
    is_transient = function(resp) {
      resp_status(resp) %in% c(429, 503, 504)
    }
  )

resp <- req_perform(req, path = "Juros_Dia.csv")


# Portabilidade ----------------------------------------------------------------
#' Juntar quantidades e valores de portabilidade baixados individualmente
#' (em bloco) pelo SGS e criar único arquivo com informações de portabilidade

quantidade_portabilidade_1 <- read_delim(
  "quantidade portabilidade 1.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)
quantidade_portabilidade_1$Date <- as.Date(
  paste0("01/", quantidade_portabilidade_1$Date),
  format = "%d/%m/%Y"
)


quantidade_portabilidade_2 <- read_delim(
  "quantidade portabilidade 2.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)
quantidade_portabilidade_2$Date <- as.Date(
  paste0("01/", quantidade_portabilidade_2$Date),
  format = "%d/%m/%Y"
)


valores_portabilidade <- read_delim(
  "valores portabilidade.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)
valores_portabilidade$Date <- as.Date(
  paste0("01/", valores_portabilidade$Date),
  format = "%d/%m/%Y"
)


portabilidade <- merge(
  quantidade_portabilidade_1,
  quantidade_portabilidade_2,
  by = "Date"
)
portabilidade <- merge(portabilidade, valores_portabilidade, by = "Date")

write.csv(
  portabilidade,
  file = "portabilidade.csv",
  row.names = FALSE
)


# MICRODADOS ------------------------------------------------------------

url <- "https://www.dropbox.com/scl/fi/hbl3p9jmg1kbdu893b9rb/Emprestimos_Portados.csv?rlkey=3lill2uq20tpg0j949t8b3sti&e=2&st=j4bc1o5s&dl=1"
# Note o final da url como ?dl=1 ao invés do default ?dl=0 que vem no link
#do dropbox. Com isso o arquivo é baixado diretamente

req <- request(url) %>%
  req_timeout(120) %>%
  req_retry(
    max_tries = 5,
    retry_on_failure = TRUE,
    is_transient = function(resp) {
      resp_status(resp) %in% c(429, 503, 504)
    }
  )

resp <- req_perform(req, path = "Emprestimos_Portados.csv")


# Se quiser ler o .csv diretamente sem baixa-lo numa pasta
df <- read.csv(url)

