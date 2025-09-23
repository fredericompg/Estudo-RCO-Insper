# Estimações com dados gerais do BCB

# Setup -----------------------------------------------------------------------
rm(list = ls())

library(lubridate)
library(tidyverse)
library(readxl)
library(readr)
library(gridExtra)
library(fixest)
library(stargazer)

setwd("PATH")


# Parte 1
dates <- seq(as.Date("2012-01-01"), as.Date("2018-12-01"), by = "1 month")
dates <- format(dates, "%Y%m")

for (i in dates) {
  df <- read_delim(
    paste0("SCR/planilha_", i, ".csv"),
    delim = ";",
    escape_double = FALSE,
    col_types = cols(carteira_ativa = col_character()),
    trim_ws = TRUE
  )

  # Manter variáveis de interesse
  df <- df %>%
    select(
      data_base,
      cliente,
      modalidade,
      indexador,
      tcb,
      sr,
      origem,
      indexador,
      numero_de_operacoes,
      carteira_ativa,
      carteira_inadimplida_arrastada,
      ativo_problematico
    )

  # Colocar como NA as linhas de crédito que não temos informação detalhada do numero de operações
  df <- df %>%
    mutate(
      numero_de_operacoes = ifelse(
        numero_de_operacoes == "<= 15",
        0,
        numero_de_operacoes
      )
    )

  #transformar saldos em variáveis numéricas
  df <- df %>%
    mutate(
      carteira_ativa = as.numeric(gsub(",", ".", carteira_ativa)),
      carteira_inadimplida_arrastada = as.numeric(gsub(
        ",",
        ".",
        carteira_inadimplida_arrastada
      )),
      ativo_problematico = as.numeric(gsub(",", ".", ativo_problematico)),
      numero_de_operacoes = as.numeric(gsub(",", ".", numero_de_operacoes))
    )

  df <- df %>%
    filter(
      modalidade %in%
        c(
          "PF - Empréstimo com consignação em folha",
          "PF - Empréstimo sem consignação em folha",
          "PF - Habitacional",
          "PF - Veículos",
          "PJ - Capital de giro",
          "PJ - Cheque especial e conta garantida",
          "PF - Outros créditos"
        )
    ) %>%
    group_by(data_base, modalidade, ) %>%
    summarise(
      carteira_ativa = sum(carteira_ativa, na.rm = TRUE),
      carteira_inadimplida_arrastada = sum(
        carteira_inadimplida_arrastada,
        na.rm = TRUE
      ),
      ativo_problematico = sum(ativo_problematico, na.rm = TRUE),
      numero_de_operacoes = sum(numero_de_operacoes, na.rm = TRUE)
    )

  assign(paste0("SCR", i), df)
}

dfs <- mget(paste0("SCR", dates))
df <- do.call(rbind, dfs)

write.csv(df, "SCR/scr1.csv")


rm(list = ls())
gc()


# Parte 2
dates <- seq(as.Date("2019-01-01"), as.Date("2024-12-01"), by = "1 month")
dates <- format(dates, "%Y%m")

for (i in dates) {
  df <- read_delim(
    paste0("SCR/planilha_", i, ".csv"),
    delim = ";",
    escape_double = FALSE,
    col_types = cols(carteira_ativa = col_character()),
    trim_ws = TRUE
  )

  # Manter variáveis de interesse
  df <- df %>%
    select(
      data_base,
      cliente,
      modalidade,
      indexador,
      tcb,
      sr,
      origem,
      indexador,
      numero_de_operacoes,
      carteira_ativa,
      carteira_inadimplida_arrastada,
      ativo_problematico
    )

  # Colocar como NA as linhas de crédito que não temos informação detalhada do numero de operações
  df <- df %>%
    mutate(
      numero_de_operacoes = ifelse(
        numero_de_operacoes == "<= 15",
        NA,
        numero_de_operacoes
      )
    )

  #transformar saldos em variáveis numéricas
  df <- df %>%
    mutate(
      carteira_ativa = as.numeric(gsub(",", ".", carteira_ativa)),
      carteira_inadimplida_arrastada = as.numeric(gsub(
        ",",
        ".",
        carteira_inadimplida_arrastada
      )),
      ativo_problematico = as.numeric(gsub(",", ".", ativo_problematico)),
      numero_de_operacoes = as.numeric(gsub(",", ".", numero_de_operacoes))
    )

  df <- df %>%
    filter(
      modalidade %in%
        c(
          "PF - Empréstimo com consignação em folha",
          "PF - Empréstimo sem consignação em folha",
          "PF - Habitacional",
          "PF - Veículos",
          "PJ - Capital de giro",
          "PJ - Cheque especial e conta garantida",
          "PF - Outros créditos"
        )
    ) %>%
    group_by(data_base, modalidade, ) %>%
    summarise(
      carteira_ativa = sum(carteira_ativa, na.rm = TRUE),
      carteira_inadimplida_arrastada = sum(
        carteira_inadimplida_arrastada,
        na.rm = TRUE
      ),
      ativo_problematico = sum(ativo_problematico, na.rm = TRUE),
      numero_de_operacoes = sum(numero_de_operacoes, na.rm = TRUE)
    )

  assign(paste0("SCR", i), df)
}
dfs <- mget(paste0("SCR", dates))
df <- do.call(rbind, dfs)

write.csv(df, "SCR/scr2.csv")

rm(list = ls())
gc()


# Juntando ambas
df1 <- read_csv("SCR/scr1.csv")
df2 <- read_csv("SCR/scr2.csv")

df <- rbind(df1, df2)

rm(df1, df2)
gc()

df$data_base <- floor_date(df$data_base, unit = "month")

df$...1 <- NULL

#Salvando versão final
write.csv(df, "SCR/SCR_clean.csv")


#Computando novas operações de crédito através da carteira ativa
df <- df %>%
  arrange(data_base, modalidade) %>%
  group_by(modalidade) %>%
  mutate(novo_credito = carteira_ativa - lag(carteira_ativa)) %>%
  ungroup()


# IFDATA -----------------------------------------------------------------------
# Dados que iremos trabalhar para análise de competição (como HHI)
dates <- seq(as.Date("2012-03-01"), as.Date("2024-12-01"), by = "3 months")
dates <- format(dates, "%Y%m")

for (i in dates) {
  df <- read_csv(paste0("IFDATA/CongFin e Inst Ind/IFData", i, ".csv"))

  # Keeping only variables to calculate HHI based on

  df <- df %>%
    filter(
      Conta == 78182 | # Ativos Totais
        Conta == 78183 | # Carteira de Crédito Classificado
        Conta == 78193 | # Operações de Crédito
        Conta == 78209 | # Despesas de captação
        # Abaixo são contas de carteira de crédito (diferentes modalidades)
        # e só contidas nos dados de 2014/06 em diante pela API
        ###  Para PF
        Conta %in% c(23259, 23243, 23267, 23275, 23227, 23235, 23251) |
        ### Para PJ
        Conta %in%
          c(23283, 23299, 23291, 23307, 23339, 23331, 23323, 23347, 23315)
    )
  assign(paste0("IFDATA", i), df)
}
rm(i)

#Append dataframes
dfs <- mget(paste0("IFDATA", dates))
ifdata <- do.call(rbind, dfs)

rm(dfs)

rm(list = ls(pattern = "IFDATA")) # Remover todos os dataframes com IFDATA no nome


# Trocando decimal por . e tornando saldos numéricos
ifdata$Saldo <- gsub(",", ".", ifdata$Saldo)
ifdata$Saldo <- as.numeric(ifdata$Saldo)


write.csv(ifdata, "IFDATA/IFDATA_clean.csv", row.names = FALSE)


### Classificação Inst Fin -----------------------------------------------------
dates <- seq(as.Date("2012-03-01"), as.Date("2024-12-01"), by = "3 months")
dates <- format(dates, "%Y%m")

for (t in dates) {
  df <- read_csv(paste0("IFDATA/Info_InstFins", t, ".csv"))

  df <- df %>% mutate(date = t)

  assign(paste0("Info_InstFins", t), df)
}

#Append dataframes
dfs <- mget(paste0("Info_InstFins", dates))
info_ifdata <- do.call(rbind, dfs)

rm(dfs, df, req, resp)
rm(df)

rm(list = ls(pattern = "Info_InstFins")) # Remove all IFDATA dataframes

write.csv(
  info_ifdata,
  "IFDATA/INFO Instituições Financeiras.csv",
  row.names = FALSE
)


# Juntando IFDATA e informações das instituições
ifdata <- read_csv("IFDATA/IFDATA_clean.csv")

info_ifdata <- read_csv("IFDATA/INFO Instituições Financeiras.csv")

info_ifdata <- rename(info_ifdata, AnoMes = date)

ifdata <- merge(
  ifdata,
  info_ifdata,
  by = c("CodInst", "AnoMes"),
  all.x = TRUE,
  no.dups = FALSE
)

rm(info_ifdata)


#Sr só começa em 2017 então para antes disso eu uso o primeiro valor definido
aux <- ifdata %>% select(Sr, AnoMes, CodInst) %>% filter(AnoMes == 201803)

ifdata <- ifdata %>% mutate(Sr = ifelse(AnoMes < 201803, NA, Sr))


ifdata <- ifdata %>%
  group_by(CodInst) %>%
  arrange(AnoMes, .by_group = TRUE) %>%
  mutate(first_value = first(na.omit(Sr))) %>%
  mutate(Sr = ifelse(is.na(Sr), first_value, Sr)) %>%
  ungroup() %>%
  select(-first_value)

ifdata <- ifdata %>% mutate(Sr = ifelse(Sr == "null", "Ind.", Sr))


#Salvando versão final do IFDATA
write.csv(ifdata, "IFDATA/IFDATA_FINAL.csv", row.names = FALSE)


# Reimportando dados "limpos" do IFDATA
ifdata <- read_csv("IFDATA/IFDATA_FINAL.csv")


# PORTABILIDADE ----------------------------------------------------------------
portabil <- read_csv(
  "portabilidade.csv",
  col_types = cols(
    Date = col_date(format = "%Y-%m-%d"), # first column stays character
    .default = col_number() # everything else parsed as double
  )
)


# Renomeando colunas
names(portabil) <- c(
  "Date",
  "payroll_Total",
  "payroll_Granted",
  "non_payroll_Total",
  "non_payroll_granted",
  "home_total",
  "home_granted",
  "others_total",
  "others_granted",
  "vehicle_total",
  "vehicle_granted",
  "other_financing_total",
  "other_financing_granted",
  "real_estate_SFH_total",
  "real_estate_SFH_granted",
  "real_estate_SFI_total",
  "real_estate_SFI_granted",
  "real_estate_enterprise_total",
  "real_estate_enterprise_granted",
  "all_credit_total",
  "all_credit_granted",
  "payroll_RS",
  "non_payroll_RS",
  "home_RS",
  "others_RS",
  "vehicle_RS",
  "other_financing_RS",
  "real_estate_SFH_RS",
  "real_estate_SFI_RS",
  "real_estate_enterprise_RS",
  "all_credit_RS"
)


# CONCESSÕES DE CRÉDITO  -------------------------------------------------------
concessoes <- read_delim(
  "concessoes credito.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(
    Date = col_date(format = "%m/%Y"),
    `20633 - New operations - Households - Total - R$ (million)` = col_number(),
    `20642 - Nonearmarked new operations - Non-financial corporations - Working capital total - R$ (million)` = col_number(),
    `20666 - Nonearmarked new operations - Households - Personal credit - R$ (million)` = col_number(),
    `20671 - Nonearmarked new operations - Households - Payroll-deducted personal loans - Total - R$ (million)` = col_number(),
    `20673 - Nonearmarked new operations - Households - Vehicles financing - R$ (million)` = col_number()
  ),
  trim_ws = TRUE
)

names(concessoes) <- c(
  "Date",
  "total_PF",
  "capital_giro",
  "nao_consig_PF",
  "consig_PF",
  "veiculos_PF"
)

#' VALORES ESTÃO EM MILHÕES DE REAIS E SE REFEREM EXCLUSIVAMENTE
#' A NOVAS OPERAÇÕES FEITAS EM CADA MES PARA CADA MODALIDADE DE CRÉDITO

# Merge de portabilidade e concessoes
portabil <- merge(portabil, concessoes, by = "Date", all.x = TRUE)


# VALORES RCO ------------------------------------------------------------------

rco <- read_delim(
  "Saldo Transferencias RCO.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(Date = col_date(format = "%m/%Y")),
  trim_ws = TRUE
)

names(rco) <- c(
  "date",
  "payroll_rco",
  "nonpayroll_rco",
  "home_rco",
  "others_rco",
  "vehicle_rco",
  "other_financing_rco",
  "real_estate_SFH_rco",
  "real_estate_SFI_rco",
  "real_estate_enterprise_rco",
  "all_credit_rco"
)


# Transformar variáveis em numéricas
rco <- rco %>% mutate(across(-date, ~ as.numeric(gsub(",", "", .))))


# ICC --------------------------------------------------------------------------
ICC <- read_delim(
  "ICC.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(
    Date = col_date(format = "%m/%Y"),
    `25355 - Average cost of outstanding loans - ICC - Nonearmarked - Non-financial corporations - Total - % p.y.` = col_number(),
    `25356 - Average cost of outstanding loans - ICC - Nonearmarked - Individuals - Total - % p.y.` = col_number(),
    `27673 - Average cost of outstanding loans - Nonearmarked - Households - Personal Credit - % p.y.` = col_number(),
    `27675 - Average cost of outstanding loans - Nonearmarked - Households - Payroll-deducted personal loans - Private sector employees - % p.y.` = col_number(),
    `27676 - Average cost of outstanding loans - Nonearmarked - Households - Payroll-deducted personal loans - Public sector employees - % p.y.` = col_number(),
    `27677 - Average cost of outstanding loans - Nonearmarked - Households - Payroll-deducted personal loans - retirees and pensioners - % p.y.` = col_number(),
    `27678 - Average cost of outstanding loans - Nonearmarked - Households - Payroll-deducted personal loans - total - % p.y.` = col_number(),
    `27680 - Average cost of outstanding loans - Nonearmarked - Households - Vehicles financing - % p.y.` = col_number(),
    `27715 - Average cost of outstanding loans - Earmarked - Households - Real estate financing - Total - % p.y.` = col_number()
  ),
  trim_ws = TRUE
)

names(ICC) <- c(
  "date",
  "icc_PJ",
  "icc_PF",
  "icc_personalcredit",
  "icc_private_payroll",
  "icc_public_payroll",
  "icc_inss_payroll",
  "icc_payroll_total",
  "icc_vehicles",
  "icc_realestate"
)


icc_decomp <- read_excel(
  "ICC decomposicao.xlsx",
  col_types = c("numeric", "numeric", "numeric", "numeric")
)

names(icc_decomp) <- c("date", "icc_med", "custo_captacao", "desp_adm")


# Calculando custo de captação e despesa administrativa aproximada pelo ICC anual
# relatado no REB
icc_decomp <- icc_decomp %>%
  mutate(
    percent_captacao = custo_captacao / icc_med,
    percent_adm = desp_adm / icc_med
  )
icc_decomp$date <- paste0(as.character(icc_decomp$date), "-01", "-01")
icc_decomp$date <- as.Date(icc_decomp$date, format = "%Y-%m-%d")


# Merge ICC e ICC decomp
ICC$year <- floor_date(ICC$date, unit = "year")


ICC <- merge(ICC, icc_decomp, by.x = "year", by.y = "date", all.x = TRUE)

ICC <- ICC %>%
  filter(date >= as.Date("2015-01-01", format = "%Y-%m-%d")) %>%
  select(-year) # Remove coluna de ano)

ICC <- ICC %>% mutate(custo_adm_PF = icc_PF * percent_adm)

ICC <- ICC %>% arrange(date)


# JUROS ------------------------------------------------------------------------
### Juros (MA 5 dias) ----------------------------------------------------------
# Semanais
juros_dia <- read_csv(
  "Juros_Dia.csv",
  col_types = cols(
    InicioPeriodo = col_date(format = "%Y-%m-%d"),
    FimPeriodo = col_date(format = "%Y-%m-%d"),
    cnpj8 = col_number()
  )
)

juros_dia$TaxaJurosAoMes <- as.numeric(gsub(",", ".", juros_dia$TaxaJurosAoMes))
juros_dia$TaxaJurosAoAno <- as.numeric(gsub(",", ".", juros_dia$TaxaJurosAoAno))

names(juros_dia) <- c(
  "inicio",
  "fim",
  "segmento",
  "modalidade",
  "InstFin",
  "juros_mes",
  "juros_ano",
  "cnpj8"
)

#Separando modalidade para ter produto e indicador de fixação como duas
# variáveis diferentes

juros_dia <- juros_dia %>%
  separate(
    modalidade,
    into = c("modalidade", "index"),
    sep = "-",
    remove = FALSE,
    extra = "merge"
  )

juros_dia <- juros_dia %>%
  mutate(
    segmento = ifelse(
      segmento == "PESSOA JURÍDICA",
      "PJ",
      ifelse(segmento == "PESSOA FÍSICA", "PF", "Outros")
    )
  )

juros_dia <- juros_dia %>%
  mutate(
    modalidade = ifelse(
      modalidade == "Adiantamento sobre contratos de câmbio (ACC) ",
      "ACC",
      ifelse(
        modalidade == "Aquisição de outros bens ",
        "Outros bens",
        ifelse(
          modalidade == "Antecipação de faturas de cartão de crédito ",
          "Antecipação cartão de crédito",
          ifelse(
            modalidade == "Aquisição de veículos ",
            "Veículos",
            ifelse(
              modalidade == "Capital de giro com prazo até 365 dias ",
              "CG < 365",
              ifelse(
                modalidade == "Capital de giro com prazo superior a 365 dias ",
                "CG > 365",
                ifelse(
                  modalidade == "Cartão de crédito ",
                  "Cartão de crédito",
                  ifelse(
                    modalidade == "Cheque especial ",
                    "Cheque especial",
                    ifelse(
                      modalidade == "Conta garantida ",
                      "Conta garantida",
                      ifelse(
                        modalidade == "Crédito pessoal consignado INSS ",
                        "Consignado INSS",
                        ifelse(
                          modalidade == "Crédito pessoal consignado privado ",
                          "Consignado privado",
                          ifelse(
                            modalidade == "Crédito pessoal consignado público ",
                            "Consignado público",
                            ifelse(
                              modalidade == "Crédito pessoal não",
                              "Crédito pessoal",
                              ifelse(
                                modalidade == "Desconto de cheques ",
                                "Desconto cheque",
                                ifelse(
                                  modalidade == "Desconto de duplicatas ",
                                  "Desconto duplicata",
                                  ifelse(
                                    modalidade == "Vendor ",
                                    "Fornecedor",
                                    ifelse(
                                      modalidade ==
                                        "Arrendamento mercantil de veículos ",
                                      "Arrendamento veiculo",
                                      modalidade
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )


#Consertando cartão de crédito
juros_dia <- juros_dia %>%
  mutate(
    modalidade = ifelse(
      modalidade == "Cartão de crédito",
      ifelse(
        index == " parcelado - Pré-fixado",
        "Cartão de crédito parcelado",
        ifelse(
          index == " rotativo total - Pré-fixado",
          "Cartão de crédito rotativo",
          ifelse(
            index == "consignado - Pré-fixado",
            "Cartão de crédito consignado",
            modalidade
          )
        )
      ),
      modalidade
    ),
    index = ifelse(
      index == " parcelado - Pré-fixado" |
        index == " rotativo total - Pré-fixado" |
        index == "consignado - Pré-fixado",
      " Pré-fixado",
      index
    )
  )

juros_dia <- juros_dia %>%
  mutate(modalidade = trimws(modalidade), index = trimws(index))


write.csv(juros_dia, "Juro Media 5 dias.csv", row.names = FALSE)


# Importanto dados "limpos" para uso futuro
juros_dia <- read_csv(
  "Juro Media 5 dias.csv",
  col_types = cols(
    inicio = col_date(format = "%Y-%m-%d"),
    fim = col_date(format = "%Y-%m-%d")
  )
)


# Custo de captação e selic para computar spreads

### Custo de captação
custo_captacao <- read_delim(
  "custo captacao.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(
    Date = col_date(format = "%m/%Y"),
    `10735 - Inflow medium cost - Central Bank securities, interest earning reserve requirements - % p.y.` = col_number(),
    `10736 - Inflow medium cost - Central Bank securities, extramarket and profit earning compulsory deposits (excludes demand deposits) - % p.y.` = col_number()
  ),
  trim_ws = TRUE
)

names(custo_captacao) <- c("date", "custo_CB", "custo_extramarket")

custo_captacao <- custo_captacao %>%
  filter(date >= as.Date("2012-01-01", format = "%Y-%m-%d"))


### Selic efetiva
selic <- read_delim(
  "selic mensal.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(
    Date = col_date(format = "%m/%Y"),
    `4390 - Interest rate - Selic accumulated in the month - % p.m.` = col_number()
  ),
  trim_ws = TRUE
)
names(selic) <- c("date", "selic_mes")

selic <- selic %>% filter(date >= as.Date("2012-01-01", format = "%Y-%m-%d"))


#Compatibilizando taxas para ser ao ano
selic <- selic %>% mutate(selic_ano = 100 * ((1 + selic_mes / 100)^12 - 1))


# Juntando custos e computando spreads
juros_dia <- juros_dia %>% mutate(mes = floor_date(fim, unit = "month"))

juros_dia <- merge(
  juros_dia,
  custo_captacao,
  by.x = "mes",
  by.y = "date",
  all.x = TRUE
)

juros_dia <- merge(juros_dia, selic, by.x = "mes", by.y = "date", all.x = TRUE)

juros_dia <- juros_dia %>%
  mutate(
    spread_cc1 = juros_ano - custo_CB,
    spread_cc2 = juros_ano - custo_extramarket,
    spread_selic = juros_ano - selic_mes
  )

write.csv(juros_dia, "Juro FINAL.csv", row.names = FALSE)


# Para importar rapidamente juros se precisar
juros_dia <- read.csv("Juro FINAL.csv")


# ESTAT DESC -------------------------------------------------------------------

### HHI ------------------------------------------------------------------------

### Ativos Totais
# Total (sem separação por tipo ou segmento) para calcular HHI
ativos <- ifdata %>%
  group_by(AnoMes) %>%
  filter(Conta == 78182) %>%
  arrange(AnoMes, desc(Saldo)) %>%
  mutate(Total_Ativo = sum(Saldo, na.rm = TRUE)) %>%
  mutate(mkt_shr = Saldo / Total_Ativo) %>%
  group_by(AnoMes) %>%
  summarise(HHI = sum(mkt_shr^2, na.rm = TRUE))

ativos$AnoMes <- as.Date(
  paste0(as.character(ativos$AnoMes), "01"),
  format = "%Y%m%d"
)

### Carteira de Crédito Classificado
cart_cred <- ifdata %>%
  group_by(AnoMes) %>%
  filter(Conta == 78183) %>%
  arrange(AnoMes, desc(Saldo)) %>%
  mutate(Total = sum(Saldo, na.rm = TRUE)) %>%
  mutate(mkt_shr = Saldo / Total) %>%
  group_by(AnoMes) %>%
  summarise(HHI = sum(mkt_shr^2, na.rm = TRUE))

cart_cred$AnoMes <- as.Date(
  paste0(as.character(cart_cred$AnoMes), "01"),
  format = "%Y%m%d"
)


### Receita de Operações de Crédito
cred_op <- ifdata %>%
  group_by(AnoMes) %>%
  filter(Conta == 78193) %>%
  arrange(AnoMes, desc(Saldo)) %>%
  mutate(Total = sum(Saldo, na.rm = TRUE)) %>%
  mutate(mkt_shr = Saldo / Total) %>%
  group_by(AnoMes) %>%
  summarise(HHI = sum(mkt_shr^2, na.rm = TRUE))

cred_op$AnoMes <- as.Date(
  paste0(as.character(cred_op$AnoMes), "01"),
  format = "%Y%m%d"
)


# GRÁFICOS
ggplot() +
  geom_line(data = ativos, aes(x = AnoMes, y = HHI, color = "Ativos Totais")) +
  geom_line(
    data = cart_cred,
    aes(x = AnoMes, y = HHI, color = "Carteira de Crédito ")
  ) +
  geom_line(
    data = cred_op,
    aes(x = AnoMes, y = HHI, color = "Receita OP Crédito")
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "HHI de Conglomerados Financeiros", x = "", y = "", color = "") +
  theme_classic()

ggsave(
  "hhi.png",
  width = 10,
  height = 6,
  dpi = 300
)


### Market Share ---------------------------------------------------------------

# Segmento Resolução (SR)
mktshare_sr <- ifdata %>%
  filter(!is.na(Sr)) %>%
  filter(NumeroRelatorio == 1, Conta == 78183) %>%
  arrange(AnoMes, desc(Saldo)) %>%
  group_by(AnoMes, Sr) %>%
  summarise(Saldo_Sr = sum(Saldo, na.rm = TRUE), .groups = "drop") %>%
  group_by(AnoMes) %>%
  mutate(Total_Ativo = sum(Saldo_Sr), mkt_shr = Saldo_Sr / Total_Ativo)

mktshare_sr$AnoMes <- as.Date(
  paste0(as.character(mktshare_sr$AnoMes), "01"),
  format = "%Y%m%d"
)


# Tipo de Consolidado Bancário (TCB)
mktshare_tcb <- ifdata %>%
  filter(!is.na(Tcb)) %>%
  filter(NumeroRelatorio == 1, Conta == 78183) %>%
  arrange(AnoMes, desc(Saldo)) %>%
  group_by(AnoMes, Tcb) %>%
  summarise(Saldo_Tcb = sum(Saldo, na.rm = TRUE), .groups = "drop") %>%
  group_by(AnoMes) %>%
  mutate(Total_Ativo = sum(Saldo_Tcb), mkt_shr = Saldo_Tcb / Total_Ativo)

mktshare_tcb$AnoMes <- as.Date(
  paste0(as.character(mktshare_tcb$AnoMes), "01"),
  format = "%Y%m%d"
)


# Plotando evolução market share médio por tipo/tamanho de instituição

### SR
ggplot() +
  geom_col(
    data = mktshare_sr,
    aes(x = AnoMes, y = mkt_shr, fill = Sr),
    position = "stack"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Market Share Médio por Segmento (Carteira de Crédito)",
    x = "",
    y = "",
    fill = "Segmento"
  ) +
  theme_classic()

ggsave(
  "mkt share by segment.png",
  width = 10,
  height = 6,
  dpi = 300
)


### TCB
ggplot() +
  geom_col(
    data = mktshare_tcb,
    aes(x = AnoMes, y = mkt_shr, fill = Tcb),
    position = "stack"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Market Share Médio por Conglomerado (Carteira de Crédito) ",
    x = "",
    y = "",
    fill = "Tipo"
  ) +
  theme_classic()

ggsave(
  "mkt share by TCB.png",
  width = 10,
  height = 6,
  dpi = 300
)


# Market share Top 10 e Top 20 Instituições
mktshare_top <- ifdata %>%
  filter(Conta == 78183, NumeroRelatorio == 1) %>%
  arrange(AnoMes, desc(Saldo)) %>%
  group_by(AnoMes) %>%
  mutate(total = sum(Saldo, na.rm = TRUE)) %>%
  mutate(mkt_shr = Saldo / total)

mktshare_top$AnoMes <- as.Date(
  paste0(as.character(mktshare_top$AnoMes), "01"),
  format = "%Y%m%d"
)

mktshare <- mktshare_top %>%
  group_by(AnoMes) %>%
  arrange(AnoMes, desc(mkt_shr)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 50) %>%
  summarise(
    avg = mean(mkt_shr, na.rm = TRUE),
    soma = sum(mkt_shr, na.rm = TRUE)
  )


mktshare_top20 <- mktshare_top %>%
  group_by(AnoMes) %>%
  arrange(AnoMes, desc(mkt_shr)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 20) %>%
  summarise(
    avg = mean(mkt_shr, na.rm = TRUE),
    soma = sum(mkt_shr, na.rm = TRUE)
  )

mktshare_top10 <- mktshare_top %>%
  group_by(AnoMes) %>%
  arrange(AnoMes, desc(mkt_shr)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10) %>%
  summarise(
    avg = mean(mkt_shr, na.rm = TRUE),
    soma = sum(mkt_shr, na.rm = TRUE)
  )

#Plot TOP 10 e 20
g1 <- ggplot() +
  geom_line(data = mktshare_top10, aes(x = AnoMes, y = avg, color = "Top 10")) +
  geom_line(data = mktshare_top20, aes(x = AnoMes, avg, color = "Top 20")) +
  geom_line(data = mktshare, aes(x = AnoMes, y = avg, color = "Top 50")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = seq(0, 0.1, by = 0.01)) +
  coord_cartesian(ylim = c(0, 0.09)) +
  labs(
    title = "Market Share Médio das maiores instituições financeiras",
    y = "",
    x = "",
    color = "Ranking"
  ) +
  theme_classic()

g2 <- ggplot() +
  geom_line(
    data = mktshare_top10,
    aes(x = AnoMes, y = soma, color = "Top 10")
  ) +
  geom_line(data = mktshare_top20, aes(x = AnoMes, soma, color = "Top 20")) +
  geom_line(data = mktshare, aes(x = AnoMes, y = soma, color = "Top 50")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Soma de market share das maiores instituições financeiras",
    y = "",
    x = "",
    color = "Ranking"
  ) +
  theme_classic()

# Plot g1 and g2 together
gg <- grid.arrange(g2, g1, ncol = 2)

ggsave(
  "TOP mkt share.png",
  plot = gg,
  width = 15,
  height = 6,
  dpi = 300
)


### Volumes e saldos portabilidade----------------------------------------------

#New datasets of fractions over new credit at same period
cred_mes <- portabil %>%
  mutate(
    payroll = payroll_RS / (consig_PF * 1000000),
    nonpay = non_payroll_RS / (nao_consig_PF * 1000000),
    pay_total = payroll_RS / (total_PF * 1000000),
    veiculos = vehicle_RS / (veiculos_PF * 1000000),
    total = all_credit_RS / (total_PF * 1000000)
  )

# Vendo empréstimos de imóveis como um todo
cred_mes <- cred_mes %>%
  mutate(
    home_broad_RS = home_RS +
      real_estate_SFH_RS +
      real_estate_SFI_RS +
      real_estate_enterprise_RS,
    home_relative = home_broad_RS / (total_PF * 1000000)
  )


#Loan amounts/quantities that were ported and granted the portability
ggplot(
  cred_mes %>%
    mutate(
      all_credit_total = all_credit_total / 100000,
      all_credit_granted = all_credit_granted / 100000
    ),
  aes(x = Date)
) +
  geom_line(aes(y = all_credit_total, color = "Total")) +
  geom_line(aes(y = all_credit_granted, color = "Efetivado")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Pedidos Mensais de Portabilidade",
    x = "",
    y = "Centenas de Milhares",
    color = ""
  ) +
  theme_classic()

ggsave(
  "port requests.png",
  width = 10,
  height = 6,
  dpi = 300
)

# Value being ported
ggplot(cred_mes, aes(x = Date)) +
  geom_line(aes(y = all_credit_RS / 1000000000), color = "steelblue") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "Valor Mensal Portado", x = "", y = "R$ Bilhões ") +
  theme_classic()

ggsave(
  "ported values.png",
  width = 10,
  height = 6,
  dpi = 300
)


# In terms of value ported relative to total monthly new credit
ggplot(cred_mes, aes(x = Date)) +
  geom_line(aes(y = total * 100), color = "steelblue") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Saldo Mensal Portado Relativo a Novos Empréstimos (em p.p.)",
    x = "",
    y = ""
  ) +
  theme_classic()

ggsave(
  "ported frac.png",
  width = 10,
  height = 6,
  dpi = 300
)


# Comparando Valores portados apenas com saldo (fluxo) de
# crédito na mesma categoria

ggplot(data = cred_mes, aes(x = Date)) +
  geom_line(aes(y = payroll, color = "Consignado")) +
  geom_line(aes(y = veiculos, color = "Veiculos")) +
  geom_line(aes(y = home_relative, color = "Imobiliário")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Fração Crédito Portado/Novo por Categoria",
    x = "",
    y = "",
    color = "Produto"
  ) +
  theme_classic()

ggplot(data = cred_mes, aes(x = Date)) +
  #geom_line(aes(y=payroll, color = "Consignado")) +
  geom_line(aes(y = veiculos, color = "Veiculos")) +
  geom_line(aes(y = home_relative, color = "Imobiliário")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Fração Crédito Portado/Novo por Categoria",
    x = "",
    y = "",
    color = "Produto"
  ) +
  theme_classic()


cred_shares <- cred_mes %>%
  select(
    Date,
    payroll_RS,
    non_payroll_RS,
    home_RS,
    others_RS,
    vehicle_RS,
    other_financing_RS,
    real_estate_SFH_RS,
    real_estate_SFI_RS,
    real_estate_enterprise_RS,
    all_credit_RS
  ) %>%
  mutate(across(
    .cols = -c(all_credit_RS, Date),
    .fns = ~ .x / all_credit_RS,
    .names = "{.col}_share"
  ))

# Plotting all ported loans
ggplot(data = cred_shares, aes(x = Date)) +
  geom_col(aes(y = payroll_RS_share, fill = "Consignado")) +
  geom_col(aes(y = non_payroll_RS_share, fill = "Crédito Pessoal")) +
  geom_col(aes(y = home_RS_share, fill = "Imobiliário Residencial")) +
  #geom_col(aes(y=others_RS_share, fill="Others")) +
  geom_col(aes(y = vehicle_RS_share, fill = "Veículos")) +
  geom_col(aes(y = other_financing_RS_share, fill = "Outros")) +
  geom_col(aes(y = real_estate_SFH_RS_share, fill = "Imobiliário (SFH)")) +
  geom_col(aes(y = real_estate_SFI_RS_share, fill = "Imobiliário (SFI)")) +
  geom_col(aes(
    y = real_estate_enterprise_RS_share,
    fill = "Imobiliário Comercial"
  )) +
  labs(
    title = "Share de Portabilidade por Produto",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_classic()

ggsave(
  "all ported.png",
  width = 10,
  height = 6,
  dpi = 300
)


#After excluding payroll since it is the most significant type of loan ported
ggplot(data = cred_shares, aes(x = Date)) +
  #geom_col(aes(y=payroll_RS_share, fill="Consignado")) +
  geom_col(aes(y = non_payroll_RS_share, fill = "Crédito Pessoal")) +
  geom_col(aes(y = home_RS_share, fill = "Imobiliário Residencial")) +
  #geom_col(aes(y=others_RS_share, fill="Others")) +
  geom_col(aes(y = vehicle_RS_share, fill = "Veículos")) +
  geom_col(aes(y = other_financing_RS_share, fill = "Outros")) +
  geom_col(aes(y = real_estate_SFH_RS_share, fill = "Imobiliário (SFH)")) +
  geom_col(aes(y = real_estate_SFI_RS_share, fill = "Imobiliário (SFI)")) +
  geom_col(aes(
    y = real_estate_enterprise_RS_share,
    fill = "Imobiliário Comercial"
  )) +
  labs(
    title = "Share de Portabilidade por Produto",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_classic()

# Como linhas ao invés de colunas
ggplot(data = cred_shares, aes(x = Date)) +
  #geom_line(aes(y=payroll_RS_share, fill="Consignado")) +
  geom_line(aes(y = non_payroll_RS_share, color = "Crédito Pessoal")) +
  geom_line(aes(y = home_RS_share, color = "Imobiliário Residencial")) +
  #geom_col(aes(y=others_RS_share, fill="Others")) +
  geom_line(aes(y = vehicle_RS_share, color = "Veículos")) +
  geom_line(aes(y = other_financing_RS_share, color = "Outros")) +
  geom_line(aes(y = real_estate_SFH_RS_share, color = "Imobiliário (SFH)")) +
  geom_line(aes(y = real_estate_SFI_RS_share, color = "Imobiliário (SFI)")) +
  geom_line(aes(
    y = real_estate_enterprise_RS_share,
    color = "Imobiliário Comercial"
  )) +
  labs(
    title = "Share de Portabilidade por Produto",
    x = "",
    y = "",
    color = ""
  ) +
  theme_classic()

ggsave(
  "ported except payroll.png",
  width = 10,
  height = 6,
  dpi = 300
)


### ICC ------------------------------------------------------------------------
# Estimativa de como custo administrativo para PF tem mudado
plot(ICC$date, ICC$custo_adm_PF, type = 'l')
plot(ICC$date, ICC$desp_adm, type = 'l')
plot(ICC$date, ICC$custo_captacao, type = 'l')


ggplot(data = ICC, aes(x = date)) +
  geom_line(aes(y = icc_PJ, color = "PJ (Total)")) +
  geom_line(aes(y = icc_PF, color = "PF (Total)")) +
  geom_line(aes(y = icc_payroll_total, color = "Consignado")) +
  geom_line(aes(y = icc_vehicles, color = "Veiculos")) +
  geom_line(aes(y = icc_realestate, color = "Imobiliário")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "ICC - Indicador de Custo de Crédito ",
    x = "",
    y = "p.p.",
    color = "Tipo"
  ) +
  theme_classic()

ggsave(
  "ICC plot.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300
)


# Despesas administrativas do ICC
ggplot(data = icc_decomp, aes(x = date)) +
  geom_line(aes(y = percent_captacao, color = "Captação")) +
  geom_line(aes(y = percent_adm, color = "Administrativo")) +
  labs(
    title = "Custos como Percentual do ICC",
    x = "",
    y = "%",
    color = "Custos"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_classic()

ggsave(
  "quebra ICC.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300
)


### RCO ------------------------------------------------------------------------

rco <- merge(rco, portabil, by.x = "date", by.y = "Date", all.x = TRUE)

write.csv(rco, "RCO_Agg.csv", row.names = FALSE)
# Saldo de transferências RCO ao longo do tempo

ggplot(data = rco, aes(x = date)) +
  geom_line(aes(y = all_credit_rco / 1000000), color = "steelblue") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Saldo Total de Transferências de RCO",
    x = "",
    y = "R$ (Milhões)"
  ) +
  theme_classic()

ggsave(
  "RCO total.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300
)


# Por categoria
g1 <- ggplot(data = rco, aes(x = date)) +
  #geom_line(aes(y=all_credit_rco/1000000), color = "steelblue")+
  geom_line(aes(y = payroll_rco / 1000000, color = "Consignado")) +
  geom_line(aes(y = nonpayroll_rco / 1000000, color = "Pessoal Excl. Cons.")) +
  geom_line(aes(
    y = (home_rco + real_estate_SFH_rco + real_estate_SFI_rco) / 1000000,
    color = "Real Estate"
  )) +
  geom_line(aes(y = vehicle_rco / 1000000, color = "Veículos")) +
  geom_line(aes(y = other_financing_rco / 1000000, color = "Outros")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Saldo de Transferências RCO",
    x = "",
    y = "R$ (Milhões)",
    color = "Modalidade"
  ) +
  theme_classic()


# Excluindo consignado para ver o resto
g2 <- ggplot(data = rco, aes(x = date)) +
  #geom_line(aes(y=all_credit_rco/1000000), color = "steelblue")+
  #geom_line(aes(y=payroll_rco/1000000, color = "Consignado")) +
  geom_line(aes(y = nonpayroll_rco / 1000000, color = "Pessoal Excl. Cons.")) +
  geom_line(aes(
    y = (home_rco + real_estate_SFH_rco + real_estate_SFI_rco) / 1000000,
    color = "Real Estate"
  )) +
  geom_line(aes(y = vehicle_rco / 1000000, color = "Veículos")) +
  geom_line(aes(y = other_financing_rco / 1000000, color = "Outros")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Excluindo Consignado",
    x = "",
    y = "R$ (Milhões)",
    color = "Modalidade"
  ) +
  theme_classic()

# Excluindo CONSIGNADO E IMOBILIÁRIO para ver o resto
g3 <- ggplot(data = rco, aes(x = date)) +
  #geom_line(aes(y=all_credit_rco/1000000), color = "steelblue")+
  #geom_line(aes(y=payroll_rco/1000000, color = "Consignado")) +
  geom_line(aes(y = nonpayroll_rco / 1000000, color = "Pessoal Excl. Cons.")) +
  #geom_line(aes(y=(home_rco+real_estate_SFH_rco+real_estate_SFI_rco)/1000000, color = "Real Estate")) +
  geom_line(aes(y = vehicle_rco / 1000000, color = "Veículos")) +
  geom_line(aes(y = other_financing_rco / 1000000, color = "Outros")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Excluindo Imobiliário",
    x = "",
    y = "R$ (Milhões)",
    color = "Modalidade"
  ) +
  theme_classic()

# Excluindo CONSIGNADO E IMOBILIÁRIO E VEICULOS   para ver o resto
g4 <- ggplot(data = rco, aes(x = date)) +
  #geom_line(aes(y=all_credit_rco/1000000), color = "steelblue")+
  #geom_line(aes(y=payroll_rco/1000000, color = "Consignado")) +
  geom_line(aes(y = nonpayroll_rco / 1000000, color = "Pessoal Excl. Cons.")) +
  #geom_line(aes(y=(home_rco+real_estate_SFH_rco+real_estate_SFI_rco)/1000000, color = "Real Estate")) +
  #geom_line(aes(y=vehicle_rco/1000000, color = "Veículos")) +
  geom_line(aes(y = other_financing_rco / 1000000, color = "Outros")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Excluindo veículos",
    x = "",
    y = "R$ (Milhões)",
    color = "Modalidade"
  ) +
  theme_classic()

gg <- grid.arrange(g1, g2, g3, g4, ncol = 2)

ggsave(
  "RCO modalidades.png",
  plot = gg,
  width = 12,
  height = 8,
  dpi = 300
)


# Valor médio pago em RCO por pedido de portabilidade
rco_shares <- rco %>%
  mutate(
    total_avg = all_credit_rco / all_credit_granted,
    consig_avg = payroll_rco / payroll_Granted,
    nonpay_avg = nonpayroll_rco / non_payroll_granted,
    houses_avg = home_rco / home_granted,
    home_avg = (home_rco +
      real_estate_SFH_rco +
      real_estate_SFI_rco +
      real_estate_enterprise_rco) /
      (home_granted +
        real_estate_enterprise_granted +
        real_estate_SFI_granted +
        real_estate_enterprise_granted),
    vehicle_avg = vehicle_rco / vehicle_granted
  )

g1 <- ggplot(data = rco_shares, aes(x = date)) +
  geom_line(aes(y = total_avg, color = "Total")) +
  geom_line(aes(y = consig_avg, color = "Consignado")) +
  geom_line(aes(y = nonpay_avg, color = "Pessoal Excl. Cons.")) +
  #geom_line(aes(y=home_avg, color = "Real Estate")) +
  #geom_line(aes(y=vehicle_avg, color = "Veículos")) +
  #geom_line(aes(y=houses_avg,color="Houses")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Valor Médio Pago em RCO por Pedido de Portabilidade",
    x = "",
    y = "R$",
    color = ""
  ) +
  theme_classic()

g2 <- ggplot(data = rco_shares, aes(x = date)) +
  geom_line(aes(y = total_avg, color = "Total")) +
  geom_line(aes(y = consig_avg, color = "Consignado")) +
  geom_line(aes(y = nonpay_avg, color = "Pessoal Excl. Cons.")) +
  #geom_line(aes(y=home_avg, color = "Real Estate")) +
  geom_line(aes(y = vehicle_avg, color = "Veículos")) +
  geom_line(aes(y = houses_avg, color = "Houses")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Valor Médio Pago em RCO por Pedido de Portabilidade",
    x = "",
    y = "R$",
    color = ""
  ) +
  theme_classic()

grid.arrange(g1, g2, ncol = 2)


# Comparando custos de RCO com ICC

# Diff in Diff------------------------------------------------------------------
juros_dia <- read_csv(
  "Juro FINAL.csv",
  col_types = cols(
    mes = col_date(format = "%Y-%m-%d"),
    inicio = col_date(format = "%Y-%m-%d"),
    fim = col_date(format = "%Y-%m-%d")
  )
)


### Regressoes (spread) -----------------------------------------------------------------------
# Criando dummies de tempo
juros <- juros_dia %>%
  mutate(
    portabilidade = ifelse(
      fim >= as.Date("2014-05-05", format = "%Y-%m-%d"),
      1,
      0
    ),
    RCO_inicio = ifelse(
      fim >= as.Date("2015-03-01", format = "%Y-%m-%d"),
      1,
      0
    ),
    RCO_update2016 = ifelse(
      fim >= as.Date("2016-11-01", format = "%Y-%m-%d"),
      1,
      0
    ),
    RCO_update2023 = ifelse(
      fim >= as.Date("2023-07-01", format = "%Y-%m-%d"),
      1,
      0
    ),
    RCO_update2021 = ifelse(
      fim >= as.Date("2021-03-01", format = "%Y-%m-%d"),
      1,
      0
    )
  )

#' Criando dummies de tratamento
#' há 2 tratamentos distintos. O primeiro é ser afetado pelo RCO como um todo
#' e o 2o é ser afetado pela 2a leva de permissões ao RCO que vem em 2021
#' para capital de giro e cheque especial apenas, com outros grupos servindo
#' de controle.

# Criando dummies de tratamento
juros <- juros |>
  mutate(
    RCO_tratamento_PFvsPJ = ifelse(segmento == "PF", 1, 0),
    RCO_tratamento_PJs = ifelse(
      modalidade %in% c("CG < 365", "CG > 365", "Conta Garantida"),
      1,
      0
    ),
    RCO_tratamento_consig = ifelse(
      modalidade %in%
        c("Consignado INSS", "Consignado privado", "Consignado público"),
      1,
      0
    ),
    RCO_tratamento_factivel = ifelse(
      modalidade %in%
        c(
          "Consignado INSS",
          "Consignado privado",
          "Consignado público",
          "Crédito pessoal",
          "Veículos"
        ),
      1,
      0
    )
  )

#'PFvsPJ foca na possibilidade de efetivar portabilidade em grande parte da
#'amostra ter ser restrita a créditos para pessoa física

#'Tratamento PJs foca somente em créditos de PJs e olha a alteração que permitiu
#'que apenas certas linhas de crédito fossem portadas.

#'Tratamento factível foca nas linhas de crédito de portabilidade para PFs
#'que são observadas como tendo espaço no mercado. Ou seja, onde portabilidade
#'realmente ocorre.

#' NOTA: É necessário trabalhar com filtros dos dados gerais para evitar incluir
#' modalidades de crédito inadequadas como controle

# Criando controle de tempo (mes-ano)
juros$mes_ano <- format(juros$inicio, "%m/%Y")


#### Daily MA ------------------------------------------------------------------
# Primeiro usando todos os dados disponíveis e testando tanto só préfixados
#' quanto pré e pós, com o tratamento sendo se a linha de crédito é de PF ou PJ

model1_PFvsPJ <- feols(
  juros_ano ~
    portabilidade +
      RCO_tratamento_PFvsPJ +
      portabilidade * RCO_tratamento_PFvsPJ |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  #data = juros)
  data = juros |> filter(index == "Pré-fixado")
)
summary(model1_PFvsPJ)


model2_PFvsPJ <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_tratamento_PFvsPJ +
      portabilidade * RCO_tratamento_PFvsPJ +
      RCO_inicio * RCO_tratamento_PFvsPJ |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  #data = juros)
  data = juros |> filter(index == "Pré-fixado")
)
summary(model2_PFvsPJ)


model3_PFvsPJ <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_tratamento_PFvsPJ +
      portabilidade * RCO_tratamento_PFvsPJ +
      RCO_inicio * RCO_tratamento_PFvsPJ +
      RCO_update2016 * RCO_tratamento_PFvsPJ |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  #data = juros)
  data = juros |> filter(index == "Pré-fixado")
)
summary(model3_PFvsPJ)


model_PFvsPJ <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_PFvsPJ +
      portabilidade * RCO_tratamento_PFvsPJ +
      RCO_inicio * RCO_tratamento_PFvsPJ +
      RCO_update2016 * RCO_tratamento_PFvsPJ +
      RCO_update2023 * RCO_tratamento_PFvsPJ |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  #data=juros)
  data = juros |> filter(index == "Pré-fixado")
)
summary(model_PFvsPJ)


#Só para comparação temos uma versão alternativa da especificação completa sem efeitos fixos
model_noFE <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_PFvsPJ +
      portabilidade * RCO_tratamento_PFvsPJ +
      RCO_inicio * RCO_tratamento_PFvsPJ +
      RCO_update2016 * RCO_tratamento_PFvsPJ +
      RCO_update2023 * RCO_tratamento_PFvsPJ,
  vcov = cluster ~ mes_ano,
  data = juros
)
#data = juros |> filter(index == "Pré-fixado"))

summary(model_noFE)


#Exportando tabela para LaTeX
etable(
  model1_PFvsPJ,
  model2_PFvsPJ,
  model3_PFvsPJ,
  model_PFvsPJ,
  model_noFE,
  tex = TRUE
)


#'Agora usando somente as linhas de crédito que sabemos terem sido afetadas
#'diretamente pela portabilidade (as que são factíveis)

model1_fac <- feols(
  juros_ano ~
    portabilidade +
      RCO_tratamento_factivel +
      portabilidade * RCO_tratamento_factivel |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros
)
#data = juros|> filter(index == "Pré-fixado"))
summary(model1_fac)


model2_fac <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_tratamento_factivel +
      portabilidade * RCO_tratamento_factivel +
      RCO_inicio * RCO_tratamento_factivel |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros
)
#data = juros|> filter(index == "Pré-fixado"))
summary(model2_fac)


model3_fac <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_tratamento_factivel +
      portabilidade * RCO_tratamento_factivel +
      RCO_inicio * RCO_tratamento_factivel +
      RCO_update2016 * RCO_tratamento_factivel |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros
)
#data = juros|> filter(index == "Pré-fixado"))
summary(model3_fac)


model_fac <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_factivel +
      portabilidade * RCO_tratamento_factivel +
      RCO_inicio * RCO_tratamento_factivel +
      RCO_update2016 * RCO_tratamento_factivel +
      RCO_update2023 * RCO_tratamento_factivel |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros
)
#data=juros |> filter(index == "Pré-fixado"))
summary(model_fac)


model_fac_noFE <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_factivel +
      portabilidade * RCO_tratamento_factivel +
      RCO_inicio * RCO_tratamento_factivel +
      RCO_update2016 * RCO_tratamento_factivel +
      RCO_update2023 * RCO_tratamento_factivel,
  vcov = cluster ~ mes_ano,
  data = juros
)
#data=juros |> filter(index == "Pré-fixado"))
summary(model_fac_noFE)

# Imprimindo em LaTeX
etable(
  model1_fac,
  model2_fac,
  model3_fac,
  model_fac,
  model_fac_noFE,
  tex = TRUE
)


#' Agora focando na categoria mais portada (consignado) e eliminando os outros
#' créditos portados para que eles não virem um controle espúrio
model1_consig <- feols(
  juros_ano ~
    portabilidade +
      RCO_tratamento_consig +
      portabilidade * RCO_tratamento_consig |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros
)
#data = juros|> filter(index == "Pré-fixado")|> filter(!modalidade %in% c("Crédito pessoal","Veículos")))
summary(model1_consig)


model2_consig <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_tratamento_consig +
      portabilidade * RCO_tratamento_consig +
      RCO_inicio * RCO_tratamento_consig |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros
)
#data = juros|> filter(index == "Pré-fixado")|> filter(!modalidade %in% c("Crédito pessoal","Veículos")))
summary(model2_consig)


model3_consig <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_tratamento_consig +
      portabilidade * RCO_tratamento_consig +
      RCO_inicio * RCO_tratamento_consig +
      RCO_update2016 * RCO_tratamento_consig |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros
)
#data = juros|> filter(index == "Pré-fixado")|> filter(!modalidade %in% c("Crédito pessoal","Veículos")))
summary(model3_consig)


model_consig <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_consig +
      portabilidade * RCO_tratamento_consig +
      RCO_inicio * RCO_tratamento_consig +
      RCO_update2016 * RCO_tratamento_consig +
      RCO_update2023 * RCO_tratamento_consig |
      mes_ano + InstFin + mes_ano^InstFin + modalidade,
  vcov = cluster ~ mes_ano,
  data = juros |> filter(!modalidade %in% c("Crédito Pessoal", "Veículos"))
)
#data = juros |> filter(index == "Pré-fixado") |> filter(!modalidade %in% c("Crédito pessoal","Veículos")))
summary(model_consig)


model_noFE <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_consig +
      portabilidade * RCO_tratamento_consig +
      RCO_inicio * RCO_tratamento_consig +
      RCO_update2016 * RCO_tratamento_consig +
      RCO_update2023 * RCO_tratamento_consig,
  vcov = cluster ~ mes_ano,
  data = juros |> filter(!modalidade %in% c("Crédito Pessoal", "Veículos"))
)
#data = juros |> filter(index == "Pré-fixado") |> filter(!modalidade %in% c("Crédito pessoal","Veículos")))
summary(model_noFE)

# Imprimindo em LaTeX
etable(
  model1_consig,
  model2_consig,
  model3_consig,
  model_consig,
  model_noFE,
  tex = TRUE
)


# Turning dataset into weekly by getting the last observation of each week
juros_week <- juros |>
  group_by(InstFin) |>
  mutate(date = floor_date(fim, " week", week_start = 1)) |>
  group_by(InstFin, date) |>
  slice_max(order_by = fim, n = 1, with_ties = FALSE)


#Running all regressions again
#### Weekly --------------------------------------------------------------------
model1_PFvsPJ <- feols(
  juros_ano ~
    portabilidade +
      RCO_tratamento_PFvsPJ +
      portabilidade * RCO_tratamento_PFvsPJ |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  #data = juros_week)
  data = juros_week |> filter(index == "Pré-fixado")
)
summary(model1_PFvsPJ)


model2_PFvsPJ <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_tratamento_PFvsPJ +
      portabilidade * RCO_tratamento_PFvsPJ +
      RCO_inicio * RCO_tratamento_PFvsPJ |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  #data = juros_week)
  data = juros_week |> filter(index == "Pré-fixado")
)
summary(model2_PFvsPJ)


model3_PFvsPJ <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_tratamento_PFvsPJ +
      portabilidade * RCO_tratamento_PFvsPJ +
      RCO_inicio * RCO_tratamento_PFvsPJ +
      RCO_update2016 * RCO_tratamento_PFvsPJ |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  #data = juros_week)
  data = juros_week |> filter(index == "Pré-fixado")
)
summary(model3_PFvsPJ)


model_PFvsPJ <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_PFvsPJ +
      portabilidade * RCO_tratamento_PFvsPJ +
      RCO_inicio * RCO_tratamento_PFvsPJ +
      RCO_update2016 * RCO_tratamento_PFvsPJ +
      RCO_update2023 * RCO_tratamento_PFvsPJ |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  #data=juros_week)
  data = juros_week |> filter(index == "Pré-fixado")
)
summary(model_PFvsPJ)


#Só para comparação temos uma versão alternativa da especificação completa sem efeitos fixos
model_noFE <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_PFvsPJ +
      portabilidade * RCO_tratamento_PFvsPJ +
      RCO_inicio * RCO_tratamento_PFvsPJ +
      RCO_update2016 * RCO_tratamento_PFvsPJ +
      RCO_update2023 * RCO_tratamento_PFvsPJ,
  vcov = cluster ~ mes_ano,
  #data=juros_week)
  data = juros_week |> filter(index == "Pré-fixado")
)

summary(model_noFE)


#Exportando tabela para LaTeX
etable(
  model1_PFvsPJ,
  model2_PFvsPJ,
  model3_PFvsPJ,
  model_PFvsPJ,
  model_noFE,
  tex = TRUE
)


#'Agora usando somente as linhas de crédito que sabemos terem sido afetadas
#'diretamente pela portabilidade (as que são factíveis)

model1_fac <- feols(
  juros_ano ~
    portabilidade +
      RCO_tratamento_factivel +
      portabilidade * RCO_tratamento_factivel |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  #data = juros_week)
  data = juros_week |> filter(index == "Pré-fixado")
)
summary(model1_fac)


model2_fac <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_tratamento_factivel +
      portabilidade * RCO_tratamento_factivel +
      RCO_inicio * RCO_tratamento_factivel |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  #data = juros_week)
  data = juros_week |> filter(index == "Pré-fixado")
)
summary(model2_fac)


model3_fac <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_tratamento_factivel +
      portabilidade * RCO_tratamento_factivel +
      RCO_inicio * RCO_tratamento_factivel +
      RCO_update2016 * RCO_tratamento_factivel |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  #data = juros_week)
  data = juros_week |> filter(index == "Pré-fixado")
)
summary(model3_fac)


model_fac <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_factivel +
      portabilidade * RCO_tratamento_factivel +
      RCO_inicio * RCO_tratamento_factivel +
      RCO_update2016 * RCO_tratamento_factivel +
      RCO_update2023 * RCO_tratamento_factivel |
      modalidade + mes_ano + InstFin + mes_ano^InstFin + segmento,
  vcov = cluster ~ mes_ano,
  #data = juros_week)
  data = juros_week |> filter(index == "Pré-fixado")
)
summary(model_fac)


model_fac_noFE <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_factivel +
      portabilidade * RCO_tratamento_factivel +
      RCO_inicio * RCO_tratamento_factivel +
      RCO_update2016 * RCO_tratamento_factivel +
      RCO_update2023 * RCO_tratamento_factivel,
  vcov = cluster ~ mes_ano,
  #data = juros_week)
  data = juros_week |> filter(index == "Pré-fixado")
)
summary(model_fac_noFE)

# Imprimindo em LaTeX
etable(
  model1_fac,
  model2_fac,
  model3_fac,
  model_fac,
  model_fac_noFE,
  tex = TRUE
)


#' Agora focando na categoria mais portada (consignado) e eliminando os outros
#' créditos portados para que eles não virem um controle espúrio
model1_consig <- feols(
  juros_ano ~
    portabilidade +
      RCO_tratamento_consig +
      portabilidade * RCO_tratamento_consig |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros_week |> filter(!modalidade %in% c("Crédito pessoal", "Veículos"))
)
#data = juros_week |> filter(index == "Pré-fixado") |> filter(!modalidade %in% c("Crédito pessoal","Veículos")))
summary(model1_consig)


model2_consig <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_tratamento_consig +
      portabilidade * RCO_tratamento_consig +
      RCO_inicio * RCO_tratamento_consig |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros_week |> filter(!modalidade %in% c("Crédito pessoal", "Veículos"))
)
#data = juros_week|> filter(index == "Pré-fixado")|> filter(!modalidade %in% c("Crédito pessoal","Veículos")))
summary(model2_consig)


model3_consig <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_tratamento_consig +
      portabilidade * RCO_tratamento_consig +
      RCO_inicio * RCO_tratamento_consig +
      RCO_update2016 * RCO_tratamento_consig |
      modalidade + mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros_week |> filter(!modalidade %in% c("Crédito pessoal", "Veículos"))
)
#data = juros_week|> filter(index == "Pré-fixado")|> filter(!modalidade %in% c("Crédito pessoal","Veículos")))
summary(model3_consig)


model_consig <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_consig +
      portabilidade * RCO_tratamento_consig +
      RCO_inicio * RCO_tratamento_consig +
      RCO_update2016 * RCO_tratamento_consig +
      RCO_update2023 * RCO_tratamento_consig |
      mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros_week |> filter(!modalidade %in% c("Crédito Pessoal", "Veículos"))
)
#data = juros_week |> filter(!modalidade %in% c("Crédito pessoal","Veículos")) |> filter(index == "Pré-fixado"))
summary(model_consig)


model_noFE <- feols(
  juros_ano ~
    portabilidade +
      RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_consig +
      portabilidade * RCO_tratamento_consig +
      RCO_inicio * RCO_tratamento_consig +
      RCO_update2016 * RCO_tratamento_consig +
      RCO_update2023 * RCO_tratamento_consig,
  vcov = cluster ~ mes_ano,
  data = juros_week |> filter(!modalidade %in% c("Crédito Pessoal", "Veículos"))
)
#data = juros_week |> filter(index == "Pré-fixado") |> filter(!modalidade %in% c("Crédito pessoal","Veículos")))
summary(model_noFE)

# Imprimindo em LaTeX
etable(
  model1_consig,
  model2_consig,
  model3_consig,
  model_consig,
  model_noFE,
  tex = TRUE
)


#### PJs -----------------------------------------------------------------------
# Daily (MA)
juros_PJ <- juros |> filter(segmento == "PJ")

model_PJ2021 <- feols(
  juros_ano ~
    1 +
      RCO_tratamento_PJs +
      RCO_tratamento_PJs * RCO_update2021 |
      mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros |> filter(segmento == "PJ")
)
#data = juros_week |> filter(segmento == "PJ") |> filter(index=="Pré-fixado"))
summary(model_PJ2021)


# Weekly
model_weekly_PJ2021 <- feols(
  juros_ano ~
    1 +
      RCO_tratamento_PJs +
      RCO_tratamento_PJs * RCO_update2021 |
      mes_ano + InstFin + mes_ano^InstFin,
  vcov = cluster ~ mes_ano,
  data = juros_week |> filter(segmento == "PJ")
)
#data = juros_week |> filter(segmento == "PJ") |> filter(index=="Pré-fixado"))

summary(model_weekly_PJ2021)


#Imprimindo no LaTeX
etable(model_PJ2021, tex = TRUE)

etable(model_weekly_PJ2021, tex = TRUE)

#' A constante do update de 2021 é absorvida pelos efeitos fixos e gera
#' multicolinearidade perfeita, então não precisa ser incluida

#' Problema é que nesse caso não conseguimos separar o efeito resultante da
#' liberação de portabilidade nessas contas vs o efeito do RCO estar sendo cobrado,
#' ao menos não nessa especificação...
#'
#' Unica tentativa seria comparar o caso específico da conta garantida mas não
#' temos os dados por Instituição de conta garantida para PF (suponho que sejam MEIs)

### Regressões (Competição) ----------------------------------------------------
# Reimportando dados "limpos" do IFDATA
ifdata <- read_csv("IFDATA/IFDATA_FINAL.csv")

# Dados são trimestrais nesse caso (vem do IFData)

# Mantendo apenas dados de modalidades de crédito
ifdata <- ifdata |>
  filter(NumeroRelatorio %in% c(11, 13)) |>
  mutate(segmento = ifelse(NumeroRelatorio == 11, "PF", "PJ"))


ifdata <- ifdata |>
  select(
    -NomeRelatorio,
    -DescricaoColuna,
    -Tc,
    -Atividade,
    -Sr,
    -CnpjInstituicaoLider,
    -TipoInstituicao,
    -Data
  )

# Consertando variável de data
ifdata$AnoMes <- as.Date(paste0(ifdata$AnoMes, "01"), format = "%Y%m%d")

# Criando HHI por produto a cada mes/ano
ifdata <- ifdata |>
  group_by(AnoMes, Grupo) |>
  mutate(Total = sum(Saldo, na.rm = TRUE)) |>
  group_by(AnoMes, Grupo, CodInst) |>
  summarise(
    mkt_share = sum(Saldo, na.rm = TRUE) / Total,
    Saldo = sum(Saldo, na.rm = TRUE),
    Grupo = unique(Grupo),
    Tcb = unique(Tcb),
    Td = unique(Td),
    SegmentoTb = unique(SegmentoTb),
    Segmento = unique(segmento)
  )

ifdata_hhi <- ifdata |>
  group_by(AnoMes, Grupo) |>
  summarise(hhi = sum(mkt_share^2, na.rm = TRUE), Segmento = unique(Segmento))


# Criando dummies de tempo
ifdata_hhi <- ifdata_hhi |>
  mutate(
    RCO_inicio = ifelse(
      AnoMes >= as.Date("2015-03-01", format = "%Y-%m-%d"),
      1,
      0
    ),
    RCO_update2016 = ifelse(
      AnoMes >= as.Date("2016-11-01", format = "%Y-%m-%d"),
      1,
      0
    ),
    RCO_update2023 = ifelse(
      AnoMes >= as.Date("2023-07-01", format = "%Y-%m-%d"),
      1,
      0
    ),
    RCO_update2021 = ifelse(
      AnoMes >= as.Date("2021-03-01", format = "%Y-%m-%d"),
      1,
      0
    )
  )

# Criando dummies de tratamento
ifdata_hhi <- ifdata_hhi |>
  mutate(
    RCO_tratamento_PFvsPJ = ifelse(Segmento == "PF", 1, 0),
    RCO_tratamento_PJs = ifelse(
      Grupo %in%
        c(
          "Capital de Giro",
          "Capital de Giro Rotativo",
          "Cheque Especial e Conta Garantida"
        ),
      1,
      0
    ),
    RCO_tratamento_consig = ifelse(
      Grupo %in% c("Empréstimo com Consignação em Folha"),
      1,
      0
    ),
    RCO_tratamento_factivel = ifelse(
      Grupo %in%
        c(
          "Empréstimo com Consignação em Folha",
          "Empréstimo sem Consignação em Folha",
          "Veículos",
          "Habitação"
        ),
      1,
      0
    )
  )


# Tratamento mais simples abordando PF vs PJ
model1_PFvsPJ <- feols(
  hhi ~
    RCO_inicio +
      RCO_tratamento_PFvsPJ +
      RCO_tratamento_PFvsPJ * RCO_inicio |
      Grupo + AnoMes,
  data = ifdata_hhi
)
summary(model1_PFvsPJ)


model2_PFvsPJ <- feols(
  hhi ~
    RCO_inicio +
      RCO_update2016 +
      RCO_tratamento_PFvsPJ +
      RCO_tratamento_PFvsPJ * RCO_inicio +
      RCO_tratamento_PFvsPJ * RCO_update2016 |
      Grupo + AnoMes,
  vcov = cluster ~ AnoMes,
  data = ifdata_hhi
)
summary(model2_PFvsPJ)


model_PFvsPJ <- feols(
  hhi ~
    RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_PFvsPJ +
      RCO_tratamento_PFvsPJ * RCO_inicio +
      RCO_tratamento_PFvsPJ * RCO_update2016 +
      RCO_tratamento_PFvsPJ * RCO_update2023 |
      Grupo + AnoMes,
  vcov = cluster ~ AnoMes,
  data = ifdata_hhi
)
summary(model_PFvsPJ)

#'Efeito fixos de Ano/Mes ser incluido ou não muda virtualmente nada
#'nos resultados já que os efeitos temporais inclusos fazem basicamente
#'o mesmo papel

#Imprimindo em LaTeX
etable(model1_PFvsPJ, model2_PFvsPJ, model_PFvsPJ, tex = TRUE)


#Com modalidades de crédito que são efetivamente portados
model1_fac <- feols(
  hhi ~
    RCO_inicio +
      RCO_tratamento_factivel +
      RCO_tratamento_factivel * RCO_inicio |
      Grupo + AnoMes,
  vcov = cluster ~ AnoMes,
  data = ifdata_hhi
)
summary(model1_fac)


model2_fac <- feols(
  hhi ~
    RCO_inicio +
      RCO_update2016 +
      RCO_tratamento_factivel +
      RCO_tratamento_factivel * RCO_inicio +
      RCO_tratamento_factivel * RCO_update2016 |
      Grupo + AnoMes,
  vcov = cluster ~ AnoMes,
  data = ifdata_hhi
)
summary(model2_fac)


model_fac <- feols(
  hhi ~
    RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_factivel +
      RCO_tratamento_factivel * RCO_inicio +
      RCO_tratamento_factivel * RCO_update2016 +
      RCO_tratamento_factivel * RCO_update2023 |
      Grupo + AnoMes,
  vcov = cluster ~ AnoMes,
  data = ifdata_hhi
)
summary(model_fac)

#Imprimindo em LaTeX
etable(model1_fac, model2_fac, model_fac, tex = TRUE)


# Olhando apenas crédito consignado vs não portados
model1_consig <- feols(
  hhi ~
    RCO_inicio +
      RCO_tratamento_consig +
      RCO_tratamento_consig * RCO_inicio |
      Grupo + AnoMes,
  vcov = cluster ~ AnoMes,
  data = ifdata_hhi |>
    filter(
      !Grupo %in%
        c("Empréstimo sem Consignação em Folha", "Veículos", "Habitação")
    )
)
summary(model1_consig)


model2_consig <- feols(
  hhi ~
    RCO_inicio +
      RCO_update2016 +
      RCO_tratamento_consig +
      RCO_tratamento_consig * RCO_inicio +
      RCO_tratamento_consig * RCO_update2016 |
      Grupo + AnoMes,
  vcov = cluster ~ AnoMes,
  data = ifdata_hhi |>
    filter(
      !Grupo %in%
        c("Empréstimo sem Consignação em Folha", "Veículos", "Habitação")
    )
)
summary(model2_consig)


model_consig <- feols(
  hhi ~
    RCO_inicio +
      RCO_update2016 +
      RCO_update2023 +
      RCO_tratamento_consig +
      RCO_tratamento_consig * RCO_inicio +
      RCO_tratamento_consig * RCO_update2016 +
      RCO_tratamento_consig * RCO_update2023 |
      Grupo + AnoMes,
  vcov = cluster ~ AnoMes,
  data = ifdata_hhi |>
    filter(
      !Grupo %in%
        c("Empréstimo sem Consignação em Folha", "Veículos", "Habitação")
    )
)
summary(model_consig)


# Imprimir em LaTeX
etable(model1_consig, model2_consig, model_consig, tex = TRUE)


#Por fim falta olhar para os PJs

model_PJ <- feols(
  hhi ~
    1 +
      RCO_update2021 +
      RCO_tratamento_PJs +
      RCO_update2021 * RCO_tratamento_PJs |
      Grupo,
  vcov = cluster ~ AnoMes,
  data = ifdata_hhi |> filter(Segmento == "PJ")
)

summary(model_PJ)

model_FE_PJ <- feols(
  hhi ~
    1 +
      RCO_update2021 +
      RCO_tratamento_PJs +
      RCO_update2021 * RCO_tratamento_PJs |
      Grupo + AnoMes,
  vcov = cluster ~ AnoMes,
  data = ifdata_hhi |> filter(Segmento == "PJ")
)

summary(model_FE_PJ)

# Imprimindo em LaTeX
etable(model_PJ, tex = TRUE)
etable(model_FE_PJ, tex = TRUE)
