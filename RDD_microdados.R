# SETUP ===================================================================================================================================================================
rm(list = ls())

library(lubridate)
library(tidyverse)
library(readxl)
library(readr)
library(gridExtra)
library(rdrobust)
library(rdmulti)
library(stargazer)

setwd("PATH") #Diretório principal a ser utilizado


# DADOS ===================================================================================================================================================================
### EMPRÉSTIMOS PORTADOS ==================================================================================================================================================
# Base específica de empréstimos portados -- IN ou OUT
port <- read_csv(
  "Emprestimos_Portados.csv",
  col_types = cols(
    data_que_foi_portado = col_date(format = "%Y-%m-%d"),
    montante_quando_foi_portado = col_double(),
    valor_rco = col_double()
  )
)

names(port)


names(port) <- c(
  "id",
  "in_out",
  "data_port",
  "if_prior",
  "juros_prior",
  "montante_portado",
  "montante_total",
  "valor_rco",
  "status",
  "razao_falha"
)
port$montante_total <- as.double(port$montante_total)


# Organizando variável do status da portabilidade
port <- port |>
  mutate(
    status = ifelse(
      status == "portabilidade_concluída",
      "sucesso",
      ifelse(
        status == "falha_na_portabilidade",
        "falha",
        ifelse(
          status == "transferencia_de_potabilidade__recebida",
          "transferencia recebida",
          ifelse(
            status == "portabilidade_aceita",
            "portabildiade aceita",
            ifelse(
              status == "portabilidade_criada",
              "portabilidade criada",
              ifelse(
                status == "portability_in_status__approved",
                "portability_in_status_approved",
                ifelse(
                  status == "cancelamento_solicitado",
                  "cancelamento solicitado",
                  ifelse(
                    status == "reques_status__confirmed",
                    "request_status_confirmed",
                    NA
                  )
                )
              )
            )
          )
        )
      )
    )
  )

port <- port |> mutate(status = factor(status))
summary(port$status)


# Organizando variável da razão de falha na portabilidade
# Primeiro vou separar em cancelado, negado e retido
port <- port |>
  mutate(falha = sub("_.*$", "", razao_falha)) |>
  mutate(falha = ifelse(falha == "pedido", "cancelado", falha))

#' Ficando somente com observações de portabilidade sucedidas e que possuem tanto
#' valor de RCO como saldo devedor
aux <- port |>
  filter(
    status == "sucesso" &
      !is.na(valor_rco) &
      !is.na(montante_portado) &
      !is.na(juros_prior)
  )

# Posso remover variáveis de sucesso/falha de portabilidade
aux <- aux |> select(-c(status, falha, razao_falha))


### EMPRESTIMOS GERAIS ====================================================================================================================================================

emp <- read_csv(
  "Emprestimos_updated.csv",
  col_types = cols(
    saldo_devedor = col_double(),
    taxa_de_juros = col_number(),
    data_de_contratacao = col_date(format = "%Y-%m-%d"),
    data_de_finalizacao_esperada = col_date(format = "%Y-%m-%d"),
    rating_credito = col_double(),
    rendimento_no_mes_de_contratacao = col_double()
  )
)
names(emp) <- c(
  "id",
  "cpf",
  "valor_total",
  "saldo_devedor",
  "juros",
  "index",
  "data_contrato",
  "data_fim",
  "tipo_colateral",
  "valor_colateral",
  "port_in",
  "port_out",
  "rating",
  "rendimento"
)

# Todos os empréstimos que passaram para gente são de crédito consignado, então vou ignorar algumas variáveis

emp <- emp |> select(-c(port_in, port_out, tipo_colateral, index))


### Juntar os dados gerais dos empréstimos com as portabilidades bem sucedidas
df <- merge(aux, emp, by = "id", all.x = TRUE)

#Criando spread de juros entre pré e pós portabilidade
df <- df |> mutate(spread = 100 * (juros - juros_prior)) # Quanto o juros mudou após a portabilidade

# Compatibilizando dados gerais entre análises ao
# remover outliers (top e bottom 1%) e
# remover observações com valores NA nas variáveis relevantes
df <- df |>
  filter(
    !is.na(data_port) &
      !is.na(if_prior) &
      !is.na(data_fim) &
      !is.na(rendimento) &
      !is.na(valor_total)
  )
df <- df |>
  filter(
    juros_prior >= quantile(juros_prior, 0.01) &
      juros_prior <= quantile(juros_prior, 0.99)
  ) # Removendo outliers de juros pré portabilidade
df <- df |>
  filter(juros >= quantile(juros, 0.01) & juros <= quantile(juros, 0.99)) # Removendo outliers de juros pré portabilidade


### PLOT - CUTOFFS E RCO ===================================================================================================================================================
ggplot(
  aux |>
    filter(
      data_port >= as.Date("2024-06-01") &
        data_port <= as.Date("2025-06-01") &
        montante_portado < 100000
    ),
  aes(x = montante_portado, y = valor_rco)
) +
  geom_point() +
  labs(
    title = "Montante Portado vs Valor RCO",
    x = "Montante Portado",
    y = "Valor RCO"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "Saldo-RCO.png",
  plot = last_plot(),
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)


ggplot(
  aux |>
    filter(
      data_port >= as.Date("2024-06-01") &
        data_port <= as.Date("2025-06-01") &
        montante_portado < 10000
    ),
  aes(x = montante_portado, y = valor_rco)
) +
  geom_point() +
  labs(
    title = "Montante Portado vs Valor RCO",
    x = "Montante Portado",
    y = "Valor RCO"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "Saldo-RCO 10mil.png",
  plot = last_plot(),
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)


# ESTATÍSTICA DESCRITIVA ===============================================================================================================================================

head(port)

str(port)

summary(port)

table(port$status)


# Função para formatar os juros como porcentagem com 2 casas decimais
format_percent <- function(x) {
  sprintf("%.2f", x * 100)
}

# Retirando os possíveis NAs que ainda estejam na
# coluna juros_prior além de retirar os 1% dos maiores valores (possíveis typos)
juros_prior_sem_na <- port$juros_prior[!is.na(port$juros_prior)]
limite_inferior_1_percentil <- quantile(
  juros_prior_sem_na,
  probs = 0.01,
  na.rm = TRUE
)
limite_superior_99_percentil <- quantile(
  juros_prior_sem_na,
  probs = 0.99,
  na.rm = TRUE
)

# Novo vetor/subconjunto de dados que exclui os valores acima do 99º percentil
juros_prior_com_trimming_central <- juros_prior_sem_na[
  juros_prior_sem_na >= limite_inferior_1_percentil &
    juros_prior_sem_na <= limite_superior_99_percentil
]

# Média após remover os 1% maiores
media_juros_prior_trimming <- mean(
  juros_prior_com_trimming_central,
  na.rm = TRUE
)
print(format_percent(media_juros_prior_trimming))

# Mediana após remover os 1% maiores
mediana_juros_prior <- median(juros_prior_com_trimming_central, na.rm = TRUE)
print(format_percent(mediana_juros_prior))

# Máximo após remover os 1% maiores
max_juros_prior_apos_trimming <- max(
  juros_prior_com_trimming_central,
  na.rm = TRUE
)
print(format_percent(max_juros_prior_apos_trimming))

# Mínimo
min_juros_prior <- min(juros_prior_com_trimming_central, na.rm = TRUE)
print(format_percent(min_juros_prior))

# 1º Quartil após remover os 1% maiores
primeiro_quartil_juros_prior_apos_trimming <- quantile(
  juros_prior_com_trimming_central,
  probs = 0.25,
  na.rm = TRUE
)
print(format_percent(primeiro_quartil_juros_prior_apos_trimming))

# 3º Quartil após remover os 1% maiores
terceiro_quartil_juros_prior_apos_trimming <- quantile(
  juros_prior_com_trimming_central,
  probs = 0.75,
  na.rm = TRUE
)
print(format_percent(terceiro_quartil_juros_prior_apos_trimming))

# Desvio Padrão após remover os 1% maiores
desvio_juros_prior_padrao_apos_trimming <- sd(
  juros_prior_com_trimming_central,
  na.rm = TRUE
)
print(format_percent(desvio_juros_prior_padrao_apos_trimming))

# Separando o dataframe port em diferentes dataframes conforme o status da portabilidade

# Status da portabilidade: sucesso (ignorando o trimming nos juros após o contrato)
port_sucesso <- port %>%
  filter(status == "sucesso")

# Taxa de juros_prior para o status sucesso:
# Retirando os possíveis NAs que ainda estejam na
# coluna juros_prior além de retirar os 1% dos maiores e menores valores (possíveis typos)
juros_prior_sem_na_sucesso <- port_sucesso$juros_prior[
  !is.na(port_sucesso$juros_prior)
]
limite_inferior_1_percentil_2 <- quantile(
  juros_prior_sem_na_sucesso,
  probs = 0.01,
  na.rm = TRUE
)
limite_superior_99_percentil_2 <- quantile(
  juros_prior_sem_na_sucesso,
  probs = 0.99,
  na.rm = TRUE
)

# Novo vetor/subconjunto de dados que exclui os valores acima do 99º percentil
juros_prior_com_trimming_central_sucesso <- juros_prior_sem_na_sucesso[
  juros_prior_sem_na_sucesso >= limite_inferior_1_percentil_2 &
    juros_prior_sem_na_sucesso <= limite_superior_99_percentil_2
]
# Média após remover os 1% maiores
media_juros_prior_trimming_sucesso <- mean(
  juros_prior_com_trimming_central_sucesso,
  na.rm = TRUE
)
print(format_percent(media_juros_prior_trimming_sucesso))

# Mediana após remover os 1% maiores
mediana_juros_prior_sucesso <- median(
  juros_prior_com_trimming_central_sucesso,
  na.rm = TRUE
)
print(format_percent(mediana_juros_prior_sucesso))

# Máximo após remover os 1% maiores
max_juros_sucesso_prior_apos_trimming <- max(
  juros_prior_com_trimming_central_sucesso,
  na.rm = TRUE
)
print(format_percent(max_juros_sucesso_prior_apos_trimming))

# Mínimo
min_juros_sucesso_prior <- min(
  juros_prior_com_trimming_central_sucesso,
  na.rm = TRUE
)
print(format_percent(min_juros_sucesso_prior))

# 1º Quartil após remover os 1% maiores
primeiro_quartil_juros_prior_apos_trimming_sucesso <- quantile(
  juros_prior_com_trimming_central_sucesso,
  probs = 0.25,
  na.rm = TRUE
)
print(format_percent(primeiro_quartil_juros_prior_apos_trimming_sucesso))

# 3º Quartil após remover os 1% maiores
terceiro_quartil_juros_prior_apos_trimming_sucesso <- quantile(
  juros_prior_com_trimming_central_sucesso,
  probs = 0.75,
  na.rm = TRUE
)
print(format_percent(terceiro_quartil_juros_prior_apos_trimming_sucesso))

# Desvio Padrão após remover os 1% maiores
desvio_juros_sucesso_prior_padrao_apos_trimming <- sd(
  juros_prior_com_trimming_central_sucesso,
  na.rm = TRUE
)
print(format_percent(desvio_juros_sucesso_prior_padrao_apos_trimming))

# Montante portado para o status sucesso:

# Média do montante portado
media_montante_portado_sucesso <- mean(
  port_sucesso$montante_portado,
  na.rm = TRUE
)
print(media_montante_portado_sucesso)

# Mediana
mediana_montante_portado_sucesso <- median(
  port_sucesso$montante_portado,
  na.rm = TRUE
)
print(mediana_montante_portado_sucesso)

# Máximo
max_montante_portado_sucesso <- max(port_sucesso$montante_portado, na.rm = TRUE)
print(max_montante_portado_sucesso)

# Mínimo
min_montante_portado_sucesso <- min(port_sucesso$montante_portado, na.rm = TRUE)
print(min_montante_portado_sucesso)

# 1º Quartil
primeiro_quartil_montante_portado_sucesso <- quantile(
  port_sucesso$montante_portado,
  probs = 0.25,
  na.rm = TRUE
)
print(primeiro_quartil_montante_portado_sucesso)

# 3º Quartil
terceiro_quartil_montante_portado_sucesso <- quantile(
  port_sucesso$montante_portado,
  probs = 0.75,
  na.rm = TRUE
)
print(terceiro_quartil_montante_portado_sucesso)

# Desvio Padrão
desvio_padrao_montante_portado_sucesso <- sd(
  port_sucesso$montante_portado,
  na.rm = TRUE
)
print(desvio_padrao_montante_portado_sucesso)

# RCO para o status sucesso:

# Média do montante portado
media_RCO_sucesso <- mean(port_sucesso$valor_rco, na.rm = TRUE)
print(media_RCO_sucesso)

# Mediana
mediana_RCO_sucesso <- median(port_sucesso$valor_rco, na.rm = TRUE)
print(mediana_RCO_sucesso)

# Máximo
max_RCO_sucesso <- max(port_sucesso$valor_rco, na.rm = TRUE)
print(max_RCO_sucesso)

# Mínimo
min_RCO_sucesso <- min(port_sucesso$valor_rco, na.rm = TRUE)
print(min_RCO_sucesso)

# 1º Quartil
primeiro_quartil_RCO_sucesso <- quantile(
  port_sucesso$valor_rco,
  probs = 0.25,
  na.rm = TRUE
)
print(primeiro_quartil_RCO_sucesso)

# 3º Quartil
terceiro_quartil_RCO_sucesso <- quantile(
  port_sucesso$valor_rco,
  probs = 0.75,
  na.rm = TRUE
)
print(terceiro_quartil_RCO_sucesso)

# Desvio Padrão
desvio_padrao_RCO_sucesso <- sd(port_sucesso$valor_rco, na.rm = TRUE)
print(desvio_padrao_RCO_sucesso)

# Agrupar por mês e contar o número de portabilidades
contagem_mensal <- port_sucesso %>%
  filter(!is.na(data_port)) %>%
  mutate(Mes_Ano = floor_date(data_port, "month")) %>%
  group_by(Mes_Ano) %>%
  summarise(Numero_Portabilidades = n()) %>%
  ungroup() %>%
  arrange(Mes_Ano)

# Gráfico do número de portabilidades mensais concluidas
ggplot(contagem_mensal, aes(x = Mes_Ano, y = Numero_Portabilidades)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Número de Portabilidades Concluídas Mensalmente",
    x = "Mês e Ano",
    y = "Número de Portabilidades"
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Status da portabilidade: falha
port_falha <- port %>%
  filter(status == "falha")

# Taxa de juros_prior para o status falha:
# Retirando os possíveis NAs que ainda estejam na
# coluna juros_prior além de retirar os 1% dos maiores valores (possíveis typos)
juros_prior_sem_na_falha <- port_falha$juros_prior[
  !is.na(port_falha$juros_prior)
]
limite_inferior_1_percentil_3 <- quantile(
  juros_prior_sem_na_falha,
  probs = 0.01,
  na.rm = TRUE
)
limite_superior_99_percentil_3 <- quantile(
  juros_prior_sem_na_falha,
  probs = 0.99,
  na.rm = TRUE
)

# Novo vetor/subconjunto de dados que exclui os valores acima do 99º percentil
juros_prior_com_trimming_central_falha <- juros_prior_sem_na_falha[
  juros_prior_sem_na_falha >= limite_inferior_1_percentil_3 &
    juros_prior_sem_na_falha <= limite_superior_99_percentil_3
]

# Média após remover os 1% maiores
media_juros_prior_trimming_falha <- mean(
  juros_prior_com_trimming_central_falha,
  na.rm = TRUE
)
print(format_percent(media_juros_prior_trimming_falha))

# Mediana após remover os 1% maiores
mediana_juros_prior_falha <- median(
  juros_prior_com_trimming_central_falha,
  na.rm = TRUE
)
print(format_percent(mediana_juros_prior_falha))

# Máximo após remover os 1% maiores
max_juros_falha_prior_apos_trimming <- max(
  juros_prior_com_trimming_central_falha,
  na.rm = TRUE
)
print(format_percent(max_juros_falha_prior_apos_trimming))

# Mínimo
min_juros_falha_prior <- min(
  juros_prior_com_trimming_central_falha,
  na.rm = TRUE
)
print(format_percent(min_juros_falha_prior))

# 1º Quartil após remover os 1% maiores
primeiro_quartil_juros_prior_apos_trimming_falha <- quantile(
  juros_prior_com_trimming_central_falha,
  probs = 0.25,
  na.rm = TRUE
)
print(format_percent(primeiro_quartil_juros_prior_apos_trimming_falha))

# 3º Quartil após remover os 1% maiores
terceiro_quartil_juros_prior_apos_trimming_falha <- quantile(
  juros_prior_com_trimming_central_falha,
  probs = 0.75,
  na.rm = TRUE
)
print(format_percent(terceiro_quartil_juros_prior_apos_trimming_falha))

# Desvio Padrão após remover os 1% maiores
desvio_juros_falha_prior_padrao_apos_trimming <- sd(
  juros_prior_com_trimming_central_falha,
  na.rm = TRUE
)
print(format_percent(desvio_juros_falha_prior_padrao_apos_trimming))

# Contando as ocorrências de cada razão de falha e retirando eventuais NAs
contagem_falhas_percentual <- port_falha %>%
  filter(!is.na(falha)) %>%
  count(falha, sort = TRUE, name = "Frequencia") %>%
  mutate(Percentual = Frequencia / sum(Frequencia))

# Gráfico de barras horizontal
ggplot(
  contagem_falhas_percentual,
  aes(x = Percentual, y = fct_reorder(falha, Percentual))
) +
  geom_col(fill = "steelblue") +
  # Adicionando o número absoluto de ocorrências em cima da barra
  geom_text(
    aes(label = scales::comma(Frequencia)),
    hjust = -0.1, # Ajustando a posição horizontal
    size = 3.5, # Ajustando o tamanho do texto
    color = "black"
  ) + # Ajustando a cor do texto
  labs(
    title = "Percentual dos Principais Motivos de Falhas na Portabilidade",
    x = "Percentual",
    y = "Razão da Falha"
  ) +
  scale_x_continuous(
    labels = percent,
    expand = expansion(mult = c(0, 0.15))
  ) + # Para fazer caber os rótulos
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 9)
  )

# Contando cada status
contagem_status <- port %>%
  group_by(status) %>%
  summarise(Frequencia = n()) %>%
  ungroup()

# Calculando o percentual de cada status
percentual_status <- contagem_status %>%
  mutate(Percentual = (Frequencia / sum(Frequencia)) * 100)
print(percentual_status)


# Avaliando as portabilidades out

port_out <- port %>%
  filter(in_out == "portabilidade-out")

# Juros_prior

# Mínimo, 1ºQuart, mediana, média, 3ºQuart, Máximo
print(summary(port_out$juros_prior))

# Desvio Padrão
desvio_padrao_juros_prior <- sd(port_out$juros_prior, na.rm = TRUE)
print(format_percent(desvio_padrao_juros_prior))

# Montante portado

# Média do montante portado
media_montante_portado_out <- mean(port_out$montante_portado, na.rm = TRUE)
print(media_montante_portado_out)

# Mediana
mediana_montante_portado_out <- median(port_out$montante_portado, na.rm = TRUE)
print(mediana_montante_portado_out)

# Máximo
max_montante_portado_out <- max(port_out$montante_portado, na.rm = TRUE)
print(max_montante_portado_out)

# Mínimo
min_montante_portado_out <- min(port_out$montante_portado, na.rm = TRUE)
print(min_montante_portado_out)

# 1º Quartil
primeiro_quartil_montante_portado_out <- quantile(
  port_out$montante_portado,
  probs = 0.25,
  na.rm = TRUE
)
print(primeiro_quartil_montante_portado_out)

# 3º Quartil
terceiro_quartil_montante_portado_out <- quantile(
  port_out$montante_portado,
  probs = 0.75,
  na.rm = TRUE
)
print(terceiro_quartil_montante_portado_out)

# Desvio Padrão
desvio_padrao_montante_portado <- sd(port_out$montante_portado, na.rm = TRUE)
print(desvio_padrao_montante_portado)

# Montante total

# Mínimo, 1ºQuart, mediana, média, 3ºQuart, Máximo
print(summary(port_out$montante_total))

# Desvio Padrão
desvio_padrao_montante_total <- sd(port_out$montante_total, na.rm = TRUE)
print(desvio_padrao_montante_total)


#Rendimento:

# Mínimo, 1ºQuart, mediana, média, 3ºQuart, Máximo
print(summary(emp$rendimento))

# Desvio Padrão
desvio_padrao_rendimento <- sd(emp$rendimento, na.rm = TRUE)
print(desvio_padrao_rendimento)

#Rating:

# Mínimo, 1ºQuart, mediana, média, 3ºQuart, Máximo
print(summary(emp$rating))

# Desvio Padrão
desvio_padrao_rating <- sd(emp$rating, na.rm = TRUE)
print(desvio_padrao_rating)

# Valor Total:

# Mínimo, 1ºQuart, mediana, média, 3ºQuart, Máximo
print(summary(emp$valor_total))

# Desvio Padrão
desvio_padrao_valor_total <- sd(emp$valor_total, na.rm = TRUE)
print(desvio_padrao_valor_total)

# Saldo Devedor:

# Mínimo, 1ºQuart, mediana, média, 3ºQuart, Máximo
print(summary(emp$saldo_devedor))

# Desvio Padrão
desvio_padrao_saldo_devedor <- sd(emp$saldo_devedor, na.rm = TRUE)
print(desvio_padrao_saldo_devedor)

# Juros

juros_sem_na <- emp$juros[!is.na(emp$juros)]

# Média
media_juros_trim <- mean(juros_sem_na, trim = 0.01, na.rm = TRUE)
print(media_juros_trim * 100)

# 1. Calcular os limites para o trimming de 1% em cada cauda
#    - Limite inferior: 1º percentil
limite_inferior <- quantile(juros_sem_na, probs = 0.01, na.rm = TRUE)

#    - Limite superior: 99º percentil
limite_superior <- quantile(juros_sem_na, probs = 0.99, na.rm = TRUE)

# 2. Criar o subconjunto de dados com os 98% centrais
juros_trim <- juros_sem_na[
  juros_sem_na >= limite_inferior &
    juros_sem_na <= limite_superior
]

#Mínimo
min_juros_trim <- min(juros_trim, na.rm = TRUE)
print(min_juros_trim * 100)

# Q1
q1_juros_trim <- quantile(juros_trim, probs = 0.25, na.rm = TRUE)
print(q1_juros_trim * 100)

# Mediana
mediana_juros_trim <- median(juros_trim, na.rm = TRUE)
print(mediana_juros_trim * 100)

# Q3
q3_juros_trim <- quantile(juros_trim, probs = 0.75, na.rm = TRUE)
print(q3_juros_trim * 100)

# Máximo
max_juros_trim <- max(juros_trim, na.rm = TRUE)
print(max_juros_trim * 100)

# Desvio Padrão
desvio_padrao_juros_trim <- sd(juros_trim, na.rm = TRUE)
print(desvio_padrao_juros_trim * 100)


#Prazo:

# Prazo esperado (dias)
emp$prazo_esperado_dias <- difftime(
  emp$data_fim,
  emp$data_contrato,
  units = "days"
)

# Convertendo o difftime para número
emp$prazo_esperado_dias <- as.numeric(emp$prazo_esperado_dias)

# Convertendo para meses
emp$prazo_esperado_meses <- emp$prazo_esperado_dias / 30

# Mínimo, 1ºQuart, mediana, média, 3ºQuart, Máximo
print(summary(emp$prazo_esperado_meses))

# Desvio Padrão
desvio_padrao_prazo <- sd(emp$prazo_esperado_meses, na.rm = TRUE)
print(desvio_padrao_prazo)


#Proporção de portabilidades de saída(S1, S2, S3, S4 e S5)

# Verificando insituições
table(port_out$if_prior)


definicao_s_categorias <- tibble::tribble(
  ~if_prior,
  ~categoria_s,
  "Banco Bradesco SA",
  "S1",
  "Banco do Brasil SA",
  "S1",
  "Banco Itau Unibanco SA",
  "S1",
  "Banco Santander (Brasil) S.A",
  "S1",
  "Caixa Economica Federal",
  "S1",

  "Banco do Estado do Rio Grande do Sul SA",
  "S2",
  "Banco Safra SA",
  "S2",

  "Banco Agibank S.A.",
  "S3",
  "Banco BMG SA",
  "S3",
  "Banco C6 Consignado S.A",
  "S3",
  "Banco Daycoval SA",
  "S3",
  "Banco do Estado do Espírito Santo",
  "S3",
  "Banco Inter S.A",
  "S3",
  "PICPAY BANK - BANCO MULTIPLO S.A",
  "S3",
  "PICPAY BANK - BANCO MULTIPLO S.A.",
  "S3",
  "BRB Credito Financiamento e Investimento",
  "S3",
  "PARATI CRÉDITO FINANCIAMENTO E INVESTIMENTO S/A",
  "S3",

  "Banco Inbursa S.A.",
  "S4",
  "BancoSeguro S/A",
  "S4",
  "Cooperativa de Crédito, Poupança e Investimento Centro Sul",
  "S4",
  "Cooperativa de Crédito, Poupança e Investimento de Lajeado",
  "S4",
  "Cooperativa de Crédito, Poupança e Investimento do Norte e Nordeste de Santa Catarina",
  "S4",
  "Cooperativa de Crédito, Poupança e Investimento Integração de Estados do Rio Grande do Sul, Santa Catarina e Minas Gerais - Sicredi Integração de Estados RS SC MG",
  "S4",
  "Cooperativa de Crédito, Poupança e Investimento União dos Estados de Mato Grosso do Sul, Tocantins e Oeste da Bahia",
  "S4",
  "Facta Financeira S/A, Crédito Financiamento e Investimento",
  "S4",
  "Cooperativa de Crédito e Investimento Liberdade - Sicredi Liberdade",
  "S4",
  "Parana Banco S.A",
  "S4",
  "Cooperativa de Crédito, Poupança e Investimento Região das Culturas",
  "S4",
  "QI TECH",
  "S4",

  "Cooperativa de Crédito de Livre Admissão de Belo Horizonte e Cidades Pólo do Estado de Minas Gerais Ltda",
  "S5",
  "COOPERATIVA DE CRÉDITO DE LIVRE ADMISSÃO DO MÉDIO PIRACICABA E DO CIRCUITO DO OURO LTDA",
  "S5"
)

# Verificando o dataframe de mapeamento
print(definicao_s_categorias)

# Definindo a ordem desejada para as categorias S para o gráfico
ordem_s <- c("S1", "S2", "S3", "S4", "S5")

# Unindo as categorias S ao dataframe de portabilidade-out
port_out_com_s <- port_out %>%
  left_join(definicao_s_categorias, by = "if_prior")

# Tratando NAs na nova coluna 'categoria_s' (instituições não mapeadas)
# Isso inclui o 'null' se ele não foi mapeado e virou NA após o join
port_out_com_s$categoria_s[is.na(port_out_com_s$categoria_s)] <- "Não Mapeado"

# Calculando as proporções para port_out (excluindo 'Não Mapeado' do percentual)
proporcoes_out_s <- port_out_com_s %>%
  filter(categoria_s != "Não Mapeado") %>% # Excluindo a categoria "Não Mapeado"
  group_by(categoria_s) %>%
  summarise(Frequencia = n()) %>%
  ungroup() %>%
  mutate(Percentual = Frequencia / sum(Frequencia)) %>%
  # Converter para fator com a ordem desejada para o gráfico
  mutate(categoria_s = factor(categoria_s, levels = ordem_s)) %>%
  arrange(desc(Percentual)) # Ordenando do maior para o menor percentual

cat(
  "\n--- Proporções de Categorias S para Portabilidades-OUT (Origem - sem 'Não Mapeado') ---\n"
)
print(proporcoes_out_s)

# --- Visualização em Gráfica para port_out ---

ggplot(
  proporcoes_out_s,
  aes(x = categoria_s, y = Percentual, fill = categoria_s)
) +
  geom_col(color = "white") +
  # Adicionando o número absoluto de transações (Frequencia) em cima da barra
  geom_text(
    aes(label = comma(Frequencia)), # Número com vírgulas para milhares
    vjust = -0.5, # Ajustando a posição vertical para cima da barra
    size = 3.5, # Ajustando o tamanho do texto
    color = "black"
  ) + # Ajustando a cor do texto
  labs(
    title = "Proporção de Portabilidades OUT por Categoria S da Instituição de Origem",
    x = "Categoria da Instituição (S1-S5)",
    y = "Percentual",
    fill = "Categoria S" # Legenda
  ) +
  scale_y_continuous(labels = percent) + # Formatando o eixo Y como percentual
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # Centralizando o título
    axis.text.x = element_text(angle = 0, hjust = 0.5) # Rótulos do eixo X sem rotação
  )


#Proporção de portabilidades de entrada(S1, S2, S3, S4 e S5)
port_in <- port %>%
  filter(in_out == "portabilidade-in")

# Verificando insituições
table(port_in$if_prior)


# 1. Tabela de mapeamento dos IDs das instituições para suas Categorias S
definicao_s_categorias <- tibble::tribble(
  ~if_prior,
  ~categoria_s,
  # S1
  "00000000000191",
  "S1", # BB
  "00360305000104",
  "S1", # CAIXA ECONÔMICA FEDERAL
  "07207996000150",
  "S1", # BRADESCO
  "30306294000145",
  "S1", # BTG PACTUAL
  "33885724000119",
  "S1", # ITAU
  "60701190000104",
  "S1", # ITAU
  "60746948000112",
  "S1", # BRADESCO
  "90400888000142",
  "S1", # SANTANDER

  # S2
  "07237373000120",
  "S2", # BCO DO NORDESTE DO BRASIL S.A.
  "33479023000180",
  "S2", # CITIBANK
  "58160789000128",
  "S2", # SAFRA
  "59588111000103",
  "S2", # VOTORANTIM
  "92702067000196",
  "S2", # BANRISUL
  "01181521000155",
  "S2", # BCO COOPERATIVO SICREDI
  "02038232000164",
  "S2", # BANCOOB

  # S3
  "00416968000101",
  "S3", # INTER
  "00000208000100",
  "S3", # BRB Credito Financiamento e Investimento
  "00643742000135",
  "S3", # FHE APE POUPEX
  "01522368000182",
  "S3", # BNP PARIBAS
  "03311443000191",
  "S3", # Parati
  "04862600000110",
  "S3", # PORTO SEGURO
  "04902979000144",
  "S3", # BCO DA AMAZONIA S.A.
  "05463212000129",
  "S3", # COOPERATIVA CENTRAL DE CRÉDITO - AILOS
  "04913711000108",
  "S3", # BCO DO EST. DO PA S.A.
  "08357240000150",
  "S3", # BCO CSF S.A.
  "09516419000175",
  "S3", # PICPAY BANK - BANCO MULTIPLO S.A
  "10664513000150",
  "S3", # AGIBANK
  "11970623000103",
  "S3", # SENFFNET IP
  "17184037000110",
  "S3", # MERCANTIL DO BRASIL
  "28127603000178",
  "S3", # BANESTES
  "28195667000106",
  "S3", # ABC-BRASIL
  "33040601000187",
  "S3", # MERCANTIL FINANCEIRA S.A
  "33136888000143",
  "S3", # BRB
  "33923798000100",
  "S3", # BANCO MASTER
  "58616418000108",
  "S3", # BCO FIBRA S.A.
  "59285411000113",
  "S3", # PAN
  "61186680000174",
  "S3", # BMG
  "61348538000186",
  "S3", # BANCO C6
  "62144175000120",
  "S3", # PINE
  "62232889000190",
  "S3", # BCO DAYCOVAL S.A
  "71027866000134",
  "S3", # BS2
  "92894922000108",
  "S3", # ORIGINAL
  "03323840000183",
  "S3", # ALFA

  # S4
  "00315557000111",
  "S4", # CONFEDERAÇÃO NACIONAL DAS COOPERATIVAS CENTRAIS UNICRED LTDA. - UNICRED DO BRASIL.
  "00517645000104",
  "S4", # BANCO RIBEIRÃO PRETO
  "00556603000174",
  "S4", # BANCO BARI DE INVESTIMENTO E FINANCI.
  "00795423000145",
  "S4", # BANCO SEMEAR
  "02923389000172",
  "S4", # COOPERATIVA DE CRÉDITO, POUPANÇA E INVESTIMENTO SICREDI SERIGY - SICREDI SERIGY
  "03000142000147",
  "S4", # COOPERATIVA DE CRÉDITO, POUPANÇA E INVESTIMENTO DOS POLICIAIS FEDERAIS DO RIO GRANDE DO SUL E SANTA CATARINA - SICREDI POL RS/SC
  "03046391000173",
  "S4", # UNIPRIME CENTRAL NACIONAL
  "03428338000137",
  "S4", # SICREDI CREDUNI
  "03566655000110",
  "S4", # COOPERATIVA DE CRÉDITO, POUPANÇA E INVESTIMENTO CELEIRO CENTRO OESTE - SICREDI CELEIRO CENTRO OESTE
  "04237413000145",
  "S4", # COOPERATIVA DE CRÉDITO DO VALE DO SÃO FRANCISCO - SICREDI VALE DO SÃO FRANCISCO
  "04814563000174",
  "S4", # AFINZ IP
  "04866275000163",
  "S4", # BCO INBURSA S.A.
  "05351887000186",
  "S4", # ZEMA CFI
  "05545390000107",
  "S4", # COOPERATIVA DE CRÉDITO - SICREDI COOMAMP
  "05676026000178",
  "S4", # CREDIARE CFI
  "07512441000111",
  "S4", # HS FINANCEIRA
  "07652226000116",
  "S4", # LECCA
  "07679404000100",
  "S4", # BANCO TOPÁZIO S.A.
  "08143326000180",
  "S4", # COOPERATIVA DE CRÉDITO DOS MÉDICOS E PROFISSIONAIS DE SAÚDE DE SÃO LUÍS - SICREDI SÃO LUÍS
  "09343038000131",
  "S4", # COOPERATIVA DE CRÉDITO SICREDI ALTO SERTÃO PARAIBANO - SICREDI ALTO SERTÃO PARAIBANO
  "10264663000177",
  "S4", # PAGSEGURO
  "10398952000169",
  "S4", # CONFEDERAÇÃO NACIONAL DAS COOPERATIVAS CENTRAIS DE CRÉDITO E ECONOMIA FAMILIAR E SOLIDÁRIA - CRESOL CONFEDERAÇÃO
  "13009717000146",
  "S4", # BCO DO EST. DE SE S.A.
  "14388334000199",
  "S4", # PARANÁ BANCO
  "15581638000130",
  "S4", # FACTA S.A. CFI
  "24654881000122",
  "S4", # COOPERATIVA DE CRÉDITO, POUPANÇA E INVESTIMENTO UNIÃO DOS ESTADOS DE MATO GROSSO DO SUL, TOCANTINS E OESTE DA BAHIA - SICREDI UNIÃO MS/TO
  "26408161000102",
  "S4", # COOPERATIVA DE CRÉDITO, POUPANÇA E INVESTIMENTO DO CENTRO SUL DO MATO GROSSO DO SUL E BAHIA - SICREDI CENTRO-SUL MS/BA
  "27214112000100",
  "S4", # AL5 S.A. CFI
  "31895683000116",
  "S4", # INDUSTRIAL DO BRASIL
  "32402502000135",
  "S4", # QI SCD
  "33603457000140",
  "S4", # RODOBENS
  "35571249000131",
  "S4", # COOPERATIVA DE CRÉDITO, POUPANÇA E INVESTIMENTO SICREDI EVOLUÇÃO - SICREDI EVOLUÇÃO
  "36583700000101",
  "S4", # QISTA S.A. CFI
  "54403563000150",
  "S4", # BCO ARBI S.A.
  "59118133000100",
  "S4", # BCO LUSO BRASILEIRO S.A.
  "61033106000186",
  "S4", # CREFISA S.A. CFI
  "61820817000109",
  "S4", # BANCO PAULISTA
  "70241658000170",
  "S4", # SICREDI RECIFE
  "72257793000130",
  "S4", # SICREDI CEARÁ
  "74828799000145",
  "S4", # NOVO BCO CONTINENTAL S.A. - BM
  "80271455000180",
  "S4", # BANCO RNX S.A.
  "83315408000180",
  "S4", # SICREDI NORTE
  "92874270000140",
  "S4", # BANCO DIGIMAIS

  # S5
  "00122327000136",
  "S5", # Santinvest
  "00694877000120",
  "S5", # COOPERATIVA DE CRÉDITO DE LIVRE ADMISSÃO DO BRASIL CENTRAL LTDA. - SICOOB EXECUTIVO
  "00952415000165",
  "S5", # COOPERATIVA DE CRÉDITO DE LIVRE ADMISSÃO CREDFAZ LTDA. - SICOOB CREDFAZ
  "01644264000140",
  "S5", # COOPERATIVA DE CRÉDITO DE LIVRE ADMISSÃO DO MÉDIO PIRACICABA, CIRCUITO DO OURO, GRANDE BH E RIO GRANDE DO NORTE LTDA. - SICOOB CREDIMEPI
  "01658426000108",
  "S5", # COOPERFORTE
  "01727929000180",
  "S5", # COOPERATIVA DE CRÉDITO UNICRED EVOLUÇÃO LTDA. - UNICRED EVOLUÇÃO
  "01760242000146",
  "S5", # COOPERATIVA DE CRÉDITO DE LIVRE ADMISSÃO DE BELO HORIZONTE E CIDADES POLO DO ESTADO DE MINAS GERAIS LTDA. - SICOOB NOSSACOOP
  "02000895000190",
  "S5", # COOPERATIVA DE ECONOMIA E CRÉDITO MÚTUO DOS SERVIDORES DO PODER JUDICIÁRIO
  "02275781000152",
  "S5", # COOPERATIVA DE CRÉDITO MÚTUO DOS SERVIDORES FEDERAIS NA PARAÍBA - SICOOB COOPERCRET
  "02338666000180",
  "S5", # COOPERATIVA DE CRÉDITO DE LIVRE ADMISSÃO CREDIEMBRAPA LTDA - SICOOB.
  "02382755000123",
  "S5", # COOPERATIVA DE CREDITO POTIGUAR - SICOOB POTIGUAR
  "02493000000104",
  "S5", # COOPERATIVA DE ECONOMIA E CRÉDITO MÚTUO NOS ESTADOS DE ALAGOAS, SERGIPE E BAHIA - SICOOB LESTE
  "02674113000106",
  "S5", # COOPERATIVA DE ECONOMIA E CRÉDITO MÚTUO DOS SERVIDORES PÚBLICOS - SICOOB COOPERPLAN CREDSEF
  "02794761000198",
  "S5", # COOPERATIVA DE ECONOMIA E CRÉDITO MÚTUO DE LIVRE ADMISSÃO DOS SERVIDORES DA UNIVERSIDADE FEDERAL DE VIÇOSA LTDA. - SICOOB UFVCREDI
  "02931668000188",
  "S5", # COOPERATIVA DE CRÉDITO DE LIVRE ADMISSÃO DO ESTADO DO RIO DE JANEIRO - SICOOB FLUMINENSE
  "03102185000133",
  "S5", # COOPERATIVA DE CRÉDITO DOS SERVIDORES DA UNIÃO, EMPRESÁRIOS, PROFISSIONAIS AUTÔNOMOS E LIBERAIS - SICOOB CENTRO NORDESTE
  "03320525000100",
  "S5", # COOPERATIVA DE CRÉDITO ARACOOP LTDA. - SICOOB ARACOOP
  "03326437000108",
  "S5", # COOPERATIVA DE CRÉDITO DE LIVRE ADMISSÃO DO CENTRO NORTE DOS ESTADOS DE MATO GROSSO E MATO GROSSO DO SUL - SICOOB UNIÃO MT/MS
  "03419786000174",
  "S5", # COOPERATIVA DE CRÉDITO NOSSA SENHORA DO DESTERRO - SICOOB CREDISC
  "03620772000114",
  "S5", # COOPERATIVA DE CRÉDITO MÚTUO DO CEARÁ - SICOOB CEARÁ
  "03639902000160",
  "S5", # COOPERATIVA DE ECONOMIA E CRÉDITO MÚTUO DOS SERVIDORES PÚBLICOS DO PODER EXECUTIVO FEDERAL NO ESTADO DO ESPÍRITO SANTO - CREDES
  "03732359000141",
  "S5", # COOPERATIVA DE CRÉDITO DE LIVRE ADMISSÃO DE PERNAMBUCO - SICOOB PERNAMBUCO
  "03881423000156",
  "S5", # SOCINAL S.A. CFI
  "04355489000175",
  "S5", # COOPERATIVA DE CRÉDITO UNICRED COOMARCA LTDA. - COOMARCA
  "04715685000103",
  "S5", # COOPERATIVA DE CRÉDITO MÚTUO DOS DESPACHANTES DE TRÂNSITO DE SANTA CATARINA E RIO GRANDE DO SUL - SICOOB CREDITRAN
  "04772908000174",
  "S5", # ATLANTA SCM LTDA
  "05203605000101",
  "S5", # COOPERATIVA DE CRÉDITO DA AMAZÔNIA - SICOOB AMAZÔNIA
  "05241619000101",
  "S5", # COOPERATIVA DE CRÉDITO SICOOB PRIMAVERA - SICOOB PRIMAVERA
  "05888589000120",
  "S5", # COOPERATIVA DE CRÉDITO SUL - SICOOB SUL
  "21332862000191",
  "S5", # CARTOS SCD
  "27302181000167",
  "S5", # COOPERATIVA DE CREDITO DOS SERVIDORES DA UNIVERSIDADE FEDERAL DO ESPIRITO SANTO
  "32615247000109",
  "S5", # COOPERATIVA DE CREDITO SICOOB COOPEC LTDA.
  "33634999000180",
  "S5", # COMPREV CFI
  "37076205000160",
  "S5", # COOPERATIVA DE CRÉDITO DE LIVRE ADMISSÃO DE ASSOCIADOS LTDA. - SICOOB JUDICIÁRIO
  "62109566000103",
  "S5", # CREDISAN COOPERATIVA DE CRÉDITO
  "71336432000116",
  "S5", # SICOOB CREDIMED
  "73092827000146",
  "S5", # COOPERATIVA DE CRÉDITO MÚTUO DOS SERVIDORES DO MINISTERIO DA EDUCAÇÃO EM SÃO PAULO - COOPEMESP
  "92843531000164",
  "S5", # ASPECIR SCMEPP LTDA

  # IDs listados como "Null" (levantamento manual)
  "04184779000101",
  "Não Mapeado",
  "07450604000189",
  "Não Mapeado",
  "08071645000127",
  "Não Mapeado",
  "14511781000193",
  "Não Mapeado",
  "17167412000113",
  "Não Mapeado",
  "17312597000102",
  "Não Mapeado",
  "21242451000105",
  "Não Mapeado",
  "27098060000145",
  "Não Mapeado",
  "29961505000102",
  "Não Mapeado",
  "32409227000181",
  "Não Mapeado",
  "41180092000116",
  "Não Mapeado",
  "42150987000170",
  "Não Mapeado",
  "71371686000175",
  "Não Mapeado",
  "87163234000138",
  "Não Mapeado",
  "88747928000185",
  "Não Mapeado",
  "92812098000108",
  "Não Mapeado",
  "95619003000114",
  "Não Mapeado"
)

# Definindo a ordem desejada para as categorias S para o gráfico
ordem_s <- c("S1", "S2", "S3", "S4", "S5")

# --- Para port_in (Instituições de Destino) ---

# Removendo espaços em branco (invisíveis ou não) do início/fim dos IDs.
port_in$if_prior <- str_trim(as.character(port_in$if_prior))


# Unindo as categorias S ao dataframe de portabilidade-in
port_in_com_s <- port_in %>%
  left_join(definicao_s_categorias, by = "if_prior")

# Tratando NAs na nova coluna 'categoria_s' (instituições não mapeadas)
port_in_com_s$categoria_s[is.na(port_in_com_s$categoria_s)] <- "Não Mapeado"

cat("\n--- DIAGNÓSTICO: Verificando as categorias após a junção ---\n")
print(summary(factor(port_in_com_s$categoria_s)))

cat(
  "\n--- DIAGNÓSTICO: IDs em 'port_in$if_prior' que NÃO foram encontrados no seu mapa ---\n"
)
ids_nao_mapeados_em_port_in <- setdiff(
  unique(port_in$if_prior),
  unique(definicao_s_categorias$if_prior)
)
print(head(ids_nao_mapeados_em_port_in, 20))
cat(
  "Número total de IDs não mapeados em 'port_in':",
  length(ids_nao_mapeados_em_port_in),
  "\n"
)

cat(
  "\n--- DIAGNÓSTICO: Número total de linhas em 'port_in' antes de qualquer filtragem ---\n"
)
print(nrow(port_in))

# --- Cálculo e Visualização ---

# Calculando as proporções para port_in (excluindo 'Não Mapeado' do percentual)
proporcoes_in_s <- port_in_com_s %>%
  filter(categoria_s != "Não Mapeado") %>%
  group_by(categoria_s) %>%
  summarise(Frequencia = n()) %>%
  ungroup() %>%
  mutate(Percentual = Frequencia / sum(Frequencia)) %>%
  mutate(categoria_s = factor(categoria_s, levels = ordem_s)) %>%
  arrange(desc(Percentual))

cat(
  "\n--- Proporções de Categorias S para Portabilidades-IN (Destino - sem 'Não Mapeado') ---\n"
)
print(proporcoes_in_s)

# Visualização gráfica para port_in
ggplot(
  proporcoes_in_s,
  aes(x = categoria_s, y = Percentual, fill = categoria_s)
) +
  geom_col(color = "white") +
  # Adicionando o número absoluto de transações (Frequencia) em cima da barra
  geom_text(
    aes(label = comma(Frequencia)), # Formatando o número com
    vjust = -0.5, # Ajustando a posição vertical para cima da barra
    size = 3.5, # Ajustando o tamanho do texto
    color = "black"
  ) + # Ajustando a cor do texto
  labs(
    title = "Proporção de Portabilidades IN por Categoria S da Instituição de Destino",
    x = "Categoria da Instituição (S1-S5)",
    y = "Percentual",
    fill = "Categoria S"
  ) +
  scale_y_continuous(labels = percent) + # Formatando o eixo Y como percentual
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )


# --- 1. Preparando os dados para o plot ---

# Convertendo o vetor juros_prior_sucesso_sem_1_porcento_maiores para um tibble
dados_juros_prior <- tibble(
  taxa_juros = juros_prior_com_trimming_central_sucesso,
  tipo_juros = "Juros Anterior (Portabilidade Sucesso - Truncado)"
)

# Convertendo a coluna emp$juros para um tibble
# Filtrando NAs diretamente aqui
dados_juros_posterior <- tibble(
  taxa_juros = juros_trim, # Remove NAs de emp$juros
  tipo_juros = "Juros Posterior (Base Geral 'df')"
)

# Combinando os dois dataframes
dados_para_kde <- bind_rows(dados_juros_prior, dados_juros_posterior)

# --- 2. Criando o gráfico de densidade de kernel ---

ggplot(
  dados_para_kde,
  aes(x = taxa_juros, color = tipo_juros, fill = tipo_juros)
) +
  geom_density(alpha = 0.4, bw = 0.0001) + # 'alpha' para transparência
  labs(
    title = "Comparação da Densidade das Taxas de Juros",
    x = "Taxa de Juros (% a.m.)",
    y = "Densidade",
    color = "Tipo de Juro", # Título da legenda para a cor da linha
    fill = "Tipo de Juro" # Título da legenda para a cor do preenchimento
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  theme_minimal() + # Um tema limpo para o gráfico
  theme(
    plot.title = element_text(hjust = 0.5) # Centralizando o título
  )


# --- 1. Preparar os dados para o plot (igual ao anterior) ---
dados_juros_prior <- tibble(
  taxa_juros = juros_prior_com_trimming_central_sucesso,
  tipo_juros = "Juros Anterior (Portabilidade Sucesso - Truncado)"
)

dados_juros_posterior <- tibble(
  taxa_juros = juros_trim,
  tipo_juros = "Juros Posterior (Base Geral 'emp')"
)

dados_para_kde <- bind_rows(dados_juros_prior, dados_juros_posterior)

# --- 2. Criar o gráfico de densidade de kernel ---

ggplot(
  dados_para_kde,
  aes(x = taxa_juros, color = tipo_juros, fill = tipo_juros)
) +
  geom_density(alpha = 0.4, bw = 0.001) +
  labs(
    title = "Comparação da Densidade das Taxas de Juros",
    subtitle = "Destacando a discretização dos valores",
    x = "Taxa de Juros (% a.m.)",
    y = "Densidade",
    color = "Tipo de Juro",
    fill = "Tipo de Juro"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )


# TESTE RDD ===============================================================================================================================================================
# Testando um Sharp RDD "simples" em torno do primeiro cutoff

# -> Incluindo todas as observações
df_aux <- df |>
  filter(
    montante_portado <= 2000 |
      (montante_portado >= 2000 & montante_portado < 4000)
  )

# -> Olhando apenas para quem fez a portabilidade nos primeiros 20% da duração do empréstimo
df_aux_cut <- df |>
  filter(
    (montante_portado <= 2000 & valor_rco >= 0.8 * 216) |
      (montante_portado >= 2000 &
        montante_portado < 4000 &
        valor_rco > 0.8 * 260)
  )

### Construindo vetor de covariadas
Z = cbind(
  df_aux$rating,
  df_aux$rendimento,
  df_aux$valor_total,
  df_aux$valor_colateral
)
Z_cut = cbind(
  df_aux_cut$rating,
  df_aux_cut$rendimento,
  df_aux_cut$valor_total,
  df_aux_cut$valor_colateral
)

### Todas as Obs
rdd <- rdrobust(
  y = df_aux$juros,
  x = df_aux$montante_portado,
  p = 1,
  c = 2000,
  kernel = "triangular",
  bwselect = "mserd",
  covs = Z
)

summary(rdd)

rdd2 <- rdrobust(
  y = df_aux$spread,
  x = log(df_aux$montante_portado),
  c = log(2000),
  p = 1,
  kernel = "triangular",
  bwselect = "mserd",
  covs = Z
)

summary(rdd2)


### Só upper quintile
rdd_upper <- rdrobust(
  y = df_aux_cut$spread,
  x = df_aux_cut$montante_portado,
  c = 2000,
  p = 1,
  kernel = "triangular",
  bwselect = "mserd",
  covs = Z_cut
)

summary(rdd_upper)


# Plotando o RDD
p <- rdplot(
  y = df_aux$spread,
  x = df_aux$montante_portado,
  c = 2000,
  p = 1,
  h = rdd$bws[1],
  kernel = "triangular",
  binselect = "qs",
  subset = (df_aux$montante_portado >= (2000 - rdd$bws[1]) &
    df_aux$montante_portado < 2000) |
    (df_aux$montante_portado >= 2000 &
      df_aux$montante_portado <= (2000 + rdd$bws[1])),
  y.lim = c(-0.4, -0.2),
  x.lim = c(2000 - rdd$bws[1], 2000 + rdd$bws[1]),
  x.label = "Montante Portado",
  y.label = "Spread de Juros",
  title = "RDD - Cutoff de R$2000"
)


rm(rdd, rdd_upper)


# Loop RDDs ================================================================================================================================================================
# Listas para armazenar múltiplos RDDs e plots que iremos usar
#Cutoffs
breaks = c(2000, 4000, 8000, 15000, 40000, 60000)
rco = c(216, 260, 330, 459, 780, 1376, 2956)

lista_rdd <- vector("list", length(breaks))

lista_rdd_cut <- vector("list", length(breaks))

lista_rdplots <- vector("list", length(breaks))


### Estimações e plots para RDD - Versão Linear (p=1) ======================================================================================================================
for (i in 1:6) {
  # Dataframes
  if (i == 1) {
    df_aux <- df |>
      filter(
        montante_portado <= breaks[i] |
          (montante_portado >= breaks[i] &
            montante_portado < breaks[i + 1])
      )
    df_aux_cut <- df |>
      filter(
        (montante_portado <= breaks[i] &
          valor_rco >= 0.8 * rco[i]) |
          (montante_portado >= breaks[i] &
            montante_portado < breaks[i + 1] &
            valor_rco >= 0.8 * rco[i + 1])
      )
  } else if (i == 6) {
    df_aux <- df |> filter((montante_portado > breaks[i - 1]))
    df_aux_cut <- df |>
      filter(
        (montante_portado > breaks[i - 1] &
          montante_portado <= breaks[i] &
          valor_rco >= 0.8 * rco[i]) |
          (montante_portado >= breaks[i] &
            valor_rco >= 0.8 * rco[i + 1])
      )
  } else {
    df_aux <- df |>
      filter(
        (montante_portado > breaks[i - 1] &
          montante_portado <= breaks[i + 1])
      )
    df_aux_cut <- df |>
      filter(
        (montante_portado > breaks[i - 1] &
          montante_portado <= breaks[i] &
          valor_rco >= 0.8 * rco[i]) |
          (montante_portado >= breaks[i] &
            montante_portado < breaks[i + 1] &
            valor_rco > 0.8 * rco[i + 1])
      )
  }
  # Covariadas
  Z <- cbind(
    df_aux$rating,
    df_aux$rendimento,
    df_aux$valor_total,
    df_aux$valor_colateral
  )
  Z_cut <- cbind(
    df_aux_cut$rating,
    df_aux_cut$rendimento,
    df_aux_cut$valor_total,
    df_aux_cut$valor_colateral
  )

  # RDD
  ### Estimação - Todas as obs
  rdd <- rdrobust(
    y = df_aux$spread,
    x = df_aux$montante_portado,
    covs = Z,
    c = breaks[i],
    p = 1,
    kernel = "triangular",
    bwselect = "mserd"
  )
  summary(rdd)
  lista_rdd[[i]] <- rdd

  ### Estimação - Só upper quintile
  rdd_cut <- rdrobust(
    y = df_aux_cut$spread,
    x = df_aux_cut$montante_portado,
    covs = Z_cut,
    c = breaks[i],
    p = 1,
    kernel = "triangular",
    bwselect = "mserd"
  )
  summary(rdd_cut)
  lista_rdd_cut[[i]] <- rdd_cut

  ### Plot do RDD
  rdd_plot <- rdplot(
    y = df_aux$spread,
    x = df_aux$montante_portado,
    c = breaks[i],
    p = 1,
    h = lista_rdd[[i]]$bws[1],
    kernel = "triangular",
    binselect = "qs",
    subset = ((df_aux$montante_portado >= (breaks[i] - lista_rdd[[i]]$bws[1]) &
      df_aux$montante_portado < breaks[i]) |
      (df_aux$montante_portado >= breaks[i] &
        df_aux$montante_portado <= (breaks[i] + lista_rdd[[i]]$bws[1]))),
    y.lim = c(-0.4, -0.2),
    x.lim = c(
      breaks[i] - lista_rdd[[i]]$bws[1],
      breaks[i] + lista_rdd[[i]]$bws[1]
    ),
    x.label = "Montante Portado",
    y.label = "Spread de Juros",
    title = paste0("RDD - Cutoff de R$", breaks[i])
  )

  lista_rdplots[[i]] <- rdd_plot
}

### Ajustando plots do RDD
for (i in 1:6) {
  plot <- lista_rdplots[[i]]$rdplot

  plot <- plot + theme_minimal(base_size = 14)
  assign(paste0("p", i), plot, envir = .GlobalEnv)
}

p6 <- p6 + scale_x_continuous(limits = c(40000, 150000))
p <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

ggsave("Plots RDD.png", plot = p, width = 12, height = 8, dpi = 300)


### Estimações e plots para RDD - Versão Quadrática (p=2) ==================================================================================================================
for (i in 1:6) {
  # Dataframes
  if (i == 1) {
    df_aux <- df |>
      filter(
        montante_portado <= breaks[i] |
          (montante_portado >= breaks[i] &
            montante_portado < breaks[i + 1])
      )
    df_aux_cut <- df |>
      filter(
        (montante_portado <= breaks[i] &
          valor_rco >= 0.8 * rco[i]) |
          (montante_portado >= breaks[i] &
            montante_portado < breaks[i + 1] &
            valor_rco >= 0.8 * rco[i + 1])
      )
  } else if (i == 6) {
    df_aux <- df |> filter((montante_portado > breaks[i - 1]))
    df_aux_cut <- df |>
      filter(
        (montante_portado > breaks[i - 1] &
          montante_portado <= breaks[i] &
          valor_rco >= 0.8 * rco[i]) |
          (montante_portado >= breaks[i] &
            valor_rco >= 0.8 * rco[i + 1])
      )
  } else {
    df_aux <- df |>
      filter(
        (montante_portado > breaks[i - 1] &
          montante_portado <= breaks[i + 1])
      )
    df_aux_cut <- df |>
      filter(
        (montante_portado > breaks[i - 1] &
          montante_portado <= breaks[i] &
          valor_rco >= 0.8 * rco[i]) |
          (montante_portado >= breaks[i] &
            montante_portado < breaks[i + 1] &
            valor_rco > 0.8 * rco[i + 1])
      )
  }
  # Covariadas
  Z <- cbind(
    df_aux$rating,
    df_aux$rendimento,
    df_aux$valor_total,
    df_aux$valor_colateral
  )
  Z_cut <- cbind(
    df_aux_cut$rating,
    df_aux_cut$rendimento,
    df_aux_cut$valor_total,
    df_aux_cut$valor_colateral
  )

  # RDD
  ### Estimação - Todas as obs
  rdd <- rdrobust(
    y = df_aux$spread,
    x = df_aux$montante_portado,
    covs = Z,
    c = breaks[i],
    p = 2,
    kernel = "triangular",
    bwselect = "mserd"
  )
  summary(rdd)
  lista_rdd[[i]] <- rdd

  ### Estimação - Só upper quintile
  rdd_cut <- rdrobust(
    y = df_aux_cut$spread,
    x = df_aux_cut$montante_portado,
    covs = Z_cut,
    c = breaks[i],
    p = 2,
    kernel = "triangular",
    bwselect = "mserd"
  )
  summary(rdd_cut)
  lista_rdd_cut[[i]] <- rdd_cut

  ### Plot do RDD
  rdd_plot <- rdplot(
    y = df_aux$spread,
    x = df_aux$montante_portado,
    c = breaks[i],
    p = 2,
    h = lista_rdd[[i]]$bws[1],
    kernel = "triangular",
    binselect = "qs",
    subset = ((df_aux$montante_portado >= (breaks[i] - lista_rdd[[i]]$bws[1]) &
      df_aux$montante_portado < breaks[i]) |
      (df_aux$montante_portado >= breaks[i] &
        df_aux$montante_portado <= (breaks[i] + lista_rdd[[i]]$bws[1]))),
    y.lim = c(-0.35, -0.15),
    x.lim = c(
      breaks[i] - lista_rdd[[i]]$bws[1],
      breaks[i] + lista_rdd[[i]]$bws[1]
    ),
    x.label = "Montante Portado",
    y.label = "Spread de Juros",
    title = paste0("RDD - Cutoff de R$", breaks[i])
  )
  lista_rdplots[[i]] <- rdd_plot
}


### Ajustando plots do RDD
for (i in 1:6) {
  plot <- lista_rdplots[[i]]$rdplot

  plot <- plot + theme_minimal(base_size = 14)
  assign(paste0("p", i), plot, envir = .GlobalEnv)
}

p6 <- p6 + scale_x_continuous(limits = c(40000, 150000))
p <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

ggsave("Plots RDD quadratico.png", plot = p, width = 12, height = 8, dpi = 300)


# RDD Pooled
### Linear (p=1)
multi_rdd <- rdmc(
  Y = df$spread,
  X = df$montante_portado,
  C = c(2000, 4000, 8000, 15000, 40000, 60000),
  kernelvec = c(
    "triangular",
    "triangular",
    "triangular",
    "triangular",
    "triangular",
    "triangular"
  ),
  pvec = c(1, 1, 1, 1, 1, 1),
  bwselectvec = c("mserd", "mserd", "mserd", "mserd", "mserd", "mserd"),
  plot = TRUE,
  covs_mat = cbind(
    df$rating,
    df$rendimento,
    df$valor_total,
    df$valor_colateral
  ),
)
summary(multi_rdd)

### Quadrático (p=2)
multi_rdd <- rdmc(
  Y = df$spread,
  X = df$montante_portado,
  C = c(2000, 4000, 8000, 15000, 40000, 60000),
  kernelvec = c(
    "triangular",
    "triangular",
    "triangular",
    "triangular",
    "triangular",
    "triangular"
  ),
  pvec = c(2, 2, 2, 2, 2, 2),
  bwselectvec = c("mserd", "mserd", "mserd", "mserd", "mserd", "mserd"),
  plot = TRUE,
  covs_mat = cbind(
    df$rating,
    df$rendimento,
    df$valor_total,
    df$valor_colateral
  ),
)
summary(multi_rdd)

#Have to manually save the plot after running the rdmcplot function since it doesn't return a ggplot object

# CHECAGENS DE ROBUSTEZ ====================================================================================================================================================

### Spread (%) =============================================================================================================================================================
df <- df |> mutate(spread = 100 * ((juros - juros_prior) / juros_prior)) #Em pontos percentuais

#Cutoffs
breaks = c(2000, 4000, 8000, 15000, 40000, 60000)
rco = c(216, 260, 330, 459, 780, 1376, 2956)

lista_rdd <- vector("list", length(breaks))

lista_rdd_cut <- vector("list", length(breaks))

lista_rdplots <- vector("list", length(breaks))

### RDD com Spread %
for (i in 1:6) {
  # Dataframes
  if (i == 1) {
    df_aux <- df |>
      filter(
        montante_portado <= breaks[i] |
          (montante_portado >= breaks[i] &
            montante_portado < breaks[i + 1])
      )
    df_aux_cut <- df |>
      filter(
        (montante_portado <= breaks[i] &
          valor_rco >= 0.8 * rco[i]) |
          (montante_portado >= breaks[i] &
            montante_portado < breaks[i + 1] &
            valor_rco >= 0.8 * rco[i + 1])
      )
  } else if (i == 6) {
    df_aux <- df |> filter((montante_portado > breaks[i - 1]))
    df_aux_cut <- df |>
      filter(
        (montante_portado > breaks[i - 1] &
          montante_portado <= breaks[i] &
          valor_rco >= 0.8 * rco[i]) |
          (montante_portado >= breaks[i] &
            valor_rco >= 0.8 * rco[i + 1])
      )
  } else {
    df_aux <- df |>
      filter(
        (montante_portado > breaks[i - 1] &
          montante_portado <= breaks[i + 1])
      )
    df_aux_cut <- df |>
      filter(
        (montante_portado > breaks[i - 1] &
          montante_portado <= breaks[i] &
          valor_rco >= 0.8 * rco[i]) |
          (montante_portado >= breaks[i] &
            montante_portado < breaks[i + 1] &
            valor_rco > 0.8 * rco[i + 1])
      )
  }
  # Covariadas
  Z <- cbind(
    df_aux$rating,
    df_aux$rendimento,
    df_aux$valor_total,
    df_aux$valor_colateral
  )
  Z_cut <- cbind(
    df_aux_cut$rating,
    df_aux_cut$rendimento,
    df_aux_cut$valor_total,
    df_aux_cut$valor_colateral
  )

  # RDD
  ### Estimação - Todas as obs
  rdd <- rdrobust(
    y = df_aux$spread,
    x = df_aux$montante_portado,
    covs = Z,
    c = breaks[i],
    p = 1,
    kernel = "triangular",
    bwselect = "mserd"
  )
  summary(rdd)
  lista_rdd[[i]] <- rdd

  ### Estimação - Só upper quintile
  rdd_cut <- rdrobust(
    y = df_aux_cut$spread,
    x = df_aux_cut$montante_portado,
    covs = Z_cut,
    c = breaks[i],
    p = 1,
    kernel = "triangular",
    bwselect = "mserd"
  )
  summary(rdd_cut)

  lista_rdd_cut[[i]] <- rdd_cut

  ### Plot do RDD
  rdd_plot <- rdplot(
    y = df_aux$spread,
    x = df_aux$montante_portado,
    c = breaks[i],
    p = 1,
    h = lista_rdd[[i]]$bws[1],
    kernel = "triangular",
    binselect = "qs",
    subset = ((df_aux$montante_portado >= (breaks[i] - lista_rdd[[i]]$bws[1]) &
      df_aux$montante_portado < breaks[i]) |
      (df_aux$montante_portado >= breaks[i] &
        df_aux$montante_portado <= (breaks[i] + lista_rdd[[i]]$bws[1]))),
    y.lim = c(-20, -10),
    x.lim = c(
      breaks[i] - lista_rdd[[i]]$bws[1],
      breaks[i] + lista_rdd[[i]]$bws[1]
    ),
    x.label = "Montante Portado",
    y.label = "Variação Percentual de Juros",
    title = paste0("RDD - Cutoff de R$", breaks[i])
  )

  lista_rdplots[[i]] <- rdd_plot
}


### Ajustando plots do RDD
for (i in 1:6) {
  plot <- lista_rdplots[[i]]$rdplot

  plot <- plot + theme_minimal(base_size = 14)
  assign(paste0("p", i), plot, envir = .GlobalEnv)
}

p6 <- p6 + scale_x_continuous(limits = c(40000, 150000))
p <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

ggsave("Plots RDD percentual.png", plot = p, width = 12, height = 8, dpi = 300)


### RDD Pooled - Spread (%)
multi_rdd <- rdmc(
  Y = df$spread,
  X = df$montante_portado,
  C = c(2000, 4000, 8000, 15000, 40000, 60000),
  kernelvec = c(
    "triangular",
    "triangular",
    "triangular",
    "triangular",
    "triangular",
    "triangular"
  ),
  pvec = c(1, 1, 1, 1, 1, 1),
  bwselectvec = c("mserd", "mserd", "mserd", "mserd", "mserd", "mserd"),
  plot = TRUE,
  covs_mat = cbind(
    df$rating,
    df$rendimento,
    df$valor_total,
    df$valor_colateral
  ),
)
summary(multi_rdd)


### Bandwidths =============================================================================================================================================================
# Como robustez, fixamos o bandwidth manualmente de forma a obter um número mínimo de observações em cada lado do cutoff
### Cutoffs
breaks = c(2000, 4000, 8000, 15000, 40000, 60000)
rco = c(216, 260, 330, 459, 780, 1376, 2956)
h <- c(75, 175, 350, 525, 3000, 5000)
h_cut <- c(175, 250, 525, 750, 3000, 6000)
# Listas
lista_rdd <- vector("list", length(breaks))

lista_rdd_cut <- vector("list", length(breaks))

lista_rdplots <- vector("list", length(breaks))

# Estimações e Plots para RDD (p=1)e menor h para que effective N seja maior que 100 em cada lado do cutoff
for (i in 1:6) {
  # Dataframes
  if (i == 1) {
    df_aux <- df |>
      filter(
        montante_portado <= breaks[i] |
          (montante_portado >= breaks[i] &
            montante_portado < breaks[i + 1])
      )
    df_aux_cut <- df |>
      filter(
        (montante_portado <= breaks[i] &
          valor_rco >= 0.8 * rco[i]) |
          (montante_portado >= breaks[i] &
            montante_portado < breaks[i + 1] &
            valor_rco >= 0.8 * rco[i + 1])
      )
  } else if (i == 6) {
    df_aux <- df |> filter((montante_portado > breaks[i - 1]))
    df_aux_cut <- df |>
      filter(
        (montante_portado > breaks[i - 1] &
          montante_portado <= breaks[i] &
          valor_rco >= 0.8 * rco[i]) |
          (montante_portado >= breaks[i] &
            valor_rco >= 0.8 * rco[i + 1])
      )
  } else {
    df_aux <- df |>
      filter(
        (montante_portado > breaks[i - 1] &
          montante_portado <= breaks[i + 1])
      )
    df_aux_cut <- df |>
      filter(
        (montante_portado > breaks[i - 1] &
          montante_portado <= breaks[i] &
          valor_rco >= 0.8 * rco[i]) |
          (montante_portado >= breaks[i] &
            montante_portado < breaks[i + 1] &
            valor_rco > 0.8 * rco[i + 1])
      )
  }
  # Covariadas
  Z <- cbind(
    df_aux$rating,
    df_aux$rendimento,
    df_aux$valor_total,
    df_aux$valor_colateral
  )
  Z_cut <- cbind(
    df_aux_cut$rating,
    df_aux_cut$rendimento,
    df_aux_cut$valor_total,
    df_aux_cut$valor_colateral
  )

  # RDD
  ### Estimação - Todas as obs
  rdd <- rdrobust(
    y = df_aux$spread,
    x = df_aux$montante_portado,
    covs = Z,
    c = breaks[i],
    p = 1,
    kernel = "triangular",
    h = h[i],
  )
  summary(rdd)
  lista_rdd[[i]] <- rdd

  ### Estimação - Só upper quintile
  rdd_cut <- rdrobust(
    y = df_aux_cut$spread,
    x = df_aux_cut$montante_portado,
    covs = Z_cut,
    c = breaks[i],
    p = 1,
    kernel = "triangular",
    h = h_cut[i]
  )
  summary(rdd_cut)
  lista_rdd_cut[[i]] <- rdd_cut

  ### Plot do RDD
  rdd_plot <- rdplot(
    y = df_aux$spread,
    x = df_aux$montante_portado,
    c = breaks[i],
    p = 1,
    h = h[i],
    kernel = "triangular",
    binselect = "qs",
    subset = ((df_aux$montante_portado >= (breaks[i] - h[i]) &
      df_aux$montante_portado < breaks[i]) |
      (df_aux$montante_portado >= breaks[i] &
        df_aux$montante_portado <= (breaks[i] + h[i]))),
    x.lim = c(breaks[i] - h[i], breaks[i] + h[i]),
    x.label = "Montante Portado",
    y.label = "Variação Percentual de Juros",
    title = paste0(
      "RDD - Cutoff de R$",
      breaks[i],
      " (h = ",
      h[i],
      ")"
    )
  )
  lista_rdplots[[i]] <- rdd_plot
}


### Ajustando plots do RDD
for (i in 1:6) {
  plot <- lista_rdplots[[i]]$rdplot

  plot <- plot + theme_minimal(base_size = 14)
  assign(paste0("p", i), plot, envir = .GlobalEnv)
}

p6 <- p6 + scale_x_continuous(limits = c(40000, 150000))
p <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

ggsave("Plots RDD h_min.png", plot = p, width = 12, height = 8, dpi = 300)

# Painel RCO =============================================================================================================================================================

# Relação contínua
rco_panel <- lm(
  spread ~
    log(valor_rco) +
      rating +
      rendimento +
      valor_total +
      valor_colateral,
  data = df
)

summary(rco_panel)


# Usando saldo devedor com dummies
df2 <- df |>
  mutate(
    categ_saldo = cut(
      montante_portado,
      breaks = c(0, 2000, 4000, 8000, 15000, 40000, 60000, Inf),
      labels = c(
        "Abaixo 2.000",
        "2.000-4.000",
        "4.000-8.000",
        "8.000-15.000",
        "15.000-40.000",
        "40.000-60.000",
        "Acima 60.000"
      ),
      include.lowest = TRUE,
      right = FALSE,
      ordered_result = FALSE
    )
  )

rco_panel_d <- lm(
  spread ~
    log(valor_rco) +
      log(valor_rco) * categ_saldo +
      rating +
      rendimento +
      valor_total +
      valor_colateral,
  data = df2
)

summary(rco_panel_d)


### Criando tabela
names(coef(rco_panel_d))

stargazer(
  rco_panel,
  rco_panel_d,
  type = "latex",
  title = "Sensibilidade do Spread ao RCO",
  dep.var.labels = "Spread Juros (%)",
  keep = c("log\\(valor_rco\\)"), #
  covariate.labels = c(
    "log(RCO Value)",
    "log(RCO) × 2.000-4.000",
    "log(RCO) × 4.000-8.000",
    "log(RCO) × 8.000-15.000",
    "log(RCO) × 15.000-40.000",
    "log(RCO) × 40.000-60.000",
    "log(RCO) × Above 60.000"
  ),
  omit.stat = c("f", "ser"),
  add.lines = list(
    c("Controles", "Sim", "Sim"),
    c("N. Obs", nrow(df), nrow(df2))
  ),
  digits = 3,
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = "Erros-padrão em robustos entre parênteses.",
  notes.align = "l"
)


# Histograma do montante portado é para checar concentrações e forma geral
ggplot(
  data = df |> filter(montante_portado < 60000),
  aes(x = montante_portado)
) +
  geom_histogram(bins = 100, fill = "blue", alpha = 0.7) +
  geom_density(aes) +
  labs(
    title = "Histograma do Montante Portado",
    x = "Montante Portado (R$)",
    y = "Frequência"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)
