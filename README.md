# Estudo-RCO-Insper
Estudo efetuado por Frederico Gomes, Lucas Negreiro e Paulo Ribeiro do Insper, em parceria com uma instituição financeira brasileira de significância no mercado.  Este trabalho faz uso de dados agregados e microdados de empréstimos provenientes da instituição parceira afim de analisar custos da portabilidade de crédito

## Arquivos  
**Get Data.R** - Puxa dados agregados referentes a empréstimos e informações contáveis das IFs do Brasil através da API do BCB.  
**Dados Agregados.R** - Pega as bases brutas baixadas em Get Data.R e conduz todas as análises com dados agregados contidas no estudo. 
**RDD_Microdados.R** - Faz uso dos microdados da instituição financeira parceira para conduzir análises descritivas sobre empréstimos (Gerais e anexos a portabilidade) e as estimações econométricas que fazem uso de um RDD tomando proveito dos valores fixados para cobrança do RCO como função do saldo devedor de empréstimos no momento que são portados.
