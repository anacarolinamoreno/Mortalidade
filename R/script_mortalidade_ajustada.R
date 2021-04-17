rm(list = ls())

library(tidyverse)
library(lubridate)
library(zoo)
library(readr)
library(tidylog)


# TAXA AJUSTADA POR DISTRITO DA CIDADE DE SÃO PAULO

# 1) Carregar os dois arquivos prontos com a população e os óbitos

pop_distritos <- read.csv2('data-raw/df_pop_distritos_capital.csv', encoding="Windows-1252")

obitos_distritos <- read.csv2('data-raw/df_obitos_distritos_capital.csv', encoding="Windows-1252")


# 2) Juntar as duas planilhas e calcular os coeficientes por faixa etária e a taxa final ajustada

df_distritos <-
  left_join(x = pop_distritos, y = obitos_distritos, by = "distrito_tabnet") %>%
  mutate(
    coef00a04 = (obitos00a04 / pop00a04) * 100000,
    coef05a09 = (obitos05a09 / pop05a09) * 100000,
    coef10a14 = (obitos10a14 / pop10a14) * 100000,
    coef15a19 = (obitos15a19 / pop15a19) * 100000,
    coef20a24 = (obitos20a24 / pop20a24) * 100000,
    coef25a29 = (obitos25a29 / pop25a29) * 100000,
    coef30a34 = (obitos30a34 / pop30a34) * 100000,
    coef35a39 = (obitos35a39 / pop35a39) * 100000,
    coef40a44 = (obitos40a44 / pop40a44) * 100000,
    coef45a49 = (obitos45a49 / pop45a49) * 100000,
    coef50a54 = (obitos50a54 / pop50a54) * 100000,
    coef55a59 = (obitos55a59 / pop55a59) * 100000,
    coef60a64 = (obitos60a64 / pop60a64) * 100000,
    coef65a69 = (obitos65a69 / pop65a69) * 100000,
    coef70a74 = (obitos70a74 / pop70a74) * 100000,
    coef75m = (obitos75m / pop75m) * 100000,
    coef_total = (obitos_total / pop_total) * 100000,
    taxa_ajustada =
      ((coef00a04*7232)+
         (coef05a09*7847)+
         (coef10a14*8999)+
         (coef15a19*8907)+
         (coef20a24*9040)+
         (coef25a29*8967)+
         (coef30a34*8254)+
         (coef35a39*7281)+
         (coef40a44*6820)+
         (coef45a49*6203)+
         (coef50a54*5316)+
         (coef55a59*4339)+
         (coef60a64*3412)+
         (coef65a69*2538)+
         (coef70a74*1961)+
         (coef75m*2883))/100000
  )

# Criar planilha CSV com o resumo da taxa ajustada, ordenado pelo distrito com a taxa mais alta

taxa_ajustada_distritos <- df_distritos %>%
  select(distrito_datawrapper, distrito_tabnet, obitos_total, pop_total, coef_total, taxa_ajustada) %>%
  arrange(desc(taxa_ajustada))

write.csv2(taxa_ajustada_distritos, "data/taxa_ajustada_distritos.csv", row.names = F)


# TAXA AJUSTADA POR MUNICÍPIO BRASILEIRO

# 1) Carregar os dois arquivos prontos com a população e os óbitos


pop_munic_br <- read.csv2('data-raw/df_pop_municipios_br.csv', encoding="Windows-1252")

obitos_munic_br <- read.csv2('data-raw/df_obitos_municipios_br.csv', encoding="Windows-1252") %>%

  # 2) Juntar as duas planilhas e calcular os coeficientes por faixa etária e a taxa final ajustada


df_munic_br <-
  left_join(x = pop_munic_br, y = obitos_munic_br, by = "cod_ibge_6") %>%
  select(-X) %>%
  mutate(
    muni_id_6 = cod_ibge_6,
    coef00a04 = (obitos00a04 / pop00a04) * 100000,
    coef05a09 = (obitos05a09 / pop05a09) * 100000,
    coef10a14 = (obitos10a14 / pop10a14) * 100000,
    coef15a19 = (obitos15a19 / pop15a19) * 100000,
    coef20a24 = (obitos20a24 / pop20a24) * 100000,
    coef25a29 = (obitos25a29 / pop25a29) * 100000,
    coef30a34 = (obitos30a34 / pop30a34) * 100000,
    coef35a39 = (obitos35a39 / pop35a39) * 100000,
    coef40a44 = (obitos40a44 / pop40a44) * 100000,
    coef45a49 = (obitos45a49 / pop45a49) * 100000,
    coef50a54 = (obitos50a54 / pop50a54) * 100000,
    coef55a59 = (obitos55a59 / pop55a59) * 100000,
    coef60a64 = (obitos60a64 / pop60a64) * 100000,
    coef65a69 = (obitos65a69 / pop65a69) * 100000,
    coef70a74 = (obitos70a74 / pop70a74) * 100000,
    coef75a79 = (obitos75a79 / pop75a79) * 100000,
    coef80m = (obitos80m / pop80m) * 100000,
    coef_total = (obitos_total / pop_total) * 100000,
    taxa_ajustada =(
        (coef00a04*6956)+
         (coef05a09*6918)+
         (coef10a14*6992)+
         (coef15a19*7457)+
         (coef20a24*8138)+
         (coef25a29*8021)+
         (coef30a34*8125)+
         (coef35a39*8041)+
         (coef40a44*7368)+
         (coef45a49*6447)+
         (coef50a54*5959)+
         (coef55a59*5316)+
         (coef60a64*4431)+
         (coef65a69*3471)+
         (coef70a74*2554)+
         (coef75a79*1707)+
         (coef80m*2097))/100000
  ) %>%
  mutate_at(vars(muni_id_6), as.character)

# Usar pacote abjData para pegar informações básicas dos municípios brasileiros

munic_id <- abjData::munic %>%
  mutate_at(vars(muni_id_6), as.integer) %>%
  select(muni_id, muni_id_6, uf_sigla)

# Criar planilha CSV com o resumo da taxa ajustada, ordenado pelo município com a taxa mais alta


taxa_ajustada_br <-
  left_join(x = df_munic_br, y = munic_id, by = "muni_id_6") %>%
  select(muni_id, muni_id_6, muni_nm, uf_sigla, obitos_total, pop_total, coef_total, taxa_ajustada) %>%
  arrange(desc(taxa_ajustada))

write.csv2(taxa_ajustada_br, "data/taxa_ajustada_br.csv", row.names = F)
