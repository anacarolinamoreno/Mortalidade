rm(list = ls())

library(tidyverse)
library(lubridate)
library(zoo)
library(readr)
library(tidylog)

pop <- read.csv2('data-raw/df_pop_distritos_capital.csv', encoding="Windows-1252")
obitos <- read.csv2('data-raw/df_obitos_distritos_capital.csv', encoding="Windows-1252")

df_distritos <-
  left_join(x = pop, y = obitos, by = "distrito_tabnet") %>%
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
