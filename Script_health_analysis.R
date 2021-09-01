library(readr)
child_mortality <- read_csv("child_mortality_0_5_year_olds_dying_per_1000_born.csv")
health_spending <- read_csv("total_health_spending_percent_of_gdp.csv")
vacc_rate <- read_csv("vacc_rate.csv")
medical_doctors <- read_csv("medical_doctors_per_1000_people.csv")
life_expectancy <- read_csv("life_expectancy_years.csv")

library(tidyverse)
cm <- child_mortality[,c(1,162:212)]
cm <- cm %>% filter(stringr::str_detect(country, 'Brazil|Argentina|Bolivia|Colombia|Peru|Uruguay|Paraguay|Venezuela|Suriname|Ecuador|Guyana'))
hs <- health_spending[,c(1, 7:17)]
hs <- hs %>% filter(stringr::str_detect(country, 'Brazil|Argentina|Bolivia|Colombia|Peru|Uruguay|Paraguay|Venezuela|Suriname|Ecuador|Guyana'))
md <- medical_doctors[,c(1,2:52)]
md <- md %>% filter(stringr::str_detect(country, 'Brazil|Argentina|Bolivia|Colombia|Peru|Uruguay|Paraguay|Venezuela|Suriname|Ecuador|Guyana'))
vr <- vacc_rate[,c(1,2:32)]
vr <- vr %>% filter(stringr::str_detect(country, 'Brazil|Argentina|Bolivia|Colombia|Peru|Uruguay|Paraguay|Venezuela|Suriname|Ecuador|Guyana'))
lf <- life_expectancy[,c(1, 162:212)]
lf <- lf %>% filter(stringr::str_detect(country, 'Brazil|Argentina|Bolivia|Colombia|Peru|Uruguay|Paraguay|Venezuela|Suriname|Ecuador|Guyana'))

#Separação das informações por país#
#Fazendo a transposição de linhas para colunas#

tmd <- md[,-1]
tmd <- t(tmd)
tmd <- as.data.frame(tmd)
tmd <- select(tmd, Argentina=V1, Bolivia=V2, Brasil=V3, Colombia=V4, 
              Equador=V5, Guiana=V6, Peru=V7, Paraguai=V8, Suriname=V9,
              Uruguai=V10, Venezuela=V11)
tcm <- cm[,-1]
tcm <- t(tcm)
tcm <- as.data.frame(tcm)
tcm <- select(tcm, Argentina=V1, Bolivia=V2, Brasil=V3, Colombia=V4, 
              Equador=V5, Guiana=V6, Peru=V7, Paraguai=V8, Suriname=V9,
              Uruguai=V10, Venezuela=V11)

ths <- hs[,-1]
ths <- t(ths)
ths <- as.data.frame(ths)
ths <- select(ths, Argentina=V1, Bolivia=V2, Brasil=V3, Colombia=V4, 
              Equador=V5, Guiana=V6, Peru=V7, Paraguai=V8, Suriname=V9,
              Uruguai=V10, Venezuela=V11)

tvr <- vr[,-1]
tvr <- t(tvr)
tvr <- as.data.frame(tvr)
tvr <- select(tvr, Argentina=V1, Bolivia=V2, Brasil=V3, Colombia=V4, 
              Equador=V5, Guiana=V6, Peru=V7, Paraguai=V8, Suriname=V9,
              Uruguai=V10, Venezuela=V11)

tlf <- lf[,-1]
tlf <- t(tlf)
tlf <- as.data.frame(tlf)
tlf <- select(tlf, Argentina=V1, Bolivia=V2, Brasil=V3, Colombia=V4, 
              Equador=V5, Guiana=V6, Peru=V7, Paraguai=V8, Suriname=V9,
              Uruguai=V10, Venezuela=V11)

#transformando em dataframes de mesmo tamanho para separar por país#

tlf <- tlf[c(41:51),]
tvr <- tvr[c(21:31),]
tcm <- tcm[c(41:51),]
tmd <- tmd[c(41:51),]

#criando um dataframe para cada país#

argentina <- cbind(tlf[,1], tvr[,1], ths[,1], tcm[,1], tmd[,1])
bolivia <- cbind(tlf[,2], tvr[,2], ths[,2], tcm[,2], tmd[,2])
brasil <- cbind(tlf[,3], tvr[,3], ths[,3], tcm[,3], tmd[,3])
colombia <- cbind(tlf[,4], tvr[,4], ths[,4], tcm[,4], tmd[,4])
equador <- cbind(tlf[,5], tvr[,5], ths[,5], tcm[,5], tmd[,5])
guiana <- cbind(tlf[,6], tvr[,6], ths[,6], tcm[,6], tmd[,6])
peru <- cbind(tlf[,7], tvr[,7], ths[,7], tcm[,7], tmd[,7])
paraguai <- cbind(tlf[,8], tvr[,8], ths[,8], tcm[,8], tmd[,8])
suriname <- cbind(tlf[,9], tvr[,9], ths[,9], tcm[,9], tmd[,9])
uruguai <- cbind(tlf[,10], tvr[,10], ths[,10], tcm[,10], tmd[,10])
venezuela <- cbind(tlf[,11], tvr[,11], ths[,11], tcm[,11], tmd[,11])

vary <- c("life_expc", "vac-rate", "healt_spend", "child_mort", "med_doc")
varx <- c("1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009")
colnames(argentina) <- vary
rownames(argentina) <- varx
colnames(bolivia) <- vary
rownames(bolivia) <- varx
colnames(brasil) <- vary
rownames(brasil) <- varx
colnames(colombia) <- vary
rownames(colombia) <- varx
colnames(equador) <- vary
rownames(equador) <- varx
colnames(guiana) <- vary
rownames(guiana) <- varx
colnames(peru) <- vary
rownames(peru) <- varx
colnames(paraguai) <- vary
rownames(paraguai) <- varx
colnames(suriname) <- vary
rownames(suriname) <- varx
colnames(uruguai) <- vary
rownames(uruguai) <- varx
colnames(venezuela) <- vary
rownames(venezuela) <- varx

uruguai_df <- as.data.frame(uruguai)
brasil_df <- as.data.frame(brasil)
library(dplyr)
mutate(uruguai_df, country = "uruguai")
mutate(brasil_df, country = "brasil")




       