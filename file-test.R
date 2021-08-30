#carregando as bibliotecas
library(readr)
library(tidyverse)
library(stringr)
library(data.table)
library(reshape)

#carregando os arquivos que vamos trabalhar
child_mortality <- read_csv("child_mortality_0_5_year_olds_dying_per_1000_born.csv")
health_spending <- read_csv("total_health_spending_percent_of_gdp.csv")
vacc_rate <- read_csv("vacc_rate.csv")
medical_doctors <- read_csv("medical_doctors_per_1000_people.csv")
life_expectancy <- read_csv("life_expectancy_years.csv")

#filtrando base de dados por países da america latina
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

#Separação das informações por país
#Fazendo a transposição de linhas para colunas
#Geração dos gráficos
tmd <- md[,-1]
tmd <- t(tmd)
tmd <- as.data.frame(tmd)
tmd <- select(tmd, Argentina=V1, Bolivia=V2, Brasil=V3, Colombia=V4, 
              Equador=V5, Guiana=V6, Peru=V7, Paraguai=V8, Suriname=V9,
              Uruguai=V10, Venezuela=V11)
#tmd <- tmd[c(41:51),]
tmd <- setDT(tmd, keep.rownames = TRUE)
tmd <- dplyr::rename(tmd, year = rn)
grafico_tmd <- melt(tmd,id.vars=c("year"))
grafico_tmd <- dplyr::rename(grafico_tmd, country = variable)
ggplot(grafico_tmd, aes(x=year, y =value)) + facet_wrap(~country, scale = "free_x", ncol = 3) + geom_point() +
    ylab("Medical doctors per 1000 people") + xlab("Year") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7))

tcm <- cm[,-1]
tcm <- t(tcm)
tcm <- as.data.frame(tcm)
tcm <- select(tcm, Argentina=V1, Bolivia=V2, Brasil=V3, Colombia=V4, 
              Equador=V5, Guiana=V6, Peru=V7, Paraguai=V8, Suriname=V9,
              Uruguai=V10, Venezuela=V11)
#tcm <- tcm[c(41:51),]
tcm <- setDT(tcm, keep.rownames = TRUE)
tcm <- dplyr::rename(tcm, year = rn)
grafico_tcm <- melt(tcm,id.vars=c("year"))
grafico_tcm <- dplyr::rename(grafico_tcm, country = variable)
ggplot(grafico_tcm, aes(x=year, y =value, fill = country)) + facet_wrap(~country, scale = "free_x", ncol = 3) + geom_bar(stat="identity") +
    ylab("Child mortality: 0-5 years old dying per 1000 born") + xlab("Year") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7)) +
    guides(fill = "none", color = "none", linetype = "none", shape = "none")

tvr <- vr[,-1]
tvr <- t(tvr)
tvr <- as.data.frame(tvr)
tvr <- select(tvr, Argentina=V1, Bolivia=V2, Brasil=V3, Colombia=V4, 
              Equador=V5, Guiana=V6, Peru=V7, Paraguai=V8, Suriname=V9,
              Uruguai=V10, Venezuela=V11)
#tvr <- tvr[c(21:31),]
tvr <- setDT(tvr, keep.rownames = TRUE)
tvr <- dplyr::rename(tvr, year = rn)
grafico_tvr <- melt(tvr,id.vars=c("year"))
grafico_tvr <- dplyr::rename(grafico_tvr, country = variable)
ggplot(grafico_tvr, aes(x=year, y =value)) + facet_wrap(~country, scale = "free_x", ncol = 3) + geom_point() +
    ylab("Vaccination rate") + xlab("Year") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7))

tlf <- lf[,-1]
tlf <- t(tlf)
tlf <- as.data.frame(tlf)
tlf <- select(tlf, Argentina=V1, Bolivia=V2, Brasil=V3, Colombia=V4, 
              Equador=V5, Guiana=V6, Peru=V7, Paraguai=V8, Suriname=V9,
              Uruguai=V10, Venezuela=V11)
#tlf <- tlf[c(41:51),]
tlf <- setDT(tlf, keep.rownames = TRUE)
tlf <- dplyr::rename(tlf, year = rn)
grafico_tlf <- melt(tlf,id.vars=c("year"))
grafico_tlf <- dplyr::rename(grafico_tlf, country = variable)
ggplot(grafico_tlf, aes(x=year, y =value)) + facet_wrap(~country, scale = "free_x", ncol = 3) + geom_point() +
    ylab("Life expectancy") + xlab("Year") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7))

ths <- hs[,-1]
ths <- t(ths)
ths <- as.data.frame(ths)
ths <- select(ths, Argentina=V1, Bolivia=V2, Brasil=V3, Colombia=V4, 
              Equador=V5, Guiana=V6, Peru=V7, Paraguai=V8, Suriname=V9,
              Uruguai=V10, Venezuela=V11)
ths <- setDT(ths, keep.rownames = TRUE)
ths <- dplyr::rename(ths, year = rn)
grafico_ths <- melt(ths,id.vars=c("year"))
grafico_ths <- dplyr::rename(grafico_ths, country = variable)
ggplot(grafico_ths, aes(x=year, y=value)) + 
    facet_wrap(~country, scale = "free_x", ncol = 3) + 
    geom_point() +
    ylab("Total health spent percent of GDP") + xlab("Year") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7))

