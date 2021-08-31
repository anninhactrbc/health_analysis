#carregando as bibliotecas
library(readr)
library(tidyverse)

#carregando os arquivos que vamos trabalhar
child_mortality <- read_csv("child_mortality_0_5_year_olds_dying_per_1000_born.csv")
health_spending <- read_csv("total_health_spending_percent_of_gdp.csv")
vacc_rate <- read_csv("vacc_rate.csv")
medical_doctors <- read_csv("medical_doctors_per_1000_people.csv")
life_expectancy <- read_csv("life_expectancy_years.csv")

#filtrando base de dados por países da america latina e por tempo
cm <- child_mortality[,c("country", '1959':'2009')]
cm <- cm %>% filter(stringr::str_detect(country, 'Brazil|Argentina|Bolivia|Colombia|Peru|Uruguay|Paraguay|Venezuela|Suriname|Ecuador|Guyana'))
hs <- health_spending[,c("country", '1999':'2009')]
hs <- hs %>% filter(stringr::str_detect(country, 'Brazil|Argentina|Bolivia|Colombia|Peru|Uruguay|Paraguay|Venezuela|Suriname|Ecuador|Guyana'))
md <- medical_doctors[,c("country",'1959':'2009')]
md <- md %>% filter(stringr::str_detect(country, 'Brazil|Argentina|Bolivia|Colombia|Peru|Uruguay|Paraguay|Venezuela|Suriname|Ecuador|Guyana'))
vr <- vacc_rate[,c("country",'1979':'2009')]
vr <- vr %>% filter(stringr::str_detect(country, 'Brazil|Argentina|Bolivia|Colombia|Peru|Uruguay|Paraguay|Venezuela|Suriname|Ecuador|Guyana'))
lf <- life_expectancy[,c("country", '1959':'2009')]
lf <- lf %>% filter(stringr::str_detect(country, 'Brazil|Argentina|Bolivia|Colombia|Peru|Uruguay|Paraguay|Venezuela|Suriname|Ecuador|Guyana'))

#juntando os bancos de dados num arquivo só
db_joined <- lapply(1:5, function(x){vetores <- list(cm, hs, lf, md, vr); 
xtype <- c("cm", "hs", "lf", "md", "vr"); 
vetores[[x]] %>% mutate(type = xtype[x])}) %>% 
    bind_rows() %>%  select(country, type, everything()) %>%  
    pivot_longer(cols = -c("country", "type"), names_to = "ano", values_to = "valores")

#geração dos gráficos
ggplot(db_joined, aes(x=ano, y=valores)) + facet_wrap(~country, scale = "free_y", ncol = 2) + 
    geom_point(aes(colour = factor(type))) +
    ylab("") + xlab("Year") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7))

