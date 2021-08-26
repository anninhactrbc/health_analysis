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


dados_2005 <- data.frame(select(cm, "country", "2005"), select(hs, "2005"), select(md, "2005"), select(vr, "2005"), select(le, "2005"))
names(dados_2005) <- c("country", "c_mortality", "h_spending", "m_doctors", "v_rate", "l_expectancy")
dados_2006 <- data.frame(select(cm, "country", "2006"), select(hs, "2006"), select(md, "2006"), select(vr, "2006"), select(le, "2006"))
names(dados_2006) <- c("country", "c_mortality", "h_spending", "m_doctors", "v_rate", "l_expectancy")
dados_2007 <- data.frame(select(cm, "country", "2007"), select(hs, "2007"), select(md, "2007"), select(vr, "2007"), select(le, "2007"))
names(dados_2007) <- c("country", "c_mortality", "h_spending", "m_doctors", "v_rate", "l_expectancy")
dados_2008 <- data.frame(select(cm, "country", "2008"), select(hs, "2008"), select(md, "2008"), select(vr, "2008"), select(le, "2008"))
names(dados_2008) <- c("country", "c_mortality", "h_spending", "m_doctors", "v_rate", "l_expectancy")
dados_2009 <- data.frame(select(cm, "country", "2009"), select(hs, "2009"), select(md, "2009"), select(vr, "2009"), select(le, "2009"))
names(dados_2009) <- c("country", "c_mortality", "h_spending", "m_doctors", "v_rate", "l_expectancy")
