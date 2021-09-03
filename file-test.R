#carregando as bibliotecas
library(readr)
library(tidyverse)

#carregando os arquivos que vamos trabalhar
child_mortality <- read_csv("child_mortality_0_5_year_olds_dying_per_1000_born.csv")
health_spending <- read_csv("total_health_spending_percent_of_gdp.csv")
vacc_rate <- read_csv("vacc_rate.csv")
medical_doctors <- read_csv("medical_doctors_per_1000_people.csv")
life_expectancy <- read_csv("life_expectancy_years.csv")

#Fiz isso porque queria deixar o "C maiusculo na legenda, mas nao consegui alterar apenas la¡
#Se conseguirem modificar la¡, podem excluir essa parte
colnames(child_mortality)[1] <- "Country"
colnames(health_spending)[1] <- "Country"
colnames(vacc_rate)[1] <- "Country"
colnames(medical_doctors)[1] <- "Country"
colnames(life_expectancy)[1] <- "Country"

#criando um vetor contendo o nome dos paises da América Latina
paises_al <- c("Brazil|Argentina|Bolivia|Colombia|Peru|Uruguay|Paraguay|Venezuela|Suriname|Ecuador|Guyana")

#filtrando base de dados por países da america latina e por tempo
cm <- child_mortality %>% 
    select(c("Country", '1959':'2009')) %>% 
    filter(str_detect(Country, paises_al))
hs <- health_spending%>% 
    select(c("Country", '1999':'2009')) %>%
    filter(str_detect(Country, paises_al))
md <- medical_doctors%>% 
    select(c("Country", '1959':'2009')) %>%
    filter(str_detect(Country, paises_al))
vr <- vacc_rate%>% 
    select(c("Country", '1979':'2009')) %>% 
    filter(str_detect(Country, paises_al))
lf <- life_expectancy %>% 
    select(c("Country", '1959':'2009')) %>%
    filter(str_detect(Country, paises_al))

#juntando os bancos de dados num arquivo só
db_joined <- lapply(1:5, function(x){vetores <- list(cm, hs, lf, md, vr); 
xtype <- c("cm", "hs", "lf", "md", "vr"); 
vetores[[x]] %>% mutate(type = xtype[x])}) %>% 
    bind_rows() %>%  select(Country, type, everything()) %>%  
    pivot_longer(cols = -c("Country", "type"), names_to = "ano", values_to = "valores")

###Gerando os gráficos
##Opção 1: sobrepor todos os dados no mesmo gráfico por países
db_joined_plot <-
    ggplot(db_joined, aes(x=ano, y=valores)) + 
    facet_wrap(~Country, scale = "free_y", ncol = 2) + 
    geom_point(aes(colour = factor(type))) +
    ylab("") + xlab("Year") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7))

##Opçao 2: fazer um gr?fico para cada dado por país
#Child mortality
cm_plot <-
    db_joined %>% filter(type == "cm") %>% 
    ggplot(aes(x=as.integer(ano), y= valores, col= Country), data = .) + 
    geom_smooth() +
    labs(title = "Child Mortality", 
         x = "", 
         y = "Death per 1,000 live births",
         subtitle = "Death of children under five years of age per 1,000 live births") +
    theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = "bold", size = 15),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.title.align = 0.5,
          plot.subtitle = element_text(hjust=0.5, size = 9))

#Health spending
hs_plot <-
    db_joined %>% filter(type == "hs") %>% 
    ggplot(aes(x=as.integer(ano), y= valores, col= Country), data = .) + 
    geom_smooth() +
    labs(title = "Total Health Spending",
         x = "",
         y = "(% of GDP)",
         subtitle = "Sum of public and privite health expenditure as a percentage of GDP",
         caption = "GDP: Gross domestic product") +
    theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = "bold", size = 15),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.title.align = 0.5,
          plot.subtitle = element_text(hjust=0.5, size = 9))

#Life expectancy
lf_plot <-
    db_joined %>% filter(type == "lf") %>% 
    ggplot(aes(x=as.integer(ano), y= valores, col= Country), data = .) + 
    geom_smooth() +
    labs(title = "Life Expectancy",
         x = "",
         y = "Years",
         subtitle = "Years that a newborn child would live if current mortality patterns were to stay the same") +
    theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = "bold", size = 15),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.title.align = 0.5,
          plot.subtitle = element_text(hjust=0.5, size = 9))

#Medical doctors
md_plot <-
    db_joined %>% filter(type == "md") %>% 
    ggplot(aes(x=as.integer(ano), y= valores, col= Country), data = .) + 
    geom_smooth() +
    labs(title = "Medical Doctors",
         x = "",
         y = "Medical doctors per 1000 people",
         subtitle = "Physicians included generalist and specialist medical doctors practitioners") +
    theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = "bold", size = 15),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.title.align = 0.5,
          plot.subtitle = element_text(hjust=0.5, size = 9))

#Vaccination rate
vr_plot <-
    db_joined %>% filter(type == "vr") %>% 
    ggplot(aes(x=as.integer(ano), y= valores, col= Country), data = .) + 
    geom_smooth() +
    labs(title = "Vaccination Rate",
         x = "",
         y = "% of vaccinated children",
         subtitle = "Share of one-years-olds who are vaccinated against at least one disease") +
    theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = "bold", size = 15),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.title.align = 0.5,
          plot.subtitle = element_text(hjust=0.5, size = 9))

#Juntando os gráficos gerados na Opçao 2
#Carregando as bibliotecas
library(lemon)
library(cowplot)

#Extraindo a legenda de um dos gráficos
legend <- g_legend(cm_plot + theme(legend.position='left'))

#Juntando gráficos
all_graphics <- 
    plot_grid(cm_plot + 
                  theme(legend.position='hidden'), 
              lf_plot + 
                  theme(legend.position='hidden'),
              md_plot + 
                  theme(legend.position='hidden'), 
              hs_plot + 
                  theme(legend.position='hidden'), 
              vr_plot + 
                  theme(legend.position='hidden'),
              labels = c("a", "b", "c", "d", "e"),
              label_fontface = "italic",
              ncol =2,
              legend) 

#Salvando o gráfico em .pdf
ggsave("all_graphics1.pdf", width = 28, height = 28, units = "cm")