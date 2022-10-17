# Setter riktig 'working directory' 
setwd("C:/Users/adria/OneDrive/Dokumenter/5. Semester/SOK-2008/Innlevering 4")

# Velger norsk språk
Sys.setlocale(locale="no_NO")

# Laster inn datasettet
women<-read.csv2("women.csv")

# Gjør om til numeriske verdier
women$tot_full_rate<-as.numeric(women$tot_full_rate)
women$fem_emp_rate_0_2<-as.numeric(women$fem_emp_rate_0_2)
women$fem_emp_rate_6_14<-as.numeric(women$fem_emp_rate_6_14)

# Laster nødvendige pakker
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# Plotter dataen
women %>%
  ggplot(aes(x=tot_full_rate,y=fem_emp_rate_0_2))+
  geom_point()+
  ylim(0, 100)+
  labs(x ="Uker med 100% støtte", 
       y = "Yrkesdeltakelse blant mødre hvis yngste barn er 0-2 år")+
  geom_smooth(method=lm, se=FALSE) -> kids_0_2

women %>%
  ggplot(aes(x=tot_full_rate,y=fem_emp_rate_6_14))+
  geom_point()+
  ylim(0, 100)+
  labs(x ="Uker med 100% støtte", 
       y = "Yrkesdeltakelse blant mødre hvis yngste barn er 6-14 år")+
  geom_smooth(method=lm, se=FALSE) -> kids_6_14

# Setter plotene sammen for å sammenligne
grid.arrange(kids_0_2,kids_6_14, nrow = 1,  
             top = textGrob("Sammenhengen mellom foreldrepermisjons lengde og mødres yrkesdelakelse etter yngste barns alder",
                            gp=gpar(fontsize=20,font=3)))
