# Setter riktig 'working directory' 
setwd("C:/Users/adria/OneDrive/Dokumenter/5. Semester/SOK-2008/Innlevering 6")

# Velger norsk språk
Sys.setlocale(locale="no_NO")

# Laster nødvendige pakker
library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyverse)

## Utfordring 6.2.4

#Laster inn dataen fra SSB
sykdom <- read_excel("Sykdom.xlsx", skip = 1, n_max = 4, col_names = F)

sykdom <- as.data.frame(t(sykdom))

colnames(sykdom) <- c("År", "Sykefravær Menn", "Sykefravær Kvinner")

sykdom <- sykdom %>% 
  filter(!row_number() %in% c(1,2,3,4))
  

ledighet <- read_excel("Arbeidsledighet.xlsx", skip = 1, n_max = 4, col_names = F)

ledighet <- as.data.frame(t(ledighet))

colnames(ledighet) <- c("År", "Arbeidsledighet Menn", "Arbeidsledighet Kvinner")

ledighet <- ledighet %>% 
  filter(!row_number() %in% c(1,2,3,4))

# Hvert kjønn til hvert sitt datasett
ledighet_kvinner <- ledighet %>% 
  select(År, `Arbeidsledighet Kvinner`)

ledighet_menn <- ledighet %>% 
  select(År, `Arbeidsledighet Menn`)

sykdom_kvinner <- sykdom %>% 
  select(År, `Sykefravær Kvinner`)

sykdom_menn <- sykdom %>% 
  select(År, `Sykefravær Menn`)

# Lager nytt datasett med sykefravær og arbeidsledighet for hvert kjønn

kvinner <- merge(ledighet_kvinner, sykdom_kvinner, by = "År")

menn <- merge(ledighet_menn, sykdom_menn, by = "År")

# Setter inn NA for manglende funn
kvinner[kvinner=="."] <- NA

menn[menn=="."] <- NA

# Endrer kolonnene i tabellene til numeric fremfor character

kvinner <- as.data.frame(sapply(kvinner, as.numeric))

menn <- as.data.frame(sapply(menn, as.numeric))

# Ploter dataen

koeffisient = 1

kvinner %>% 
  ggplot(aes(x=År, y=`Arbeidsledighet Kvinner`, group = 1)) +
  geom_line(aes(color = "red")) +
  geom_line(aes(y = `Sykefravær Kvinner`,
                color = "blue")) +
  scale_y_continuous(name = "Arbeidsledighet i %",
                     sec.axis = sec_axis(~.*koeffisient,
                                         name = "Sykefravær i %")) +
  scale_x_continuous(breaks = kvinner$År) +
  scale_colour_manual('Situasjon', values = c('blue','red'),
                      labels = c("Sykefravær","Arbeidsledighet")) + 
  labs(x = "År",
       title = "Arbeidsledighet og sykefravær",
       subtitle = " - For kvinner") +
  theme(legend.position = c(0.15, 0.50))

menn %>% 
  ggplot(aes(x=År, y=`Arbeidsledighet Menn`, group = 1)) +
  geom_line(aes(color = "red")) +
  geom_line(aes(y = `Sykefravær Menn`,
                color = "blue")) +
  scale_y_continuous(name = "Arbeidsledighet i %",
                     sec.axis = sec_axis(~.*koeffisient,
                                         name = "Sykefravær i %")) +
  scale_x_continuous(breaks = menn$År) +
  scale_colour_manual('Situasjon', values = c('blue','red'),
                      labels = c("Sykefravær","Arbeidsledighet")) + 
  labs(x = "År",
       title = "Arbeidsledighet og sykefravær",
       subtitle = " - For menn") +
  theme(legend.position = c(0.85, 0.22))

