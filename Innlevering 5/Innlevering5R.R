# Setter riktig 'working directory' 
setwd("C:/Users/adria/OneDrive/Dokumenter/5. Semester/SOK-2008/Innlevering 5")

# Velger norsk språk
Sys.setlocale(locale="no_NO")

# Laster nødvendige pakker
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(janitor)
library(tidyverse)
library(reshape2)

# Utfordring 5.1.1
landbakgrunn <- read_excel("landbakgrunn.xlsx", skip = 1, n_max = 25)

landbakgrunn <- landbakgrunn %>% 
  subset(select = -c(...1, ...2)) %>% 
  rename(Bakgrunn  = ...3) %>% 
  group_by(Bakgrunn) %>% 
  summarise_all(sum)

t <- as.tibble(landbakgrunn)

t <- melt(t, id.vars = "Bakgrunn", variable.name = "series")

ggplot(t, aes(y = series, x = value, fill = Bakgrunn)) +
  geom_col() +
  labs(x = "Antall innvandrere",
       y = "Årstall",
       title = "Antall innvandrere til Norge årlig",
       subtitle = "Fra hver region") +
  scale_fill_brewer(palette = "Paired",
                    name = "Region") +
  theme_light()
  

# Utfordring 5.1.2
sysselsatte <- read_excel("sysselsatte_innvandrere.xlsx", skip = 4,
                          n_max = 23)

sysselsatte <- sysselsatte %>% 
  subset(select = -c(...1, ...2, ...3)) %>% 
  rename(Sektor = ...4)

sysselsatte <- sysselsatte %>% 
  separate(Sektor, c("0", "Sektor"), " ... ") %>% 
  subset(select = c(Sektor, Innvandrere))
  
sysselsatte <- sysselsatte[-c(1),]

ggplot(sysselsatte, aes(y = Sektor, x = Innvandrere)) + 
  geom_col(aes(fill = Sektor), show.legend = F) +
  labs(x = "Antall sysselsatte innvandrere",
       y = "Sektor",
       title = "Innvandrere fra EU-land i Øst-Europa",
       subtitle = "fordeling mellom ulike sektorer i Norge") +
  theme_light()
