# Setter 'working directory' og endrer til norsk språk.
setwd("C:/Users/adria/OneDrive/Dokumenter/5. Semester/SOK-2008/Innlevering 2")

Sys.setlocale(locale="no_NO")

# Laster inn pakker jeg trenger

library(readr)
library(ggplot2)
library(tidyverse)


union<- read_csv("union_unempl.csv") # laster inn datasettet

# Endrer navn fra United Kingdom til UK (for å matche med map-datasettet)
union$country <- gsub("United Kingdom", "UK", union$country)


# Endrer navn på kolonne for land fra country til region (for å matche med map-datasettet)
names(union)[names(union) == "country"] <- "region"


# Oppgave 1
map <- map_data("world")

map <- left_join(map, union, by="region")

map <- map %>% 
  filter(!is.na(map$iso3c))

ggplot(map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = unempl), color = "black") +
  scale_fill_gradient(name = "Europeisk \narbeidsledighets-\nrate",
                      low = "white",
                      high = "red",
                      na.value = "grey50") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        rect = element_blank())

# Oppgave 2
fagforening <- ggplot(map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = density), color = "black") +
  scale_fill_gradient(name = "Europeisk \nfagforeningsdensitet",
                      low = "white",
                      high = "red",
                      na.value = "grey50") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        rect = element_blank())
fagforening

coverage <- ggplot(map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = coverage), color = "black") +
  scale_fill_gradient(name = 'Europeisk \n"Excess coverage"',
                      low = "white",
                      high = "red",
                      na.value = "grey50") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        rect = element_blank())
coverage

lønnsfastsettelse <- ggplot(map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = coord), color = "black") +
  scale_fill_brewer(name = "Europeisk \nkoordinering av \nlønnsfastsettelse",
                      palette = "OrRd") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        rect = element_blank())
lønnsfastsettelse

# Inspirasjon er hentet fra: https://www.youtube.com/watch?v=AgWgPSZ7Gp0
