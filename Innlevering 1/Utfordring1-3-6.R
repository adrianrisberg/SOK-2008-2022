library(PxWebApiData)
#Hvilke variabler som finnes i tabellen
variables <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                     returnMetaFrames = TRUE)
names(variables)

#hvilke verdier har ulike variablene
values <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                  returnMetaData = TRUE)
#Kommunekoder
values[[1]]$values
#Inntekt fC8r/etter skatt
values[[2]]$values # 00 = Samlet inntekt, 00S=Inntekt etter skatt
#Desiler
values[[3]]$values
#Statistikkvariabel
values[[4]]$values
#Cr
values[[5]]$values
data <- ApiData("https://data.ssb.no/api/v0/en/table/12558/",
                Tid =c("2005","2020"), # Velg C%rene 2005 og 2020
                Desiler=c("01", "02", "03" ,"04", "05", "06" ,"07", "08" ,"09", "10"), #Vi velger alle desiler
                InntektSkatt="00", #Vi velger samlet inntekt
                ContentsCode="VerdiDesil", #Velger den høyeste verdien i desilen
                Region=c("5401","1902")) #Tromsø endret kommunenummer i 2020

library(dplyr)
library(gglorenz)

# Endrer listen til en dataframe
df <- data$dataset %>%
  as_tibble()

# Rydder i dataframen
df05 <- df %>%
  filter(Tid == 2005) %>% # filtrerer etter kun år 2005
  subset(select = -NAstatus) %>% # fjerner unødvendig kolonne
  na.omit() # fjerner rader med NA i kolonnen value

# Plotter Lorenz-kurven for år 2005
df05 %>% 
  ggplot(aes(value)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  labs(title = "Inntektsfordeling i Tromsø kommune i 2005",
       x = "Andel av befolkning",
       y = "Andel av inntekt") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)+
  annotate_ineq(df05$value,
                decimals = 5,
                x = 0.5) +
  theme_light()

# Rydder
df20 <- df %>%
  filter(Tid == 2020) %>%  # filtrerer etter kun år 2020
  subset(select = - NAstatus) %>% # fjerner unødvendig kolonne
  na.omit() # fjerner rader med NA som value


# Plotter Lorenz-kurven for år 2020
df20 %>% 
  ggplot(aes(value)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  labs(title = "Inntektsfordeling i Tromsø kommune i 2020",
       x = "Andel av befolkning",
       y = "Andel av inntekt") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)+
  annotate_ineq(df20$value,
                decimals = 5,
                x = 0.5) +
  theme_light()

df05
df20

# Kommentar ligger i PDF-fil

 
