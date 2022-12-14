library(plotly)
library(tidyverse)

months <- c('202101','202102','202103','202104','202105','202106','202107','202108','202109','202110','202111','202112'
            ,'202201','202202','202203','202204','202205','202206','202207','202208','202209')
url.dummy <- 'https://mobidata-bw.de/daten/eco-counter/eco_counter_fahrradzaehler_'
fileext <- '.csv.gz'
paste(url.dummy,fileext, sep='')

df <- data.frame()
for (month in months) {
  url <- paste(paste(url.dummy, month, fileext, sep=''))
  df <- rbind(df,read_csv(url))
}

df <- df %>%
  filter(!standort %in% c('Stadt Ulm','Stadt Offenburg'))

zeitreihe <- df %>%
  mutate(
    #Tag = as.Date(timestamp)
    Tag = as.Date(cut(timestamp, "week"))
    ) %>%
  group_by(Tag, standort) %>%
  summarize(Zählstand = sum(zählstand))

head(zeitreihe)

# Most basic bubble plot
p <- ggplot(zeitreihe, aes(x=Tag, y=Zählstand)) +
  geom_line(aes(color = standort), size = 0.5) + # + xlab("") # beschriftung x Achse ausblenden
  theme_bw() + 
  #guides(fill=guide_legend(title="Standort")) +
  labs(
    title = "Fahrradzähldaten je Woche",
    subtitle = "Summe der Zählstände je Woche",
    caption = "Quelle: https://mobidata-bw.de/dataset/eco-counter-fahrradzahler") +
  stat_smooth(
    #color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  ) +
  xlab("")
p
ggsave("/home/alex/R/projekte/fahrrad_bw/image/zeitreihe.png", plot = p)

df$Wochentag <- weekdays(df$timestamp)
df$Stunde <- format(df$timestamp, '%H')
df$Monat <- format(df$timestamp, '%m')

heatmap.df <- df %>%
  group_by(Wochentag, Stunde) %>%
  summarize(Zählstand = sum(zählstand))

heatmap.df %>%
  mutate(Wochentag = factor(Wochentag, levels=c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))) %>%
  ggplot(aes(Stunde, Wochentag)) + 
    geom_tile(aes(fill = Zählstand), color = "white") +
    #scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
    theme_minimal()


heatmap.town.df <- df %>%
  group_by(standort,Wochentag, Stunde) %>%
  summarize(Zählstand = sum(zählstand))

p2 <- heatmap.town.df %>%
  mutate(Wochentag = factor(Wochentag, levels=c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))) %>%
  ggplot(aes(Stunde, Wochentag)) + 
  geom_tile(aes(fill = Zählstand), color = "white") +
  scale_fill_gradient(low = "white", high = "blue", na.value = NA) +
  facet_grid(rows = vars(standort)) +
  labs(title = "Fahrradzähldaten im Tagesverlauf",
       subtitle = "Summe der Zählstände je Wochentag und Stunde",
       caption = "Quelle: https://mobidata-bw.de/dataset/eco-counter-fahrradzahler") +
  theme_bw()
ggsave("/home/alex/R/projekte/fahrrad_bw/image/heatmap.png", plot = p2)

ggplot(heatmap.df, aes(Stunde, Wochentag)) + 
  geom_tile(aes(fill = Zählstand),
            colour = "white", 
            na.rm = TRUE) +
  #scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
  #scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Zählstand")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Das ist der Titel",
       x = "Stunde des Tages", y = "Wochentag") 
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

