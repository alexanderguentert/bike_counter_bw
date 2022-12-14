library(plotly)
library(tidyverse)

# set working directory to the dir of the script (Rstudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source <- 'eco_counter_fahrradzaehler.csv'
source <- 'https://mobidata-bw.de/daten/eco-counter/eco_counter_fahrradzaehler.csv'
f <- read.csv(source, header=TRUE, sep=',')
f$time <- strptime(f$timestamp,format = '%Y-%m-%dT%H:%M:%S')

beispiel <- f[f$standort=='Stadt Heidelberg' & f$channel_name=='Channel 1 Richtung West',]
beispiel
plot(x=beispiel$timestamp, y=beispiel$zählstand)

fig <- plot_ly(beispiel, x = ~timestamp, y = ~zählstand, 
               type = 'scatter', mode = 'lines') %>%
  layout(title=
           paste(beispiel$standort[1],beispiel$channel_name[1],beispiel$counter_site[1]) 
                 ) 
fig

fig <-  plot_ly(data = f[f$standort=='Stadt Heidelberg',],
    type = 'scatter', mode = 'lines',
    x = ~timestamp,
    y = ~zählstand,
  )
fig

beispiel2 <- f[f$standort=='Stadt Heidelberg' & f$counter_site=='Plöck',]

fig <- plot_ly(type = 'scatter', mode = 'lines') %>%
  layout(
    title = paste(beispiel2$standort[1],beispiel2$counter_site[1]) 
  )
for (ch_name in unique(beispiel2$channel_name)) {
  filter.df <- beispiel2[beispiel2$channel_name == ch_name,]
  fig <- fig %>% add_trace(x = filter.df$timestamp,y = filter.df$zählstand, name = ch_name, mode = 'lines') 
}
fig

f.grouped <- f %>%
  group_by(longitude,latitude,counter_site,channel_name,standort) %>% summarise(
    summe = sum(zählstand)
  )


map <- f.grouped 
map <- map %>%
  plot_ly(
    lat = ~latitude,
    lon = ~longitude,
    marker = list(opacity=0.5), #,color ="blue"),
    type = 'scattermapbox',
    size = ~summe,
    #alpha = 0.9,
    # frame = ~timestamp(),
    #hoverinfo = 'text',
    hovertext = ~counter_site #f[,c("counter_site")]
) 
map <- map %>%
  layout(title = 'Fahrradzähldaten BW',
    mapbox = list(
      style = 'open-street-map',
      zoom =5.5,
      center = list(lat = ~mean(latitude),
                    lon = ~mean(longitude))
      )
    ) 
map

  



