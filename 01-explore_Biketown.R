# Chris Muhs, Jun 2018

library(tidyverse)
library(lubridate)
library(ggmap)
library(here)

# Read data
mydata <- list.files(
    path = here::here("data", "PublicTripData"),
    pattern = "*.csv",
    full.names = T ) %>%
    map_df(~ read_csv(., col_names = T,
                      cols(
                          RouteID = col_integer(),
                          PaymentPlan = col_character(),
                          StartHub = col_character(),
                          StartLatitude = col_double(),
                          StartLongitude = col_double(),
                          StartDate = col_date(format = "%m/%d/%Y"),
                          StartTime = col_time(format = "%H:%M"),
                          EndHub = col_character(),
                          EndLatitude = col_double(),
                          EndLongitude = col_double(),
                          EndDate = col_date(format = "%m/%d/%Y"),
                          EndTime = col_time(format = "%H:%M"),
                          TripType = col_character(),
                          BikeID = col_integer(),
                          BikeName = col_character(),
                          Distance_Miles = col_double(),
                          Duration = col_time(format = "%H:%M:%S"),
                          RentalAccessPath = col_character(),
                          MultipleRental = col_logical()
                      ))) %>%
    mutate(start_wkday = wday(StartDate, label = T)) %>%
    drop_na(start_wkday)

# testdata = sample_n(mydata, 5000)
mymap <- get_map(location = c(lon = median(mydata$EndLongitude, na.rm = T) + 0.01, 
                              lat = median(mydata$EndLatitude, na.rm = T) + 0.005), 
                 zoom = 13, maptype = "toner-lite")

pdxmap <- ggmap(mymap, 
                base_layer = ggplot(data = mydata, 
               aes(x = EndLongitude, y = EndLatitude)))

pdf(file = here::here("plot.pdf"), paper = "USr", width = 10, height = 8)
pdxmap +
    stat_density2d(aes(x = EndLongitude, y = EndLatitude,
                       fill = ..level.., alpha = ..level..),
                   bins = 5, geom = "polygon", data = mydata) +
    scale_fill_gradient(low = "gold", high = "#ff4716") +
    facet_wrap(~ start_wkday, nrow = 2) +
    ggtitle("Where are you going?",
            subtitle = "Density plot of BIKETOWN trip ends by day of week \nChris Muhs, cdmuhs@gmail.com\ngithub.com/cdmuhs/cRaggy")
dev.off()