### Parking Pilot ODP
### EDA
### nathancday@gmail.com
### 2018-02-18
setwd("~/future/parking_meters/")

### Packs ---------------------------------------------------------
library(geojsonio)
library(spdep)
library(leaflet)
library(lubridate)
library(viridis)
library(ggsci)
library(magrittr)
library(tidyverse)

### Hacks ---------------------------------------------------------

scale_fill_discrete <- function(...) {scale_fill_viridis(option = "D", ...)}
scale_color_discrete <- function(...) {ggsci::scale_color_d3(...)}

### Ins -----------------------------------------------------------

# data
dat <- geojson_read("https://opendata.arcgis.com/datasets/d68e620e74e74ec1bd0184971e82ffaa_15.geojson",
                    parse = TRUE)
dat %<>% .[["features"]] %>% .[[2]]

### Clean -------------------------------------------------------
names(dat) %<>% tolower() # easy

# get dates classed correctly

dat$date <- as.POSIXct(dat$date_payment, format = "%Y/%m/%d")

# get time class correctly ()

tmp <- strsplit(dat$parkingendtime, " ") # split column into date, clock:time, AM/PM parts

dat$time <- map_chr(tmp, ~paste(.[1], .[2])) %>% # get clock:time from each 2nd slot
    as.POSIXct(format = "%m/%d/%Y %H:%M:%S")

pm_add <- ifelse(grepl("PM", dat$parkingendtime) & !grepl(" 12:", dat$time), #
                 43200,
                 0)

dat$time %<>% add(pm_add)

### Explore ----------------------------------------------------

sum(dat$total)

# * Locations ----
locs <- distinct(dat, meter_lat, meter_long) # 41 of them

leaflet(locs) %>%
    addTiles() %>%
    addMarkers(lat= ~meter_lat, lng= ~meter_long)

# * Time of Day -----

dat$hour <- hour(dat$time)

ggplot(dat, aes(hour)) +
    geom_bar()

# calculate revenue
group_by(dat, hour) %>%
    summarise(rev = sum(total)) %>%
    ggplot(aes(hour, rev)) +
    geom_col()

# * Day of week -----
dat$day <- wday(dat$date, T)

ggplot(dat, aes(day)) +
    geom_bar()

# nothing happens on Sundays.
dat %<>% filter(day != "Sun")

rev_by_day <- group_by(dat, day) %>% 
    summarise(revenue = sum(total))

ggplot(rev_by_day, aes(day, revenue)) +
    geom_col()

# look at hour and day
group_by(dat, hour, day) %>%
    summarise(rev = sum(total)) %>%
    filter(rev > 0) %>% # well see why below
    ggplot(aes(hour, rev, colour = day, group = day)) +
    geom_path(size = 1.5) +
    scale_color_d3()

# * Average Fair ----

hist(dat$total) # huh negative

filter(dat, transactiontype == "Collect. Card") %>% with(hist(total))
# some sort of cash-out event for the system?

dat %<>% filter(transactiontype != "Collect. Card")

hist(dat$total)
mean(dat$total)
sum(dat$total)

# * Time course -----

dat$week <- isoweek(dat$date) %>% as.factor()

ggplot(dat, aes(week)) +
    geom_bar()

week_rev <- group_by(dat, week, day) %>%
    summarise(rev = sum(total))

ggplot(week_rev, aes(week, rev, fill = day)) +
    geom_col()

# average week
week_rev %>% filter(week %in% 37:45) %>%
    summarise(week_total = sum(rev)) %>%
    with(mean(week_total))

# * $$$ $pots -----

spot_rev <- group_by(dat, spacename) %>%
    summarise(rev = sum(total))

hist(spot_rev$rev)

low_spots <- filter(spot_rev, rev < 1000)
sum(low_spots$rev)
high_spots <- filter(spot_rev, rev > 1000)
sum(high_spots$rev)

spot_rev <- group_by(dat, spacename, day) %>%
    summarise(rev = sum(total))



ggplot(spot_rev, aes(spacename, rev, fill = day)) +
    geom_col()

ggplot(spot_rev, aes(spacename, rev, fill = rev)) +
    geom_col()

# map it
spot_locs <- group_by(dat, spacename, meter_lat, meter_long) %>%
    summarise(rev = sum(total))

pal <- colorNumeric(
    palette = "viridis",
    domain = spot_locs$rev)

leaflet(spot_locs) %>%
    addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
    addCircleMarkers(lat= ~meter_lat, lng= ~meter_long,
                     color = ~pal(rev), fillOpacity = ~scale(rev))



### * Occupied ----

# Caluclate parking paid for in seconds
dat %<>% mutate(paid_seconds = (total / 1.8) * 60 * 60)

# Subtract to get start time
dat %<>% mutate(start_time = time - paid_seconds,
                occ = interval(start_time, time))

# saveRDS(dat, "~/Desktop/parking_dat.RDS")

# test with one spot
space <- filter(dat, spacename == "S42S")

# calculate time sequence on half hour from start to finish
test_times <- seq(min(dat$start_time) + (25*60) + 20 , max(dat$time), by = "1 hour")

tt <- tibble(test_times)
tt$hour <- hour(test_times)
tt %<>% filter(hour %in% c(8:19))
tt$interval <- interval(tt$test_times, tt$test_times + hours(1))

# rep it parallel because 15-cores took > 2 hrs
library(parallel)
# res <- mclapply(tt$interval, function(x){any(map_lgl(space$occ, ~ int_overlaps(., x) ) )})

# do this for all single spaces
singles <- filter(dat, metertype == "Singlespace")

singles %<>% split(.$spacename)

# # set occupied interval appropriately
# for (i in 1:length(singles)) {
#     singles[[i]]$occ <- interval(singles[[i]]$start_time, singles[[i]]$time)
# }
# 
# 
# res <- list()
# for (i in 1:length(singles)) {
#     res[[i]] <- mclapply(tt$interval,
#                          function(x){any(map_lgl(singles[[i]]$occ, ~ int_overlaps(., x) ) )},
#                          mc.cores = 15)
# }

res <- readRDS("~/Desktop/parking_interval_result.RDS")
names(res) <- names(singles)

df <- map_df(res, unlist)

# average occupancy
space_occ <- (colSums(df) / 888)

hist(space_occ)

# bind to tt
names(df) %<>% paste0("p_", .)

df %<>% bind_cols(tt, .)

# go long
dfl <- gather(df, "spacename", "occ_bool", starts_with("p_"))

saveRDS(dfl, "dfl.RDS")

# hourly average
dfl %>% group_by(spacename, hour) %>%
    summarise(pct_occ = sum(occ_bool) / n()) %>%
    ggplot(aes(hour, spacename, fill = pct_occ)) +
    geom_tile() +
    scale_fill_viridis() +
    scale_x_continuous(breaks = c(8,12,17), labels(c("8:30a", "12:30p", "5:30p"))) +
    labs(y = NULL,
         fill = "% Occupied")
