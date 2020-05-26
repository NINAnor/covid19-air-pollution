
### Import population data for cities ---------------------------------------------------
cityPop <- read_csv('./DATA/Misc/pop_dens_cities.csv') %>%
  mutate(population = mean) %>%
  dplyr::select(city_id, population)
cityPop


### Import and clean OpenAQ data from AWS server ----------------------------------------
# Convert to common units
#concentration (mg/m3) = 0.0409 x concentration (ppm) x molecular weight
o2mol = 48
no2mol = 46.01
cityAir <- read_delim('./DATA/Stations/openaq/city_day_agg.csv', delim=',') %>%
  mutate(countryCode = countrycode(country, origin = 'iso2c', destination = 'iso3c')) %>%
  drop_na() %>%
  mutate(date = ymd(paste(year, month, day, sep='-'))) %>%
  filter(date < ymd(20220101), date > ymd(20150101), parameter %in% c("no2", "o3", "pm25")) %>%
  mutate(unit = factor(unit), parameter=factor(parameter)) %>%
  mutate(mean = ifelse(unit == "ppm" & parameter == "no2", (0.0409*mean*no2mol), 
                              ifelse(unit == "ppm" & parameter == "o3", (0.0409*mean*o2mol), 
                                     mean)),
         unit = "Âµg/mÂ³") %>%
  drop_na(mean) %>%
  mutate(month=as.numeric(month))

# Inspect for outliers
View(cityAir %>%
       group_by(countryCode, city, parameter) %>%
       mutate(zscore = outliers::scores(mean)) %>%
       mutate(outlier = ifelse((zscore >3 | zscore < -3), 1, 0)) %>%
       summarise(outs = sum(outlier)))

# Filter out:
# Dates older than 2017
# Extreme pollution values manually and with zscore detection
# any day with < 24 readings (less than one station operating)
cityAir <- cityAir %>%
  filter (date >= ymd(20170101)) %>%
  group_by(countryCode, city, parameter) %>%
  mutate(zscore = outliers::scores(mean)) %>%
  mutate(outlier = ifelse((zscore >3 | zscore < -3), 1, 0)) %>%
  ungroup() %>%
  filter(outlier == 0) %>% 
  filter(mean < 1000, mean >0 ,count > 24) 
unique(cityAir$countryCode)

# Assign unique city ID
cityAir <- cityAir %>% 
  mutate(city_id = group_indices(., city))


# Export city Lat-Lon coords for Google Earth Engine - only run once
#cityAir %>%
#  group_by(city_id, countryCode) %>%
#  summarise(Lon = mean(Lon), Lat = mean(Lat)) %>%
#  write_csv('./DATA/For GEE/openaq_cities.csv')

# Export cleaned city daily time series
cityAirClean <- cityAir %>%
  dplyr::select(country, countryCode, city_id, date, parameter, mean)
cityAirClean %>%
    write_csv('./DATA/Stations/openaq/city_day_agg_cleaned.csv')


cityAirClean %>%
  ggplot(aes(x=date)) +
  geom_histogram()

### Aggregate to country level -------------------------------------------------
# Need to calcualte population weighted averages

countryAir<- cityAirClean  %>%
  left_join(cityPop, by=c('city_id') ) %>%
  group_by(countryCode, date, parameter) %>%
  summarise(mean = weighted.mean(mean, population, na.rm=TRUE))

# Write out for use in other scripts
countryAir %>%
  write_csv('./DATA/Stations/openaq/country_day_agg_cleaned.csv')
