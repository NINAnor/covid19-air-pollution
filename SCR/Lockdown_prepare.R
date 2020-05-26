### Import lockdown data --------------------------------------------------------------
mobility <- read_csv('./DATA/COVID/internal-movement-covid.csv') %>%
  mutate(Date = anytime(Date))
names(mobility) <- c('country', 'countryCode', 'date', 'restriction')
mobility <- mobility  %>%
  mutate(lag = restriction-lag(restriction),
         lockdown = ifelse(lag != 0 & restriction == lag, 1, NA))  %>%
  drop_na() %>%
  dplyr::select(-restriction, -lag)

home <- read_csv('./DATA/COVID/stay-at-home-covid.csv') %>%
  mutate(Date = anytime(Date))
names(home) <- c('country', 'countryCode', 'date', 'restriction')
home <- home %>%
  mutate(lag = restriction-lag(restriction),
         lockdown = ifelse(lag != 0 & restriction == lag, 1, NA)) %>%
  drop_na() %>%
  dplyr::select(-restriction, -lag)

work <- read_csv('./DATA/COVID/workplace-closures-covid.csv') %>%
  mutate(Date = anytime(Date))
names(work) <- c('country', 'countryCode', 'date', 'restriction')
work <- work  %>%
  mutate(lag = restriction-lag(restriction),
         lockdown = ifelse(lag != 0 & restriction == lag, 1, NA))  %>%
  drop_na() %>%
  dplyr::select(-restriction, -lag)


lockdownAll <- mobility %>% mutate(type = 'mobility') %>%
  bind_rows(home %>% mutate(type = 'home') ) %>%
  bind_rows(work %>% mutate(type = 'work') )


lockdown <- lockdownAll %>%
  group_by(country, countryCode) %>%
  summarise(date = mean(date))

# Write out for other scripts to use
lockdown %>%
  write_csv('./DATA/COVID/lockdown_dates.csv')

# Write out shapefile of lockdown countries
world_shp %>%
  left_join(lockdown %>% mutate(iso_a3 = countryCode)) %>%
  drop_na(date) %>%
  dplyr::select(country, countryCode, date, geometry) %>%
  st_write('./DATA/For GEE/countries_select.shp')
