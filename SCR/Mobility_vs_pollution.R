google <- read_csv('./DATA/Misc/Global_Mobility_Report.csv')
names(google) <- c('country_code', 'country', 'sr1', 'sr2', 'date',
                   'Retail','Grocery', 'Parks', 'Transit', 'Workplace','Residential')
googleAgg <- google %>%
  filter(date < ymd(20200515)) %>%
  left_join(lockdown %>% dplyr::select(country, lcDate), by='country') %>%
  mutate(dateRel = yday(date) - yday(lcDate)) %>%
  filter(dateRel > 0) %>%
  group_by(country) %>%
  summarise_at(vars(Retail:Residential), mean, na.rm=TRUE)

apple <- read_csv('./DATA/Misc/applemobilitytrends-2020-05-21.csv') %>%
  filter(geo_type == "country/region") %>%
  gather(date, val, "2020-01-13":"2020-05-21") %>%
  mutate(date = ymd(date), country = region) %>%
  dplyr::select(country, date, transportation_type, val) %>%
  filter(date < ymd(20200515))

appleAg <- apple %>%
  left_join(lockdown %>% dplyr::select(country, lcDate), by='country') %>%
  mutate(dateRel = yday(date) - yday(lcDate)) %>%
  filter(dateRel > 0) %>%
  group_by(country,transportation_type) %>%
  summarise_at(vars(val), mean, na.rm=TRUE)
appleAg

tsAirDiff %>%
  left_join(co2, by='country') %>%
  gather(key, val, Power:Aviation) %>%
  ggplot(aes(x=val, y=diffRel, color=param)) +
  geom_point() +
  geom_smooth(method='lm')+
  scale_x_log10()+
  facet_grid(.~key, scales='free')

tsAirDiff %>%
  left_join(googleAgg, by='country') %>%
  gather(key, val, Retail:Residential) %>%
  ggplot(aes(x=val, y=diffRel, color=param)) +
  geom_point() +
  geom_smooth(method='lm')+
  facet_grid(.~key, scales='free')

dev.off()

tsAirDiff %>%
  left_join(appleAg, by='country')  %>%
  drop_na(diffRel) %>%
  ggplot(aes(x=val, y=diffRel, color=param)) +
  geom_point() +
  geom_smooth(method='lm')+
  facet_grid(.~transportation_type, scales='free')

cyl_names <- c(
  'no2' = 'NO[2]',
  'o3' = 'O[3]',
  'pm25' = 'PM2.5'
)
labs <- c(expression(NO[2]), expression(O[3]), "PM2.5")
cols <- c('#f8766d', '#31cbcf', '#5fd603')

dev.off()

t1 <- tsAirDiff %>%
  filter(!country %in% c('Norway', 'Sweden')) %>%
  left_join(googleAgg, by='country') %>%
  ggplot(aes(x=Workplace, y=diffRel, color=param, fill=param, linetype=param)) +
  geom_point(size=3, alpha=0.3) +
  geom_smooth(method='lm', alpha=0.2) +
  scale_color_manual(values = cols, labels =labs)+
  scale_fill_manual(values = cols, labels =labs)+
  scale_linetype_manual(values = c(1,2,2)) +
  ylab( expression(paste("Relative pollutant change (%)")))  +
  xlab('Google reports: change in movement to/at workplace (%)') + 
 # theme(plot.margin = grid::unit(c(0,0,0,10), "mm"),
#        plot.background = element_rect(fill = "darkgrey"))+
  theme(legend.position = 'none')

t2<-  tsAirDiff %>%
  filter(!country %in% c('Norway', 'Sweden')) %>%
  left_join(appleAg, by='country') %>%
  filter(transportation_type == "driving") %>%
  ggplot(aes(x=val-100, y=diffRel, color=param, fill=param, linetype = param)) +
  geom_point(size=3, alpha=0.3) +
  geom_smooth(method='lm', alpha=0.2) +
  scale_linetype_manual(values = c(1,2,2))+
  scale_color_manual(values = cols, labels =labs)+
  scale_fill_manual(values = cols, labels =labs)+ 
  guides(linetype = FALSE) +
  xlab('Apple reports: change in driving direction requests (%)') +
 
  theme(axis.title.y = element_blank(),
        legend.text.align = 0,
        legend.title = element_blank())  

grid.arrange(t1,t2, ncol=2, nrow=1,
             widths=c(0.47,0.53), 
             padding = unit(1, "cm"), newpage = T)
cowplot::plot_grid(t1, t2, labels = "AUTO", rel_widths = c(0.47, 0.53))
# Export 1000 x 400
dev.off()
