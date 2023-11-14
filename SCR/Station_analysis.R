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
  bind_rows(work %>% mutate(type = 'work') ) %>%
  mutate(date = ymd(date))


lockdown <- lockdownAll %>%
  group_by(country, countryCode) %>%
  summarise(lcDate = mean(date))

### Import other data needed -------------------------------------------------

continents <- ne_countries(scale = "medium", returnclass = "sf")   %>%
  dplyr::select(iso_a3, economy, continent, -geometry) %>%
  as_tibble() %>% dplyr::select(-geometry)

countries <- ne_countries(scale = "medium", returnclass = "sf")  %>%
  mutate(country = countrycode(iso_a3, origin = 'iso3c', destination = 'country.name'))  %>%
  dplyr::select(country) 
world_shp <- ne_countries(scale = "medium", returnclass = "sf")

countPop <- read_csv('./DATA/Health/pop_count_18_country.csv') %>%
  mutate(iso_a3 = adm0_a3) %>%
  left_join(continents) %>%
  mutate(country = countrycode(adm0_a3, origin = 'iso3c', destination = 'country.name')) %>% 
  dplyr::select(-adm0_a3, -iso_a3)
cityPop <- read_csv('./DATA/Misc/pop_dens_cities.csv') %>%
  mutate(population = mean) %>%
  dplyr::select(city_id, population)

cityAirClean <- read_csv( './DATA/Stations/openaq/city_day_agg_cleaned.csv')
cityLookup <- read_csv('./DATA/For GEE/openaq_cities.csv')
cityAirClean <- cityAirClean %>% left_join(cityLookup, by=c('countryCode', 'city_id'))

countryAir <- read_csv('./DATA/Stations/openaq/country_day_agg_cleaned.csv')
unique(countryAir$countryCode)

countryAir %>%
  left_join(lockdown , by='countryCode') %>%
  drop_na(lcDate) %>%
  filter(date > ymd(20200101), parameter == "no2") %>%
  ggplot(aes(x=date, y=mean)) +
  geom_point() +
  facet_wrap(~countryCode)

# Filter out countries with missing data
countryAir <- countryAir %>%
  filter(!countryCode %in% c('BEL', 'BGR', 'CYP', 'MNG', 'SWE', 'ROU', 'EST', 'GRC'))

# Climate data for meteorogical corrections later on
clim <- read_csv('./DATA/Climate/gfsDaily_allcities_updated.csv') %>%
  dplyr::select(-"system:index",-".geo")%>%
  mutate(date = as.Date(date)) %>%
  #mutate(countryCode = countrycode(cntryCd, origin = 'iso3c', destination = 'iso3c')) %>%
  group_by(countryCode, date) %>%
  summarise_at(vars(downward_shortwave_radiation_flux:v_component_of_wind_10m_above_ground), mean, na.rm=TRUE)%>%
  mutate(wind_abs = sqrt(u_component_of_wind_10m_above_ground^2 + v_component_of_wind_10m_above_ground^2)) %>%
  ungroup()
unique(clim$countryCode)


### Make point change maps -----------------------------------------------------
# Calculate simple differnece in air pollution concentration between 3-yr baseline and lockdown months
ptDiff <- cityAirClean %>%
  mutate( month = month(date)) %>%
  mutate(test1 = ifelse(date < ymd(20200201) & month %in% c(2,3,4,5), "yBaseline",
                        ifelse(date >= ymd(20200201), "y2020", NA)))%>%
  drop_na(test1) %>%
  group_by(Lat, Lon, city_id, countryCode, test1, parameter) %>%
  summarise(mean =mean(mean, na.rm=TRUE)) %>%
  pivot_wider( names_from= test1, values_from=mean) %>%
  mutate(diffPerc = (y2020-yBaseline)/yBaseline*100)%>%
  mutate(diffAbs = y2020-yBaseline) %>%
  drop_na(diffPerc)
hist(ptDiff$diffPerc)
unique(ptDiff$parameter)

labelNo2H <- expression(paste(NO[2]," (", mu, g, "/", m^3,")", sep=""))
labelO3H <- expression(paste(O[3]," (",mu, g, "/", m^3,")", sep=""))
labelPM25H <- expression(paste(" PM2.5 ",mu, g, "/", m^3, sep=""))

no2LimsH <- c(0,70)
o3LimsH <- c(0,200)
pm25LimsH <- c(0,120)

makeHistPlotPt <- function(param, limits, label){
  
  toPlot <- ptDiff  %>%
    filter(parameter == param)  %>% 
    mutate("2020" = y2020, "3-yr av." = yBaseline) %>%
    gather(key, val, "2020","3-yr av.") %>%
    group_by(key) %>%
    mutate(med = median(val)) %>%
    filter(val < limits[2], val>limits[1]) 
  
  plot <- toPlot %>%
    ggplot(aes(x=val, fill=key)) +
    geom_density(alpha=0.4, color=NA) +
    #geom_histogram(position='dodge') +
    geom_vline(aes(xintercept = med, color=key), linetype=1, size=1) +
    ylab("Data density") +
    xlab(label)  +
    theme(
      legend.title = element_blank(),
      legend.position = c(.75, .85),
      panel.background = element_rect(fill = alpha('white', 0.2)),
      plot.background=element_rect(fill = alpha('white', 0.2)),
      legend.background = element_rect(fill=alpha('white', 0.2))
    )
  return (plot)
}


h1Pt <- makeHistPlotPt("no2", no2LimsH,labelNo2H)
h2Pt <- makeHistPlotPt("o3", o3LimsH,labelO3H)
h3Pt <- makeHistPlotPt("pm25", pm25LimsH,labelPM25H)

labelNo2Pt <- expression(paste(Delta,NO[2]," (%)", sep=""))
labelO3Pt <- expression(paste(Delta,O[3]," (%)", sep=""))
labelPM25Pt <- expression(paste(Delta," PM2.5 "," (%)", sep=""))


makePointMap <- function(param, label1, label2, inset,panelLab){
  plot <- df %>%
    filter(parameter == param)  %>%
    ggplot()  +
    geom_sf(data = world_shp, 
            fill = NA, 
            color = '#000000',
            size = 0.09,
            alpha=0.9) +
    geom_point(aes(x = Lon, y = Lat, color = diffPerc), shape=21, stroke=0.5, size=2)+
    coord_sf( expand = FALSE)+
    xlim(-130,155) +
    ylim(-60,75)+ 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = c(.05, .05),
      legend.justification = c("left", "bottom"),
      legend.box.just = "left",
      plot.margin=grid::unit(c(0,0,0,0), "mm")
    )+
    xlab('') +ylab('') + 
    #scale_size_continuous(name=label2, limits = c(0, 25),breaks=c(5,15,25),range = c(0, 10)) +
    #scale_size_continuous(name=label2) +
    scale_color_gradientn(
      name=label1,
      limits = c(-25, 25), 
      oob = scales::squish,
      #breaks = c(-25,15,0,15,25),
      colours = rev(selectPal))
  g2 <- ggplotGrob(inset)
  g3 <- grobTree(textGrob(panelLab, x=0.01,  y=0.95, hjust=0,
                          gp=gpar(fontsize=20, fontface="bold")))
  plot <- plot + 
    #annotation_custom(grob = g2, xmin=-16, xmax=10, ymin=60, ymax=74)+ 
    annotation_custom(grob = g2, xmin=40, xmax=115, ymin=-58, ymax=5) + 
    annotation_custom(g3)
  
  return (plot)
}
makePointMap("no2", labelNo2Pt, labelNo2Pt2, h1Pt, "A")

p1 <- makePointMap("no2", labelNo2Pt, labelNo2Pt2, h1Pt, "A")
p2 <- makePointMap("o3", labelO3Pt, labelO3Pt2, h2Pt, "B")
p3 <- makePointMap("pm25", labelPM25Pt, labelPM25Pt2, h3Pt, "C")

p <- grid.arrange(p1,p2,p3, ncol=1,nrow=3, heights=c(0.33,0.33,0.33), padding = unit(0, "line"), newpage = T)
#Export as 1200:1500
dev.off()

ggsave(filename = "fig1.svg",
       width = 1200, height=1500, units='px',
       plot = p)

getCityChangeStats <- function(){
  ptDiff %>%
    left_join(cityPop, by='city_id') %>%
    drop_na(population) %>%
    ungroup() %>% 
    group_by(parameter) %>%
    summarise(diffPerc_IQR = IQR(diffPerc, na.rm=TRUE),
              diffAbs_IQR = IQR(diffAbs, na.rm=TRUE),
              diffPerc = weighted.mean(diffPerc, population, na.rm=TRUE),
              diffAbs = weighted.mean(diffAbs, population, na.rm=TRUE))
}
getCityChangeStats()


### Climate model ----------------------------------------------------------

# Merge climate and station data
stat <- countryAir %>%
  left_join(clim, by=c('date', 'countryCode')) %>%
  drop_na() %>%
  filter(date < ymd(20200515))%>%
  group_by(countryCode) %>%
  mutate(meanRel = scale2(mean))
unique(stat$countryCode)

# populate with predictor variables
stat <- stat  %>%
  mutate(month = month(date) ,
         day = yday(date), 
         day_week = wday(date), 
         week = week(date))%>%
  mutate(day_sin = sin((day - 1)*(2*3.14/365)), day_cos = cos((day - 1)*(2*3.14/365)))%>%
  mutate(day_week_sin = sin((day_week - 1)*(2*3.14/7)), day_week_cos = cos((day_week - 1)*(2*3.14/7))) %>%
  mutate(mnth_sin = sin((month - 1)*(2*3.14/12)), mnth_cos = cos((month - 1)*(2*3.14/12)))%>%
  mutate(week_sin = sin((week - 1)*(2*3.14/56)), week_cos = cos((week - 1)*(2*3.14/56)))%>% 
  ungroup() %>%
  mutate_at(vars(total_precipitation_surface, temperature_2m_above_ground, wind_abs), list(lag=lag))

# Create output data frame to populate in modelling loop
output <-data.frame(predVar=NA, countryCode=NA,param=NA,r2=NA,fstat=NA,pval=NA,
                    predic=NA,predLwr=NA, predUpr=NA, observed=NA, 
                    nObs=NA, date=NA)

params <- unique(stat$parameter)
lcDatesSel <- lockdown %>% drop_na()
countries <- unique(lcDatesSel$countryCode)

predVars <- c('mean', 'meanRel')
x <- 1
i <- 2
p <- "mean"

for (p in predVars){
  predVar <- p
  
  for (i in 1:length(countries)){
    
    countSelect <- countries[i]
    print(countSelect)
    
    lcDate <- lcDatesSel %>% filter(countryCode == countSelect)
    lcDate <- lcDate$lcDate
    dateThresh <- lcDate - days(1)
    
    for (x in 1:length(params)){
      
      paramSelect <- params[x]
      
      lmDatTrain <- stat %>% 
        filter(countryCode == countSelect, parameter == paramSelect, date < dateThresh) %>%
        drop_na()
      #Sys.sleep(2)
      
      if (nrow(lmDatTrain) < 10){
        print('breaking params 1')
        next
      }
      lmDatTest <- stat %>% 
        filter(countryCode ==countSelect, parameter == paramSelect, date >= dateThresh)
      #hist(lmDatTrain$mean)
      if (nrow(lmDatTest) == 0){
        print('breaking params 2')
        next
      }
      
      lm <- lm(as.data.frame(lmDatTrain)[, predVar] ~ mnth_sin + mnth_cos + week_sin + week_cos + 
                 day_sin + day_cos + day_week_sin + day_week_cos +
                 downward_shortwave_radiation_flux + 
                 precipitable_water_entire_atmosphere + relative_humidity_2m_above_ground + 
                 temperature_2m_above_ground + total_cloud_cover_entire_atmosphere +
                 total_precipitation_surface + wind_abs +
                 total_precipitation_surface_lag + wind_abs_lag + temperature_2m_above_ground_lag , 
               data=lmDatTrain)
      sum <- summary(lm)
      fstat <- sum$fstatistic[[1]]
      sum$coefficients
      pval <- broom::glance(lm)$p.value 
      r2 <- sum$r.squared
      
      for (r in 1:nrow(lmDatTest)){
        
        lmDatTestSelect <- as.data.frame(lmDatTest)[r, ]
        observed <- as.data.frame(lmDatTest)[r, predVar]
        date <-  as.data.frame(lmDatTest)[r, "date"]
        
        pred <- predict(lm, newdata = lmDatTestSelect, interval = "confidence")
        predic <- pred[1,1]
        predLwr <- pred[1,2]
        predUpr <- pred[1,3]
        
        outputNew <- data.frame(predVar=predVar, countryCode=countSelect,param=paramSelect,r2=r2, fstat=fstat,pval=pval,
                                predic=predic,predLwr=predLwr, predUpr=predUpr,
                                observed=observed, nObs = nrow(lmDatTrain), date = date)
        output <- output %>%
          bind_rows(outputNew)
        
        
      }
      
    }
  }
}

unique(output$countryCode)
length(unique(output$countryCode))

lmResults <- as_tibble(output) %>% 
  drop_na(predVar) %>%
  mutate(country = countrycode(countryCode, origin = 'iso3c', destination = 'country.name')) %>%
  group_by(country, date, param, predVar) %>%
  summarise_at(vars(r2:nObs), mean)

tsAir <- lmResults %>%
  left_join(lockdown  %>% dplyr::select(country, lcDate), 
            by="country") %>%
  mutate(dateRel = yday(date) - yday(lcDate))  %>% ungroup() %>%
  ungroup()

tsAirDiff <- tsAir %>%
  filter(dateRel > 0) %>%
  filter(predVar == "mean")  %>%
  group_by(country, param) %>%
  drop_na() %>%
  summarise_at(vars(r2:observed), mean) %>%
  mutate(diff = observed-predic,
         diffUpr=observed- predUpr, 
         diffLwr=observed-predLwr,
         diffRel = (observed-predic)/observed*100,
         diffUprRel = (observed-predUpr)/observed*100,
         diffLwrRel = (observed-predLwr)/observed*100)

countSelect <- unique(tsAirDiff$country)

lmResults %>%
  filter(predVar == 'mean')  %>%
  filter(country %in% countSelect) %>%
  group_by(country, param) %>%
  summarise_at(vars(r2, fstat, pval), mean) %>%
  pivot_wider(names_from=param, values_from = c(r2,fstat,pval)) %>%
  write_csv('./DATA/Output/regression_results.csv')

### Make aggregate change plot -----------------------------------------------------

LabP <-  expression(paste("Absolute change [obs. - benchmark]"," (", mu, g, "/", m^3,")", sep=""))

LabP2 <-  expression(paste("Relative change [(obs. - benchmark)/obs.*100]"," (%)", sep=""))

cyl_names <- c(
  'no2' = 'NO[2]',
  'o3' = 'O[3]',
  'pm25' = 'PM2.5'
)

par <- "no2"
df <- tsAirDiff
makePredicPlot <- function(df, label){
  plot <- df %>%
    #filter(param == par) %>%
    ggplot(aes(x=country, y=diff, color=diff)) + 
    #geom_bar(stat="identity", color="black") +
    geom_point(shape=20,aes(size=r2)) +
    geom_errorbar(aes(ymin=diffLwr, ymax=diffUpr), width=0.5) +
    scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(10, 'PuOr')),
                          limits = c(-10, 10), 
                          oob = scales::squish,
                          name="") + 
    facet_grid(~param, scales='free',
               labeller = labeller(param  = as_labeller(cyl_names,  label_parsed))
    )+
    coord_flip() +
    geom_hline(yintercept = 0, linetype=2) +
    ylab(label) +
    xlab('')+ 
    theme(
      strip.background =  element_rect(
        fill=NA, linetype="solid"
      ),
      panel.grid.major.y = element_line( size=.1, color="grey" ),
      legend.position='none',
      strip.text = element_text(
        size = 10, color = "black", face = "bold.italic"
      )
    )
  return (plot)
}
makePredicPlotRel <- function(df, label){
  plot <- df %>%
    #filter(param == par) %>%
    ggplot(aes(x=country, y=diffRel, color=diffRel)) + 
    #geom_bar(stat="identity", color="black") +
    geom_point(shape=20,aes(size=r2)) +
    geom_errorbar(aes(ymin=diffLwrRel, ymax=diffUprRel), width=0.5) +
    scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(10, 'PuOr')),
                          limits = c(-60, 60), 
                          oob = scales::squish,
                          name="") + 
    facet_grid(~param, scales='free',
               labeller = labeller(param  = as_labeller(cyl_names,  label_parsed))
    )+
    coord_flip() +
    geom_hline(yintercept = 0, linetype=2) +
    ylab(label) +
    xlab('')+ 
    theme(
      strip.background =  element_rect(
        fill=NA, linetype="solid"
      ),
      legend.position='none',
      panel.grid.major.y = element_line( size=.1, color="grey" ),
      strip.text = element_text(
        size = 10, color = "black", face = "bold.italic"
      )
    )
  return (plot)
}

pp1<- makePredicPlot(tsAirDiff,LabP)
pp2<- makePredicPlotRel(tsAirDiff,LabP2)

ggarrange(pp1, pp2, nrow = 2, labels = c("A", "B"))
# Export 850 x 750


getTotalChangeStats <- function(){
  
  tsAirDiff %>%
    left_join(countPop, by='country') %>%
    gather(key, val, diff:diffLwrRel) %>%
    group_by(key) %>%
    summarise(val = weighted.mean(val, pop18))
}
getTotalChangeStats()


getChangeStats <- function(){
  
  tsAirDiff %>%
    left_join(countPop, by='country') %>%
    gather(key, val, diff:diffLwrRel) %>%
    group_by(param, key) %>%
    summarise(val = weighted.mean(val, pop18)) %>%
    pivot_wider(values_from = val, names_from = param)
}
getChangeStats()

### Make time series plot -----------------------------------------------------

tsAirSmooth <- tsAir  %>% 
  filter(country %in% countSelect) %>%
  group_by(param, predVar) %>%
  mutate(lag1=lag(observed),lag2=lag(observed,2),lag3=lag(observed,3),lag4=lag(observed,4),observed=(observed+lag1+lag2+lag3 +lag4)/5,
         lag1=lag(predic),lag2=lag(predic,2),lag3=lag(predic,3),lag4=lag(predic,4),predic=(predic+lag1+lag2+lag3 +lag4)/5,
         lag1=lag(predUpr),lag2=lag(predUpr,2),lag3=lag(predUpr,3),lag4=lag(predUpr,4),predUpr=(predUpr+lag1+lag2+lag3 +lag4)/5,
         lag1=lag(predLwr),lag2=lag(predLwr,2),lag3=lag(predLwr,3),lag4=lag(predLwr,4),predLwr=(predLwr+lag1+lag2+lag3 +lag4)/5)

continents <- continents %>%
  mutate(country = countrycode(iso_a3, origin = 'iso3c', destination = 'country.name'))


makeTSplot <- function(){
  lab1 <- expression(paste(NO[2], " (", mu, g, "/", m^3,")", sep=""))
  lab2 <- expression(paste(O[3], " (", mu, g, "/", m^3,")", sep=""))
  lab3 <- expression(paste("PM2.5", " (", mu, g, "/", m^3,")", sep=""))
  
  makeSubPlot <- function(par, lab, legend, xlab){
    #par <- 'no2'
    
    plot <- tsAirSmooth %>%
      left_join(countPop, by='country') %>%
      filter(dateRel > -30, param == par) %>%
      filter(continent %in% c('Asia', 'Europe', 'North America')) %>%
      #filter(continent %in% c('Europe')) %>%
      gather(key, val, predic, observed) %>%
      filter(predVar == "mean") %>%
      mutate(key = factor(key, levels=c('predic', 'observed'))) %>%
      group_by(param, dateRel, key) %>%
      summarise(val = weighted.mean(val, pop18, na.rm=TRUE), 
                predUpr =weighted.mean(predUpr, pop18, na.rm=TRUE), 
                predLwr=weighted.mean(predLwr, pop18, na.rm=TRUE)) %>%
      #filter(country == "China") %>%
      ggplot(aes(x=dateRel, y=val, color=key)) +
      geom_line(linejoin = 'round') +
      geom_ribbon(aes(ymin=predLwr, ymax=predUpr), alpha=0.2, color=NA) +
      geom_vline(xintercept = 0, linetype=2) +
      ylab(lab) +
      xlab('') +
      #facet_wrap(~continent, ncol=1, scales='free_y') +
      scale_color_manual(values=c('black', 'red'), labels=c('Benchmark','Observed'))+
      theme(legend.position = 'none')
    if (legend){
      plot <- plot +
        theme(legend.position = c(0.45, 0.9),
              legend.background = element_blank(),
              legend.title= element_blank())
    }
    if(xlab){
      plot <- plot + xlab('Days since lockdown')
    }
    return (plot)
  }
  p1 <- makeSubPlot('no2', lab1, F, F)
  p2 <- makeSubPlot('o3', lab2,T, T)
  p3 <- makeSubPlot('pm25', lab3,F, F)
  
  ggarrange(p1, p2,p3, nrow = 1, labels = c("A", "B", "C"))
  
}
makeTSplot()
# Export 1300 x 400
dev.off()

polvspol <- tsAirSmooth %>%
  filter(predVar == 'mean') %>%
  #filter(dateRel > 0) %>%
  mutate(check = ifelse(param == 'no2' & country %in% c('Canada', 'Mexico', 'United States', 'Thailand') & predic > 1, 1, 0),
         check2 = ifelse(param == 'o3' & country %in% c('Australia','Canada', 'Mexico', 'United States', 'Thailand') & predic > 1, 1, 0),
         check3 = ifelse(param == 'pm25' & country %in% c('Ireland','Finland', 'United Kingdom') & predic > 20, 1, 0)) %>%
  filter(check == 0, check2 == 0, check3 == 0) %>%
  mutate(diff = observed - predic) %>%
  dplyr::select(country, date, dateRel, param, diff) %>%
  drop_na(diff) %>%
  pivot_wider(names_from=param, values_from=diff)

polvspol %>%
  left_join(continents) %>%
  ggplot(aes(x=no2, y=o3)) +
  geom_point(size=0.2) +
  geom_smooth(method='lm', se=F) +
  facet_wrap(~country, scales='free') +
  theme(legend.position='none')

countAirDifTs <- tsAirSmooth %>% 
  filter(country %in% countSelect)  %>%
  drop_na(predic) %>%
  mutate(check = ifelse(param == 'no2' & country %in% c('Canada', 'Mexico', 'United States', 'Thailand') & predic > 1, 1, 0),
         check2 = ifelse(param == 'o3' & country %in% c('Australia','Canada', 'Mexico', 'United States', 'Thailand') & predic > 1, 1, 0),
         check3 = ifelse(param == 'pm25' & country %in% c('Ireland','Finland', 'United Kingdom') & predic > 20, 1, 0)) %>%
  filter(check == 0, check2 == 0, check3 == 0)  %>%
  left_join(countPop, by='country') %>%
  filter(dateRel > -14)  %>%
  mutate(diff = observed-predic, diffUpr=observed- predUpr, diffLwr=observed-predLwr,
         diffRel = (observed-predic)/observed*100,
         diffUprRel = (observed-predUpr)/observed*100,
         diffLwrRel = (observed-predLwr)/observed*100)



makeTSplotSI <- function(){
  countAirDifTs  %>%
    filter(predVar == 'meanRel') %>%
    mutate(lcDate = as.Date(lcDate)) %>%
    ggplot(aes(x=date, y=diff, color=param)) +
    geom_smooth(se=F) +
    # geom_point() +
    #geom_line() +
    #facet_grid(economy~param, scales='free') +
    facet_wrap(~country, scales='free') + 
    ylab(expression(paste("Absolute change [obs. - benchmark]"," (", mu, g, "/", m^3,")", sep=""))) +
    xlab('') +
    theme(
      strip.background =  element_rect(
        fill=NA, linetype="solid"
      ),
      legend.position="top",
      legend.title = element_blank(),
      strip.text = element_text(
        size = 10, color = "black",
        margin = grid::unit(c(1,0,1,0), "mm")
      )
    ) +
    geom_vline(aes(xintercept = lcDate), linetype=2)+
    scale_x_date(labels = date_format("%b"))
}

makeTSplotSI()

dev.off()
# Export 1000 x 1000


lab1 <- expression(paste(NO[2], " (", mu, g, "/", m^3,")", sep=""))
lab2 <- expression(paste(O[3], " (", mu, g, "/", m^3,")", sep=""))
lab3 <- expression(paste("PM2.5", " (", mu, g, "/", m^3,")", sep=""))

pollutant <- 'no2'
makeTSplotSIseparate <- function(pollutant, lab){
  
  tsAirSmooth %>%
    mutate(lcDate = as.Date(lcDate)) %>%
    filter(predVar == 'mean')%>%
    filter(dateRel > -30) %>%
    group_by(country, param) %>%
    drop_na(predic) %>%
    mutate(check = ifelse(param == 'no2' & country %in% c('Canada', 'Mexico', 'United States', 'Thailand') & predic > 1, 1, 0),
           check2 = ifelse(param == 'o3' & country %in% c('Australia','Canada', 'Mexico', 'United States', 'Thailand') & predic > 1, 1, 0),
           check3 = ifelse(param == 'pm25' & country %in% c('Ireland','Finland', 'United Kingdom') & predic > 20, 1, 0)) %>%
    filter(check == 0, check2 == 0, check3 == 0) %>%
    filter(param == pollutant)  %>%
    gather(key, val, predic, observed) %>%
    mutate(key = factor(key, levels=c('predic', 'observed'))) %>%
    ggplot(aes(x=date, y=val, color=key)) +
    geom_vline(aes(xintercept = lcDate), linetype=2)+
    ylab(lab) +
    xlab('')+
    geom_ribbon(aes(ymin=predLwr, ymax=predUpr), alpha=0.2, color=NA)+
    geom_line() +
    scale_color_manual(values=c('black', 'red'), labels=c('Benchmark','Observed'))+
    #geom_smooth(span=0.2, se=F) +
    facet_wrap(~country, scales='free')+
    theme(
      strip.background =  element_rect(
        fill=NA, linetype="solid"
      ),
      legend.position="top",
      legend.title = element_blank(),
      strip.text = element_text(
        size = 10, color = "black",
        margin = grid::unit(c(1,0,1,0), "mm")
      )
    ) +
    geom_vline(aes(xintercept = lcDate), linetype=2)+
    scale_x_date(labels = date_format("%b"))
}
makeTSplotSIseparate('no2', lab1)
# Export 1000 x 1000
makeTSplotSIseparate('o3', lab2)
makeTSplotSIseparate('pm25', lab3)

### Make lockdown summary graph --------------------------------------------------------

makeLockdownGraph <- function(){
  
  toPlot1 <- lockdownAll %>%
    filter(country %in% countSelect) %>%
    mutate(date = as.Date(floor_date(date, 'week')))
  c1 <-  toPlot1%>% 
    group_by(type, date) %>%
    summarise(count = n())  %>%
    mutate(date = as.Date(date)) %>%
    ggplot(aes(x=date, y=count, fill=type)) + 
    geom_bar(stat='identity', position='stack', color='grey', size=0.2) +
    ylab('Number of countries') + 
    #scale_x_date(breaks = pretty_breaks(10)) +
    scale_fill_manual(name='Policy regulation',values=c('red','blue','#219c51'), 
                      labels=c('Stay-at-home restriction', 'Mobility restriction', 'Workplace closure')) +
    theme(legend.position = c(0.3, 0.6),
          axis.title.x = element_blank())
  c2 <- lockdown%>%
    filter(country %in% countSelect) %>%
    mutate(lcDate = as.Date(lcDate), days = round(as.numeric(ymd(20200515)-lcDate))) %>%
    ggplot(aes(y=reorder(country, days))) +
    geom_segment(aes(x=lcDate, xend=ymd(20200515), yend=country), alpha=0.4) +
    geom_point(aes(x=lcDate), size=2, alpha=0.5) +
    geom_point(aes(x=ymd(20200515)), size=2, alpha=0.5) +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlim(ymd(20200120), ymd(20200615))+
    geom_point(inherit.aes=FALSE, data=toPlot1, aes(y=country, x=date, color=type), shape=4, size=2, alpha=0.7) +
    geom_text(aes(x=ymd(20200525), label= country),hjust = 'left', nudge_x = -2, size=3)+
    geom_text(aes(x=ymd(20200515), label= days),hjust = 'left', nudge_x = 1, size=3)+
    scale_color_manual(name='Policy regulation',values=c('red','blue','#219c51'), 
                       labels=c('Stay-at-home restriction', 'Mobility restriction', 'Workplace closure'))+
    theme(legend.position = "none")
  cowplot::plot_grid(c1, c2, labels = "AUTO")
  
}
makeLockdownGraph()

dev.off()


