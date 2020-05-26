#### Prepare pediatric population data ------------------------------------------------------------------

# Pediatric pop data wrangle ** only need to do this once
pop19 <- raster('./DATA/Health/population2015_age19.nc')
plot(pop19)
pop15 <- raster('./DATA/Health/population2015_age15to19.nc')
pop15 <- pop15 - pop19
plot(pop15)
pop10 <- raster('./DATA/Health/population2015_age10to14.nc')
pop5 <- raster('./DATA/Health/population2015_age5to9.nc')
pop0 <- raster('./DATA/Health/population2015_ageUnder5.nc')

popYoung <- pop15 + pop10 + pop5 + pop0

plot(popYoung)
add <-  function(x, na.rm = TRUE) (sum(x, na.rm = TRUE))
add(c(1,2))

# Takes a while - only run once
#youngPopCount <-
#  world_shp %>% mutate(
#    pop18 = raster::extract(popYoung, world_shp, add, na.rm = TRUE)
#  )

names(youngPopCount)

youngPopCount %>% as_tibble() %>%
  group_by(adm0_a3) %>%
  summarise(pop18 = sum(pop18, na.rm=TRUE)) %>%
  write_csv('./DATA/Health/pop_count_18_country.csv')

#### Import and prepare input data ------------------------------------------------------------------
population <- read_csv('./DATA/Health/pop_dens_country.csv') %>%
  mutate(pop = mean*(area/1000000), 
         country =countrycode(brk_name, origin = 'country.name', destination = 'country.name') ) %>%
  group_by(country) %>%
  summarise(pop = sum(pop)) %>%
  mutate(pop = pop/100000) %>% # Correct to 1 per 100,000
  drop_na()

populationAsth <- read_csv('./DATA/Health/pop_count_18_country.csv') %>%
  mutate(pop =pop18, 
         country =countrycode(adm0_a3, origin = 'iso3c', destination = 'country.name') ) %>%
  group_by(country) %>%
  summarise(pop = sum(pop)) %>%
  mutate(pop = pop/100000) %>% # Correct to 1 per 100,000
  drop_na()

rateCorrection <- populationAsth %>% mutate(popyoung = pop) %>% dplyr::select(-pop) %>%
  left_join(population) %>%
  drop_na() %>%
  mutate(correc = pop/popyoung)%>% dplyr::select( country, correc)

asthmaFracs <- read_csv('./DATA/Health/Anenberg_asthma_fractions.csv') %>%
  mutate(country = countrycode(country, origin = 'country.name', destination = 'country.name'))

asthmaFracMean <- mean(asthmaFracs$fraction)

asthmaRates <- read_csv('./DATA/Health/IHME-GBD_2017_asthma_incidence_under18.csv') %>%
  filter(metric_name == "Rate", age_name =="<20 years")%>%
  mutate(country = countrycode(location_name, origin = 'country.name', destination = 'country.name'), asthRate = val)%>%
  dplyr::select(country, asthRate) %>%
  left_join(rateCorrection) %>%
  drop_na() %>%
  mutate(asthRate = asthRate*correc)

asthmaRates <- read_csv('./DATA/Health/IHME-GBD_2017_asthma_prevalence_under18.csv') %>%
  filter(metric_name == "Rate", age_name =="<20 years")%>%
  mutate(country = countrycode(location_name, origin = 'country.name', destination = 'country.name'), asthRate = val)%>%
  dplyr::select(country, asthRate) %>%
  left_join(asthmaFracs, by='country') %>%
  mutate(fraction = ifelse(is.na(fraction), asthmaFracMean, fraction))%>%
  mutate(asthRate = asthRate*fraction)%>%
  left_join(rateCorrection)  %>%
  mutate(asthRate = asthRate*correc)%>%
  drop_na()

mortRates <- read_csv('./DATA/Health/IHME-GBD_2017_mortality.csv')%>%
  filter(metric_name == "Rate", measure_name == "Deaths")%>%
  mutate(country = countrycode(location_name, origin = 'country.name', destination = 'country.name'), mortRate = val) %>%
  dplyr::select(country, mortRate)

vicedo <- read_csv('./DATA/health/vicedo_response_rates_RR.csv')%>%
  mutate(country = countrycode(country, origin = 'country.name', destination = 'country.name'))
vicedo

liu <- read_csv('./DATA/health/liu_response_rates_percChng.csv')%>%
  mutate(country = countrycode(country, origin = 'country.name', destination = 'country.name'))
liu

lockdown <- read_csv('./DATA/COVID/lockdown_dates.csv') %>%
  mutate(lcDate = as.Date(date))


polChng <- lmResults %>%
  left_join(lockdown  %>% dplyr::select(country, lcDate), 
            by="country") %>%
  mutate(dateRel = yday(date) - yday(lcDate))  %>% ungroup() %>%
  filter(dateRel > 0) %>%
  filter(predVar == 'mean') %>%
  group_by(country, param) %>%
  mutate(nDays = n()) %>%
  drop_na() %>%
  summarise_at(vars(predic, observed, nDays), mean, na.rm=TRUE) %>%
  mutate(base = predic, exp=observed, delta = observed-predic, parameter = param) %>%
  mutate(countryCode = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  dplyr::select(country, base, exp, parameter, countryCode, nDays) %>%
  ungroup()

#### O3 health burden ----------------------------------------------------------------------------------
# Using data from Vicedo-Cabrera et al. 2020
#RR = 
LCCa = 0.00376
LCC = 70 # lowest concentration below which no mortality
e = 2.71828
mB = 20 # baseline mortality per year per 100,000
POP = 900 # in 100,000

RR = 1.12 

# Get beta coeff
getB <- function(rr, del){
  return (log(rr,e)/del)
}
getB(RR, 10)

# Get corrected relative risk
getNewRR <- function(b, dc,lcc){
  return (exp(b*(dc-lcc)))
}
getNewRR(getB(1.12, 10), 50,LCC)

# Get deaths per day
getDeathsDay <- function(new_rr,mb, pop){
  c1 = (new_rr-1)/new_rr
  c2 = (mb/365)*pop 
  return (c1*c2)
}
getDeathsDay(getNewRR(getB(RR, 10), 80,LCC),mB,POP)


meanRR <- mean(vicedo$RR)
meanRR_upr <- mean(vicedo$RR_upr)
meanRR_lwr <- mean(vicedo$RR_lwr)

o3mort <- polChng %>%
  left_join(vicedo)%>%
  left_join(population)%>%
  left_join(mortRates) %>%
  filter(parameter == "o3") %>%
  mutate(RR = ifelse(is.na(RR), meanRR, RR),
         RR_lwr = ifelse(is.na(RR_lwr), meanRR_lwr, RR_lwr),
         RR_upr = ifelse(is.na(RR_upr), meanRR_upr, RR_upr)) %>%
  drop_na()  %>%
  mutate(baseMort = getDeathsDay(getNewRR(getB(RR, 10), base,LCC),mortRate,pop)*nDays,
         expMort = getDeathsDay(getNewRR(getB(RR, 10), exp,LCC),mortRate,pop)*nDays,
         baseMortL = getDeathsDay(getNewRR(getB(RR_lwr, 10), base,LCC),mortRate,pop)*nDays,
         expMortL = getDeathsDay(getNewRR(getB(RR_lwr, 10), exp,LCC),mortRate,pop)*nDays,
         baseMortU = getDeathsDay(getNewRR(getB(RR_upr, 10), base,LCC),mortRate,pop)*nDays,
         expMortU = getDeathsDay(getNewRR(getB(RR_upr, 10), exp,LCC),mortRate,pop)*nDays) %>%
  mutate(mortChng = expMort - baseMort,
         mortChngU = expMortU-baseMortU,
         mortChngL = expMortL-baseMortL)

o3mort %>%
  ggplot(aes(x=country, y=mortChng)) +
  geom_point() +
  geom_errorbar(aes(ymin=mortChngL, ymax=mortChngU)) 


RR_a = 1.02 # convert to ug/m3 from 1.06 (1.04–1.07) ppb from https://ehp.niehs.nih.gov/doi/full/10.1289/EHP3766
upCI <- (1.07-1.06) /1.06
lowCI <- (1.06-1.04) /1.06

RR_upr <- 1.03
RR_lwr <- 1.01

exp(0.87/1000)


o3asthma <- polChng %>%
  left_join(populationAsth) %>%
  left_join(asthmaRates) %>%
  filter(parameter == "o3") %>%
  drop_na()  %>%
  mutate(baseI = getDeathsDay(getNewRR(getB(RR_a, 5), base,LCCa),asthRate,pop)*nDays,
         expI = getDeathsDay(getNewRR(getB(RR_a, 5), exp,LCCa),asthRate,pop)*nDays,
         baseupr = getDeathsDay(getNewRR(getB(RR_upr, 5), base,LCCa),asthRate,pop)*nDays,
         expUpr = getDeathsDay(getNewRR(getB(RR_upr, 5), exp,LCCa),asthRate,pop)*nDays,
         baseLwr = getDeathsDay(getNewRR(getB(RR_lwr, 5), base,LCCa),asthRate,pop)*nDays,
         expLwr = getDeathsDay(getNewRR(getB(RR_lwr, 5), exp,LCCa),asthRate,pop)*nDays) %>%
  mutate(asthChng = expI - baseI,
         asthChngL = expLwr - baseLwr,
         asthChngU = expUpr -baseupr)
o3asthma %>%
  ggplot(aes(x=country, y=asthChng)) +
  geom_point() +
  geom_errorbar(aes(ymin=asthChngL, ymax=asthChngU)) 




#### PM2.5 health burden ----------------------------------------------------------------------------------
# In Liu
deltaP = -100
mB = 200
POP = 900


PM25getDeathsAvoided = function(deltaP,rate, mb,pop, duration){
  mbDay = mb/365
  mAdj = (rate/100*mbDay)*(deltaP/10)
  out = mAdj*pop*duration
  return (out)
}
PM25getDeathsAvoided(deltaP,1.32, mB,POP, 14)


perc_changeM <- mean(liu$perc_change, na.rm=TRUE)
perc_change_uprM <- mean(liu$perc_change_upr, na.rm=TRUE)
perc_change_lwrM <- mean(liu$perc_change_lwr, na.rm=TRUE)

pm25Mort <- polChng %>%
  left_join(liu)%>%
  left_join(population)%>%
  left_join(mortRates) %>%
  filter(parameter == "pm25") %>%
  mutate(perc_change = ifelse(is.na(perc_change), perc_changeM, perc_change),
         perc_change_lwr = ifelse(is.na(perc_change_lwr), perc_change_lwrM, perc_change_lwr),
         perc_change_upr = ifelse(is.na(perc_change_upr), perc_change_uprM, perc_change_upr)) %>%
  mutate(polChng = (exp-base)) %>%
  mutate(mortChngPM = PM25getDeathsAvoided(polChng,perc_change, mortRate,pop, nDays),
         mortChngPMupr = PM25getDeathsAvoided(polChng,perc_change_upr,mortRate,pop, nDays),
         mortChngPMlwr = PM25getDeathsAvoided(polChng,perc_change_lwr,mortRate,pop, nDays))
pm25Mort %>%
  ggplot(aes(x=country, y=mortChngPM)) +
  geom_point() +
  geom_errorbar(aes(ymin=mortChngPMlwr, ymax=mortChngPMupr)) 


RR_a <- 1.03
RR_a_lwr <- 1.01
RR_a_upr <- 1.04
upCI <- (1.04-1.03) /1.01
lowCI <- (1.03-1.01) /1.01
LCCa = 0.00376

pm25asthma <- polChng %>%
  left_join(populationAsth)%>%
  left_join(asthmaRates) %>%
  filter(parameter == "pm25") %>%
  mutate(RR = RR_a,
         RR_lwr = RR_a_lwr,
         RR_upr = RR_a_upr) %>%
  drop_na()  %>%
  mutate(baseI = getDeathsDay(getNewRR(getB(RR_a, 10), base,LCCa),asthRate,pop)*nDays,
         expI = getDeathsDay(getNewRR(getB(RR_a, 10), exp,LCCa),asthRate,pop)*nDays,
         expUpr = getDeathsDay(getNewRR(getB(RR_a_upr, 10), exp,LCCa),asthRate,pop)*nDays,
         expLwr = getDeathsDay(getNewRR(getB(RR_a_lwr, 10), exp,LCCa),asthRate,pop)*nDays,
         baseUpr = getDeathsDay(getNewRR(getB(RR_a_upr, 10), base,LCCa),asthRate,pop)*nDays,
         baseLwr = getDeathsDay(getNewRR(getB(RR_a_lwr, 10), base,LCCa),asthRate,pop)*nDays) %>%
  mutate(asthChng = expI - baseI,
         asthChngL = asthChng - (asthChng*lowCI),
         asthChngU = asthChng + (asthChng*upCI))%>%
  mutate(asthChng = expI - baseI,
         asthChngU = expUpr - baseUpr,
         asthChngL = expLwr - baseLwr)
unique(pm25asthma$country)
pm25asthma %>%
  ggplot(aes(x=country, y=asthChng)) +
  geom_point() +
  geom_errorbar(aes(ymin=asthChngL, ymax=asthChngU)) 


#### NO2 health burden ----------------------------------------------------------------------------------

LCCa = 0.00376 # lowest concentration below which no incidence
# For No2 mortality we use from https://www.ncbi.nlm.nih.gov/pubmed/27443553
# 0.60% (0.33% to 0.87%) 
# 1.0006 (1.00033 to 1.00087)
#1000*ln(RR)=%increase 
log(100, e)
exp(4.6)
exp(0.87/1000)

RR <- 1.0006
RR_u <- 1.00087
RR_l <- 1.00033
upCI <- (1.00087-1.0006) /1.0006
lowCI <- (1.0006-1.00033) /1.0006

# Use percentage change estimates rather
perc_change<- 0.6
perc_change_upr <- 0.87
perc_change_lwr <- 0.33

no2Mort <- polChng %>%
  left_join(population)%>%
  left_join(mortRates) %>%
  filter(parameter == "no2") %>%
  drop_na()   %>%
  mutate(baseI = getDeathsDay(getNewRR(getB(RR, 10), base,LCCa),mortRate,pop)*nDays,
         expI = getDeathsDay(getNewRR(getB(RR, 10), exp,LCCa),mortRate,pop)*nDays,
         expUpr = getDeathsDay(getNewRR(getB(RR_u, 10), exp,LCCa),mortRate,pop)*nDays,
         expLwr = getDeathsDay(getNewRR(getB(RR_l, 10), exp,LCCa),mortRate,pop)*nDays,
         baseUpr = getDeathsDay(getNewRR(getB(RR_u, 10), base,LCCa),mortRate,pop)*nDays,
         baseLwr = getDeathsDay(getNewRR(getB(RR_l, 10), base,LCCa),mortRate,pop)*nDays) %>%
  mutate(mortChng = expI - baseI,
         mortChngL =expLwr - baseLwr,
         mortChngU = expUpr - baseUpr) 

no2Mort %>%
  ggplot(aes(x=country, y=mortChng)) +
  geom_point() +
  geom_errorbar(aes(ymin=mortChngL, ymax=mortChngU)) 



#In Achakulwist they argue for lower limit of 2ppb = 0.002ppm = 0.00376 ug m-3
RR = 1.03 # 1.03 (1.03–1.04)

no2Asth <- polChng %>%
  left_join(population)%>%
  left_join(asthmaRates)%>%
  filter(parameter == "no2") %>%
  drop_na()  %>%
  mutate(baseI = getDeathsDay(getNewRR(getB(1.018, 5), base,LCCa),asthRate,pop)*nDays,
         expI = getDeathsDay(getNewRR(getB(1.018, 5), exp,LCCa),asthRate,pop)*nDays,
         baseIup = getDeathsDay(getNewRR(getB(1.022, 5), base,LCCa),asthRate,pop)*nDays,
         expIup = getDeathsDay(getNewRR(getB(1.022, 5), exp,LCCa),asthRate,pop)*nDays,
         baseIlw = getDeathsDay(getNewRR(getB(1.014, 5), base,LCCa),asthRate,pop)*nDays,
         expIlw = getDeathsDay(getNewRR(getB(1.014, 5), exp,LCCa),asthRate,pop)*nDays) %>%
  mutate(asthChng = expI - baseI,
         asthChngL = expIlw - baseIlw,
         asthChngU = expIup - baseIup)
hist(no2Asth$asthChng)
no2Asth %>%
  ggplot(aes(x=country, y=asthChng)) +
  geom_point() +
  geom_errorbar(aes(ymin=asthChngL, ymax=asthChngU)) 


### Join and plotting ----------------------------------------------------------------------------
allDeaths <- o3mort %>%
  mutate(val = mortChng, valU = mortChngU, valL = mortChngL, parameter = "o3", health='death') %>%
  dplyr::select(country, parameter,  val, valU, valL, health) %>%
  bind_rows(no2Mort %>%
              mutate(val = mortChng, valU = mortChngU, valL = mortChngL, parameter = "no2", health='death') %>%
              dplyr::select(country, parameter, val, valU, valL, health)) %>%
  bind_rows(pm25Mort%>%
              mutate(val = mortChngPM, valU = mortChngPMupr, valL = mortChngPMlwr, parameter = "pm25", health='death') %>%
              dplyr::select(country, parameter,  val, valU, valL, health)) %>%
  
  bind_rows(o3asthma%>%
              mutate(val = asthChng, valU = asthChngU, valL = asthChngL, parameter = "o3", health='asthma') %>%
              dplyr::select(country, parameter,  val, valU, valL, health)) %>%
  bind_rows(pm25asthma%>%
              mutate(val = asthChng, valU = asthChngU, valL = asthChngL, parameter = "pm25", health='asthma') %>%
              dplyr::select(country, parameter,  val, valU, valL, health))%>%
  bind_rows(no2Asth%>%
              mutate(val = asthChng, valU = asthChngU, valL = asthChngL, parameter = "no2", health='asthma') %>%
              dplyr::select(country, parameter,  val, valU, valL, health))

View(
  allDeaths %>% 
    group_by(health, parameter) %>%
    summarise_at(vars(val:valL), funs(sum), na.rm=TRUE) )


View(allDeaths %>%
       group_by(health, parameter) %>%
       summarise_at(vars(val:valL), funs(sum), na.rm=TRUE))

weird <- scales::trans_new("signed_log",
                           transform=function(x) sign(x)*log(abs(x)),
                           inverse=function(x) sign(x)*exp(abs(x)))
cl <- function(val){
  return (round(val)+(1*sign(val)))
}

sigNo2 <-tsAirDiff %>% filter(param == "no2", (diffUpr < 0 & diffLwr < 0) |  (diffUpr > 0 & diffLwr > 0))
no2Sig <- sigNo2$country
sigO3 <- tsAirDiff %>% filter(param == "o3", (diffUpr < 0 & diffLwr < 0) |  (diffUpr > 0 & diffLwr > 0))
o3Sig <- sigO3$country
sigPm25 <- tsAirDiff %>% filter(param == "pm25", (diffUpr < 0 & diffLwr < 0) |  (diffUpr > 0 & diffLwr > 0))
pmSig <- sigPm25$country


allDeaths %>% 
  group_by(health, parameter, country) %>%
  summarise_at(vars(val:valL), funs(sum), na.rm=TRUE) %>%
  ungroup() %>%
  complete(country, nesting(parameter, health), fill = list(val = NA,valU = NA,valL  = NA)) %>%
  mutate(sig = ifelse(parameter == 'no2' & country %in% no2Sig, 'sig',
                      ifelse(parameter == 'o3' & country %in% o3Sig, 'sig',
                             ifelse(parameter == 'pm25' & 
                                      country %in% pmSig, 'sig', 'nonSig'))))%>%
  arrange(health,parameter)  %>%
  write_csv('./DATA/Output/all_deaths_stats.csv')

parSel <- 'o3'

cyl_names <- c(
  'no2' = 'NO[2]',
  'o3' = 'O[3]',
  'pm25' = 'PM2.5'
)

allDeathsPlot <- allDeaths %>%
  complete(country, nesting(parameter, health), fill = list(val = 0,valU = 0,valL  = 0))

makeDeathPlotSub <- function(perSel, sig, xLab, yText, legend){
  toPlot <- allDeathsPlot %>% 
    mutate(val = cl(val)) %>%
    mutate(Avoided = ifelse(val<0, val, NA), Incurred = ifelse(val>=0, val, NA)) %>% 
    mutate(error = cl((abs(valU)-abs(valL))/2)) %>%
    
    mutate(errLoc = ifelse(val<0, val-error, val+error)) %>% 
    
    #filter(country != "Peru") %>%
    filter(parameter == perSel) 
  
  plot <- toPlot%>%
    ggplot(aes(x = country)) +
    geom_col(data = toPlot%>% filter(health == 'death') %>% filter(country %in% sig), aes(y = Avoided,fill = "#f59142"), width=0.8) +
    geom_col(data = toPlot%>% filter(health == 'death')%>% filter(!country %in% sig), aes(y = Avoided ),color = "#f59142",fill=NA, width=0.8) +
    geom_col(data = toPlot%>% filter(health == 'death')%>% filter(country %in% sig), aes(y = Incurred ,fill = "#f59142"), width=0.8) +
    geom_col(data = toPlot%>% filter(health == 'death')%>% filter(!country %in% sig), aes(y = Incurred ),color = "#f59142",fill=NA, width=0.8)+
    
    geom_col(data = toPlot%>% filter(health == 'asthma')%>% filter(country %in% sig), aes(y = Avoided,fill="#000000" ),color = "#000000", width=0.15)+
    geom_col(data = toPlot%>% filter(health == 'asthma')%>% filter(!country %in% sig), aes(y = Avoided,fill="#000000" ),color = "#000000", alpha=0.25,width=0.15)+
    geom_col(data = toPlot%>% filter(health == 'asthma')%>% filter(country %in% sig), aes(y = Incurred ,fill="#000000"),color = "#000000", width=0.15)+
    geom_col(data = toPlot%>% filter(health == 'asthma')%>% filter(!country %in% sig), aes(y = Incurred,fill="#000000" ),color = "#000000", alpha=0.25,width=0.15)+
    
    geom_linerange(data = toPlot %>% filter(health == 'death'), aes(ymin=val, ymax=errLoc), color='#f59142') +
    geom_linerange(data = toPlot %>% filter(health == 'asthma'), aes(ymin=val, ymax=errLoc), color='black') +
    coord_flip() +
    scale_y_continuous(trans=weird, breaks = c(-10000, -1000, -100,-10,0,10, 100, 1000, 10000)) +
    geom_hline(yintercept = 0, linetype=2) +
    scale_fill_identity(name = "Model fit",
                        breaks = c( "#f59142", "#000000"),
                        labels = c("Mortality burden", "Asthma burden"),
                        guide = "legend")+
    theme(
      legend.title = element_blank(),
      legend.position = 'none', #c(.05, .95),
      legend.justification = c("left", "top")
    ) +
    # geom_text(aes(label=country, y=Avoided), color="black", size=3.5)+
    ylab("") +xlab('') +
    geom_hline(yintercept = 1, linetype=2, size=0.4)+ 
    facet_grid(~parameter, scales='free',
               labeller = labeller(parameter  = as_labeller(cyl_names,  label_parsed)))+ 
    theme(
      strip.background =  element_rect(
        fill=NA, linetype="solid"
      ),
      strip.text = element_text(
        size = 10, color = "black", face = "bold.italic"
      ),
      axis.text.x = element_text(
        size = 7
      )
    )
  if(xLab){
    plot <- plot +
      ylab("Deaths / Asthma ERVs")
  }
  if(!yText){
    plot <- plot +
      theme(axis.text.y=element_blank())
  }
  if (legend){
    plot <- plot +
      theme(legend.position = c(0.02, 0.95),
            legend.text = element_text(size=8),
            legend.key = element_rect(colour = NA),
            legend.background = element_rect(fill=alpha('white', 0)))+ 
      guides(color = guide_legend(override.aes = list(color='white', size=0)),
             fill = guide_legend(keyheight = 0.75, keywidth=0.75),
             color = guide_legend(override.aes = list(color='white', size=0)),
             label.theme = element_text(size=1))
  }
  return (plot)
}

makeDeathPlotSub('no2', no2Sig, F, T, T)


makeDeathPlot <- function(){
  d1 <- makeDeathPlotSub('no2', no2Sig, F, T, F)
  d2 <- makeDeathPlotSub('o3', o3Sig, T, F, F)
  d3 <- makeDeathPlotSub('pm25', pmSig, F, F, T)
  
  #ggarrange(d1, d2, d3, nrow = 1)
  grid.arrange(d1,d2,d3, ncol=3, nrow=1,
               widths=c(0.42,0.29,0.29), 
               padding = unit(0, "line"), newpage = F)
  
}
makeDeathPlot()
# Export 900 x 400
dev.off()
