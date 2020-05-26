### Bring in and sort data -------------------------------------------
# World polygons from the maps package
world_shp <- ne_countries(scale = "medium", returnclass = "sf")
cnts <- unique(world_shp$name)
cntsNum <- seq(1,length(cnts),1)
cntsDF <- data.frame(name=cnts, code=cntsNum)

rgns <- unique(world_shp$continent)
rgnsNum <- seq(1,length(rgns),1)
rgnsDF <- data.frame(continent=rgns, code2=rgnsNum)

world_shp <-world_shp %>%
  left_join(cntsDF, by='name')%>%
  left_join(rgnsDF, by='continent')

names(world_shp)
worldLookup <- world_shp %>% as_tibble() %>% dplyr::select(name, continent, code,code2) 

# Bring in rasters
mask <- raster('./DATA/Satellite/inhabited_mask2.tif')
mask[mask == 0] <- NA
plot(mask)

popRast <- flip(raster('./DATA/Satellite/population.tif'), direction='y')
plot(popRast)
popRast <- crop(popRast, extent(mask))
extent(popRast) <- extent(mask)

no22020 <- raster('./DATA/Satellite/no2_img_2020_updated.tif')
no22019 <- raster('./DATA/Satellite/no2_img_2019_updated.tif')
no2Diff <- no22020-no22019
no2DiffRel <- (no22020-no22019)/no22020*100

o32020 <- raster('./DATA/Satellite/o3_img_2020_updated.tif')
o32019 <- raster('./DATA/Satellite/o3_img_2019_updated.tif')
o3Diff <- o32020-o32019
o3DiffRel <- (o32020-o32019)/o32020*100

aod2020 <- raster('./DATA/Satellite/aod_img_2020_updated.tif')/1000
aod2019 <- raster('./DATA/Satellite/aod_img_2019_updated.tif')/1000
aodDiff <- (aod2020-aod2019)
aodDiffRel <- (aod2020-aod2019)/aod2019*100

### Make boxplot graph --------------------------------------------------------
worldR <- rasterize(world_shp, mask, field='code')
plot(worldR)

rastDiff <- no2DiffRel
makeViolinPlot <- function(rastDiff, label, lims){
  stack <- stack(rastDiff,worldR)
  stack <- mask(stack, mask)
  names(stack) <- c('diff','code')
  
  toPlot <- as.data.frame(stack, xy=TRUE) %>% as_tibble() %>% drop_na(diff) %>%
    left_join(worldLookup) %>%
    drop_na(name) %>% 
    #filter(name %in% countSelect) %>%
    group_by(name) %>%
    mutate(median = median(diff, na.rm = TRUE), n=n()) %>%
    filter(n >200)
  
  toPlot %>%
    ggplot(aes(x=diff, y=reorder(name, median), fill=median)) +
    geom_boxplot(outlier.colour = NA,weight=0.2, size=0.2) +
    #coord_flip() +
    coord_cartesian(xlim = c(lims[1],lims[2])) +
    geom_vline(xintercept = 0, linetype=2, color='red') +
    scale_fill_gradientn(name="", 
                         colours=rev(RColorBrewer::brewer.pal(10, 'PuOr')),
                         limits = c(lims[1], lims[2]), 
                         oob = scales::squish)  +
    theme(legend.position='none') +
    ylab('')+
    xlab(label)+ theme(
      legend.position = c(.05, .95),
      legend.justification = c("left", "top"),
      legend.box.just = "left",
      #legend.margin = margin(6, 6, 6, 6), 
      legend.background = element_rect(fill=alpha('white', 0.2))
    ) 
}
v1 <- makeViolinPlot(no2DiffRel,  expression(paste(Delta, NO[2]," (%)", sep="")), c(-100,100))
v2 <- makeViolinPlot(o3DiffRel,  expression(paste(Delta, O[3]," (%)", sep="")), c(-30,30))
v3 <- makeViolinPlot(aodDiffRel,  expression(paste(Delta, " AOD (%)", sep="")), c(-100,100))
ggarrange(v1, v2, v3, ncol = 3, labels = c("A", "B", "C"))

# Export 900 x 700

### Make raster maps of change --------------------------------------------------------
## Make hist plots
limitsNo2 <- c(0, 0.00004)
limitsO3 <- c(0.1, 0.25)
limitsAOD <- c(0, 0.8)

labelNo2H <- expression(paste(NO[2]," (", mol, "/", m^2,")", sep=""))
labelO3H <- expression(paste(O[3]," (", mol, "/", m^2,")", sep=""))
labelAodH <- expression(paste(" AOD", sep=""))

makeHistPlot <- function(rast1, rast2, limits, label){
  
  stacked <- stack(rast1,rast2)
  stacked <- mask(stacked, mask)
  sDF <- as.data.frame(stacked, xy=TRUE) %>% na.omit()
  names(sDF) <- c('x','y','2020','2019')
  sDF %>%
    gather(key, val,"2020", "2019") %>%
    filter(val < limits[2], val>limits[1]) %>%
    group_by(key) %>%
    mutate(med = median(val)) %>%
    ggplot(aes(x=val, fill=key)) +
    geom_density(alpha=0.4, color=NA) +
    #geom_histogram(position='dodge') +
    scale_color_manual(values=c('#31cbcf', '#f8766d' ))+
    scale_fill_manual(values=c('#31cbcf', '#f8766d' )) +
    geom_vline(aes(xintercept = med, color=key), linetype=1, size=1) +
    ylab("Data density") +
    xlab(label) +
    theme(
      legend.title = element_blank(),
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      #legend.margin = margin(6, 6, 6, 6),
      panel.background = element_rect(fill = alpha('white', 0.2)),
      plot.background=element_rect(fill = alpha('white', 0.2)),
      legend.background = element_rect(fill=alpha('white', 0.2))
    ) 
}

h1 <- makeHistPlot(no22020, no22019, limitsNo2, labelNo2H)
h2 <- makeHistPlot(o32020, o32019, limitsO3, labelO3H)
h3 <- makeHistPlot(aod2020, aod2019, limitsAOD, labelAodH)


# make raster plots
labelNo2 <- expression(paste(Delta,NO[2]," (", mol, "/", m^2,")", sep=""))
labelO3 <- expression(paste(Delta,O[3]," (", mol, "/", m^2,")", sep=""))
labelAod <- expression(paste(Delta," AOD", sep=""))

makeRastMap <- function(rast, limits,label, inset,panelLab) {
  toPlotDF <- as.data.frame(rast, xy=TRUE) %>% na.omit()
  names(toPlotDF) <- c('Lon', 'Lat', 'value')
  plot <- toPlotDF %>%
    ggplot()  +
    geom_raster(aes(x = Lon, y = Lat, fill = value)) +
    geom_sf(data = world_shp, 
            fill = NA, 
            color = '#000000',
            size = 0.1) +
    coord_sf( expand = FALSE)+
    xlim(-140,160) +
    ylim(-60,75)+ theme(
      legend.position = c(.05, .05),
      legend.justification = c("left", "bottom"),
      legend.box.just = "left",
      plot.margin=grid::unit(c(0,0,0,0), "mm"), 
      legend.background = element_rect(fill=alpha('white', 0.2))
    )+
    xlab('') +ylab('') + 
    scale_fill_gradientn(
      name=label,
      na.value = NA,
      limits = limits,
      oob = scales::squish,
      colours = rev(RColorBrewer::brewer.pal(10, 'PuOr')))
  g2 <- ggplotGrob(inset)
  g3 <- grobTree(textGrob(panelLab, x=0.01,  y=0.95, hjust=0,
                          gp=gpar(fontsize=20, fontface="bold")))
  plot <- plot + annotation_custom(grob = g2, xmin=40, xmax=115, ymin=-58, ymax=5)+ 
    annotation_custom(g3)
  
  return (plot)
}

r1 <- makeRastMap(no2Diff, c(-0.00002,0.00002),labelNo2,h1, "A")
r2 <- makeRastMap(o3Diff, c(-0.03,0.03),labelO3,h2, "B")
r3 <- makeRastMap(aodDiff, c(-0.5,0.5),labelAod,h3, "C")

percNo2Diff <- (no22020-no22019)/no22019*100
plot(percNo2Diff)
makeRastMap(no2Diff, c(-100,100),labelNo2,h1)

grid.arrange(r1,r2,r3, ncol=1,nrow=3, heights=c(0.33,0.33,0.33), padding = unit(0, "line"), newpage = F)
dev.off()
#Export as 1200:1500


### Get regional statistics --------------------------------------------------

## Region statistics
worldRstat <- rasterize(world_shp, mask, field='code2')
stack <- stack(no2DiffRel, no2Diff, o3DiffRel, o3Diff,aodDiffRel, aodDiff, worldRstat, popRast)
stack <- mask(stack, mask)
names(stack) <- c('no2Rel','no2','o3Rel','o3','aodRel','aod','code2', 'population')

rastDF <- as.data.frame(stack, xy=TRUE) %>% as_tibble() %>%
  left_join(worldLookup, by='code2')

calcRastStats1 <- function(){
  View(rastDF %>% 
         mutate(aodRel = ifelse(aodRel == -Inf |aodRel == Inf , NA, aodRel)) %>%
         gather(key, val, no2Rel:aod) %>%
         drop_na(val, population) %>%
         group_by(key) %>%
         summarise(mean = weighted.mean(val, population, na.rm=TRUE), IQR = IQR(val, na.rm=TRUE)))
}
calcRastStats1()

calcRastStats2 <- function(){
  View(rastDF %>%
         gather(key, val, no2Rel:aod) %>%
         drop_na(val, population) %>%
         group_by(key, continent) %>%
         summarise(mean = weighted.mean(val, population, na.rm=TRUE), IQR = IQR(val, na.rm=TRUE)))
  
}
calcRastStats3 <- function(){
  View(rastDF %>%
         gather(key, val, no2Rel:aod) %>%
         drop_na(val, population) %>%
         group_by(key, continent, code2) %>%
         summarise(mean = weighted.mean(val, population, na.rm=TRUE), IQR = IQR(val, na.rm=TRUE)))
  
}
calcRastStats1()
calcRastStats2()
calcRastStats3()