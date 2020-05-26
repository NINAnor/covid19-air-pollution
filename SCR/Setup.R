### Import libraries and set plot themes ----------------------------------------
library(scales)
library(lubridate)
library(grid)
library(gridExtra)
library(tidyverse) # for general data wrangling and plotting
library(lubridate) # for working with dates
library(sf) # for vector data 
library(raster) # for working with rasters
library(maps) # additional helpful mapping packages
library(maptools)
library(rgeos)
library(ggpubr)
library(ggrepel)
library(purrr)
library(randomForest)

library("rnaturalearth")
library("rnaturalearthdata")
library(countrycode)
library(anytime)
library(stars)
library(nngeo)

theme_set(theme_bw()+
            theme(
              panel.grid = element_blank()
            ))

pal <- c('#000004', '#2C105C', '#711F81', '#B63679', '#EE605E', '#FDAE78', '#FCFDBF')
palCurl <- c('#151d44', '#156c72', '#7eb390', '#fdf5f4', '#db8d77', '#9c3060', '#340d35')
palOxy <- c('#400505', '#850a0b', '#6f6f6e', '#9b9a9a', '#cbcac9', '#ebf34b', '#ddaf19')
palOxy2 <- c('#400505', '#9b9a9a', '#ddaf19')
palHawai <- c('#8C0273','#922A59','#964742','#996330','#9D831E','#97A92A','#80C55F','#66D89C','#6CEBDB','#B3F2FD')
palTofinio <- c('#DED9FF','#93A4DE','#4A6BAC','#273C65','#121926','#122214','#244D28','#3F8144','#88B970','#DBE69B')
palNuuk <- c('#05598C','#296284','#4A7283','#6F878D','#929C96','#ABAD96','#BAB98D','#C7C684','#E0E08E','#FEFEB2')

RColorBrewer::brewer.pal(10, 'BrBG')
selectPal <- c("#7F3B08", "#B35806", "#E08214", "#FDB863", "#FEE0B6","#c4c2c2", "#D8DAEB", "#B2ABD2", "#8073AC", "#542788" ,"#2D004B")

scale2 <- function(x, na.rm = TRUE) {return (x - mean(x, na.rm = TRUE))}

