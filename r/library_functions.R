########################
## Library and functions
########################

library(confintr)
library(cowplot)
library(colorspace)
library(FactoMineR)
library(raster)
library(rcompanion)
library(rnaturalearth)
library(RNetCDF)
library(sf)
library(tidyverse)

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

# Map
mapBase <- ne_countries(scale = 'large', returnclass = "sf")
grid_projection <- "+proj=longlat +datum=WGS84"
