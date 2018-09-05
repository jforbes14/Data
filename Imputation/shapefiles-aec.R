## This script reads in the shapefiles containing Election division boundaries, for the Election years in which a Census does not occur
## The shapefiles are simplified and are used in subsequent programs to impute Census profiles at election time.

## Shapefiles obtained from: http://www.aec.gov.au/Electorates/gis/gis_datadownload.htm

## This script requires: raw, downloaded shapefiles from AEC

## This script generates:
## sFsmall_aec10, sFsmall_aec13, sFsmall_aec16
## nat_data_aec10, nat_data_aec13, nat_data_aec16
## nat_map_aec10, nat_map_aec13, nat_map_aec16


library(knitr)
library(maptools)
library(rmapshaper)
library(dplyr)
library(rgdal)

# Define centroid function
centroid <- function(i, polys) {
  ctr <- Polygon(polys[i])@labpt
  data.frame(long_c=ctr[1], lat_c=ctr[2])
}

# Read in AEC 2016 shapefiles
sF <- readOGR(dsn="Raw/Shapefiles/national-midmif-09052016/COM_ELB.TAB", layer="COM_ELB")
sFsmall <- ms_simplify(sF, keep=0.05)
nat_data16 <- sF@data
nat_data16$id <- row.names(nat_data16)
nat_map16 <- ggplot2::fortify(sFsmall)
nat_map16$group <- paste("g",nat_map16$group,sep=".")
nat_map16$piece <- paste("p",nat_map16$piece,sep=".")
nms <- sFsmall@data %>% select(Elect_div, State)
nms$id <- as.character(1:150)
nat_map16 <- left_join(nat_map16, nms, by="id")
polys <- as(sF, "SpatialPolygons")
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
nat_data16 <- data.frame(nat_data16, centroids)

sF_aec16 <- sF
sFsmall_aec16 <- sFsmall
nat_data_aec16 <- nat_data16
nat_map_aec16 <- nat_map16



# Read in AEC 2013 shapefiles
sF <- readOGR(dsn="Raw/Shapefiles/national-midmif-16122011/COM20111216_ELB.MIF", layer="COM20111216_ELB")
sFsmall <- ms_simplify(sF, keep=0.05)
nat_data13 <- sF@data
nat_data13$id <- row.names(nat_data13)
nat_map13 <- ggplot2::fortify(sFsmall)
nat_map13$group <- paste("g",nat_map13$group,sep=".")
nat_map13$piece <- paste("p",nat_map13$piece,sep=".")
nms <- sFsmall@data %>% select(Elect_div, State)
nms$id <- as.character(1:150)
nat_map13 <- left_join(nat_map13, nms, by="id")
polys <- as(sF, "SpatialPolygons")
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
nat_data13 <- data.frame(nat_data13, centroids)

sF_aec13 <- sF
sFsmall_aec13 <- sFsmall
nat_data_aec13 <- nat_data13
nat_map_aec13 <- nat_map13

# Read in AEC 2010 shapefiles
shapeFile <- "Raw/Shapefiles/national-esri-2010/COM_ELB_2010_region.shp"
sF <- readShapeSpatial(shapeFile)
sFsmall <- ms_simplify(sF, keep=0.05)
nat_data10 <- sF@data
nat_data10$id <- row.names(nat_data10)
nat_map10 <- ggplot2::fortify(sFsmall)
nat_map10$group <- paste("g",nat_map10$group,sep=".")
nat_map10$piece <- paste("p",nat_map10$piece,sep=".")
nms <- sFsmall@data %>% select(Elect_div, State)
nms$id <- as.character(1:150)
nat_map10 <- left_join(nat_map10, nms, by="id")
polys <- as(sF, "SpatialPolygons")
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
nat_data10 <- data.frame(nat_data10, centroids)

sF_aec10 <- sF
sFsmall_aec10 <- sFsmall
nat_data_aec10 <- nat_data10
nat_map_aec10 <- nat_map10


# Remove duplicated objects
remove(sF)
remove(sF_aec10)
remove(sF_aec13)
remove(sF_aec16)
remove(sFsmall)
remove(nat_map10)
remove(nat_map13)
remove(nat_map16)
remove(nat_data10)
remove(nat_data13)
remove(nat_data16)
remove(centroids)
remove(polys)
remove(nms)


# Rename/remove columns from nat_map to be consistent
colnames(nat_data_aec10) <- colnames(nat_data_aec13)


# Save as .rda
save(sFsmall_aec10, file = "Clean/Maps/sFsmall_aec10.rda")
save(sFsmall_aec13, file = "Clean/Maps/sFsmall_aec13.rda")
save(sFsmall_aec16, file = "Clean/Maps/sFsmall_aec16.rda")

#save(nat_map_aec10, file = "Clean/Maps/nat_map_aec10.rda")
#save(nat_map_aec13, file = "Clean/Maps/nat_map_aec13.rda")
#save(nat_map_aec16, file = "Clean/Maps/nat_map_aec16.rda")

#save(nat_data_aec10, file = "Clean/Maps/nat_data_aec10.rda")
#save(nat_data_aec13, file = "Clean/Maps/nat_data_aec13.rda")
#save(nat_data_aec16, file = "Clean/Maps/nat_data_aec16.rda")

