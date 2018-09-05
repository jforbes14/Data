## This script reads in the shapefiles containing Census division boundaries, for the Census years in which an election does not occur
## The shapefiles are simplified and are used in subsequent programs to impute Census profiles at election time.


## Download ABS 2001 shapefiles from: https://data.gov.au/dataset/0b939a62-e53e-4616-add5-77f909b58ddd
## Download ABS 2006 ERSI shapefiles from: http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/2923.0.30.0012006?OpenDocument
## Download ABS 2011 and 2016 ERSI shapefiles from: https://datapacks.censusdata.abs.gov.au/datapacks/

## This script generates:
## sFsmall_abs01, sFsmall_abs06e04, sFsmall_abs06e07, sFsmall_abs11, sFsmall_abs16
## nat_map_abs01, nat_map_abs06e04, nat_map_abs06e07 nat_map_abs11, nat_map_abs16
## nat_data_abs01, nat_data_abs06e04, nat_data_abs06e07, nat_data_abs11, nat_data_abs16

#Read in ABS Census shapefiles
library(knitr)
library(maptools)
library(sf)
library(rmapshaper)
library(dplyr)
library(rgdal)

# Define centroid function
centroid <- function(i, polys) {
  ctr <- Polygon(polys[i])@labpt
  data.frame(long_c=ctr[1], lat_c=ctr[2])
}

# Census 2016
shapeFile <- "Raw/Shapefiles/2016_CED_shape/CED_2016_AUST.shp"
sF <- readShapeSpatial(shapeFile, delete_null_obj = TRUE)
sFsmall <- ms_simplify(sF, keep=0.05)
nat_data16 <- sF@data
nat_data16$id <- row.names(nat_data16)
nat_map16 <- ggplot2::fortify(sFsmall)
nat_map16$group <- paste("g",nat_map16$group,sep=".")
nat_map16$piece <- paste("p",nat_map16$piece,sep=".")
nms <- sFsmall@data %>% select(CED_NAME)
nms$id <- as.character(1:150)
nat_map16 <- left_join(nat_map16, nms, by="id")
polys <- as(sF, "SpatialPolygons")
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys) # ERROR: some polygons do not have 4+ joins
nat_data16 <- data.frame(nat_data16, centroids)

sF_abs16 <- sF
sFsmall_abs16 <- sFsmall
nat_data_abs16 <- nat_data16
nat_map_abs16 <- nat_map16



# Census 2011
shapeFile <- "Raw/Shapefiles/2011_CED_shape/CED_2011_AUST.shp"
sF <- readShapeSpatial(shapeFile, delete_null_obj = TRUE)
sFsmall <- ms_simplify(sF, keep=0.05)
nat_data11 <- sF@data
nat_data11$id <- row.names(nat_data11)
nat_map11 <- ggplot2::fortify(sFsmall)
nat_map11$group <- paste("g",nat_map11$group,sep=".")
nat_map11$piece <- paste("p",nat_map11$piece,sep=".")
nms <- sFsmall@data %>% select(CED_NAME)
nms$id <- as.character(1:150)
nat_map11 <- left_join(nat_map11, nms, by="id")
polys <- as(sF, "SpatialPolygons")
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
nat_data11 <- data.frame(nat_data11, centroids)

sF_abs11 <- sF
sFsmall_abs11 <- sFsmall
nat_data_abs11 <- nat_data11
nat_map_abs11 <- nat_map11



# Census 2006 - with CED from 2007
shapeFile <- "Raw/Shapefiles/2923030001ced07aaust/CED07aAUST_region.shp"
sF <- readShapeSpatial(shapeFile, delete_null_obj = TRUE)
sFsmall <- ms_simplify(sF, keep=0.05)
nat_data06e07 <- sF@data
nat_data06e07$id <- row.names(nat_data06e07)
nat_map06e07 <- ggplot2::fortify(sFsmall)
nat_map06e07$group <- paste("g",nat_map06e07$group,sep=".")
nat_map06e07$piece <- paste("p",nat_map06e07$piece,sep=".")
nms <- sFsmall@data %>% select(CED_2007, NAME_2007, STATE_2006)
nms$id <- as.character(1:150)
nat_map06e07 <- left_join(nat_map06e07, nms, by="id")
polys <- as(sF, "SpatialPolygons")
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
nat_data06e07 <- data.frame(nat_data06e07, centroids)

sF_abs06e07 <- sF
sFsmall_abs06e07 <- sFsmall
nat_data_abs06e07 <- nat_data06e07
nat_map_abs06e07 <- nat_map06e07


# Census 2006 - with CED from 2004
shapeFile <- "Raw/Shapefiles/2923030001ced04aaust/CED04aAUST_region.shp"
sF <- readShapeSpatial(shapeFile, delete_null_obj = TRUE)
sFsmall <- ms_simplify(sF, keep=0.05)
nat_data06e04 <- sF@data
nat_data06e04$id <- row.names(nat_data06e04)
nat_map06e04 <- ggplot2::fortify(sFsmall)
nat_map06e04$group <- paste("g",nat_map06e04$group,sep=".")
nat_map06e04$piece <- paste("p",nat_map06e04$piece,sep=".")
nms <- sFsmall@data %>% select(CED_2004, NAME_2004, STATE_2006)
nms$id <- as.character(1:150)
nat_map06e04 <- left_join(nat_map06e04, nms, by="id")
polys <- as(sF, "SpatialPolygons")
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
nat_data06e04 <- data.frame(nat_data06e04, centroids)

sF_abs06e04 <- sF
sFsmall_abs06e04 <- sF
nat_data_abs06e04 <- nat_data06e04
nat_map_abs06e04 <- nat_map06e04



# Census 2001
shapeFile <- "Raw/Shapefiles/asgc2001.gpkg"
layers <- ogrListLayers(shapeFile)
sF <- readOGR(shapeFile, layers[3])
sFsmall <- ms_simplify(sF, keep=0.05)
nat_data01 <- sF@data
nat_data01$id <- row.names(nat_data01)
nat_map01 <- ggplot2::fortify(sFsmall)
nat_map01$group <- paste("g",nat_map01$group,sep=".")
nat_map01$piece <- paste("p",nat_map01$piece,sep=".")
nms <- sFsmall@data %>% select(CED_CODE_2001, CED_NAME_2001)
nms$id <- as.character(1:150)
nat_map01 <- left_join(nat_map01, nms, by="id")
polys <- as(sF, "SpatialPolygons")
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
nat_data01 <- data.frame(nat_data01, centroids)

sF_abs01 <- sF
sFsmall_abs01 <- sFsmall
nat_data_abs01 <- nat_data01
nat_map_abs01 <- nat_map01




# Remove duplicated objects
remove(sF)
remove(sFsmall)
remove(nat_map01)
remove(nat_map06e04)
remove(nat_map06e07)
remove(nat_map11)
remove(nat_map16)
remove(nat_data01)
remove(nat_data06e04)
remove(nat_data06e07)
remove(nat_data11)
remove(nat_data16)
remove(centroids)
remove(polys)
remove(nms)

remove(sF_abs01)
remove(sF_abs06e04)
remove(sF_abs06e07)
remove(sF_abs11)
remove(sF_abs16)



# Rename/remove columns from nat_data to be consistent
colnames(nat_data_abs01)[1:2] <- c("CED_CODE", "CED_NAME")
nat_data_abs01 <- nat_data_abs01[,c(1:2,4:7)]

colnames(nat_data_abs06e04)[2:3] <- c("CED_CODE", "CED_NAME")

colnames(nat_data_abs06e07)[2:3] <- c("CED_CODE", "CED_NAME")
nat_data_abs06e07 <- nat_data_abs06e07[,1:6]

nat_data_abs16 <- nat_data_abs16[,c(1,3:7)]


# Rename/remove columns from nat_map to be consistent
nat_map_abs01 <- nat_map_abs01[,c(1:7,9)]
colnames(nat_map_abs01)[8] <- "CED_NAME"

nat_map_abs06e04 <- nat_map_abs06e04[,c(1:7,9)]
colnames(nat_map_abs06e04)[8] <- "CED_NAME"

nat_map_abs06e07 <- nat_map_abs06e07[,c(1:7,12)]
colnames(nat_map_abs06e04)[8] <- "CED_NAME"


# Save as .rda
save(sFsmall_abs01, file = "Clean/Maps/sFsmall_abs01.rda")
save(sFsmall_abs06e04, file = "Clean/Maps/sFsmall_abs06e04.rda")
save(sFsmall_abs06e07, file = "Clean/Maps/sFsmall_abs06e07.rda")
save(sFsmall_abs11, file = "Clean/Maps/sFsmall_abs11.rda")
save(sFsmall_abs16, file = "Clean/Maps/sFsmall_abs16.rda")

#save(nat_map_abs01, file = "Clean/Maps/nat_map_abs01.rda")
#save(nat_map_abs06e04, file = "Clean/Maps/nat_map_abs06e04.rda")
#save(nat_map_abs06e07, file = "Clean/Maps/nat_map_abs06e07.rda")
#save(nat_map_abs11, file = "Clean/Maps/nat_map_abs11.rda")
#save(nat_map_abs16, file = "Clean/Maps/nat_map_abs16.rda")

#save(nat_data_abs01, file = "Clean/Maps/nat_data_abs01.rda")
#save(nat_data_abs06e04, file = "Clean/Maps/nat_data_abs06e04.rda")
#save(nat_data_abs06e07, file = "Clean/Maps/nat_data_abs06e07.rda")
#save(nat_data_abs11, file = "Clean/Maps/nat_data_abs11.rda")
#save(nat_data_abs16, file = "Clean/Maps/nat_data_abs16.rda")
