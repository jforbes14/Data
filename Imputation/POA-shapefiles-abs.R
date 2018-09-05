## This script reads in the shapefiles containing POST CODE boundaries, for the Census years in which an election does not occur
## The shapefiles are simplified and are used in subsequent programs to impute Census profiles at election time.

## Download ABS 2001 shapefiles from: https://data.gov.au/dataset/0b939a62-e53e-4616-add5-77f909b58ddd
## Download ABS 2006 ERSI shapefiles from: http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/2923.0.30.0012006?OpenDocument
## Download ABS 2011 and 2016 ERSI shapefiles from: https://datapacks.censusdata.abs.gov.au/datapacks/

## This script generates:
## sF_abs01_POA, sF_abs06_POA, sF_abs11_POA, sF_abs16_POA
## nat_map_abs01_POA, nat_map_abs06_POA, nat_map_abs11_POA, nat_map_abs16_POA
## nat_data_abs01_POA, nat_data_abs06_POA, nat_data_abs11_POA, nat_data_abs16_POA

# Simplified shapefiles are to be used in "mapping-POA.R"

#Read in ABS Census shapefiles
library(rgdal)
library(sp)
library(sf)
library(knitr)
library(maptools)
library(rmapshaper)
library(dplyr)


# Define centroid function
centroid <- function(i, polys) {
  ctr <- Polygon(polys[i])@labpt
  data.frame(long_c=ctr[1], lat_c=ctr[2])
}

# Census 2016
shapeFile <- "Raw/Shapefiles/2016_POA_shape/POA_2016_AUST.shp"
sF <- readShapeSpatial(shapeFile, delete_null_obj = TRUE)
sFsmall <- ms_simplify(sF, keep=0.05) 
nat_data16 <- sFsmall@data
nat_data16$id <- as.character(1:nrow(nat_data16))
nat_map16 <- ggplot2::fortify(sFsmall)
nat_map16$group <- paste("g",nat_map16$group,sep=".")
nat_map16$piece <- paste("p",nat_map16$piece,sep=".")
nms <- sFsmall@data %>% select(POA_NAME)
nms$id <- as.character(1:nrow(nms))
nat_map16 <- left_join(nat_map16, nms, by="id")
polys <- as(sFsmall, "SpatialPolygons")
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys) # ERROR: some polygons do not have 4+ joins
nat_data16 <- data.frame(nat_data16, centroids)
sFsmall@data <- cbind(sFsmall@data, centroids)

#sF_abs16_POA <- sF
sFsmall_abs16_POA <- sFsmall
nat_data_abs16_POA <- nat_data16
nat_map_abs16_POA <- nat_map16



# Census 2011
shapeFile <- "Raw/Shapefiles/2011_POA_shape/POA_2011_AUST.shp"
sF <- readShapeSpatial(shapeFile, delete_null_obj = TRUE)
sFsmall <- ms_simplify(sF, keep=0.05)
nat_data11 <- sFsmall@data
nat_data11$id <- as.character(1:nrow(nat_data11))
nat_map11 <- ggplot2::fortify(sFsmall)
nat_map11$group <- paste("g",nat_map11$group,sep=".")
nat_map11$piece <- paste("p",nat_map11$piece,sep=".")
nms <- sFsmall@data %>% select(POA_NAME)
nms$id <- as.character(1:nrow(nms))
nat_map11 <- left_join(nat_map11, nms, by="id")
polys <- as(sF, "SpatialPolygons")
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
nat_data11 <- data.frame(nat_data11, centroids)
sF@data <- cbind(sF@data, centroids)

#sF_abs11_POA <- sF
sFsmall_abs11_POA <- sF # can't use simplified due to issue with centroids, so use original
nat_data_abs11_POA <- nat_data11
nat_map_abs11_POA <- nat_map11


# Census 2006
shapeFile <- "Raw/Shapefiles/asgc2006.gpkg"
layers <- ogrListLayers(shapeFile)
sF <- readOGR(shapeFile, layers[8]) # Postal code area
sFsmall <- ms_simplify(sF, keep=0.05)
nat_data06 <- sFsmall@data
nat_data06$id <- row.names(nat_data06)
nat_map06 <- ggplot2::fortify(sFsmall)
nat_map06$group <- paste("g",nat_map06$group,sep=".")
nat_map06$piece <- paste("p",nat_map06$piece,sep=".")
nms <- sFsmall@data %>% select(POA_CODE_2006)
nms$id <- as.character(1:nrow(nms))
nat_map06 <- left_join(nat_map06, nms, by="id")
polys <- as(sF, "SpatialPolygons")
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
nat_data06 <- data.frame(nat_data06, centroids)
sFsmall@data <- cbind(sFsmall@data, centroids)

#sF_abs06_POA <- sF
sFsmall_abs06_POA <- sFsmall
nat_data_abs06_POA <- nat_data06
nat_map_abs06_POA <- nat_map06



# Census 2001
shapeFile <- "Raw/Shapefiles/asgc2001.gpkg"
layers <- ogrListLayers(shapeFile)
sF <- readOGR(shapeFile, layers[8]) # Postal code area
sFsmall <- ms_simplify(sF, keep=0.05)
nat_data01 <- sF@data
nat_data01$id <- row.names(nat_data01)
nat_map01 <- ggplot2::fortify(sFsmall)
nat_map01$group <- paste("g",nat_map01$group,sep=".")
nat_map01$piece <- paste("p",nat_map01$piece,sep=".")
nms <- sFsmall@data %>% select(POA_CODE_2001)
nms$id <- as.character(1:nrow(nms))
nat_map01 <- left_join(nat_map01, nms, by="id")
polys <- as(sFsmall, "SpatialPolygons")
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
nat_data01 <- data.frame(nat_data01, centroids)
sFsmall@data <- cbind(sFsmall@data, centroids)


#sF_abs01_POA <- sF
sFsmall_abs01_POA <- sFsmall
nat_data_abs01_POA <- nat_data01
nat_map_abs01_POA <- nat_map01

# Rename to match other files
simp_abs01_POA <- sFsmall_abs01_POA
simp_abs06_POA <- sFsmall_abs06_POA
simp_abs11_POA <- sFsmall_abs11_POA
simp_abs16_POA <- sFsmall_abs16_POA

# Common column names for data
simp_abs01_POA@data <- simp_abs01_POA@data %>% 
  rename(POA_CODE = POA_CODE_2001)
simp_abs06_POA@data <- simp_abs06_POA@data  %>% 
  rename(POA_CODE = POA_CODE_2006)
simp_abs11_POA@data <- simp_abs11_POA@data  %>% 
  select(-c(POA_CODE)) %>%
  rename(POA_CODE = POA_NAME)
simp_abs16_POA@data <- simp_abs16_POA@data  %>% 
  select(-c(POA_CODE, POA_NAME)) %>%
  rename(POA_CODE = POA_CODE16)


# Remove duplicated objects
remove(sF)
remove(sFsmall)
remove(nat_map01)
remove(nat_map06)
remove(nat_map11)
remove(nat_map16)
remove(nat_data01)
remove(nat_data06)
remove(nat_data11)
remove(nat_data16)
remove(centroids)
remove(polys)
remove(nms)

remove(sF_abs01_POA)
remove(sF_abs06_POA)
remove(sF_abs11_POA)
remove(sF_abs16_POA)
remove(sFsmall_abs01_POA)
remove(sFsmall_abs06_POA)
remove(sFsmall_abs11_POA)
remove(sFsmall_abs16_POA)



# Rename/remove columns from nat_data to be consistent
colnames(nat_data_abs01_POA)[1] <- "POA_CODE"

colnames(nat_data_abs06_POA)[1] <- "POA_CODE"

nat_data_abs11_POA <- nat_data_abs11_POA[,c(2,4:6)]
colnames(nat_data_abs11_POA)[1] <- "POA_CODE"

nat_data_abs16_POA <- nat_data_abs16_POA[,c(3,5:7)]
colnames(nat_data_abs16_POA)[1] <- "POA_CODE"


# Rename/remove columns from nat_map to be consistent
colnames(nat_map_abs01_POA)[8] <- "POA_CODE"

colnames(nat_map_abs06_POA)[8] <- "POA_CODE"

colnames(nat_map_abs11_POA)[8] <- "POA_CODE"

colnames(nat_map_abs16_POA)[8] <- "POA_CODE"


# Save as .rda
save(simp_abs01_POA, file = "Clean/Maps/simp_abs01_POA.rda")
save(simp_abs06_POA, file = "Clean/Maps/simp_abs06_POA.rda")
save(simp_abs11_POA, file = "Clean/Maps/simp_abs11_POA.rda")
save(simp_abs16_POA, file = "Clean/Maps/simp_abs16_POA.rda")

