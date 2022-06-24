library(readxl)
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(viridis)
library(RColorBrewer)





setwd("~/Current Work/R_projects/Blocking_GIS")
toMatch <- c("~")
file.list_gis <- list.files(path = './FACE_MAPPING_GIS', pattern = '.xlsx', recursive = TRUE, full.names = TRUE)
file.list_gis <- file.list_gis[!grepl(paste(toMatch,collapse="|"), file.list_gis)]


########### Loading Face maps and coordinates ############


face_map_gis<- function(i) {
  x = read_xlsx(i,sheet = 1)
  colnames(x) <-
    c(
      "c1",
      "c2",
      "c3",
      "c4",
      "c5",
      "c6",
      "c7",
      "c8",
      "c9",
      "c10",
      "c11",
      "c12"
    )
  
  x <- x %>% transmute(
    HOLE_ID = as.character(c1),
    LOCATIONX = as.numeric(c2),
    LOCATIONY = as.numeric(c3),
    LOCATIONZ = as.numeric(c4),
    LENGTH = as.numeric(c5),
    LEVEL = as.numeric(c6),
    AREA = as.character(c7),
    ROCKCODE = as.character(c8),
    SAMP_BY = as.character(c9),
    TENEMENT = as.character(c11)) %>%
    filter(!is.na(HOLE_ID))
} 






############# Applying Function to the file list gis ###############
df_gis_coords <- lapply(file.list_gis, face_map_gis) %>%
  bind_rows %>%
  as.data.frame() %>%
distinct(.keep_all = TRUE)





########### Loading Assay Values ###############
face_map_gis_assay<- function(i) {
  x = read_xlsx(i,sheet = 2)
  
  
  # emptycols <- colSums(is.na(x)) == nrow(x)
  # x<- x[!emptycols]
  
  colnames(x) <-
    c(
      "c1",
      "c2",
      "c3",
      "c4",
      "c5",
      "c6",
      "c7",
      "c8",
      "c9",
      "c10",
      "c11",
      "c12",
      "c13",
      "c14",
      "c15",
      "c16",
      "c17",
      "c18",
      "c19",
      "c20",
      "c21",
      "c22",
      "c23",
      "c24",
      "c25"
    )
  
  x <- x %>% transmute(
    HOLE_ID = as.character(c1),
   FROM = as.numeric(c2),
    TO = as.numeric(c3),
    LENGTH = as.numeric(c4),
    SAMPLE_NO = as.numeric(c5),
    AU_GPT = as.numeric(c6),
   AG_GPT = as.numeric(c12),
    CU_PPM = as.numeric(c7),
    PB_PPM = as.numeric(c8),
    ZN_PPM = as.numeric(c9),
   LEN_AU = LENGTH * AU_GPT,
   LEN_AG = LENGTH * AG_GPT,
   LEN_CU = LENGTH * CU_PPM,
   LEN_PB = LENGTH * PB_PPM,
   LEN_ZN = LENGTH * ZN_PPM,
    SG = as.numeric(c10),
    REASSAY = as.numeric(c11),
   ROCK_TYPE = as.character(c13),
   MV_WIDTH = as.numeric(c14),
   BATCH_NO = as.character(c17)
   
   ) %>%
    filter(!is.na(HOLE_ID))
} 


############# Applying Function to the file list gis ###############
df_gis_assay <- lapply(file.list_gis, face_map_gis_assay) %>%
  bind_rows %>%
  as.data.frame() %>%
  distinct(.keep_all = TRUE)




df_gis_assay <- df_gis_assay %>% group_by(HOLE_ID,ROCK_TYPE) %>%
  summarize(LENGTH = sum(LENGTH),
            AU = sum(LEN_AU)/ sum(LENGTH),
            AU = ifelse(AU >=25 ,25,AU),
            AG = sum(LEN_AG)/ sum(LENGTH),
            CU = sum(LEN_CU)/ sum(LENGTH),
            ZN = sum(LEN_ZN)/ sum(LENGTH),
            PB = sum(LEN_PB)/ sum(LENGTH)) %>%
  mutate(LEN_AU = LENGTH * AU)


MV_L <- df_gis_assay %>%
  group_by(HOLE_ID) %>% 
  summarize(MV_L = ifelse(ROCK_TYPE == "MV",LENGTH,0 )) %>%
  filter(MV_L != 0)


df_gis_assay <- left_join(df_gis_assay, MV_L, by = "HOLE_ID")

df_gis_assay <- df_gis_assay %>% mutate(LENGTH = ifelse(ROCK_TYPE != "MV", (3 - MV_L)/2, MV_L),
                                        LEN_AU = LENGTH * AU)


df_gis_assay_block <- df_gis_assay %>% group_by(HOLE_ID) %>% 
  summarize(COMP_AU = sum(LEN_AU)/sum(LENGTH))


############# Join Coordinates and assay##############

df_joined <- left_join(df_gis_assay_block ,df_gis_coords,by = "HOLE_ID")
df_joined <- df_joined %>% filter(!is.na(LOCATIONX))


############ plotting of df files ############


df_points<- st_as_sf(df_joined , coords = c("LOCATIONX", "LOCATIONY"), crs = 3125)
face_map_plot <- ggplot(data = df_points, aes(color = COMP_AU, text = HOLE_ID)) + 
  geom_sf(size = 2.0) +
  scale_colour_gradientn(colours = c("purple","red","#FF5349","yellow", "green", "blue", "gray"),
                         values = c(1.0,0.4,0.32,0.2,0.12,0.04,0))

ggplotly(face_map_plot,tooltip = "text")



############ INPUT SHAPEFILE POSITION LINES ###############  


# setwd("~/Current Work/R_projects/Blocking_GIS/Shapefiles")
# 
# POS_LINES <- st_read(
#   "./N_S_Positions.shp")
# POS_LINES<- POS_LINES[,-c(1:2)]
# POS_LINES_PLOT <- ggplot() + 
#   geom_sf(data = POS_LINES, size = 0.1, color = "cyan") + 
#   ggtitle("POS_LINES_PLOT") + 
#   coord_sf()
# 
# ggplotly(POS_LINES_PLOT) 




######## PLOTTING ##############


# face_map_names <- st_centroid(face_map_plot) ############### creating centroids on the data
# face_map_names <- cbind(face_map_names, st_coordinates(st_centroid(face_map_plot$geometry)))############### addng X and Y to points



############# Intersection of the position lines and the face mapping plots #############

# 
# POS_FACE_MAP <- st_intersection(POS_LINES,face_map_plot)
# POS_FACE_MAP_PLOT <- ggplot(data = POS_FACE_MAP, aes(color = SOURCE, text = SHEET)) +
#   geom_sf()
# 
# ggplotly(POS_FACE_MAP_PLOT,tooltip = "text")
