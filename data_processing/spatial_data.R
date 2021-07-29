library(VicmapR)
library(sf)
library(rmapshaper)
library(tidyverse)

# listLayers(pattern = "lga", ignore.case = T)

# lga file from datavic
lga_vic_raw <- vicmap_query(layer = "datavic:VMADMIN_LGA_POLYGON") %>% 
  select(LGA_NAME) %>% 
  collect() 

lga_vic <- lga_vic_raw %>% 
  transmute(lga_name = str_to_title(LGA_NAME)) %>% 
  st_transform(4326)

# simplify the polygon
lga_simplify <- ms_simplify(lga_vic, 0.05)

# export
st_write(lga_simplify, "./data_out/lga.gpkg", delete_layer = TRUE)
