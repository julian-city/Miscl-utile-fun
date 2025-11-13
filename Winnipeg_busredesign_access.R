#Evaluating change in access to jobs in Winnipeg pre-post bus network redesign

#13 November 2025

#Julian Villafuerte Diaz
#contact@julian.city

library(tidyverse)
library(sf)
#library(mapview)
library(readr)
library(cancensus)
options(cancensus.cache_path = 'C:\\r_files\\cancensus_cache')
options(cancensus.api_key='YOUR CENSUSMAPPER API KEY')
#you can obtain an api key for census mapper by signing up :
#https://censusmapper.ca/users/sign_in
#it's free

#THIS SCRIPT ASSUMES that in the project directory 
#you have the following sub-directories: scenario, baseline, output

#DOWNLOAD JOBS BY CT FROM STATCAN CENSUS 2021------------------------

# Create a temporary file for the zip download
temp_zip <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

# Download the zip file from statcan that contains jobs by CT
download.file("https://www150.statcan.gc.ca/n1/tbl/csv/98100506-eng.zip", 
              destfile = temp_zip, 
              mode = "wb")

# Extract the zip file
unzip(temp_zip, exdir = temp_dir)

# Find the CSV file (assuming there's only one CSV in the zip)
csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)

#The file path directing to the table (as opposed to the metadata) is the second in the list

# Read the CSV into a dataframe
jobsct <- read_csv(csv_files[2])

# Clean up temporary files
unlink(temp_zip)
unlink(csv_files)

jobsct <- 
jobsct[,c(2:7,9,11)]
#keeping only relevant fields

colnames(jobsct) <- c("geo","DGUID","gender","edu","coord","total","wfh","pres")

jobs_pres_by_ct <- 
jobsct %>% 
  select(DGUID,gender,edu,total,wfh,pres) %>% 
  filter(gender=="Total - Gender",
         edu=="Total - Highest certificate, diploma or degree") %>% 
  #distinct(DGUID,.keep_all = TRUE) # all good
  select(DGUID,pres) %>% 
  rename(jobs=pres)
  
#next step : link to geography------------

#download all census tract geometries using census mapper api
ca_cts <- 
get_census(dataset = 'CA21', 
           regions = list(C = 01),
           level = 'CT', 
           geo_format = 'sf') %>% 
  st_transform(4326) %>% 
  as_tibble() %>% 
  st_as_sf()

#number of jobs at a regular place of work by CT
jobs_pres_by_ct_geouid <- 
jobs_pres_by_ct %>% 
  rowwise() %>% 
  mutate(GeoUID_len=str_length(DGUID)) %>% 
  filter(GeoUID_len==19) %>% 
  #only retain CT-level data, not CMA-level data in this dataset associated with
  #entries of length 12
  mutate(GeoUID=str_sub(DGUID,10,19)) %>% 
  select(GeoUID,jobs) %>% 
  ungroup()

#53 NA cases
#jobs_pres_by_ct_geouid %>% 
 # filter(is.na(jobs))

#joining number of jobs to census tract geometry
ca_cts_jobs_sf <- 
ca_cts %>% 
  select(GeoUID,geometry) %>% 
  left_join(jobs_pres_by_ct_geouid,
            by="GeoUID") %>% 
  #filter(is.na(jobs)) # 53 NAs, successful
  mutate(jobs=replace_na(jobs,0)) 

#identify CTs for the city---------------

city <- "Winnipeg"

#can modify code for CMA instead of CSD. To keep the extent smaller,
#CSD was chosen for this analysis

#identify the CSD code for the city
city_code <- 
list_census_regions("CA21") %>% 
  filter(level=="CSD",
         name==city) %>% 
  pull(region)

#identify GeoUIDs for the CTs within the CSD
city_cts <- 
  get_census(dataset = 'CA21', 
             regions = list(CSD = city_code),
             level = 'CT') %>% 
  pull(GeoUID)

#OSM data import : POIs-------------

library(osmdata)

bbox <- 
st_bbox(ca_cts_jobs_sf %>% 
          filter(GeoUID%in%city_cts))
#needed for the OSM requests

city_zone <- 
ca_cts_jobs_sf %>% 
  filter(GeoUID%in%city_cts) %>% 
  select(geometry) %>% summarise()
#needed to exclude polygons / amenities out of scope when assembling employment area

#download landuse geometries from OSM
landuse <- 
  opq(bbox) %>% 
  add_osm_feature(key = "landuse") %>% 
  osmdata_sf()

#from experience, a non-exhaustive list of landuse tags associated with zones of employment
jobs_landuse <- c("industrial","retail","commercial","construction","institutional",
                  "greenhouse_horticulture","military","education","civic_admin","healthcare",
                  "research","depot","garages","animal_keeping")

#download amenities
amenity <- 
  opq(bbox) %>% 
  add_osm_feature(key = "amenity") %>% 
  osmdata_sf()

#assuming that amenities can be associated with employment a priori, a non-exhaustive list
#of tags that are NOT associated with zones of employment
amenities_excl <- c("parking","loading_dock","fountain","bicycle_parking",
                  "grave_yard","parking_space","waste_disposal","waste_transfer_station",
                  "give_box","bicycle_repair_station","motorcycle_parking",
                  "boat_storage","snow_disposal","traffic_park","bicycle_parking",
                  "post_box","bench","toilets","recycling")

#download buildings
building <- 
  opq(bbox) %>% 
  add_osm_feature(key = "building") %>% 
  osmdata_sf()

#assuming that buildings can be associated with employment a priori, a non-exhaustive list
#of tags that are NOT associated with zones of employment
building_excl <- c(
  "parking","yes","apartments","detached","residential","house","terrace","roof",
  "steps","ruins","shed","semidetached_house","dormitory","stable","toilets","apartment_block",
  "vacant","disused","semidetached","no","pavilion","hut","static_caravan","folie",
  "carport","gazebo","bungalow","barn","outbuilding","bridge","container","farm_auxiliary",
  "guardhouse","proposed","deck","stage","counterweight","advertising",
  "column","allotment_house","clubhouse","garages","garage"
)

#now, create one employment area sf

amenity_polygons <- 
amenity$osm_polygons %>% st_make_valid() %>% 
  filter(!amenity%in%amenities_excl) %>% 
  select(geometry) %>%
  mutate(valid=st_is_valid(geometry)) %>% filter(valid) %>% 
  st_filter(city_zone) %>% 
  summarise()

amenity_multipolygons <- 
  amenity$osm_multipolygons %>% st_make_valid() %>% 
  filter(!amenity%in%amenities_excl) %>% 
  select(geometry) %>%
  mutate(valid=st_is_valid(geometry)) %>% filter(valid) %>% 
  #for Winnipeg, one lot downtown that remains invalid
  st_filter(city_zone) %>% 
  summarise()

landuse_polygons <- 
  landuse$osm_polygons %>% 
  select(name,landuse,geometry) %>% 
  filter(landuse%in%jobs_landuse) %>% 
  select(geometry) %>% st_make_valid() %>% 
  st_filter(city_zone) %>% 
  summarise()

landuse_multipolygons <- 
  landuse$osm_multipolygons %>% 
  select(name,landuse,geometry) %>% 
  filter(landuse%in%jobs_landuse) %>% 
  select(geometry) %>% st_make_valid() %>% 
  st_filter(city_zone) %>% 
  summarise()

building_polygons <- 
  building$osm_polygons %>% 
  select(name,building,geometry) %>% 
  filter(!building%in%building_excl) %>% 
  select(geometry) %>% st_make_valid() %>% 
  st_filter(city_zone) %>% summarise()

building_multipolygons <- 
  building$osm_multipolygons %>% 
  select(name,building,geometry) %>% 
  filter(!building%in%building_excl) %>% 
  select(geometry) %>% st_make_valid() %>% 
  st_filter(city_zone) %>% summarise()

employment_zone <- 
rbind(landuse_polygons,
      landuse_multipolygons,
      amenity_multipolygons,
      amenity_polygons,
      building_polygons,
      building_multipolygons) %>% 
  st_make_valid() %>% 
  summarise()

#Jobs by hexagon-------------------

#Takes 2 minutes for Winnipeg

#create hexagonal grid
city_hex <- 
st_make_grid(city_zone,
             n=c(120,120),
             square=FALSE)

city_hex <- 
city_hex %>% as_tibble() %>% 
  mutate(id=row_number(),.before=geometry) %>% 
  st_as_sf() %>% 
  st_transform(4326)

city_hex_centroids <- 
  st_centroid(city_hex)

#identify hex ids to retain (filter out hexes in the river & outside of the zone)
hex_ids_city_zone <- 
city_hex_centroids %>% 
  st_filter(city_zone) %>% 
  pull(id)

city_hex <- 
city_hex %>% 
  filter(id%in%hex_ids_city_zone)

#Loop by CT to assign number of jobs to hexagons

#first, create empty object for jobs by hex by geouid

hex_jobs_frag <- 
  tibble(id=character(),
         GeoUID=character(),
         jobs=double())

city_cts_jobs_sf <- 
ca_cts_jobs_sf %>% 
  filter(GeoUID%in%city_cts)

for(i in 1:nrow(city_cts_jobs_sf)){

ct_i <- city_cts_jobs_sf[i,]

geouid_i <- ct_i$GeoUID

n_jobs_i <- 
  ct_i$jobs

cat("\rProcessing CT",geouid_i,i,"of",nrow(city_cts_jobs_sf))
 
#filter for the employment zone of the CT

employment_zone_i <- 
st_intersection(
  employment_zone,
  ct_i
)

if(nrow(employment_zone_i)==0){
  
  #IF NO EMPLOYMENT ZONE DETECTED IN THIS CT, use whole CT area to calculate
  #jobs by hex
  
  ct_area <- 
    as.numeric(st_area(ct_i$geometry))
  
  city_hex_i <- 
    city_hex %>% 
    st_filter(ct_i)
  
  hex_jobs_frag_i <- 
  st_intersection(
    city_hex_i,
    ct_i
  ) %>% 
    mutate(frag_area=as.numeric(st_area(geometry))) %>% 
    mutate(jobs=round(n_jobs_i*(frag_area/ct_area),1)) %>% 
    as_tibble() %>% 
    select(id,GeoUID,jobs)
  
}else{

#ELSE assign number of jobs by hex based on an intersection with employment_zone_i

employment_zone_area <- 
  as.numeric(st_area(employment_zone_i$geometry))

city_hex_i <- 
city_hex %>% 
  st_filter(employment_zone_i)

hex_jobs_frag_i <- 
st_intersection(
  city_hex_i,
  employment_zone_i
) %>% 
  mutate(frag_area=as.numeric(st_area(geometry))) %>% 
  mutate(jobs=round(n_jobs_i*(frag_area/employment_zone_area),1)) %>% 
  as_tibble() %>% 
  select(id,GeoUID,jobs)

}

hex_jobs_frag <- 
  rbind(hex_jobs_frag,
        hex_jobs_frag_i)

}

#number of jobs by hexagon
jobs_hex <- 
hex_jobs_frag %>% 
  select(id,jobs) %>% 
  group_by(id) %>% 
  summarise(jobs=sum(jobs))

city_hex_sf <- 
city_hex %>% 
  left_join(jobs_hex,by="id") %>% 
  mutate(jobs=replace_na(jobs,0))

#Download OSM data for r5r scenarios and for street networks for maps---------------

library(osmextract)

osm_roadnetwork <- oe_get(city,stringsAsFactors=FALSE,quiet=TRUE,
                          download_directory = "baseline")

osm_roadnetwork <- oe_get(city,stringsAsFactors=FALSE,quiet=TRUE,
                          download_directory = "scenario")

#very voluminous gpkg get downloaded into each of these directories. These can be
#deleted with no harm done !

primary_roadnetwork <- 
osm_roadnetwork %>% 
  filter(highway%in%c("trunk","motorway","primary")) %>% 
  select(name,highway,geometry)

primary_roadnetwork <- 
st_intersection(primary_roadnetwork,
                city_zone)

secondary_roadnetwork <- 
osm_roadnetwork %>% 
  filter(highway%in%c("secondary")) %>% 
  select(name,highway,geometry)

secondary_roadnetwork <- 
  st_intersection(
    secondary_roadnetwork,
    city_zone
  )

remove(osm_roadnetwork)

#Bonus : validate jobs by hexagon by mapping---------------

#ggplot()+
#  geom_sf(data=city_zone,fill="grey70",colour="transparent")+
#  geom_sf(data=primary_roadnetwork,
#          color="white",size=1)+
#  geom_sf(data=secondary_roadnetwork,
#          color="white",size=0.4)+
#  geom_sf(data=city_hex_sf %>% filter(jobs>0),
#          aes(fill=jobs),colour="transparent",alpha=0.7)+
#  scale_fill_viridis_c(option="A",direction=-1)+
#  theme_void()

#DOWNLOAD GTFS----------------------------

library(gtfstools)

#WINNIPEG TRANSIT GTFS FOR NOVEMBER 2024
gtfs_24n <- read_gtfs("https://files.mobilitydatabase.org/mdb-717/mdb-717-202411070031/mdb-717-202411070031.zip")

write_gtfs(gtfs_24n,"baseline/gtfs_24n.zip")

#WINNIPEG TRANSIT GTFS FOR NOVEMBER 2025
gtfs_25n <- read_gtfs("https://files.mobilitydatabase.org/mdb-717/mdb-717-202511090030/mdb-717-202511090030.zip")

write_gtfs(gtfs_25n,"scenario/gtfs_25n.zip")

#create origins surface, i.e. areas where people live and/or work------------------

#now necessary to filter the hexes for origins (including residential areas)

landuse_incl <- c("industrial","retail","residential","commercial","construction","institutional",
                  "greenhouse_horticulture","military","education","civic_admin","healthcare",
                  "civil","religious","community_centre",
                  "research","depot","garages","animal_keeping")

building_excl_resi <- c(
  "parking","yes","terrace","roof",
  "steps","ruins","shed","stable","toilets",
  "vacant","disused","no","pavilion","hut","static_caravan","folie",
  "carport","gazebo","bungalow","barn","outbuilding","bridge","container","farm_auxiliary",
  "guardhouse","proposed","deck","stage","counterweight","advertising",
  "column","allotment_house","garages","garage"
)

#start_time <- Sys.time()

landuse_polygons_resi <- 
  landuse$osm_polygons %>% 
  filter(landuse%in%landuse_incl) %>% 
  select(geometry) %>% st_make_valid() %>% 
  mutate(valid=st_is_valid(geometry)) %>% filter(valid) %>% 
  st_filter(city_zone) %>% 
  summarise()

landuse_multipolygons_resi <- 
  landuse$osm_multipolygons %>% 
  filter(landuse%in%landuse_incl) %>% 
  select(geometry) %>% st_make_valid() %>% 
  mutate(valid=st_is_valid(geometry)) %>% filter(valid) %>% 
  st_filter(city_zone) %>% 
  summarise()

building_polygons_resi <- 
  building$osm_polygons %>% 
  filter(!building%in%building_excl_resi) %>% 
  select(geometry) %>% st_make_valid() %>% 
  mutate(valid=st_is_valid(geometry)) %>% filter(valid) %>% 
  st_filter(city_zone) %>% summarise()

building_multipolygons_resi <- 
  building$osm_multipolygons %>% 
  filter(!building%in%building_excl_resi) %>% 
  select(geometry) %>% st_make_valid() %>% 
  mutate(valid=st_is_valid(geometry)) %>% filter(valid) %>% 
  st_filter(city_zone) %>% summarise()

origins_surface <- 
  rbind(
    building_polygons_resi,
    building_multipolygons_resi,
    amenity_polygons,
    amenity_multipolygons,
    landuse_multipolygons_resi,
    landuse_polygons_resi
  ) %>% st_make_valid() %>% 
  summarise()
#INVALID

#end_time <- Sys.time()
#end_time-start_time

origins_surface <- 
  origins_surface %>% st_make_valid()

#R5R SIMULATIONS-----------------------------

library(hms)
library(sf)
options(java.parameters="-Xmx6G")
library(r5r)

#determine origin points for simulation using origins surface

city_hex_origins <- 
  city_hex_sf %>% 
  st_filter(origins_surface) %>% 
  st_centroid() %>% 
  select(id,geometry)

city_hex_destinations <- 
  city_hex_sf %>% 
  filter(jobs>0) %>% 
  st_centroid() %>% 
  select(id,geometry)

#baseline : r5r travel time matrix (ttm)-----------

#set up r5
r5r_core_baseline <- setup_r5(data_path = "baseline")

#calculate ttm

mode <- c("WALK", "TRANSIT")
max_walk_dist <- 2000 
max_trip_duration <- 120
departure_datetime <- as.POSIXct("13-11-2024 08:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
time_window <- 30 
percentiles <- c(5, 25, 50, 75, 95)

ttm_baseline_transit <- travel_time_matrix(r5r_core_baseline,
                                           origins = city_hex_origins,
                                           destinations = city_hex_destinations,
                                           mode = mode,
                                           departure_datetime = departure_datetime,
                                           max_trip_duration = max_trip_duration,
                                           time_window = time_window,
                                           percentiles = percentiles,
                                           verbose = FALSE,
                                           progress = TRUE)


city_hex_accessjobs_transit_45min <- 
ttm_baseline_transit %>% as_tibble() %>% 
  select(from_id,to_id,travel_time_p50) %>% 
  filter(travel_time_p50<=45) %>% 
  #add job information
  left_join(city_hex_sf %>% as_tibble() %>% 
              mutate(id=as.character(id)) %>% select(id,jobs),
            by=c("to_id"="id")) %>% 
  rename(id=from_id) %>% 
  group_by(id) %>% 
  summarise(access_jobs_45min=sum(jobs,na.rm=TRUE))

#map to validate results
#city_hex_sf %>% 
#  left_join(city_hex_accessjobs_transit_45min %>% 
#              mutate(id=as.numeric(id)),
#            by="id") %>% 
#  ggplot()+
#  geom_sf(aes(fill=access_jobs_45min),color="transparent")+
#  scale_fill_viridis_c()+
#  theme_void()

#scenario (redesign) : r5r ttm-----------

#set up r5
r5r_core_scenario <- setup_r5(data_path = "scenario")

#calculate ttm

mode <- c("WALK", "TRANSIT")
max_walk_dist <- 2000 
max_trip_duration <- 120
departure_datetime <- as.POSIXct("12-11-2025 08:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
time_window <- 30 
percentiles <- c(5, 25, 50, 75, 95)

ttm_scenario_transit <- travel_time_matrix(r5r_core_scenario,
                                           origins = city_hex_origins,
                                           destinations = city_hex_destinations,
                                           mode = mode,
                                           departure_datetime = departure_datetime,
                                           max_trip_duration = max_trip_duration,
                                           time_window = time_window,
                                           percentiles = percentiles,
                                           verbose = FALSE,
                                           progress = TRUE)

city_hex_accessjobs_transit_45min_scenario <- 
  ttm_scenario_transit %>% as_tibble() %>% 
  select(from_id,to_id,travel_time_p50) %>% 
  filter(travel_time_p50<=45) %>% 
  #add job information
  left_join(city_hex_sf %>% as_tibble() %>% 
              mutate(id=as.character(id)) %>% select(id,jobs),
            by=c("to_id"="id")) %>% 
  rename(id=from_id) %>% 
  group_by(id) %>% 
  summarise(access_jobs_45min=sum(jobs,na.rm=TRUE))

#map to validate results
#city_hex_sf %>% 
#  left_join(city_hex_accessjobs_transit_45min_scenario %>% 
#              mutate(id=as.numeric(id)),
#            by="id") %>% 
#  ggplot()+
#  geom_sf(aes(fill=access_jobs_45min),color="transparent")+
#  scale_fill_viridis_c()+
#  theme_void()

#difference-----------

city_hex_scenario_v_baseline_transit_45min <- 
  city_hex_accessjobs_transit_45min_scenario %>% 
  rename(scenario=access_jobs_45min) %>% 
  left_join(city_hex_accessjobs_transit_45min %>% rename(baseline=access_jobs_45min),
            by="id") %>% 
  mutate(gain_access_45min=scenario-baseline)
  
hex_accessdiff_sf <- 
city_hex_sf %>% 
  st_filter(origins_surface) %>% 
  mutate(id=as.character(id)) %>% 
  left_join(city_hex_scenario_v_baseline_transit_45min,
            by="id") %>% 
  filter(!is.na(gain_access_45min)) %>% 
  select(id,gain_access_45min,geometry)

#extract Rapid transit, Frequent Express, Frequent and Direct lines

#service_ids
#only wednesday and for november 12

#gtfs_25n$calendar %>% 
#  filter(start_date<=as.Date("2025-11-12")&
#           end_date>=as.Date("2025-11-12"))

#rapid transit

#get route id and colour

rt_routes <- 
gtfs_25n$routes %>% 
  filter(str_detect(route_id,"^BLUE")) %>% 
  pull(route_id)

rt_colour <- 
gtfs_25n$routes %>% 
  filter(str_detect(route_id,"^BLUE")) %>% 
  pull(route_color) %>% unique()

rt_colour <- paste0("#",rt_colour)

rt_shape_ids <- 
gtfs_25n$trips %>% 
  filter(route_id%in%rt_routes,
         service_id=="1") %>% 
  pull(shape_id) %>% unique()

rt_shapes <- 
  gtfs_25n$shapes %>% 
  filter(shape_id%in%rt_shape_ids) %>% 
  arrange(shape_id,shape_pt_sequence) %>% 
  st_as_sf(coords=c("shape_pt_lon","shape_pt_lat"),crs=4326) %>% 
  group_by(shape_id) %>% 
  summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING")

#fx2

fx2_routes <- 
  gtfs_25n$routes %>% 
  filter(str_detect(route_id,"^FX2")) %>% 
  pull(route_id)

fx2_colour <- 
  gtfs_25n$routes %>% 
  filter(str_detect(route_id,"^FX2")) %>% 
  pull(route_color) %>% unique()

fx2_colour <- paste0("#",fx2_colour)

fx2_shape_ids <- 
  gtfs_25n$trips %>% 
  filter(route_id%in%fx2_routes,
         service_id=="1") %>% 
  pull(shape_id) %>% unique()

fx2_shapes <- 
  gtfs_25n$shapes %>% 
  filter(shape_id%in%fx2_shape_ids) %>% 
  arrange(shape_id,shape_pt_sequence) %>% 
  st_as_sf(coords=c("shape_pt_lon","shape_pt_lat"),crs=4326) %>% 
  group_by(shape_id) %>% 
  summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING")

#fx3

fx3_routes <- 
  gtfs_25n$routes %>% 
  filter(str_detect(route_id,"^FX3")) %>% 
  pull(route_id)

fx3_colour <- 
  gtfs_25n$routes %>% 
  filter(str_detect(route_id,"^FX3")) %>% 
  pull(route_color) %>% unique()

fx3_colour <- paste0("#",fx3_colour)

fx3_shape_ids <- 
  gtfs_25n$trips %>% 
  filter(route_id%in%fx3_routes,
         service_id=="1") %>% 
  pull(shape_id) %>% unique()

fx3_shapes <- 
  gtfs_25n$shapes %>% 
  filter(shape_id%in%fx3_shape_ids) %>% 
  arrange(shape_id,shape_pt_sequence) %>% 
  st_as_sf(coords=c("shape_pt_lon","shape_pt_lat"),crs=4326) %>% 
  group_by(shape_id) %>% 
  summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING")


#fx4

fx4_routes <- 
  gtfs_25n$routes %>% 
  filter(str_detect(route_id,"^FX4")) %>% 
  pull(route_id)

fx4_colour <- 
  gtfs_25n$routes %>% 
  filter(str_detect(route_id,"^FX4")) %>% 
  pull(route_color) %>% unique()

fx4_colour <- paste0("#",fx4_colour)

fx4_shape_ids <- 
  gtfs_25n$trips %>% 
  filter(route_id%in%fx4_routes,
         service_id=="1") %>% 
  pull(shape_id) %>% unique()

fx4_shapes <- 
  gtfs_25n$shapes %>% 
  filter(shape_id%in%fx4_shape_ids) %>% 
  arrange(shape_id,shape_pt_sequence) %>% 
  st_as_sf(coords=c("shape_pt_lon","shape_pt_lat"),crs=4326) %>% 
  group_by(shape_id) %>% 
  summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING")

#

f_routes <- 
gtfs_25n$routes %>% 
  filter(str_detect(route_id,"^F")&
           !str_detect(route_id,"^FX")) %>% pull(route_id)

f_colour <- 
  gtfs_25n$routes %>% 
  filter(str_detect(route_id,"^F")&
           !str_detect(route_id,"^FX")) %>% pull(route_color) %>% unique()

f_colour <- paste0("#",f_colour)

f_shape_ids <- 
  gtfs_25n$trips %>% 
  filter(route_id%in%f_routes,
         service_id=="1") %>% 
  pull(shape_id) %>% unique()

f_shapes <- 
  gtfs_25n$shapes %>% 
  filter(shape_id%in%f_shape_ids) %>% 
  arrange(shape_id,shape_pt_sequence) %>% 
  st_as_sf(coords=c("shape_pt_lon","shape_pt_lat"),crs=4326) %>% 
  group_by(shape_id) %>% 
  summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING")

#d routes

d_routes <- 
gtfs_25n$routes %>% 
  filter(str_detect(route_id,"^D")) %>% 
  pull(route_id) %>% unique()

d_colour <- 
  gtfs_25n$routes %>% 
  filter(str_detect(route_id,"^D")) %>% 
  pull(route_color) %>% unique()

d_colour <- paste0("#",d_colour)

d_shape_ids <- 
  gtfs_25n$trips %>% 
  filter(route_id%in%d_routes,
         service_id=="1") %>% 
  pull(shape_id) %>% unique()

d_shapes <- 
  gtfs_25n$shapes %>% 
  filter(shape_id%in%d_shape_ids) %>% 
  arrange(shape_id,shape_pt_sequence) %>% 
  st_as_sf(coords=c("shape_pt_lon","shape_pt_lat"),crs=4326) %>% 
  group_by(shape_id) %>% 
  summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING")

#rest of the routes

rest_shapes <- 
  gtfs_25n$shapes %>% 
  filter(!shape_id%in%c(rt_shapes,fx2_shapes,fx3_shapes,fx4_shapes,
                       f_shapes,d_shapes)) %>% 
  arrange(shape_id,shape_pt_sequence) %>% 
  st_as_sf(coords=c("shape_pt_lon","shape_pt_lat"),crs=4326) %>% 
  group_by(shape_id) %>% 
  summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING")

#Map !! Change in access to jobs

winnipeg_jobs_diff_gg <- 
ggplot()+
  geom_sf(data=city_zone,fill="grey90",colour="white",linewidth=1)+
  geom_sf(data=primary_roadnetwork,
          color="white",linewidth=0.8)+
  geom_sf(data=secondary_roadnetwork,
          color="white",linewidth=0.4)+
  geom_sf(data=hex_accessdiff_sf,
          aes(fill=gain_access_45min),colour="transparent",alpha=0.8)+
  geom_sf(data=rest_shapes,
          colour="#F7F7F7",linewidth=0.1,alpha=0.8)+
  geom_sf(data=d_shapes,
          colour=d_colour,linewidth=0.2,alpha=0.5)+
  geom_sf(data=f_shapes,
          colour=f_colour,linewidth=0.3,alpha=0.5)+
  geom_sf(data=fx4_shapes,
          colour=fx4_colour,linewidth=0.5)+
  geom_sf(data=fx3_shapes,
          colour=fx3_colour,linewidth=0.5)+
  geom_sf(data=fx2_shapes,
          colour=fx2_colour,linewidth=0.5)+
  geom_sf(data=rt_shapes,
          colour=rt_colour,linewidth=0.8)+
  #scale_fill_gradient2(low="#B2182B",mid="#F7F7F7",high="#2166AC")+
  scale_fill_gradientn(colours=c("#B2182B","#D6604D","#F7F7F7","#4393C3","#2166AC"),
                       rescaler=~ scales::rescale_mid(.x,mid=0))+
  labs(title="Change in access to jobs, 8AM Weekday",
    fill="Number of jobs")+
  theme_void()

ggsave("output/winnipeg_jobs.png",
       winnipeg_jobs_diff_gg,
       width=6,height=5,dpi=200)

#time to Portage and Main difference---------------

portage_n_main <- 
  st_sf(
    geometry=st_sfc(st_point(c(-97.13844,49.89549)),crs=4326)
  )

#which hexagon intersects with this point ?

portage_n_main_id <- 
city_hex_sf %>% 
  st_filter(portage_n_main) %>% 
  pull(id)

tt_pnm_baseline <- 
ttm_baseline_transit %>% 
  filter(to_id==as.character(portage_n_main_id)) %>% 
  select(from_id,travel_time_p50) %>% 
  rename(id=from_id,
         tt_baseline=travel_time_p50)

tt_pnm_scenario <- 
  ttm_scenario_transit %>% 
  filter(to_id==as.character(portage_n_main_id)) %>% 
  select(from_id,travel_time_p50) %>% 
  rename(id=from_id,
         tt_scenario=travel_time_p50)

tt_pnm_savings <- 
tt_pnm_baseline %>% 
  left_join(tt_pnm_scenario,
            by="id") %>% 
  mutate(tt_savings=tt_baseline-tt_scenario) %>% 
  select(id,tt_savings)

city_hex_pnm_ttdiff <- 
city_hex_sf %>% 
  st_filter(origins_surface) %>% 
  mutate(id=as.character(id)) %>% 
  left_join(tt_pnm_savings,
            by="id") %>% 
  mutate(tt_savings=replace_na(tt_savings,0))

ttdiff_pnm_gg <- 
ggplot()+
  geom_sf(data=city_zone,fill="grey90",colour="white",linewidth=1)+
  geom_sf(data=primary_roadnetwork,
          color="white",linewidth=0.8)+
  geom_sf(data=secondary_roadnetwork,
          color="white",linewidth=0.4)+
  geom_sf(data=city_hex_pnm_ttdiff,
          aes(fill=tt_savings),colour="transparent",alpha=0.8)+
  geom_sf(data=rest_shapes,
          colour="#F7F7F7",linewidth=0.1,alpha=0.8)+
  geom_sf(data=d_shapes,
          colour=d_colour,linewidth=0.2,alpha=0.5)+
  geom_sf(data=f_shapes,
          colour=f_colour,linewidth=0.3,alpha=0.5)+
  geom_sf(data=fx4_shapes,
          colour=fx4_colour,linewidth=0.5)+
  geom_sf(data=fx3_shapes,
          colour=fx3_colour,linewidth=0.5)+
  geom_sf(data=fx2_shapes,
          colour=fx2_colour,linewidth=0.5)+
  geom_sf(data=rt_shapes,
          colour=rt_colour,linewidth=0.8)+
  geom_sf(data=portage_n_main,
          colour="#FFE999",size=2)+
  scale_fill_gradientn(colours=c("#D6604D","#F4A582","#F7F7F7","#4393C3","#2166AC"),
                       rescaler=~ scales::rescale_mid(.x,mid=0))+
  #scale_fill_gradient2(low="#B2182B",mid="#F7F7F7",high="#2166AC")+
  labs(title="Travel time savings to Portage & Main, 8AM Weekday",
       fill="Minutes")+
  theme_void()

ggsave("output/ttdiff_pnm.png",
       ttdiff_pnm_gg,
       height=5,width=6,dpi=200)

#Time to HSCW difference----------

hscw <- 
  st_sf(
    geometry=st_sfc(st_point(c(-97.158162,49.904947)),crs=4326)
  )

#which hexagon intersects with this point ?

hscw_id <- 
  city_hex_sf %>% 
  st_filter(hscw) %>% 
  pull(id)

tt_hscw_baseline <- 
  ttm_baseline_transit %>% 
  filter(to_id==as.character(hscw_id)) %>% 
  select(from_id,travel_time_p50) %>% 
  rename(id=from_id,
         tt_baseline=travel_time_p50)

tt_hscw_scenario <- 
  ttm_scenario_transit %>% 
  filter(to_id==as.character(hscw_id)) %>% 
  select(from_id,travel_time_p50) %>% 
  rename(id=from_id,
         tt_scenario=travel_time_p50)

tt_hscw_savings <- 
  tt_hscw_baseline %>% 
  left_join(tt_hscw_scenario,
            by="id") %>% 
  mutate(tt_savings=tt_baseline-tt_scenario) %>% 
  select(id,tt_savings)

city_hex_hscw_ttdiff <- 
  city_hex_sf %>% 
  st_filter(origins_surface) %>% 
  mutate(id=as.character(id)) %>% 
  left_join(tt_hscw_savings,
            by="id") %>% 
  mutate(tt_savings=replace_na(tt_savings,0))

ttdiff_hscw_gg <- 
ggplot()+
  geom_sf(data=city_zone,fill="grey90",colour="white",linewidth=1)+
  geom_sf(data=primary_roadnetwork,
          color="white",linewidth=0.8)+
  geom_sf(data=secondary_roadnetwork,
          color="white",linewidth=0.4)+
  geom_sf(data=city_hex_hscw_ttdiff,
          aes(fill=tt_savings),colour="transparent",alpha=0.8)+
  geom_sf(data=rest_shapes,
          colour="#F7F7F7",linewidth=0.1,alpha=0.8)+
  geom_sf(data=d_shapes,
          colour=d_colour,linewidth=0.2,alpha=0.5)+
  geom_sf(data=f_shapes,
          colour=f_colour,linewidth=0.3,alpha=0.5)+
  geom_sf(data=fx4_shapes,
          colour=fx4_colour,linewidth=0.5)+
  geom_sf(data=fx3_shapes,
          colour=fx3_colour,linewidth=0.5)+
  geom_sf(data=fx2_shapes,
          colour=fx2_colour,linewidth=0.5)+
  geom_sf(data=rt_shapes,
          colour=rt_colour,linewidth=0.8)+
  geom_sf(data=hscw,
          colour="#FFE999",size=2)+
  #scale_fill_gradient2(low="#B2182B",mid="#F7F7F7",high="#2166AC")+
  scale_fill_gradientn(colours=c("#D6604D","#F4A582","#F7F7F7","#4393C3","#2166AC"),
                       rescaler=~ scales::rescale_mid(.x,mid=0))+
  #scale_fill_gradient2(low="#B2182B",mid="#F7F7F7",high="#2166AC")+
  labs(title="Travel time savings to Health Sciences Centre Winnipeg, 8AM Weekday",
       fill="Minutes")+
  theme_void()

ggsave("output/ttdiff_hscw.png",
       ttdiff_hscw_gg,
       height=5,width=6,dpi=200)

#hexagons don't gain or lose access : people do. How many people per hexagon ?----

city_das <- 
  get_census(dataset = 'CA21', 
             regions = list(CSD = city_code),
             level = 'DA',
             geo_format = 'sf')

#in this case, we can benefit from a more granular level of census data : DAs

city_das_pop_sf <- 
city_das %>% 
  select(GeoUID,Population,geometry) %>% 
  rename(pop=Population)

#Loop by CT to assign number of pop to hexagons

#first, create empty object for pop by hex by geouid

hex_pop_frag <- 
  tibble(id=character(),
         GeoUID=character(),
         pop=double())

for(i in 1:nrow(city_das_pop_sf)){
  
  ct_i <- city_das_pop_sf[i,]
  
  geouid_i <- ct_i$GeoUID
  
  n_pop_i <- 
    ct_i$pop
  
  cat("\rProcessing DA",geouid_i,i,"of",nrow(city_das_pop_sf))
  
  #filter for the livable zone of the DA, the origins surface
  
  #we will keep the term employment zone because lazy
  
  employment_zone_i <- 
    st_intersection(
     origins_surface,
      ct_i
    )
  
  #ideally, we would use only residential (easy to add above, just takes time and would
  #add processing time to this whole script. But this is better than nothing !
  
  if(nrow(employment_zone_i)==0){
    
    #IF NO EMPLOYMENT ZONE DETECTED IN THIS CT, use whole CT area to calculate
    #pop by hex
    
    ct_area <- 
      as.numeric(st_area(ct_i$geometry))
    
    city_hex_i <- 
      city_hex %>% 
      st_filter(ct_i)
    
    hex_pop_frag_i <- 
      st_intersection(
        city_hex_i,
        ct_i
      ) %>% 
      mutate(frag_area=as.numeric(st_area(geometry))) %>% 
      mutate(pop=round(n_pop_i*(frag_area/ct_area),1)) %>% 
      as_tibble() %>% 
      select(id,GeoUID,pop)
    
  }else{
    
    #ELSE assign number of pop by hex based on an intersection with employment_zone_i
    
    employment_zone_area <- 
      as.numeric(st_area(employment_zone_i$geometry))
    
    city_hex_i <- 
      city_hex %>% 
      st_filter(employment_zone_i)
    
    hex_pop_frag_i <- 
      st_intersection(
        city_hex_i,
        employment_zone_i
      ) %>% 
      mutate(frag_area=as.numeric(st_area(geometry))) %>% 
      mutate(pop=round(n_pop_i*(frag_area/employment_zone_area),1)) %>% 
      as_tibble() %>% 
      select(id,GeoUID,pop)
    
  }
  
  hex_pop_frag <- 
    rbind(hex_pop_frag,
          hex_pop_frag_i)
  
}

#end_time <- Sys.time()
#end_time-start_time

pop_hex <- 
  hex_pop_frag %>% 
  select(id,pop) %>% 
  group_by(id) %>% 
  summarise(pop=sum(pop))

city_hex_sf_pop <- 
  city_hex %>% 
  left_join(pop_hex,by="id") %>% 
  mutate(pop=replace_na(pop,0))

#city_hex_sf_pop %>% 
#  filter(pop>0) %>% 
#  ggplot()+
#  geom_sf(aes(fill=pop),colour="transparent")+
#  scale_fill_viridis_c()+
#  theme_void()

#city_hex_sf_pop %>% 
#  as_tibble() %>% 
#  summarise(pop=sum(pop))
#total is 745 250

#How many people lost or gained more than X% of jobs within 45 minutes ?-----

city_hex_sf %>% 
  st_filter(origins_surface) %>% 
  mutate(id=as.character(id)) %>% 
  left_join(city_hex_scenario_v_baseline_transit_45min,
            by="id") %>% 
  filter(!is.na(gain_access_45min)) %>% 
  as_tibble() %>% 
  mutate(perc_change=case_when(
    (baseline==0&scenario>0)|(scenario/baseline)>=1.05~"5% rise",
  (scenario/baseline)<=0.95~"5% drop",
  TRUE~"Marginal change"
  )) %>% 
  left_join(pop_hex %>% mutate(id=as.character(id)),
            by="id") %>% 
  group_by(perc_change) %>% 
  summarise(pop=sum(pop,na.rm=TRUE)) %>% 
  mutate(tot=sum(pop)) %>% 
  mutate(perc=round((pop/tot)*100,1))

#the new network allows the average resident to get to more things compared to before, 
#within the same amount of time.

city_hex_sf %>% 
  st_filter(origins_surface) %>% 
  mutate(id=as.character(id)) %>% 
  left_join(city_hex_scenario_v_baseline_transit_45min,
            by="id") %>% 
  filter(!is.na(gain_access_45min)) %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  arrange(gain_access_45min) %>% 
  left_join(pop_hex %>% mutate(id=as.character(id)),
            by="id") %>% 
  filter(!is.na(pop)) %>% 
  mutate(tot_pop=sum(pop)) %>% 
  mutate(popcumsum=cumsum(pop)) %>% 
  filter(popcumsum>=(tot_pop/2))
  
#median increase : 1498 more jobs accessible

#city_cts_jobs_sf %>% 
#  filter(jobs>1400&
#           jobs<1600) %>% mapview()

#it's the equivalent of North St Boniface including Provencher Road
#CT GeoUID 6020117.00

city_hex_sf %>% 
  st_filter(origins_surface) %>% 
  mutate(id=as.character(id)) %>% 
  left_join(city_hex_scenario_v_baseline_transit_45min,
            by="id") %>% 
  filter(!is.na(gain_access_45min)) %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  mutate(perc_change=if_else(
    (baseline==0&scenario>0),100,
    (scenario/baseline)
  )) %>% 
  arrange(perc_change) %>% 
  left_join(pop_hex %>% mutate(id=as.character(id)),
            by="id") %>% 
  filter(!is.na(pop)) %>% 
  mutate(tot_pop=sum(pop)) %>% 
  mutate(popcumsum=cumsum(pop)) %>% 
  filter(popcumsum>=(tot_pop/2))

#median increase : 4% increase on average

#travel time savings : to portage and main

city_hex_sf %>% 
  st_filter(origins_surface) %>% 
  mutate(id=as.character(id)) %>% 
  as_tibble() %>% 
  select(-c(geometry,jobs)) %>% 
  left_join(city_hex_pnm_ttdiff %>% as_tibble() %>% select(id,tt_savings),
            by="id") %>% 
  left_join(pop_hex %>% mutate(id=as.character(id)),
            by="id") %>% 
  filter(!is.na(tt_savings),
         !is.na(pop)) %>% 
  summarise(tt_savings_avg=
              sum(tt_savings*pop)/
              sum(pop))

#to hscw

city_hex_sf %>% 
  st_filter(origins_surface) %>% 
  mutate(id=as.character(id)) %>% 
  as_tibble() %>% 
  select(-c(geometry,jobs)) %>% 
  left_join(city_hex_hscw_ttdiff %>% as_tibble() %>% select(id,tt_savings),
            by="id") %>% 
  left_join(pop_hex %>% mutate(id=as.character(id)),
            by="id") %>% 
  filter(!is.na(tt_savings),
         !is.na(pop)) %>% 
  summarise(tt_savings_avg=
              sum(tt_savings*pop)/
              sum(pop))

#closer to one minute...

#how many stops were removed ?---------------

#stops before

stop_ids_24n <- 
gtfs_24n$stop_times %>% 
  pull(stop_id) %>% unique()

stop_ids_25n <- 
  gtfs_25n$stop_times %>% 
  pull(stop_id) %>% unique()

n_stops_removed <- 
length(stop_ids_24n)-
length(stop_ids_25n)

n_stops_removed/length(stop_ids_24n)
