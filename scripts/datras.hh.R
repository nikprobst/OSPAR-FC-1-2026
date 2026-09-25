# Function to extract haul-data
# by W. N. Probst, Thuenen-Institute, September 2026

# ospar.region: OSPAR-region in roman number, e.g. 'II" for Greater North Sea
# srvys: Vector of surveys , e.g. 'NS-IBTS' or c('NS-IBTS','BTS','DYFS')
# yrs: selecting years, e.g. 1985:2025

datras.hh<-function(ospar.region,srvys,yrs){
  
  # Load functions
  require(obus)
  require(mapplots)
  require(crayon)
  
  # Load EEZ & OSPAR data
  eezs.15<-read_sf("../spatial data/eezs.ospar.regions.shp")
  ospar.regs<-read_sf("../spatial data/ospar_regions_simplified.shp")
  
  # Get haul information, e.g. lon, lat, tow duration, etc. from ICES DATRAS#
  # via Duck DB (very fast and by species)
  "Retrieve haul data" %>% crayon::red() %>% cat 
  hh<-obus::dr_con("HH") |> 
    dplyr::mutate(Year = as.integer(Year)) |> 
    dplyr::filter(Survey %in% srvys,
                  Year %in% yrs) |>
    dplyr::collect() |>
    dplyr::glimpse() |> 
    as.data.frame()
  
  # Convert data into spatial data frame
  na.pos.idx<-hh[,c("ShootLongitude","ShootLatitude")] %>% is.na %>% which
  if(length(na.pos.idx)==0) 
    hh.sf<-hh %>% st_as_sf(coords=c("ShootLongitude","ShootLatitude"),crs=4326) else 
    hh.sf<-hh[-na.pos.idx,] %>% st_as_sf(coords=c("ShootLongitude","ShootLatitude"),crs=4326)
  
  hh.sf$ShootLongitude<-st_coordinates(hh.sf)[,1]
  hh.sf$ShootLatitude<-st_coordinates(hh.sf)[,2]
  
  # Complement entries without lon/lat data by mid-position of ICES rectangle
  hh.sf$ShootLongitude<-ifelse(hh.sf$ShootLongitude %>% is.na,
                                ices.rect(hh.sf$StatisticalRectangle)[,1],
                                hh.sf$ShootLongitude)
  hh.sf$ShootLatitude<-ifelse(hh.sf$ShootLatitude %>% is.na,
                               ices.rect(hh.sf$StatisticalRectangle)[,2],
                               hh.sf$ShootLatitude)  
  
  # Crop by region and country
  "Intersect with spatial geometry" %>% crayon::yellow() %>% cat 
  sf.intersect.geom<-subset(eezs.15,Region %in% ospar.region) 
  
  hh.sf.i<-st_intersection(hh.sf,sf.intersect.geom)
  
  # Correct and give out final table
  hh.sf.i<-hh.sf.i[,c(".id","Year","Quarter","Survey","Gear","Region","Country.1","StatisticalRectangle",
                      "HaulDuration","ShootLongitude","ShootLatitude")]
  names(hh.sf.i)[c(7,10,11)]<-c("Country","lon","lat")
  hh.sf.i
  
}
