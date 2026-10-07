# Script to explore ICES DATRAS data
# using OBUS

# Load functions
require(obus)
require(mapplots)
require(crayon)
require(magrittr)
require(sf)
require(tidyverse)
require(xlsx)

# ***** ----
# Explore DATRAS data ----
## Spatial coverage of surveys ----
"Retrieve haul data" %>% crayon::red() %>% cat 
hh.all<-obus::dr_con("HH") |> 
  dplyr::mutate(Year = as.integer(Year)) |> 
  dplyr::collect() |>
  dplyr::glimpse() |> 
  as.data.frame()

# Complement entries without lon/lat data by mid-position of ICES rectangle
hh.all$ShootLongitude<-ifelse(hh.all$ShootLongitude %>% is.na,
                              ices.rect(hh.all$StatisticalRectangle)[,1],
                              hh.all$ShootLongitude)
hh.all$ShootLatitude<-ifelse(hh.all$ShootLatitude %>% is.na,
                             ices.rect(hh.all$StatisticalRectangle)[,2],
                             hh.all$ShootLatitude)

# Remove hauls without lon/lat data
hh.all.idx<-hh.all[,c("ShootLongitude","ShootLatitude")] %>% is.na %>% which
hh.all.sf<-hh.all[-hh.all.idx,] %>% st_as_sf(coords=c("ShootLongitude","ShootLatitude"),crs=4326)
hh.all.sf$lon<-st_coordinates(hh.all.sf)[,1]
hh.all.sf$lat<-st_coordinates(hh.all.sf)[,2]

# Intersect haul data with OSPAR-regions
hh.all.i<-st_intersection(hh.all.sf,subset(ospar.regs,Region=="I"))
hh.all.ii<-st_intersection(hh.all.sf,subset(ospar.regs,Region=="II"))
hh.all.iii<-st_intersection(hh.all.sf,subset(ospar.regs,Region=="III"))
hh.all.iv<-st_intersection(hh.all.sf,subset(ospar.regs,Region=="IV"))
hh.all.v<-st_intersection(hh.all.sf,subset(ospar.regs,Region=="V"))

# Remove full haul data
rm(hh.all,hh.all.sf)

# Explore number of hauls and spatial extent
ovrvw.hh.iv<-ovrvw.hh(hh.all.iv)

ovrvw.hh.iv$n.hauls %>% 
  as.data.frame %>%
  #data.table::melt(id.vars="Year") %>% 
  ggplot(aes(x=Var1,y=Freq,fill=Var2))+
  geom_col(show.legend=F)+
  facet_wrap(.~Var2,scales="free_y")+
  scale_fill_discrete(palette=pals::tol.rainbow)

x11(15,15)
ovrvw.hh.iv$spatial.overview

# Plot by region
ggplot()+geom_sf(data=hh.all.i,aes(col=Survey))
ggplot()+geom_sf(data=hh.all.ii,aes(col=Survey))
ggplot()+geom_sf(data=hh.all.iii,aes(col=Survey))
ggplot()+geom_sf(data=hh.all.iv,aes(col=Survey))
ggplot()+geom_sf(data=hh.all.v,aes(col=Survey))+
  annotation_map(map_data("world"))+
  coord_quickmap()

## Survey list per region ----
srvys.i<-hh.all.i$Survey %>% unique
srvys.ii<-hh.all.ii$Survey %>% unique
srvys.iii<-hh.all.iii$Survey %>% unique
srvys.iv<-hh.all.iv$Survey %>% unique
srvys.v<-hh.all.v$Survey %>% unique

## Explore year range ----
yrs.i<-hh.all.i$Year %>% unique %>% sort
yrs.ii<-hh.all.ii$Year %>% unique %>% sort
yrs.iii<-hh.all.iii$Year %>% unique %>% sort
yrs.iv<-hh.all.iv$Year %>% unique %>% sort
yrs.v<-hh.all.v$Year %>% unique %>% sort

# Species per region ----
## Species per region ----
"\nRetrieve species data" %>% crayon::red() %>% cat 
hl.all<-obus::dr_con("HL",trim=FALSE) |> 
  dplyr::mutate(Year = as.integer(Year)) |> 
  #dplyr::filter(Survey %in% srvys,
  #              Year %in% yrs) |>
  dplyr::collect() |>
  dplyr::glimpse() |> 
  as.data.frame()

hl.all.i<-subset(hl.all,.id %in% hh.all.i$.id)
hl.all.ii<-subset(hl.all,.id %in% hh.all.ii$.id)
hl.all.iii<-subset(hl.all,.id %in% hh.all.iii$.id)
hl.all.iv<-subset(hl.all,.id %in% hh.all.iv$.id)
hl.all.v<-subset(hl.all,.id %in% hh.all.v$.id)

#spcs.i<-(hl.all.i$latin %>% unique)[hl.all.i$latin %>% unique %>% strsplit(" ") %>% lapply(function(x) length(x)) %>% is_greater_than(1) %>% which]
#spcs.ii<-(hl.all.ii$latin %>% unique)[hl.all.ii$latin %>% unique %>% strsplit(" ") %>% lapply(function(x) length(x)) %>% is_greater_than(1) %>% which]
#spcs.iii<(hl.all.iii$latin %>% unique)[hl.all.iii$latin %>% unique %>% strsplit(" ") %>% lapply(function(x) length(x)) %>% is_greater_than(1) %>% which]
#spcs.iv<-(hl.all.iv$latin %>% unique)[hl.all.iv$latin %>% unique %>% strsplit(" ") %>% lapply(function(x) length(x)) %>% is_greater_than(1) %>% which]
#spcs.v<-(hl.all.v$latin %>% unique)[hl.all.v$latin %>% unique %>% strsplit(" ") %>% lapply(function(x) length(x)) %>% is_greater_than(1) %>% which]

# Extract & sort species lists by region
hl.dats<-c("hl.all.i","hl.all.ii","hl.all.iii","hl.all.iv","hl.all.v")
ors<-c("i","ii","iii","iv","v")
spcs.dats<-c("spcs.i","spcs.ii","spcs.iii","spcs.iv","spcs.v")

# Loop through regions
pb<-txtProgressBar(min=1,max=length(hl.dats),style=3)
for (i in 0:length(hl.dats)){
  
  # Get species data for region
  hl.dat<-get(hl.dats[i])
  
  # Get species, classes and orders
  spcs.dat<-data.table::rbindlist(wm_record_((hl.dat$ValidAphiaID %>% unique %>% sort)))[,c("scientificname","order","class")] %>% 
    data.frame
  
  # Get frequencies within hauls and across hauls
  hl.freq<-hl.dat %>% group_by(latin,.id) %>% reframe(freq=length(latin)) 
  spcs.freq<-hl.freq %>% group_by(latin) %>% reframe(freq=length(latin))
  
  # Subset sharks, rays, teleosts & elasmos and merge with freuwqncies
  spcs.dat<-spcs.dat[strsplit(spcs.dat$scientificname," ") %>% lapply(length) %>% is_greater_than(1) %>% which,] %>%
    subset(class %in% c("Teleostei","Elasmobranchii","Cephalopoda","Myxini","Petromyzonti")) %>% 
    merge(spcs.freq,by.x="scientificname",by.y="latin")
  
  # Add OSPAR region
  spcs.dat$opsar.region<-ors[i]
  
  # Sort by species name
  spcs.dat<-spcs.dat[spcs.dat$scientificname %>% order,]
  
  # Save data
  write.csv(spcs.dat,
            paste0("D:/MSRL/OSPAR/2026 FC1/OSPAR_FC_1_2026_GitHub/lists/species_by_region/spcs.",ors[i],".csv"),
            row.names=F)
  
  setTxtProgressBar(pb,i)
}

# Save lists ----
# survey lists
write.csv(srvys.i,"./lists/surveys_by_region/srvys.i",row.names=F)
write.csv(srvys.ii,"./lists/surveys_by_region/srvys.ii",row.names=F)
write.csv(srvys.iii,"./lists/surveys_by_region/srvys.iii",row.names=F)
write.csv(srvys.iv,"./lists/surveys_by_region/srvys.iv",row.names=F)
write.csv(srvys.v,"./lists/surveys_by_region/srvys.v",row.names=F)

# Years
write.csv(yrs.i,"./lists/years_by_region/yrs.i",row.names=F)
write.csv(yrs.ii,"./lists/years_by_region/yrs.ii",row.names=F)
write.csv(yrs.iii,"./lists/years_by_region/yrs.iii",row.names=F)
write.csv(yrs.iv,"./lists/years_by_region/yrs.iv",row.names=F)
write.csv(yrs.v,"./lists/years_by_region/yrs.v",row.names=F)

# Species lists
write.csv(spcs.i,"./lists/species_by_region/spcs.i.csv",row.names=F)
write.csv(spcs.ii,"./lists/species_by_region/spcs.ii.csv",row.names=F)
write.csv(spcs.iii,"./lists/species_by_region/spcs.iii.csv",row.names=F)
write.csv(spcs.iv,"./lists/species_by_region/spcs.iv.csv",row.names=F)
write.csv(spcs.v,"./lists/species_by_region/spcs.v.csv",row.names=F)

# Check surveys-request by Isla - 07.10.2026  -----
hh.dats<-c("hh.all.i","hh.all.ii","hh.all.iii","hh.all.iv","hh.all.v")

for (i in 1:length(hh.dats)){
  srv.inf<-get(hh.dats[i]) %>%
           group_by(Survey,Quarter) %>%
           reframe(start.year=min(Year),
                   end.year=max(Year),
                   main.gear=Gear %>% table %>% which.max %>% names,
                   n.gear.types=Gear %>% unique %>% length,
                   min.trawl.dur=min(HaulDuration,na.rm=T),
                   max.trawl.dur=max(HaulDuration,na.rm=T),
                   n.hauls.total=length(.id),
                   mean.hauls.per.year=(length(.id)/(max(Year)-min(Year)+1)) %>% round(1)) %>%
    as.data.frame()
  
  if(i==1) srvs.inf<-srv.inf else srvs.inf<-rbind(srvs.inf,srv.inf)
}

write.xlsx(srvs.inf,"../OBUS_survey_overview.xlsx",row.names=F)



