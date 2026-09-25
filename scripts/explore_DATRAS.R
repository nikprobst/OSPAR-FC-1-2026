# Script to explore ICES DATRAS data
# using 
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
"Retrieve species data" %>% crayon::red() %>% cat 
hl.all<-obus::dr_con("HL",trim=FALSE) |> 
  dplyr::mutate(Year = as.integer(Year)) |> 
  dplyr::collect() |>
  dplyr::glimpse() |> 
  as.data.frame()

hl.all.i<-subset(hl.all,.id %in% hh.all.i$.id)
hl.all.ii<-subset(hl.all,.id %in% hh.all.ii$.id)
hl.all.iii<-subset(hl.all,.id %in% hh.all.iii$.id)
hl.all.iv<-subset(hl.all,.id %in% hh.all.iv$.id)
hl.all.v<-subset(hl.all,.id %in% hh.all.v$.id)

# Extract & sort species lists by region
spcs.i<-data.table::rbindlist(wm_record_((hl.all.i$ValidAphiaID %>% unique %>% sort)))[,c("scientificname","order","class")] %>% 
  data.frame
spcs.i<-spcs.i[strsplit(spcs.i$scientificname," ") %>% lapply(length) %>% is_greater_than(1) %>% which,] %>%
  subset(class %in% c("Teleostei","Elasmobranchii","Cephalopoda"))
spcs.i<-spcs.i[spcs.i$scientificname %>% order,]

spcs.ii<-data.table::rbindlist(wm_record_((hl.all.ii$ValidAphiaID %>% unique %>% sort)))[,c("scientificname","order","class")] %>% 
  data.frame
spcs.ii<-spcs.ii[strsplit(spcs.ii$scientificname," ") %>% lapply(length) %>% is_greater_than(1) %>% which,] %>%
  subset(class %in% c("Teleostei","Elasmobranchii","Cephalopoda"))
spcs.ii<-spcs.ii[spcs.ii$scientificname %>% order,]

spcs.iii<-data.table::rbindlist(wm_record_((hl.all.iii$ValidAphiaID %>% unique %>% sort)))[,c("scientificname","order","class")] %>% 
  data.frame
spcs.iii<-spcs.iii[strsplit(spcs.iii$scientificname," ") %>% lapply(length) %>% is_greater_than(1) %>% which,] %>%
  subset(class %in% c("Teleostei","Elasmobranchii","Cephalopoda"))
spcs.iii<-spcs.iii[spcs.iii$scientificname %>% order,]

spcs.iv<-data.table::rbindlist(wm_record_((hl.all.iv$ValidAphiaID %>% unique %>% sort)))[,c("scientificname","order","class")] %>% 
  data.frame
spcs.iv<-spcs.iv[strsplit(spcs.iv$scientificname," ") %>% lapply(length) %>% is_greater_than(1) %>% which,] %>%
  subset(class %in% c("Teleostei","Elasmobranchii","Cephalopoda"))
spcs.iv<-spcs.iv[spcs.iv$scientificname %>% order,]

spcs.v<-data.table::rbindlist(wm_record_((hl.all.v$ValidAphiaID %>% unique %>% sort)))[,c("scientificname","order","class")] %>% 
  data.frame
spcs.v<-spcs.v[strsplit(spcs.v$scientificname," ") %>% lapply(length) %>% is_greater_than(1) %>% which,] %>%
  subset(class %in% c("Teleostei","Elasmobranchii","Cephalopoda"))
spcs.v<-spcs.v[spcs.v$scientificname %>% order,]

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