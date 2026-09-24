## Function to extract abundance/occurrence data
# by W. N. Probst, Thuenen-Institute, September 2026

# hh.dat: Haul data created with the function 'datras.hh'
# spc: Species in latin name, e.g. 'Gadus morhua'

datras.merge.hhhl<-function(hh.dat,spc){
  
  # Define required packages
  require(obus)
  require(mapplots)
  require(crayon)
  
  # Get species abundance by length 
  srvys<-hh.dat$Survey %>% unique %>% sort
  yrs<-hh.dat$Year %>% unique %>% sort
  
  "Retrieve abundance data" %>% crayon::red() %>% cat 
  hl<-obus::dr_con("HL", trim = FALSE) |> 
    # this bug needs to be fixed
    dplyr::mutate(Year = as.integer(Year)) |>  
    dplyr::filter(Survey %in% srvys,
                  Year %in% yrs,
                  latin %in% spc) |> 
    dplyr::collect() |>
    dplyr::glimpse() |> 
    as.data.frame() 
  
  # Merge haul and abundance information
  spc.dat<-base::merge(
    hh.dat[,c(".id","Year","Quarter","Survey","Gear","Region","Country","StatisticalRectangle",
              "HaulDuration","ShootLongitude","ShootLatitude")],
    hl[,c(".id","latin","LengthClass","NumberAtLength")],
    by=".id",
    all.x=T) %>%
    group_by(.id,Year,Quarter,Survey,Gear,Region,Country,StatisticalRectangle,HaulDuration,
             ShootLongitude,ShootLatitude,latin) %>%
    reframe(total.n=sum(NumberAtLength,na.rm=T)) 
  
  # Correct some missing data
  spc.dat$latin<-ifelse(spc.dat$latin %>% is.na,spc,spc.dat$latin)
  spc.dat$ShootLongitude<-ifelse(spc.dat$ShootLongitude %>% is.na,
                                 ices.rect(spc.dat$StatisticalRectangle)[,1],
                                 spc.dat$ShootLongitude)
  spc.dat$ShootLatitude<-ifelse(spc.dat$ShootLatitude %>% is.na,
                                ices.rect(spc.dat$StatisticalRectangle)[,2],
                                spc.dat$ShootLatitude)
  spc.dat$occ<-ifelse(spc.dat$total.n>0,1,0)
  
  # Rename columns
  names(spc.dat)<-c("haul.id","year","quarter","survey","gear","ospar.region","country",
                    "ices.rect","hauldur","lon","lat","species","total.n","occ")
  
  # Final arrangement
  #spc.dat<-spc.dat[,c(1:6,13:14,8:9,7,10:12)]
  spc.dat
}
