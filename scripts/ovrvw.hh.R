# Function to analyse aggregated survey data
# by W. N. Probst, Thuenen-Institute, September 2026

# hh.dat: Aggregated survey data obtained from 'datras.hh'. Important!! Should contain columns named 'lon' & 'lat'
ovrvw.hh<-function(hh.dat) {
  
  # Load required packages
  require(pals)
  
  # Overview on number og hauls per year & survey
  srv.tbl<-table(hh.dat$Year,hh.dat$Survey) 

  # Overview on spatial coverage by year and survey
  spat.ovw.plt<-ggplot(data=hh.dat,aes(x=lon,y=lat,col=Survey))+
    geom_point(show.legend=T,size=0.5)+
    facet_wrap(.~Year)+
    scale_colour_discrete(palette=pals::tol.rainbow(hh.dat$Survey %>% unique %>% length) %>% alpha(0.4),
                          name="Survey")+
    annotation_map(map_data("world"))+
    coord_quickmap()+
    guides(color = guide_legend(override.aes=list(size=4)))
  
  ovws<-list(srv.tbl,spat.ovw.plt)
  names(ovws)<-c("n.hauls","spatial.overview")
  
  # Give out results
  ovws
}
