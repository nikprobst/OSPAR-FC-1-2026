# Function to analyse aggregated survey data
# by W. N. Probst, Thuenen-Institute, September 2026

# spc.dat: Aggregated survey data by species obtained from 'datras.hh' & 'datras.merge.hhhl'
ovrvw.spc<-function(spc.dat) {
  
  # Overview on number og hauls per year & survey
  srv.tbl<-table(spc.dat$year,spc.dat$survey) 
  
  # Overview on freuency of occurences by year & survey
  frq.ovw.tbl<-spc.dat %>% group_by(year,survey) %>% reframe(occ.freq=sum(occ)/length(occ)) %>% as.data.frame
  frq.ovw.plt<- frq.ovw.tbl %>% 
    ggplot(aes(x=year,y=occ.freq,col=survey))+
    geom_line(show.legend=F)+
    scale_colour_discrete(palette=pals::tol.rainbow(spc.dat$survey %>% unique %>% length),
                          name="Survey")+
    facet_wrap(.~survey)+
    labs(x="Year",y="Freq. of occurrence [% of hauls]")
  
  # Overview on spatial coverage by year and survey
  spat.ovw.plt<-ggplot(data=spc.dat,aes(x=lon,y=lat,col=survey,pch=as.factor(occ),size=as.factor(occ)))+
    scale_shape_manual(values=c(3,15),name="Occurrence")+
    scale_size_manual(name="Occurrence",values=c(1,3))+
    geom_point(show.legend=T)+
    facet_wrap(.~year,ncol=srv.tbl %>% nrow %>% sqrt %>% multiply_by(1.6) %>% floor)+
    scale_colour_discrete(palette=pals::tol.rainbow(spc.dat$survey %>% unique %>% length) %>% alpha(0.4),
                          name="Survey")+
    annotation_map(map_data("world"))+
    coord_quickmap()+
    guides(color=guide_legend(override.aes=list(size=4)))+
    labs(x="Longitude [°]",y="Latitude [°]")+
    scale_x_continuous(n.breaks=3)+
    scale_y_continuous(n.breaks=3)
  
  ovws<-list(srv.tbl,frq.ovw.plt,spat.ovw.plt)
  names(ovws)<-c("n.hauls","occ.freq.plot","spatial.overview")
  
  # Give out results
  ovws
}