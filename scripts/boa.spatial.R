## Function for spatial BOA result plot
# by W. N. Probst, Thuenen-Institute, September 2026

# boa.asmt: object created by 'boa.asmnt'-function
# appr: 'bi' for binomial survey integration, 'ps' for probability score and 'both' for both 

boa.spatial<-function(boa.asmt,appr){
  
  # Load required packages
  require(ggplot2)
  require(magrittr)
  
  # Load EEZ & OSPAR data
  eezs.15<-read_sf("./spatial data/eezs.ospar.regions.shp")
  ospar.regs<-read_sf("./spatial data/ospar_regions_simplified.shp")

  # Subset EEZs or entire region
  if(boa.asmt$binomial.integration$country[1]=="all countries"){
    reg.dat<-subset(ospar.regs,Region==boa.asmt$binomial.integration$ospar.reg)
    reg.dat$Country<-"all countries"
  } else 
    reg.dat<-subset(eezs.15,
                    Region==boa.asmt$binomial.integration$ospar.reg[1] & 
                    Country %in% boa.asmt$integrated.prob.score$country)

  # Select approach   
  if(appr=="both") {
    # Double shape for both assessment approaches
    reg.dat<-rbind(reg.dat,reg.dat)
    
    bi.res<-boa.asmt$binomial.integration[,c("country","binomial.intgr")]
    names(bi.res)<-c("country","res.final")
    bi.res$method<-"BI"
    ps.res<-subset(boa.asmt$integrated.prob.score,res.fl==1,select=c("country","res.final"))
    ps.res$method<-"PS"
    asmt.res<-rbind(bi.res,ps.res)
  } else
 
  if(appr=="bi"){
    asmt.res<-boa.asmt$binomial.integration[,c("country","binomial.intgr")]
    names(asmt.res)<-c("country","res.final")
    asmt.res$method<-"BI"
  } else
    
  if(appr=="ps"){
    asmt.res<-subset(boa.asmt$integrated.prob.score,res.fl==1,select=c("country","res.final"))
    asmt.res$method<-"PS"
  }
    
  # Merge shapefiles and assessment results
  reg.dat<-merge(reg.dat,asmt.res,by.x="Country",by.y="country") 
    
  # Plotting 
  stat.plot<-ggplot()+
              geom_sf(data=reg.dat,
                      aes(fill=factor(res.final,
                                      levels=c("Recovering","Stable","Mixed",
                                               "Declining","Unknown"))),
                      col=1,
                      show.legend=T)+
               scale_fill_manual(name="Result prob. score",
                        values=c("olivedrab3","orange","yellow","red4","grey"),
                        drop=F)+
      facet_wrap(.~method)+
      labs(title=paste("Species:",boa.asmt$integrated.prob.score$species,"|",
                       "Region:",boa.asmt$integrated.prob.score$ospar.reg,"|"),
           subtitle=paste("Spatial resolution:",
                          ifelse(boa.asmt$integrated.prob.score$country=="all countries","Regional |","National |"),
                          "RP:",boa.asmt$binomial.integration$ref.per[1],"|",
                          "AP:",boa.asmt$binomial.integration$asmnt.per[1]))+
      theme_gray()  
    
    stat.plot
}
    