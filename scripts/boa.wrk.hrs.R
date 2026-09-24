## Function for binomial occurrence assessment ----
# by W. N. Probst, Thuenen-Institute, September 2026
# This function should not be necessary to use directly, just a sub function  for 'boa.asmnt'
# wrk.hrs here means 'work horse'

# boa.dat: Survey data with columns 'ospar.region',country','year','quarter','survey','gear','quarter','species'
#          Can be obtained from 'datras.hh' & 'datras.merge.hhhl'  
# rp: years of reference period, numeric e.g. 2000:2015
# ap: years of assessment period, numeric e.g. 2022:2027

boa.wrk.hrs<-function(boa.dat,rp,ap){
  
  # Load required packages
  require(magrittr)
  require(ggplot2)
  require(tidyverse)

  # Mute messages during function execution
  options(dplyr.summarise.inform = FALSE)
  
  # Define countries to be included 
  if((boa.dat$country %>% unique %>% length)>1) cntry<-"multiple" else cntry<-boa.dat$country[1] 
  
  if(nrow(boa.dat)>0) {
    
    spc.dat<-boa.dat
    spc.dat$code<-paste(spc.dat$survey,spc.dat$gear,spc.dat$quarter)
    
    # Get data for reference & assessment periods 
    spc.rp<-spc.dat %>% subset(year %in% rp)
    spc.ap<-spc.dat %>% subset(year %in% ap)
    
    # Summarize occurrences for reference period
    if(nrow(spc.rp)==0) {
      
      od.rp<-data.frame(survey="None",gear="None",quarter="none",ospar.region=boa.dat$ospar.region[1],
                        country=cntry,species=boa.dat$species[1],sum.occ=NA,n.hls=NA,prob=NA,code=NA,
                        ref.period=paste0(rp[1],"-",tail(rp,1)))
      od.ap<-data.frame(survey="None",gear="None",quarter="none",ospar.region=boa.dat$ospar.region[1],
                        country=cntry,species=boa.dat$species[1],sum.occ=NA,n.hls=NA,prob=NA,code=NA,
                        asmnt.period=paste0(ap[1],"-",tail(ap,1)),thr.dec=NA,thr.inc=NA,
                        res=NA,qf="Assessment not possible",res.final="Unknown")
      prb.tbl<-data.frame(ospar.region=boa.dat$ospar.region[1],
                          country=cntry,species=boa.dat$species[1],
                          ref.per=paste0(rp[1],"-",tail(rp,1)),asmnt.per=paste0(ap[1],"-",tail(ap,1)),
                          res.final="Unknown",mean.prob=NA,hls.prob=NA,prob.score=NA,res.fl=1)
      bi<-data.frame(ospar.region=boa.dat$ospar.region[1],
                     country=cntry,species=boa.dat$species[1],
                     ref.per=paste0(rp[1],"-",tail(rp,1)),asmnt.per=paste0(ap[1],"-",tail(ap,1)),
                     decs=NA,stbs=NA,recs=NA,ukws=NA,thr=NA,binomial.intgr="Unknown")
      ts.plt<-NA
      ts.dat<-data.frame(year=c(rp,ap),code=NA,n.hls=NA,sum.occ=NA,rel.occ=NA,rel.occ.w=NA,period=NA)
      
    } else {
      
      od.rp<-spc.rp %>% 
        group_by(survey,gear,quarter) %>% 
        reframe(ospar.region=boa.dat$ospar.region[1],
                country=cntry,
                species=boa.dat$species[1],
                sum.occ=sum(occ),
                n.hls=length(survey)) %>% 
        mutate(prob=round(sum.occ/n.hls,5)) %>% as.data.frame
      
      od.rp$code<-paste(od.rp$survey,od.rp$gear,od.rp$quarter)
      od.rp$ref.period<-paste0(rp[1],"-",tail(rp,1))
      
      # Summarize occurrences for assessment period
      od.ap<-spc.ap %>% 
        group_by(survey,gear,quarter) %>% 
        summarise(ospar.region=boa.dat$ospar.region[1],
                  country=cntry,
                  species=boa.dat$species[1],
                  sum.occ=sum(occ),
                  n.hls=length(survey)) %>% 
        mutate(prob=round(sum.occ/n.hls,5)) %>% as.data.frame
      
      od.ap$code<-paste(od.ap$survey,od.ap$gear,od.ap$quarter)
      
      # Select only surveys,gear, quarter combinations that exist in both periods 
      od.rp<-od.rp[which(od.rp$code %in% od.ap$code),]
      od.ap<-od.ap[which(od.ap$code %in% od.rp$code),]
      
      # Calculate thresholds ----
      od.ap$asmnt.period<-paste0(ap[1],"-",tail(ap,1))
      od.ap$thr.dec<-qbinom(c(0.05),od.ap$n.hls,od.rp$prob)
      od.ap$thr.inc<-qbinom(c(0.95),od.ap$n.hls,od.rp$prob)
      
      od.ap$res<-ifelse(od.ap$sum.occ >= od.ap$thr.dec & od.ap$sum.occ <= od.ap$thr.inc,"Stable",
                        ifelse(od.ap$sum.occ > od.ap$thr.inc,"Recovering","Declining"))
      
      # Add quality flags
      od.ap$qf<-ifelse(od.ap$thr.dec==0 & od.ap$thr.inc > 0,"Decline not detectable",
                       ifelse(od.ap$thr.dec==0,"No assessment possible","Full assessment possible"))
      
      # Add final result under consideration of quality flag
      od.ap$res.final<-ifelse(od.ap$qf=="Full assessment possible",od.ap$res,
                              ifelse(od.ap$qf=="Decline not detectable" & od.ap$res=="Recovering",od.ap$res,"Unknown"))
      
      # Integration of single surveys ----
      ## Probability table for prob-score ----
      tot.n.hls<-od.ap  %>% summarize(sum(n.hls)) %>% as.numeric
      tot.prob<-od.ap %>% summarize(sum(prob)) %>% as.numeric
      
      # Prob-score table
      prb.tbl<-od.ap %>% 
        group_by(res.final) %>% 
        summarize(mean.prob=mean(prob) %>% round(5),
                  hls.prob=((sum(n.hls)/tot.n.hls) %>% round(5))) %>%
        mutate(prob.score=(mean.prob*hls.prob) %>% round(5)) %>%
        as.data.frame
      
      # Add more info to prob-score table
      prb.tbl$ospar.region<-boa.dat$ospar.region[1]
      prb.tbl$country<-cntry
      prb.tbl$species<-boa.dat$species[1]
      prb.tbl<-prb.tbl[,c(5:7,1:4)]
      
      # Add results flag to prob-score table
      prb.tbl$res.fl<-0
      prb.tbl$res.fl[which.max(prb.tbl$prob.score)]<-1
      prb.tbl$ref.per<-paste0(rp[1],"-",tail(rp,1))
      prb.tbl$asmnt.per<-paste0(ap[1],"-",tail(ap,1))
      
      prb.tbl<-prb.tbl[,c("ospar.region","country","species","ref.per","asmnt.per",
                          "res.final","mean.prob","hls.prob","prob.score","res.fl")]
      
      ## Binomial integration ----
      incs<-which(od.ap$res.final=="Recovering") %>% length()
      decs<-which(od.ap$res.final=="Declining") %>% length()
      stbs<-which(od.ap$res.final=="Stable") %>% length()
      ukws<-which(od.ap$res.final=="Unknown") %>% length()
      tots<-sum(incs,stbs,decs)
      
      thr.rpl<-qbinom(0.95,tots,0.05)+1
      #thr.rpl<-which(1-pbinom(-1:tot.n.rpl,tot.n.rpl,0.04999)<0.05) %>% min()-1
      
      bi.ospar.inc<-ifelse(incs>=thr.rpl&thr.rpl>0,"Recovering","Not.recovering")
      bi.ospar.dec<-ifelse(decs>=thr.rpl&thr.rpl>0,"Declining","Not.declining")
      bi.ospar.stb<-ifelse(stbs>0,"Stable",ifelse(ukws>0,"Unknown",NA))
      
      bi.ospar<-ifelse(bi.ospar.inc=="Recovering" & bi.ospar.dec=="Declining","Mixed",
                       ifelse(bi.ospar.inc=="Recovering" & bi.ospar.dec=="Not.declining","Recovering",
                              ifelse(bi.ospar.inc=="Not.recovering" & bi.ospar.dec=="Declining","Declining",
                                     ifelse(bi.ospar.inc=="Not.recovering" & bi.ospar.dec=="Not.declining" & ((decs>0)|(incs>0)|(bi.ospar.stb=="Stable")),"Stable",
                                            ifelse(bi.ospar.inc=="Not.recovering" & bi.ospar.dec=="Not.declining" & bi.ospar.stb=="Unknown","Unknown",NA)))))      
      
      bi<-data.frame(ospar.region=boa.dat$ospar.region[1],
                     country=cntry,
                     species=boa.dat$species[1],
                     ref.per=paste0(rp[1],"-",tail(rp,1)),
                     asmnt.per=paste0(ap[1],"-",tail(ap,1)),
                     decs=decs,
                     stbs=stbs,
                     recs=incs,
                     ukws=ukws,
                     thr=thr.rpl,
                     binomial.intgr=bi.ospar)
      
      # Time series plots occurrence plots ----
      ts.dat<-rbind(spc.rp,spc.ap) %>% 
        group_by(year,code) %>% 
        reframe(n.hls=length(haul.id),
                sum.occ=sum(occ)) %>% 
        mutate(rel.occ=(sum.occ/n.hls) %>% round(5)) %>%
        mutate(rel.occ.w=(rel.occ*n.hls/sum(n.hls)) %>% round(5))
      ts.dat$period<-ifelse(ts.dat$year %in% rp,"RP",ifelse(ts.dat$year %in% ap,"AP",NA))
      
      # Select only surveys which are reprented in rp & ap
      ts.dat<-subset(ts.dat,code %in% od.ap$code)
      ts.dat<-ts.dat %>% as.data.frame
      
      ts.plt<-ggplot()+
        geom_line(data=ts.dat %>% subset(period=="RP"),aes(x=year,y=rel.occ,col=code,group=code),show.legend=F)+
        geom_point(data=ts.dat %>% subset(period=="RP"),aes(x=year,y=rel.occ,col=code,group=code),show.legend=F)+
        geom_line(data=ts.dat %>% subset(period=="AP"),aes(x=year,y=rel.occ,col=code,group=code),show.legend=F)+
        geom_point(data=ts.dat %>% subset(period=="AP"),aes(x=year,y=rel.occ,col=code,group=code),show.legend=F)+
        geom_vline(xintercept=ap[1],col="grey20",lty=2)+
        facet_wrap(.~code)+
        labs(x="Year",y="Relative occurrence",
             title=paste(boa.dat$species[1],"|",
                         "Country:",cntry,"| PS Res:",
                         prb.tbl$res.final[which(prb.tbl$res.fl==1)],"| BI Res:",bi$binomial.intgr))+
        scale_colour_discrete(palette=pals::tol.rainbow(nrow(od.ap)+1),name="Group")+
        theme_grey()
    }
  } else { # Option if no catches were made
    od.rp<-data.frame(survey="None",gear="None",quarter="None",ospar.region=ospar.reg,
                      country=cntry,species=spc,sum.occ=NA,
                      n.hls=NA,prob=NA,code="None")
    od.ap<-data.frame(survey="None",gear="None",quarter="None",ospar.region=ospar.reg,
                      country=cntry,species=spc,sum.occ="None",
                      n.hls=NA,prob=NA,code="None",thr.dec=NA,thr.inc=NA,
                      res="None",qf="None",res.final="None")
    
    # Prob-score table
    prb.tbl<-data.frame(ospar.region=ospar.region,country=cntry,species=spc,
                        res.final="None",mean.prob=0,hls.prob=0,prob.score=0)
    prb.tbl$res.fl<-0
    prb.tbl$res.fl[which.max(prb.tbl$prob.score)]<-1
    prb.tbl$ref.per<-paste0(rp[1],"-",tail(rp,1))
    prb.tbl$asmnt.per<-paste0(ap[1],"-",tail(ap,1))
    
    prb.tbl<-prb.tbl[,c("ospar.region","country","species","ref.per","asmnt.per",
                        "res.final","mean.prob","hls.prob","prob.score","res.fl")]
    
    # Binomial integration
    bi<-data.frame(ospar.region=boa.dat$ospar.region[1],
                   country=cntry,
                   species=boa.dat$species[1],
                   ref.per=paste0(rp[1],"-",tail(rp,1)),
                   asmnt.per=paste0(ap[1],"-",tail(ap,1)),
                   decs="None",
                   stbs="None",
                   recs="None",
                   ukws="None",
                   thr="None",
                   binomial.intgr="Not applicable")
    
    ts.plt<-"Not applicable"
    ts.dat<-data.frame(year=c(rp,ap),code=NA,n.hls=NA,sum.occ=NA,rel.occ=NA,rel.occ.w=NA,period=NA)
  }
  
  # Compile results
  rslts<-list(od.rp,
              od.ap,
              prb.tbl,
              bi,
              ts.dat,
              ts.plt)
  
  names(rslts)<-c("occ.data.rp","occ.data.ap","integrated.prob.score","binomial.integration","time.series","time.series.plot")
  rslts
}


