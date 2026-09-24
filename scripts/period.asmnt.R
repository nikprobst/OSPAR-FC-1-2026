## Function for BOA across multiple time periods ----
# by W. N. Probst, Thuenen-Institute, September 2026

# boa.dat: survey data from DATRAS as prepared by 'datras.hh' & 'datras.merge.hhhl'-function
# rp: years of reference period, numeric e.g. 2000:2015
# asp: assessment periods as list e.g. list(2016:2021,2022:2025)

period.asmnt<-function(boa.dat,rp,asp) {
  
  # Load required packages
  require(ggplot2)
  require(reshape2)
  require(magrittr)
  
  # Identify relevant countries
  cntry<-boa.dat$country %>% unique
  pb<-txtProgressBar(min=0,max=length(cntry),style=3)
  
  # Loop through countries
  for (i in 1:length(cntry)){
    aspr<-list()
    asr.ps<-vector()
    asr.bi<-vector()
    
    # Loop through assessment periods
    for (j in 1:length(asp)){
      aspr[[j]] <- boa.wrk.hrs(boa.dat %>% subset(country==cntry[i]),rp,asp[[j]])
      asr.ps[j]<-aspr[[j]]$integrated.prob.score$res.final[which(aspr[[j]]$integrated.prob.score$res.fl==1)]
      asr.bi[j]<-aspr[[j]]$binomial.integration$binomial.intgr
    } 
    
    # Combine national assessment results by period
    ps.res<-do.call(rbind,lapply(aspr,function(x) rbind(x$integrated.prob.score)))
    bi.res<-do.call(rbind,lapply(aspr,function(x) rbind(x$binomial.integration)))
    
    asr.cntry<-data.frame(
      species=boa.dat$species[1],
      region=boa.dat$ospar.region[1],
      country=cntry[i],
      ref.period=bi.res$ref.per,
      asmnt.period=bi.res$asmnt.per,
      asmnt.res.ps=asr.ps,
      asmnt.res.bi=asr.bi)
    
    if(i==1) {
      asr.cntries<-asr.cntry
      ps.ress<-ps.res
      bi.ress<-bi.res
    } else {
      asr.cntries<-rbind(asr.cntries,asr.cntry)
      ps.ress<-rbind(ps.ress,ps.res)
      bi.ress<-rbind(bi.ress,bi.res)
    }
    setTxtProgressBar(pb,i)
  }
  
  # Conduct regional assessment
  aspr.reg<-list()
  asr.ps.reg<-vector()
  asr.bi.reg<-vector()
  
  # Loop through assessment periods
  for (j in 1:length(asp)){
    aspr.reg[[j]] <- boa.wrk.hrs(boa.dat,rp,asp[[j]])
    asr.ps.reg[j]<-aspr.reg[[j]]$integrated.prob.score$res.final[which(aspr.reg[[j]]$integrated.prob.score$res.fl==1)]
    asr.bi.reg[j]<-aspr.reg[[j]]$binomial.integration$binomial.intgr
  } 
  
  # Combine regoinal assessment results by period
  ps.res.reg<-do.call(rbind,lapply(aspr.reg,function(x) rbind(x$integrated.prob.score)))
  bi.res.reg<-do.call(rbind,lapply(aspr.reg,function(x) rbind(x$binomial.integration)))
  
  asr.reg<-data.frame(
    species=boa.dat$species[1],
    region=boa.dat$ospar.region[1],
    country="Entire region",
    ref.period=bi.res.reg$ref.per,
    asmnt.period=bi.res.reg$asmnt.per,
    asmnt.res.ps=asr.ps.reg,
    asmnt.res.bi=asr.bi.reg)
  
  # Combine regional & national assessments
  asr<-rbind(asr.cntries,asr.reg)
  ps.ress<-rbind(ps.ress,ps.res.reg)
  bi.ress<-rbind(bi.ress,bi.res.reg)
  
  # Plotting
  plt.dat<-asr %>% 
    reshape2::melt(id.vars=c("species","region",
                             "country","ref.period",
                             "asmnt.period"))
  plt.dat$value<-plt.dat$value %>% factor(levels=c("Recovering","Stable","Mixed","Declining","Unknown"))
  smry.plt<-ggplot(data=plt.dat,
                   aes(x=asmnt.period,y=country,fill=value))+
    geom_raster(show.legend=T)+
    geom_vline(xintercept=(1:length(asp))+0.5,col="white")+
    facet_grid(.~variable,
               labeller=as_labeller(c('asmnt.res.ps'="Probability score",
                                      'asmnt.res.bi'="Binomial integration")))+ 
    scale_fill_manual(name="Result",
                      drop=F,
                      labels=c("Recovering","Stable","Mixed","Declining","Unknown"),
                      values=c("olivedrab3","orange","yellow","darkred","grey"))+
    labs(x="Assessment period",y="Country")
  
  # Combine results
  res<-list(asr,ps.ress %>% subset(res.fl==1),bi.ress,smry.plt)
  names(res)<-c("assment.results","prob.score.asmnts","binomial.int.asmnts","summary.plot")
  res
}