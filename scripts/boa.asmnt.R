# Function to perfom binomial occurrence assessment after Probst, W. N., Lynam, C. P., Bluemel, J. K., and Clarke, M. 2023. Assessing change in the occurrence of rare species using the binomial distribution. Ecological Indicators, 156.
# by W. N. Probst, Thuenen-Institute, September 2026

# boa.dat: Survey data with columns 'ospar.region',country','year','quarter','survey','gear','quarter','species'
# rp: years of reference period, numeric e.g. 2000:2015
# ap: years of assessment period, numeric e.g. 2022:2027
# rgnl: Should assessment be on national levels (in region, then flag as FALSE) or for the entire region (flag as TRUE)?

boa.asmnt<-function(boa.dat,rp,ap,rgnl){
  
  # Load required packages
  require(magrittr)
  
  # Loop through countries in region
  if(rgnl==T) {
    
    # Perform assessment
    bar<-boa.wrk.hrs(boa.dat,rp,ap)
    
    # Replace country names with dummy for 
    bar[[1]]$country<-"all countries"
    bar[[2]]$country<-"all countries"
    bar[[3]]$country<-"all countries"
    bar[[4]]$country<-"all countries"
    bar[[5]]$country<-"all countries"
    bar[[6]]$country<-"all countries"
  
  # else perform BOA by each country in region    
  } else{
    
    bdn<-split(boa.dat,boa.dat$country) 
    for(n in 1:length(bdn)){
      
      bar.n<-boa.wrk.hrs(bdn[[n]],rp,ap)
      bar.n$time.series$country<-bar.n$occ.data.rp$country[1]
      
      if(n==1) bar<-bar.n else {
        bar[[1]]<-rbind(bar[[1]],bar.n[[1]]) %>% na.omit
        bar[[2]]<-rbind(bar[[2]],bar.n[[2]]) %>% na.omit
        bar[[3]]<-rbind(bar[[3]],bar.n[[3]]) %>% na.omit
        bar[[4]]<-rbind(bar[[4]],bar.n[[4]]) %>% na.omit
        bar[[5]]<-rbind(bar[[5]],bar.n[[5]])
        bar[[6]]<-c(bar[[6]],bar.n[[6]])
      }
    }
    names(bar[[6]])<-names(bdn)
  }
  bar
}

