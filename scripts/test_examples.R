# FC1-Update routines 2026
# Authors: W.N. Probst, 

# Load packages ----
library(magrittr);library(reshape);library(tidyverse);library(data.table)
library(sf);library(raster);library(terra)
library(ggplot2);library(patchwork);library(ggpubr);library(pals);library(crayon)
library(worrms)     
library(mapplots)

# remotes::install_github("einarhjorleifsson/obus@2a6c1f64ce0fda8c0167888488ae129b29e1e0f6")
# Source of package 'obus' to extract DATRAS data
# https://github.com/ices-tools-prod/icesDatras/issues/32
# To install:
# remotes::install_github("einarhjorleifsson/obus@2a6c1f64ce0fda8c0167888488ae129b29e1e0f6")
library(obus)

# Source functions ----
source("./scripts/datras.hh.R")
source("./scripts/datras.merge.hhhl.R")
source("./scripts/boa.wrk.hrs.R")
source("./scripts/boa.asmnt.R")
source("./scripts/period.asmnt.R")
source("./scripts/boa.spatial.R")
source("./scripts/ovrvw.spc.R")
source("./scripts/ovrvw.hh.R")

# Examples of command chain ----

# Get all hauls from all surveys in OSPAR region III
# Need to do this once per region, than can be merged separately for each species
hh.iii.dat<-datras.hh(ospar.region="III",
                      srvys=c("BTS","EVHOE","FR-WCGFS","IE-IAMS","IE-IGFS",
                        "NIGFS","SCOWCGFS","SP-PORC","SWC-IBTS"),
                      yrs=1985:2026)

# Explore number of hauls and spatial extent
ovrvw.hh.iii<-ovrvw.hh(hh.iii.dat)

ovrvw.hh.iii$n.hauls %>% 
  as.data.frame %>%
  #data.table::melt(id.vars="Year") %>% 
  ggplot(aes(x=Var1,y=Freq,fill=Var2))+
  geom_col(show.legend=F)+
  facet_wrap(.~Var2,scales="free_y")+
  scale_fill_discrete(palette=pals::tol.rainbow)

x11(15,15)
ovrvw.hh.iii$spatial.overview

# Merge with abundance/occurrence data for cod
cod.iii<-datras.merge.hhhl(hh.iii.dat,"Gadus morhua")
sqa.iii<-datras.merge.hhhl(hh.iii.dat,"Squalus acanthias")
tra.iii<-datras.merge.hhhl(hh.iii.dat,"Trachurus trachurus")

# Assess binomial occurrence (BOA) for cod for all countries
cod.iii.nat<-boa.asmnt(boa.dat=cod.iii,
                       rp=1985:2015,
                       ap=2022:2025,
                       rgnl=F)

sqa.iii.nat<-boa.asmnt(boa.dat=sqa.iii,
                       rp=1985:2003,
                       ap=2022:2025,
                       rgnl=F)

tra.iii.reg<-boa.asmnt(boa.dat=tra.iii,
                       rp=1985:2003,
                       ap=2022:2025,
                       rgnl=T)

# BOA assessment for cod for entire region III
cod.iii.reg<-boa.asmnt(boa.dat=cod.iii,
                       rp=1985:2003,
                       ap=2016:2021,
                       rgnl=T)

# Get an overview on data coverage for spurdog
ovw.iii<-ovrvw.spc(cod.iii)
ovw.iii$n.hauls
ovw.iii$occ.freq.plot
ovw.iii$spatial.overview

# Spatial plots
boa.spatial(cod.iii.nat,"bi")
boa.spatial(sqa.iii.nat,"ps")
boa.spatial(tra.iii.reg,"both")

# Assess across multiple periods
cod.iii.per<-period.asmnt(boa.dat=cod.iii,
                         rp=1985:2003,
                         asp=list(2004:2009,2010:2015,2016:2021,2022:2025))

cod.iii.per$summary.plot

tra.iii.per<-period.asmnt(boa.dat=tra.iii,
                          rp=1985:2003,
                          asp=list(2004:2009,2010:2015,2016:2021,2022:2025))
tra.iii.per$summary.plot










