#We generate the annual benefits of climate change mitigation under RCP6.0 and SSP2 at the global level. The code is originally revised from Ricke et al. 2018.

library(data.table)
library(docopt)

'usage: generate_cscc.R -s <ssp> -c <rcp> [ -r <runid> -p <type> -l <clim> -f <name>] [-a] [-o] [-d] [-w]

options:
 -s <ssp>   SSP baseline (random(default), SSP1, SSP2,..., SSP5)
 -c <rcp>   RCP (random(default), rcp45, rcp60, rcp85)
 -r <runid> Bootstart run for the damage function parameter, 0 is estimates (0<=id<=1000)
 -p <type>  projection type (constant (default),horizon2100)
 -l <clim>  climate models (ensemble (default), mean[-ensemble])
 -o         does not allow for out-of-sample damage prediction (default, allows)
 -d         rich/poor damage function specification (default, pooled)
 -a         5-lag damage function specification (default, 0-lag)
 -f <name>  damage function (default=bhm (Burke et al.), djo (Dell et al.))
 -w         save raw data' -> doc
opts <- docopt(doc, "-s SSP2 -c rcp60 -r 1 -l mean -w")

# Some tests
#opts <- docopt(doc, "-s SSP2 -c rcp60 -w") # Default case
#opts <- docopt(doc, "-s SSP3 -c rcp85 -r 1 -w -a -d")
#opts <- docopt(doc, "-s SSP2 -c rcp60 -r 0 -l mean -w -a -d")
#opts <- docopt(doc, "-s SSP2 -c rcp60 -r 0 -w -d -f djo")

t0 <- Sys.time()

# GLOBAL VARIABLES
if (is.null(opts[["s"]])) {
  ssp = sample(paste0("SSP",1:5),1) # SSP{1,2,3,4,5}
} else {
  ssp = as.character(opts["s"])
}
if (is.null(opts[["c"]])) {
  .rcp = sample(c("rcp26","rcp45","rcp60","rcp85"),1)
} else {
  .rcp = as.character(opts["c"]) 
}
if (is.null(opts[["r"]])) {
  dmg_func = "estimates" # dmg function
  runid = 0
} else {
  print(paste("r:",opts['r']))
  runid = as.integer(max(0,min(1000,as.numeric(opts['r']))))
  if (runid == 0) {
    dmg_func = "estimates"
  }else{
    dmg_func = paste0("bootstrap")
  }
}
if (is.null(opts[["p"]])) {
  project_val = "constant" # growth rate constant
} else {
  project_val = as.character(opts["p"])
}
if (is.null(opts[["l"]])) {
  clim = "ensemble"
} else {
  clim = "mean"
  if (runid != 0) {
    dmg_func = "bootstrap"
    runid = 1:1000
  }
}
if (is.null(opts[["f"]])) {
  dmg_ref = "bhm"
} else {
  dmg_ref = as.character(opts["f"])
}

out_of_sample = !opts[['o']]
rich_poor = opts[['d']]
lag5 = opts[['a']]
save_raw_data = opts[['w']]
very_last_year = 2100
impulse_year = 2020
preffdir = "res"
pulse_scale = 1e6 # Gt=1e9 Mt=1e6 kt=1e3 t=1 
reftemplastyear = F

if (dmg_ref == "djo") {
  rich_poor = T
  out_of_sample = T
  lag5 = F
  dmg_func = "estimates"
  reftemplastyear = T
}

# Print simulation paramters
print(paste("SSP: ",ssp))
print(paste("RCP: ",.rcp))
print(paste("dmg_func: ",dmg_func))
print(paste("last year: ",very_last_year))
print(paste("prefix dir: ",preffdir))
print(paste("climate ensemble: ",clim))
print(paste("impulse year: ",impulse_year))
print(paste("projection post2100: ",project_val))
print(paste("out_of_sample: ",out_of_sample))
print(paste("richpoor: ",rich_poor))
print(paste("LR (lag5): ", lag5))
print(paste("damage function:",dmg_ref))

if (dmg_ref == "bhm") {
  dmg_ref = ""
}else{
  dmg_ref = paste0("_",dmg_ref)
}

resdir = paste0(preffdir,"_stat",dmg_ref)
if (!out_of_sample) {resdir = paste0(resdir,"_30C")}
if (rich_poor) {resdir = paste0(resdir,"_richpoor")}
if (lag5) {resdir = paste0(resdir,"_lr")}

resboot = paste0(preffdir,"_boot")
if (!out_of_sample) {resboot = paste0(resboot,"_30C")}
if (rich_poor) {resboot = paste0(resboot,"_richpoor")}
if (lag5) {resboot = paste0(resboot,"_lr")}

if (dmg_func == "bootstrap" & clim == "ensemble") {
  ddd = file.path(resboot,paste0(ssp,"-",.rcp))
  filename = file.path(ddd,paste0("store_scc_",project_val,"_",runid,dmg_ref,".RData"))
  if (file.exists(filename)) {
    stop("already computed")
  }
}

# Load data 
source("modules/gdpssp.R")
if (dmg_ref == "") {
  source("modules/bhm_replica.R")
} else {
  source("modules/djo_replica.R")
}
source("modules/cmip5.R")
source("modules/impulse_response.R")
print(Sys.time() - t0)

# All combination of models available (CC x GCM for each RCP)
temp_Burke=read.csv('temp.csv')
temp_new=temp_Burke[.rcp]
rcp26_new=temp_Burke["rcp26"];
basetemp_new=temp_Burke["basetemp"];

# Future years
fyears <- impulse_year:2100

# Impulse year
cpulse[,year := mid_year - 0.5 + fyears[1]]
epulse[,year := mid_year - 0.5 + fyears[1]]

project_gdpcap_nocc <- function(SD){
  .gdpcap <- SD$gdpcap
  .gdpr <- SD$gdpr
  .gdpcap_tm1 <- SD$gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap in 2019
  for (i in seq_along(c(fyears))) {
    .gdpcap[i] <- .gdpcap_tm1 * (1 + SD$gdpr[i])
    .gdpcap_tm1 <- .gdpcap[i]
  }
  return(list(year = fyears, 
              gdpcap = .gdpcap,
              gdprate = SD$gdpr,
              delta = NA))
}

project_gdpcap_cc <- function(SD){
  .gdpcap <- SD$gdpcap
  .gdprate <- SD$gdpr
  .delta <- rep(NA,length(SD$gdpr))
  .gdpcap_tm1 <- .gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap nocc in 2019
  .ref_temp <- SD$temp[1] # reftemp is baseline temp for BHM and temp_tm1 for DJO
  for (i in seq_along(c(fyears))) {
    if (dmg_ref == "") {.ref_temp <- SD$basetemp[i]}
    .delta[i] <- warming_effect(SD$temp[i], .ref_temp, .gdpcap_tm1, nid)
    .gdprate[i] <- (SD$gdpr[i] + .delta[i])
    .gdpcap[i] <- .gdpcap_tm1 * (1 + .gdprate[i])
    .gdpcap_tm1 <- .gdpcap[i]
    .ref_temp <- SD$temp[i]
  }
  return(list(year = fyears, 
              gdpcap = .gdpcap,
              gdprate = .gdprate,
              delta = .delta))
}

project_gdpcap_cc_pulse <- function(SD){
  .gdpcap <- SD$gdpcap
  .gdprate <- SD$gdpr
  .delta <- rep(NA,length(SD$gdpr))
  .gdpcap_tm1 <- .gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap nocc in 2019
  .ref_temp <- SD$temp[1] # reftemp is baseline temp for BHM and temp_tm1 for DJO
  for (i in seq_along(c(fyears))) {
    if (dmg_ref == "") {.ref_temp <- SD$basetemp[i]}
    .delta[i] <- warming_effect(SD$temp_pulse[i], .ref_temp, .gdpcap_tm1, nid)
    .gdprate[i] <- (SD$gdpr[i] + .delta[i])
    .gdpcap[i] <- .gdpcap_tm1 * (1 + .gdprate[i])
    .gdpcap_tm1 <- .gdpcap[i]
    .ref_temp <- SD$temp[i]
  }
  return(list(year = fyears, 
              gdpcap = .gdpcap,
              gdprate = .gdprate,
              delta = .delta))
}

# Project all scenarios
project_gdpcap <- function(SD){
  .gdpcap <- SD$gdpcap
  .gdpcap_cc <- SD$gdpcap
  
  .gdprate <- SD$gdpr
  .gdprate_cc <- SD$gdpr

  
  .gdpcap_tm1 <- .gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap nocc in 2019
  .gdpcap_tm1_cc <- .gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap nocc in 2019
  
  .ref_temp <- SD$temp[1] # reftemp is baseline temp for BHM and temp_tm1 for DJO
  if (!reftemplastyear) {.ref_temp <- SD$basetemp[1]}
  
  for (i in seq_along(c(fyears))) {
    # No climate change
    .gdprate[i] <- SD$gdpr[i] + warming_effect(SD$rcp26_temp[i], .ref_temp, .gdpcap_tm1_cc, nid, out_of_sample)
    .gdpcap[i] <- .gdpcap_tm1 * (1 + .gdprate[i])
    .gdpcap_tm1 <- .gdpcap[i]
    
    # With climate change
    .gdprate_cc[i] <- SD$gdpr[i] + warming_effect(SD$temp[i], .ref_temp, .gdpcap_tm1_cc, nid, out_of_sample)
    .gdpcap_cc[i] <- .gdpcap_tm1_cc * (1 + .gdprate_cc[i])
    .gdpcap_tm1_cc <- .gdpcap_cc[i]
    if (reftemplastyear) {.ref_temp <- SD$temp[i]}
  }
  return(list(year = fyears, 
              gdpcap = .gdpcap,
              gdpcap_cc = .gdpcap_cc,
              gdprate_cc = .gdprate_cc
  ))
}


lcscc = NULL
lwscc = NULL
lwdscc=NULL

for (nid in runid) {
  
  # Create dataset for SSP
  # ISO3 x model x ccmodel x years
  ssp_gr <- growthrate[SSP == ssp & year %in% fyears]
  
  if (clim == "ensemble") {
    ssp_temp <- ctemp[rcp == "rcp45" & year %in% fyears]
  } else {
    ssp_temp <- etemp[rcp == "rcp45" & year %in% fyears]
  }
  
  ssp_temp = merge(ssp_temp,basetemp,by = c("ISO3")) # add basetemp
  
  ssp_gdpr <- merge(ssp_gr,ssp_temp,by = c("ISO3","year")) # merge growth rate and temp
  ssp_gdpr = merge(ssp_gdpr, sspgdpcap[SSP == ssp & year == fyears[1]],
                   by = c("SSP","ISO3","year"),all.x = T) # add gdpcap0
  
  
  miss_val_iso3 <- unique(ssp_gdpr[year == impulse_year & is.na(gdpcap),ISO3])
  ssp_gdpr <- ssp_gdpr[!ISO3 %in% miss_val_iso3]
  
  ssp_gdpr <- ssp_gdpr[,.(model_id = nid,ISO3,year,temp,basetemp,gdpr,gdpcap)]
  
  setkey(ssp_gdpr,model_id,ISO3)
  
  ssp_gdpr[,temp:=temp_new[[1]]]
  ssp_gdpr[,rcp26_temp:=rcp26_new[[1]]]
  ssp_gdpr[,basetemp:=basetemp_new[[1]]]
  
  
  print(Sys.time() - t0)
  
  res_scc <- ssp_gdpr[,project_gdpcap(.SD),by = c("model_id","ISO3")]
  print(Sys.time() - t0)
  
  # yearly population 
  popyear <- pop[SSP == ssp,approx(year,pop,fyears),by = c("SSP","ISO3")]
  setnames(popyear,c("x","y"),c("year","pop"))
  res_scc <- merge(res_scc,popyear,by = c("ISO3","year"))
  print(Sys.time() - t0)
  
  # create main table for world
  res_wscc <- res_scc[,.(gdpcap_cc = weighted.mean(gdpcap_cc,pop)),
                      by = c("year",c("model_id"),"SSP")]
  
  # Compute average annual growth rate of per capita consumption between now and year t
  # for the computation of discount factor
  #countries
  gdprate_cc_impulse_year = res_scc[year == impulse_year,
                                    .(gdpcap_cc_impulse_year = gdpcap_cc),
                                    by = c("model_id","ISO3")]
  res_scc <- merge(res_scc,gdprate_cc_impulse_year,by = c("model_id","ISO3"))
  res_scc[, gdprate_cc_avg := ifelse(year == impulse_year,
                                     gdprate_cc,
                                     (gdpcap_cc/gdpcap_cc_impulse_year)^(1/(year - impulse_year)) - 1)]
  
  #World
  gdprate_cc_impulse_year = res_wscc[year == impulse_year,
                                     .(gdpcap_cc_impulse_year = gdpcap_cc),
                                     by = c("model_id")]
  res_wscc <- merge(res_wscc,gdprate_cc_impulse_year,
                    by = c("model_id"))
  res_wscc[, gdprate_cc_avg := ifelse(year == impulse_year,
                                      NA,
                                      (gdpcap_cc/gdpcap_cc_impulse_year)^(1/(year - impulse_year)) - 1)]
  res_wscc = merge(res_wscc,res_wscc[year == (impulse_year + 1),
                                     .(model_id,gdprate_cc_avg_impulse_year = gdprate_cc_avg)],
                   by = "model_id")
  res_wscc[year == impulse_year,gdprate_cc_avg := gdprate_cc_avg_impulse_year]
  res_wscc[,gdprate_cc_avg_impulse_year := NULL]
  
  print(Sys.time() - t0)
  
  # Compute SCC according to Anthoff and Tol equation A3 in Appendix
  # \dfrac {\partial C_{t}} {\partial E_{0}}\times P_{t}
  # approximate by change in GDP rather than consumption
  res_scc[, scc := (gdpcap_cc-gdpcap) * pop * (1e6 / pulse_scale)] # $2005/tCO2  **I delete impulse here to calculate total GDP loss
  sum_res_scc = res_scc[, .(scc = sum(scc)), 
                        by = c("year",c("model_id"))]
  res_wscc = merge(res_wscc,sum_res_scc,
                   by = c("year",c("model_id")))
  
 
  print(Sys.time() - t0)
  
  # Discount SCC according to Anthoff and Tol equation A3 in Appendix
  # elasticity of marginal utility of consumption = 1
  # based on Table 3.2 in IPCC AR5 WG2 Chapter 3
  # added 3% prtp to be compatible with EPA
  prtps = c(1.5,2) # %
  etas = c(2) 
  
  cscc = NULL
  dscc_p=NULL
  for (.prtp in prtps) {
    for (.eta in etas) {
      dscc = res_scc[,list(ISO3,model_id,year,gdprate_cc_avg,scc)]
      dscc[,dfac := (1/(1 + .prtp/100 + .eta * gdprate_cc_avg)^(year - impulse_year))]
      dscc[,dscc := dfac * scc]
      dscc[,prtp:=.prtp]
      dscc[,eta:=.eta]
      dscc_p=rbind(dscc_p,dscc) #add by me
      cscc = rbind(cscc,dscc[,.(prtp = .prtp,eta = .eta,scc = sum(dscc)),
                             by = c("ISO3","model_id")],fill = T)
      
    }
  }
  wscc = cscc[,list(scc = sum(scc)),by = c("prtp","eta","model_id")]
  wdscc = dscc_p[,list(dscc = sum(dscc)),by = c("model_id",'year','prtp','eta')]
  wdscc = wdscc[,dr:=0]
  
  
  drs = c(3,5) #%
  cscc0 = NULL
  dscc1 =NULL
  for (.dr in drs) {
    dscc = res_scc[,list(ISO3,model_id,year,scc)]
    dscc[,dfac := (1/(1 + .dr/100)^(year - impulse_year))]
    dscc[,dscc := dfac * scc]
    dscc[,dr:=.dr] #add by me
    dscc[,prtp:=0]
    dscc[,eta:=0]
    dscc1=rbind(dscc1,dscc) #add by me
    cscc0 = rbind(cscc0,dscc[,.(dr = .dr,scc = sum(dscc)),
                             by = c("ISO3","model_id")])
  }
  cscc = rbindlist(list(cscc0,cscc),fill = T)
  wdscc = rbindlist(list(wdscc,dscc1[,.(dscc = sum(dscc)),
                                   by = c("dr",'prtp',"eta","model_id","year")]),
                   fill = T)
  
  print(Sys.time() - t0)
  
  # ID to be used
  
  lwdscc = rbind(lwdscc,wdscc)
  
}


lwdscc1=lwdscc
setorder(lwdscc1,model_id,prtp,eta,dr,year)

pr=c(1.5,2)
et=c(2)

level1=NULL
level2=NULL
for (.ii in pr) {
  for (.jj in et){
lwdscc_d0=lwdscc1[prtp==.ii & eta==.jj,]
lwdscc_d0=lwdscc_d0[,.(model_id,year,dscc)]
lwdscc_d0=dcast(lwdscc_d0,
            year ~ model_id,
            value.var = c("dscc"))

temp2=t(apply(lwdscc_d0[,c(2:1001)],1,quantile, probs = c(0.05,0.95)))
temp1=apply(lwdscc_d0[,c(2:1001)],1,mean)

level1=cbind(level1,temp1)

level2=cbind(level2,temp2)
  }
}

drr=c(3,5)

for (.kk in drr) {
  lwdscc_d0=lwdscc1[dr==.kk,]
  lwdscc_d0=lwdscc_d0[,.(model_id,year,dscc)]
  lwdscc_d0=dcast(lwdscc_d0,
                  year ~ model_id,
                  value.var = c("dscc"))
  
  temp2=t(apply(lwdscc_d0[,c(2:1001)],1,quantile, probs = c(0.05,0.95)))
  temp1=apply(lwdscc_d0[,c(2:1001)],1,mean)
  
  level1=cbind(level1,temp1)
  level2=cbind(level2,temp2)
}

write.csv(level1,file='Burke_RCP60_RCP26_SSP2_mean_estimate.csv')
write.csv(level2,file='Burke_RCP60_RCP26_SSP2_confidence.csv')









###########
#add by me

#dscc3=dscc2[dr==3,]
#dscc5=dscc2[dr==5,]

#write.csv(dscc2,file='RCP85_SSP5_dscc2.csv')
#write.csv(dscc3,file='dscc3.csv')
#write.csv(dscc5,file='dscc5.csv')

#######
wscc = rbindlist(lwscc)

store_scc <- wscc
store_scc_flat <- split(store_scc$scc, store_scc$ID)

print(Sys.time() - t0)

compute_stat <- function(.data) {
  res <- c(list(mean = mean(.data)),
           as.list(quantile(.data, probs = c(
             0.1, 0.25, 0.5, 0.75, 0.9
           ))))
  return(as.data.table(res))
}

# Bayesian bootstrap to check quality of statistics
#lapply(store_scc_flat, bayesboot, mean)

if (save_raw_data) {
  dir.create(file.path(resdir), recursive = T, showWarnings = F)
  filename = file.path(resdir,paste0("raw_scc_",ssp,"_",.rcp,"_",project_val,"_",dmg_func,"_clim",clim,dmg_ref,".RData"))
  save(store_scc_flat, file = filename)
}

if (dmg_func == "estimates" | clim == "mean") {
  stat_scc <- rbindlist(lapply(store_scc_flat, compute_stat))
  stat_scc$ID <- names(store_scc_flat)
  dir.create(file.path(resdir), recursive = T, showWarnings = F)
  filename = file.path(resdir,paste0("statscc_",ssp,"_",.rcp,"_",project_val,"_",dmg_func,"_clim",clim,dmg_ref,".RData"))
  save(stat_scc, file = filename)
  print(paste(filename,"saved"))
} else {
  ddd = file.path(resboot,paste0(ssp,"-",.rcp))
  dir.create(ddd, recursive = T, showWarnings = F)
  filename = file.path(ddd,paste0("store_scc_",project_val,"_",runid,dmg_ref,".RData"))
  save(store_scc_flat, file = filename)
  print(paste(filename,"saved"))
}
print(Sys.time() - t0)
print("end")
