#--- Utility functions


get_vax_OTW <- function(pop.OTW=1000000){
  
  # get vaccination population for agegroup 12+ 
  vax = read.csv('vaccines_by_age_phu.csv')
  
  # add up vax type for a given date
  vax = vax %>%
    filter(PHU.name == 'CITY OF OTTAWA',
           Agegroup %in% c("12-17yrs",
                           "18-29yrs",
                           "30-39yrs",
                           "40-49yrs",
                           "50-59yrs",
                           "60-69yrs",
                           "70-79yrs",
                           "80+"))
  
  Adult12 = sum(unique(vax$Total.population))
  tot.pop = pop.OTW
  
  #------- cumulative full vaccine coverage for population 12+
  # data read from Ottawa public health site
  # https://www.ottawapublichealth.ca/en/reports-research-and-statistics/COVID-19_Vaccination_Dashboard.aspx
  cum.cov.12 = c(0,0.01,0.02,0.03,0.03,0.06,0.37,0.73,0.80,0.84,0.87)
  
  # cumulative full vaccine coverage for total population
  cum.cov.tot = cum.cov.12*Adult12 / tot.pop 
  cum.cov.t = as.Date(c('2021-01-01',
                        '2021-02-01',
                        '2021-03-01',
                        '2021-04-01',
                        '2021-05-01',
                        '2021-06-01',
                        '2021-07-01',
                        '2021-08-01',
                        '2021-09-01',
                        '2021-10-01',
                        '2021-11-01'))
  
  
  #proportional vaccine rate in every month beginning
  #January 2021 to November 2021
  prop.rate =  c(cum.cov.tot[2:11],cum.cov.tot[11]) - cum.cov.tot
  time = as.numeric(ymd(cum.cov.t) - min(cum.cov.t))
  
  return(list(v = prop.rate, t = time))
}

#--- spline smooth function for Rt 
spl_Rt <- function(datasource.type,R,deg){
  Rt = R %>%
    filter(datasource == datasource.type)
  
  d  = Rt$date
  t  = Rt$time
  m  = Rt$Reff.m
  lo = Rt$Reff.lo 
  hi = Rt$Reff.hi
  
  s.m  = smooth.spline(x=t,y=m, spar = deg)
  s.lo = smooth.spline(x=t,y=lo, spar = deg)
  s.hi = smooth.spline(x=t,y=hi, spar = deg)
  
  d0 = min(d) - 1
  date = as.Date(d0 + s.m$x)
  
  df = data.frame(time=s.m$x,date=date,
                  Reff.m     = s.m$y,
                  Reff.lo    = s.lo$y,
                  Reff.hi    = s.hi$y,
                  spl.smooth = 'yes',
                  datasource = Rt$datasource)
  
  df = mutate(df, width.ci = Reff.hi - Reff.lo)
  
  return(df)
}


#------ Calculate Rt from model with/out smoothing
# fitting sources include datasource.type = c('clin','ww','combine')
calc_Rt_loc <- function(ci,
                        datasource.list,
                        n.cores,
                        do.smooth){
  
  res=list()
  for(datasource.type in datasource.list){
    print(datasource.type)
    fitobj.name = paste0('./fitted-object/fitted-OTW-',datasource.type,'.RData')    
    load(fitobj.name)
    
    # ---- estimate Rt
    sim.Rt = estimate_Rt(fitobj=fitobj, ci=0.95, n.cores=n.cores)
    
    sim.Rt = sim.Rt %>%
      mutate(spl.smooth = 'no', 
             datasource = datasource.type)
    
    # Final result
    res[[datasource.type]] = sim.Rt
    
    # combine list components as a dataframe 
    df.res = do.call('rbind', res)
  }
  #----- do spline smoothing
  if(do.smooth){
    Rt.spl = lapply(datasource.list,spl_Rt,
                    R   = df.res,
                    deg = 0.6)
    
    Rt.spl = do.call('rbind', Rt.spl)
    
    # combine spline smooth to previous res
    df.res = do.call('rbind', list(df.res,Rt.spl))
    
  }
  return(df.res)
}