#####################################################
# Biavaschi & Elsner
# Let's be selective about migrant self-selection
# Parameters & data
#####################################################


beta<-0.5
theta<-1.5
sigma<-4

#gy<-c(0.9,1)
#gx<-c(2,1.6)
gy<-c(1,0.9)
#gx<-c(2,1.6)
gx<-c(1.6,2)
Fx<-c(1,2)
#Fx<-c(2,4)
#Fx<-c(0.45,0.5)
tau<-4


# data - single values
  #gdp_us<-45055 # Source: St.Louis FED, in constant prices
  #gdp_mex<-14198 # Source: World bank?
  gdp_mex<-0.4 ## productivity rather than GDP  


  #pop_us<-282 ## total population in 2000; US census bureau
  #pop_mex<-102 ## total population living in the country; source: World bank
  pop_us <-143 ## US census bureau
  pop_mex <-40 ## source: World bank
  
  mig<-11 ## number of Mexican migrants in million
  othermig<-20 ## number of other migrants in the US in million
  pop_us_nat<-pop_us-mig

# Parameters for computation
  gridlength<-50 # over how many cells should the optimal zbar be searched
  grid_fx<-10 # length of grid for searching fixed costs
  nrbins<-5 # number of wage bins to be reported in the outputs
  
  target_shy_mex<-0.7 # target number of workers in sector Y
  target_shy_us<-0.3
  
  
    
  
# data - skill distributions
  usdata.sel<-data.frame(read_excel('./data/selection.xlsx', sheet=1, col_names=TRUE, skip=2))
  usdata.neu<-data.frame(read_excel('./data/selection.xlsx', sheet=2, col_names=TRUE, skip=2))
  mexdata.sel<-data.frame(read_excel('./data/selection.xlsx', sheet=3, col_names=TRUE, skip=2))  
  mexdata.neu<-data.frame(read_excel('./data/selection.xlsx', sheet=4, col_names=TRUE, skip=2))  
    
  skilldist<-read.xls('./data/skilldistrib_trim.xls')
  skills_mig_neutral<-read.xls('./data/skilldistrib_weighted_neutral_trim.xls')[1:10,]
  wagedec<-data.frame(read_excel('./data/wage_deciles_trim.xls', sheet=1, col_names=FALSE, skip=1)[,11:18])
  
  # Mexican data from 20 bins
  mex.sel<-mexdata.sel[,2] ## workforce in Mexico under negative self-selection  
  mex.mig<-mexdata.sel[,3]
  mex.tot<-mexdata.neu[,2]
  mex.neu<-1/pop_mex*(pop_mex*mex.sel + mig*mex.mig - mig*mex.tot) ## workforce in Mexico under neutral selection
  mex.diff<-mex.sel-mex.neu
  index<-1:length(mex.sel)
  mexdata1<-data.frame(index, mex.sel, mex.mig, mex.tot, mex.neu, mex.diff)
  
  # Mexican data from 10 bins
  mex.sel1<-skilldist$Mexico[skilldist$Mig.Status=="non-migrant"]   
  mex.mig1<-skilldist$Mexico[skilldist$Mig.Status=="migrants"]
  mex.tot1<-skilldist$Mexico[skilldist$Mig.Status=="tot"] 
  mex.neu1<-1/pop_mex*(pop_mex*mex.sel1 + mig*mex.mig1 - mig*mex.tot1)
  mex.diff1<-mex.sel1-mex.neu1
  
  index<-1:length(mex.sel1)
  mexdata2<-data.frame(index, mex.sel1, mex.mig1, mex.tot1, mex.neu1, mex.diff1)
  
    
    
  # US data from 20 bins
  us.sel<-usdata.sel[,2]*mig/pop_us+usdata.sel[,3]*(pop_us-mig-othermig)/pop_us+usdata.sel[,4]*othermig/pop_us
  us.mig<-usdata.sel[,2]
  us.nat<-usdata.sel[,3]
  us.othermig<-usdata.sel[,4]
  us.migneu<-usdata.neu[,2]
  us.neu<-1/pop_us*(pop_us*us.sel - mig*us.mig + mig*us.migneu)
  us.diff<-us.sel-us.neu
  
  index<-1:length(us.sel)
  usdata1<-data.frame(index, us.sel, us.mig, us.nat, us.othermig, us.neu, us.diff)
  
  # US data
  us.sel1<-skilldist$US2000[skilldist$Mig.Status=="tot"]
  us.mig1<-skilldist$US2000[skilldist$Mig.Status=="migrants"]
  us.nat1<-skilldist$US2000[skilldist$Mig.Status=="non-migrant"]
  us.migneu1<-skills_mig_neutral$US2000
  us.neu1<-1/pop_us*(pop_us*us.sel1 - mig*us.mig1 + mig*us.migneu1)
  us.diff1<-us.sel1-us.neu1
  index<-1:length(us.sel1)
  usdata2<-data.frame(index, us.sel1, us.mig1, us.nat1, us.migneu1, us.neu1, us.diff1)
  
  


  skills<-list(cbind(us.sel,mex.sel),cbind(us.neu, mex.neu))
  #skills<-list(cbind(us.sel1,mex.sel1),cbind(us.neu1, mex.neu1))

# parameters to be calculated from data
  lambda<-c(1, gdp_mex)
  M<-c(1, pop_mex/pop_us)
  
  a<-1/length(mex.sel)
  z<-round(seq(a, 1, a),2) 

# parameter vector  
  vars<-list(beta, theta, sigma, lambda, M, gy, gx, Fx, tau, z)
  
  
  