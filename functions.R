#####################################################
# Biavaschi & Elsner
# Let's be selective about migrant self-selection
# Functions
#####################################################

# Trade model

equil <- function(zbar, skilldist, vars) {
  # Program returns the equilibrium conditions
  
  # input variables
  beta<-vars[[1]]
  theta<-vars[[2]]
  sigma<-vars[[3]]
  lambda<-vars[[4]]
  M<-vars[[5]]
  gy<-vars[[6]]
  gx<-vars[[7]]
  Fx<-vars[[8]]
  tau<-vars[[9]]
  #skilldist<-vars[[10]]
  z<-vars[[10]]
  
  # Equation 1: unit costs
  cx <- exp((gy-gx)*zbar)
  
  # Equation 2: Price index <- requires several intermediate steps
  
  # Step 1: vector indicating cutoffs
  cutvec<-matrix(NA, nrow=length(z), ncol=length(zbar))
  weights1<- function(i) {
    a<-zbar[i]*ones(length(z),1)-z
    a<-c(diff(sign(a)),0)
  }
  cutvec<-sapply(1:2, weights1)  
  
  # incorporate special cases
  for (i in 1:2){
    b<-which.min(cutvec[,i])
    if (min(cutvec[,i])==-1){cutvec[b,i]<-0} # zbar exactly equals a value of z
    if (sum(cutvec[,i])==0){cutvec[length(z),i]<--2} # corner solution: zbar=1
  } 
  
  # generate vector of sign changes
  signch<-which(cutvec<0, arr.ind=TRUE)[,1]
  
  # share of sector y in bin that contains zbar
  sh_y<-(zbar-z[signch])/(1/length(z))
  
  
  # Step 2: weighting matrixes for Z for both sectors  
  yweight<-matrix(NA,nrow = length(z), ncol=2)
  for (i in 1:2){
    b[i]<-length(z)-signch[i]-1
    if (b[i]<0){
      yweight[,i]<-1
    } else {
      yweight[,i]<-c(rep(1,signch[i]),sh_y[i],rep(0,b[i]))
    }
  } 
  
  xweight<-ones(length(z),2)-yweight  
  
  
  # Step 3: integrals for production in sectors X and Y
  expXZ<-matrix(NA,nrow = length(z), ncol=2)
  expYZ<-matrix(NA,nrow = length(z), ncol=2)
  for (i in 1:2){
    expXZ[,i]<-exp(gx[i]*z)*xweight[,i]*skilldist[,i]
    expYZ[,i]<-exp(gy[i]*z)*yweight[,i]*skilldist[,i]    
  }
  
  intX<-colSums(expXZ)        # value of integral X
  intY<-colSums(expYZ)        # value of integral Y    
  
  
  # Step 4: price index
  Px1=sigma/(sigma-1)*(M[1]*lambda[1]/(sigma*Fx[1])*cx[1]^(1-sigma)*intX[1]+M[2]*lambda[2]/(sigma*Fx[2])*(cx[2]*tau)^(1-sigma)*intX[2])^(1/(1-sigma))
  Px2=sigma/(sigma-1)*(M[2]*lambda[2]/(sigma*Fx[2])*cx[2]^(1-sigma)*intX[2]+M[1]*lambda[1]/(sigma*Fx[1])*(cx[1]*tau)^(1-sigma)*intX[1])^(1/(1-sigma))
  Px<-c(Px1, Px2)
  P<-(beta^theta*Px^(1-theta)+(1-beta)^theta)^(1/(1-theta))
  
  
  # Equation 3: price per variety
  p<-(sigma)/(sigma-1)*cx
  
  # Equation 4: average nominal wages
  Wbar<-lambda*(intY+cx*intX) 
  
  # Equation 5: expenditure share for good X
  sPx<-(beta^theta*Px^(1-theta))/((1-beta)^theta+beta^theta*Px^(1-theta))
  
  
  
  # EQUILIBRIUM CONDITIONS (written so they equal 0)
  EQ1<-sPx[1]*M[1]*Wbar[1]*(p[1]/Px[1])^(1-sigma) + sPx[2]*M[2]*Wbar[2]*((p[1]*tau)/Px[2])^(1-sigma)-(sigma-1)*Fx[1]*p[1] 
  EQ2<-sPx[1]*M[1]*Wbar[1]*((p[2]*tau)/Px[1])^(1-sigma) + sPx[2]*M[2]*Wbar[2]*(p[2]/Px[2])^(1-sigma)-(sigma-1)*Fx[2]*p[2] 
  EQ3<-(1-sPx[1])*M[1]*Wbar[1]+(1-sPx[2])*M[2]*Wbar[2]-M[1]*lambda[1]*intY[1]-M[2]*lambda[2]*intY[2]
  
  # equilibrium only for tau=1
  EQ4<-(exp((gx[1]-gy[1])*zbar[1]))/(exp((gx[2]-gy[2])*zbar[2]))-(Fx[1]/Fx[2])^(1/sigma)
  
  # fixed costs to ensure equilibrium in sector X
  Fxratio<-((exp((gx[1]-gy[1])*zbar[1]))/(exp((gx[2]-gy[2])*zbar[2])))^sigma
  
  # exports
  exports1 <- M[1]*lambda[1]/(sigma*Fx[1])*intX[1]*sPx[2]*M[2]*Wbar[2]*Px[2]^(sigma-1)*(p[2]*tau)^(1-sigma) ## exports US->Mexico
  exports2 <- M[2]*lambda[2]/(sigma*Fx[2])*intX[2]*sPx[1]*M[1]*Wbar[1]*Px[1]^(sigma-1)*(p[1]*tau)^(1-sigma) ## exports Mexico->US
  exports<-c(exports1, exports2)
  
  
  #output<-abs(abs(EQ1)+abs(EQ2))
  #return(output)
  EQ<-list(EQ1, EQ2, EQ3, EQ4, abs(EQ1)+abs(EQ2), Fxratio) 
  EQoutcomes<-list(Wbar, P, yweight, xweight, cx, exports)
  outputs<-list(EQ, EQoutcomes)
  return(outputs)
}


#########################################################
# Find equilibrium for given variables
findeq<-function(skilldist, vars){
  
  # set up the grid [i,j]
    grid<-seq(from=1/gridlength, to=1, by=1/gridlength)
    
  # compute the equilibrium for each [i,j]-combination
    eqmatrix<-sapply(1:gridlength, function(j) sapply(1:gridlength, function(i) equil(c(grid[i],grid[j]), skilldist,vars)[[1]][[5]]))
  
  # replace NaN with very high value
    eqmatrix[is.nan(eqmatrix)]<-10^6 
  
  # locate the minimum value of the equilibrium conditions
    indmin<-which(eqmatrix == min(eqmatrix), arr.ind = TRUE) # locate minimum value
    zbar_opt<-c(grid[indmin[1,1]],grid[indmin[1,2]])
    
  return(zbar_opt)
}



#########################################################
# Produce outputs

outputs<-function(zbar, skilldist, vars){
  eqvalues<-equil(zbar, skilldist, vars)[[2]]
    Wbar<-eqvalues[[1]]
    P<-eqvalues[[2]]
    yweight<-eqvalues[[3]]
    xweight<-eqvalues[[4]]
    cx<-eqvalues[[5]]  
    exports<-eqvalues[[6]]
    
    #real GDP per capita
    gdppc<-Wbar/P  
    
    # share of workers in sector Y
    shwork_y<-as.matrix(colSums(yweight*skilldist), ncol=2)
    
    # average nominal wage per bin
    wY<-sapply(1:2, function(i) lambda[i]*exp(gy[i]*z)) # nominal wages in each bin
    wX<-sapply(1:2, function(i) lambda[i]*cx[i]*exp(gx[i]*z))
    nomwage<-sapply(1:2, function(i) wY[,i]*yweight[,i] + wX[,i]*xweight[,i])
    realwage<-sapply(1:2, function(i) nomwage[,i]/P[i])  
    
    # trade
    #expgdp<-exports/Wbar*P
    expgdp<-exports/(gdppc*vars[[5]])
    
    stats1<-data.frame(rbind(gdppc, Wbar, P))
    stats2<-data.frame(shwork_y)
    outputs<-list(stats1, stats2, nomwage, realwage, wY, wX, cx, exports, expgdp)
  return(outputs)
}


#################################
# Grid search algorithm for Fx


fxfind_int<-function(skilldist, Fx, var){
  var[[8]]<-Fx
  zbar<-findeq(skilldist, var)  
  shwork_y<-outputs(zbar, skilldist, var)[[2]]
  return(shwork_y)
}






#################################
# Wage bins



