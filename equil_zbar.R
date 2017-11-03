#####################################################
# Biavaschi & Elsner
# Let's be selective about migrant self-selection
# Compute optimal zbar for different set-ups
#####################################################

# Find the right fixed costs AND the optimal zbar in the baseline

  vec_fx<-seq(from=1, to=grid_fx, by=1)
  gridfx<-as.matrix(expand.grid(x=vec_fx, y=vec_fx))
  gridcells_fx<-dim(gridfx)[1]
  #gridfx.list<-split(gridfx, seq(nrow(gridfx)))
  
  vars.list<-list(vars)[rep(1,dim(gridfx)[1])]
  for (i in 1:gridcells_fx){
    vars.list[[i]][[8]]<-gridfx[i,]
  }
  
  z_bar_fx<-t(sapply(1:gridcells_fx, function(i) findeq(skills[[1]], vars.list[[i]])))
  z_bar_fx<-split(z_bar_fx, seq(nrow(z_bar_fx)))
  shwork_y_fx<-sapply(1:gridcells_fx, function(i) outputs(z_bar_fx[[i]], skills[[1]], vars.list[[i]])[[2]])


########################################################
# Find the optimal zbar
  # skills: list with skill distributions; 
  zbar<-lapply(skills, findeq, vars)
  
  
# Compute outputs based on zbar
  eqvars1<-lapply(1:length(skills), function(i) outputs(zbar[[i]], skills[[i]], vars)[[1]])     # variables with percentage changes
  eqvars2<-lapply(1:length(skills), function(i) outputs(zbar[[i]], skills[[i]], vars)[[2]])     # variables with pp changes 
  eqvars2<-lapply(1:length(skills), function(i) t(eqvars2[[i]]))  
  eqnomwages<-lapply(1:length(skills), function(i) outputs(zbar[[i]], skills[[i]], vars)[[3]])  # nominal wages
  eqrealwages<-lapply(1:length(skills), function(i) outputs(zbar[[i]], skills[[i]], vars)[[4]]) # real wages
  eq_cx<-lapply(1:length(skills), function(i) outputs(zbar[[i]], skills[[i]], vars)[[7]]) # unit costs
  eq_exports<-lapply(1:length(skills), function(i) outputs(zbar[[i]], skills[[i]], vars)[[8]]) # exports 
  eq_expgdp<-lapply(1:length(skills), function(i) outputs(zbar[[i]], skills[[i]], vars)[[9]]) # exports 
  
# Compute relative difference selected-neutral
  col<-c("US", "Mexico")
  
  ch_vars1<-data.frame(sapply(1:2, function(i) (eqvars1[[1]][,i]-eqvars1[[2]][,i])/eqvars1[[2]][,i]*100))
    rownames(ch_vars1)<-c("GDP pc", "Nominal wages", "Prices")
    colnames(ch_vars1)<-col
  ch_vars2<-sapply(1:2, function(i) (eqvars2[[1]][,i]-eqvars2[[2]][,i])*100)
    #rownames(ch_vars2)<-c("Share workers, sector Y")
  ch_nomwage<-sapply(1:2, function(i) (eqnomwages[[1]][,i]-eqnomwages[[2]][,i])/eqnomwages[[2]][,i]*100)
  ch_realwage<-sapply(1:2, function(i) (eqrealwages[[1]][,i]-eqrealwages[[2]][,i])/eqrealwages[[2]][,i]*100)
  
  
  # wages for several bins of the support 
  wagebins<-function(skills, eqwages, nrbins, z){
    group<-rep(1:nrbins,each=length(skills[,1])/nrbins)
    d<-data.frame(z, group, skills, eqwages*skills)
      colnames(d)<-c("z", "group", "X1", "X2", "X3", "X4")
    wage_bin<-ddply(d, "group", function(x){
      gweight1<-sum(x$X1)
      gweight2<-sum(x$X2)
      meanw1<-(sum(x$X3)/gweight1)
      meanw2<-(sum(x$X4)/gweight2)       
      data.frame(meanw1, meanw2)
    })
    return(wage_bin)
  }
  
  
  realwage_bins<-lapply(1:length(skills), function(i) wagebins(skills[[i]],eqrealwages[[i]], nrbins, z))
  ch_realwage_bin<-data.frame(sapply(2:(length(skills)+1), function(i) (realwage_bins[[1]][,i]-realwage_bins[[2]][,i])/realwage_bins[[2]][,i]*100))  

  nomwage_bins<-lapply(1:length(skills), function(i) wagebins(skills[[i]], eqnomwages[[i]], nrbins, z))
  ch_nomwage_bin<-data.frame(sapply(2:(length(skills)+1), function(i) (nomwage_bins[[1]][,i]-nomwage_bins[[2]][,i])/nomwage_bins[[2]][,i]*100))  
  
  