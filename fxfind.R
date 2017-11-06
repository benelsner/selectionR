#####################################################
# Biavaschi & Elsner
# Let's be selective about migrant self-selection
# Find the right fixed costs to match the share of workers in both countries
#####################################################


vec_fx1<-as.matrix(seq(from=1, to=grid_fx, by=1)) # set up grid
vec_fx2<-as.matrix(seq(from=0.1, to=0.9, by=0.1)) #  fixed costs below 1 (a possibility they are in one country)
vec_fx<-rbind(vec_fx2, vec_fx1)

gridfx<-as.matrix(expand.grid(x=vec_fx, y=vec_fx))
gridcells_fx<-dim(gridfx)[1]

vars.list<-list(vars)[rep(1,dim(gridfx)[1])] # make list with parameters; fx differ in each cell of list
for (i in 1:gridcells_fx){
  vars.list[[i]][[8]]<-gridfx[i,]
}

z_bar_fx<-t(sapply(1:gridcells_fx, function(i) findeq(skills[[1]], vars.list[[i]]))) # find the zbar in each fx combination
z_bar_fx<-split(z_bar_fx, seq(nrow(z_bar_fx)))


shwork_y_fx<-sapply(1:gridcells_fx, function(i) outputs(z_bar_fx[[i]], skills[[1]], vars.list[[i]])[[2]]) # compute share of workers in each fx combination

# find values with minimum squared distance
  mse_y<-function(shwork_y_fx){
    mse<-(target_shy_us-shwork_y_fx[1])^2+(target_shy_mex-shwork_y_fx[2])^2 
    return(mse)
  }
  mse_shy<-matrix(unlist(lapply(shwork_y_fx, mse_y)), nrow=length(shwork_y_fx), byrow=TRUE)
  indmin<-as.matrix(which(mse_shy == min(mse_shy), arr.ind = TRUE))[1,] # ATTENTION: check if there are duplicates
  fxmin<-gridfx[indmin[1],] 
