#####################################################
# Biavaschi & Elsner
# Let's be selective about migrant self-selection
# Find the right fixed costs to match the share of workers in both countries
#####################################################


  
#############################
  # stepwise algorithm
  
  # idea: start with large intervals for the fixed costs in each country; narrow down the intervals until the share of workers in sector Y converges

  # function: find minimizing fixed cost combination
    fx_fun<-function(vec_fx, skills, vars){
      
      gridfx<-as.matrix(expand.grid(x=vec_fx[[1]], y=vec_fx[[2]]))
      gridcells_fx<-dim(gridfx)[1]
      
      vars.list<-list(vars)[rep(1,dim(gridfx)[1])] # make list with parameters; fx differ in each cell of list
      for (i in 1:gridcells_fx){
        vars.list[[i]][[8]]<-gridfx[i,]
      }
      
      z_bar_fx<-t(sapply(1:gridcells_fx, function(i) findeq(skills, vars.list[[i]]))) # find the zbar in each fx combination
      z_bar_fx<-split(z_bar_fx, seq(nrow(z_bar_fx)))
      shwork_y_fx<-sapply(1:gridcells_fx, function(i) outputs(z_bar_fx[[i]], skills, vars.list[[i]])[[2]]) # compute share of workers in each fx combination
      
      # find values with minimum squared distance
      #mse_y<-function(shwork_y_fx){
      #  mse<-(target_shy_us-shwork_y_fx[1])^2+(target_shy_mex-shwork_y_fx[2])^2 
      #  return(mse)
      #}
      
      mse_shy<-matrix(unlist(lapply(shwork_y_fx, mse_y)), nrow=length(shwork_y_fx), byrow=TRUE)
      indmin<-as.matrix(which(mse_shy == min(mse_shy), arr.ind = TRUE))[1,] # ATTENTION: check if there are duplicates
      fxmin<-as.matrix(gridfx[indmin[1],])
      
      shwork_y_opt<-as.matrix(shwork_y_fx[[indmin[1]]])
      outputs<-list(fxmin, shwork_y_opt)
      return(outputs)
    }
    
    
  # subroutine: mean squared error
    mse_y<-function(shwork_y_fx){
      mse<-popweight_us*(target_shy_us-shwork_y_fx[1])^2+popweight_mex*(target_shy_mex-shwork_y_fx[2])^2 
      return(mse)
    }
    
    

  # EXECUTE algorithm    
    steps<-c(5,1,0.5,0.1, 0.05 , 0.01, 0.005, 0.001)
    start_from<-10
    start_to<-100
    start_by<-10
    vec_fx_start<-list(seq(from=start_from, to=start_to, by=start_by), seq(from=start_from, to=start_to, by=start_by))
    
    fx_results <- vector(mode = "list", length = length(steps))
    fx_results[[1]]<-fx_fun(vec_fx_start, skills[[1]], vars)
    

    #fxmin_list[[1]]<-as.matrix(fx_fun(vec_fx_start, skills[[1]], vars)[[1]])
    #shy_list[[1]]<-fx_fun(vec_fx_start, skills[[1]], vars)[[2]]  
    
    
    for (i in 2:length(steps)){
      vec_fx<-vector(mode="list", length=2)
      vec_fx[[1]]<-seq(from=fx_results[[i-1]][[1]][1]-steps[i-1], to=fx_results[[i-1]][[1]][1]+steps[i-1], by=steps[i])
      vec_fx[[2]]<-seq(from=fx_results[[i-1]][[1]][2]-steps[i-1], to=fx_results[[i-1]][[1]][2]+steps[i-1], by=steps[i])
      fx_results[[i]]<-fx_fun(vec_fx, skills[[1]], vars)
      if (abs(fx_results[[i]][[2]][1]-fx_results[[i-1]][[2]][1])<0.1){
        break}
      #shy_list[[1]]<-fx_fun(vec_fx_start, skills[[1]], vars)[[2]]    
    }
   fx_results <- fx_results[-which(sapply(fx_results, is.null))]
   id_fx<-length(fx_results)
   fx_final<-fx_results[[id_fx]][[1]]
   vars[[7]]<-t(fx_final)
