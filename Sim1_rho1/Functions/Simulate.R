#multivariate data simulation function
simulate.multivariate <-function(data, nsim, m=5, mis.p = .5, method = "rstanarm"){
  OUT <- list(NA)
  inv.log		<- function(x){ exp(x) / (1+exp(x))} #GV15/11/2016
  pb <- txtProgressBar(min = 0, max = nsim, style = 3)
  for(i in 1:nsim){
    mis <- data
    #make missing
    for(q in 2: ncol(data)){
      ML <- (mean(mis[, 1]) - mis[, 1])/sd(mis[, 1])#GV15/11/2016
      if(mis.p == .1){p.MAR <- inv.log(ML - 2.566)}#GV15/11/2016
      if(mis.p == .2){p.MAR <- inv.log(ML - 1.65)}#GV15/11/2016
      if(mis.p == .3){p.MAR <- inv.log(ML - 1.015)}#GV15/11/2016
      if(mis.p == .4){p.MAR <- inv.log(ML - 0.485)}#GV15/11/2016
      if(mis.p == .5){p.MAR <- inv.log(ML)}#GV15/11/2016
      if(mis.p == .6){p.MAR <- inv.log(ML + 0.495)}#GV15/11/2016
      if(mis.p == .7){p.MAR <- inv.log(ML + 1.021)}#GV15/11/2016
      if(mis.p == .8){p.MAR <- inv.log(ML + 1.66)}#GV15/11/2016
      if(mis.p == .9){p.MAR <- inv.log(ML + 2.57)}#GV15/11/2016
      if(mis.p == .95){p.MAR <- inv.log(ML + 3.369)}#GV15/11/2016
      mis[rbinom(nrow(data), 1, p.MAR)==1, q] <- NA#GV15/11/2016
      }	
      #mis[rbinom(nrow(data), 1, mis.p)==1, q] <- NA #MCAR #GV15/11/2016 
    #avoid multicollinearity issues (when only a few joint observed)
    # while(!is.na(cor(mis, use = "pairwise.complete.obs")[2,3]) & abs(cor(mis, use = "pairwise.complete.obs")[2,3]) > .99){
    #  mis <- data
    #  #make missing
    #  for(z in 2: ncol(data)){
    #    mis[rbinom(nrow(data), 1, mis.p)==1, z] <- NA #MCAR
    #  }
    # }
    #impute data
    OUT[[i]] <- mice(mis, m=m, maxit=10, meth=method, print=F, threshold = 1.1)
    setTxtProgressBar(pb, i)
  }
  close(pb)  
  return(OUT)
}

