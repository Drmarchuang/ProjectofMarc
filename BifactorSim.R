BifactorSim <- function(type= 'no.meth',               # no.meth, one.meth, two.meth, interact
                        sample.size= 100,              #100 200 500
                        fl = 'noncongeneric',          # noncongeneric, congeneric
                        mean.dif= .1                   # 0, .1, .2, .3, .4,
                        ){
  start.time <- Sys.time()
  library(simsem)
  source('https://raw.githubusercontent.com/MaksimRudnev/AlignmentMplusAutomation/master/source_repo.R')
  
  set.seed(1)
  
  meth <- 'constr =~ y1 + y2 +y3 + y4 + y5 + y6
         meth =~ y4 + y5 + y6
         meth ~~ 0*constr'
  
  # factor loadings of meth factor
  if(fl=='noncongeneric'){
    meth.loadings <- 'runif(3,sqrt(.01), sqrt(.03))'
  }else{
    meth.loadings <- runif(3,sqrt(.01), sqrt(.03))
  }
  
  # check the dat type
  if(type == 'no.meth'){
    loadings <- matrix(NA,6,1)
    loadingVals <- matrix(.7,6,1)
    LY <- bind(loadings, loadingVals)
    
    error.cov <- matrix(0,6,6)
    diag(error.cov) <- 1
    RTE <- binds(error.cov)
    
    latent.cov <- as.matrix(1)
    RPS <- binds(latent.cov)
    

    # default factor mean is zero
    AL.2 <- binds(as.matrix(mean.dif))
    
    model.1 <- model.cfa(LY=LY, RTE= RTE, RPS = RPS,
                         indLab=c('y1','y2','y3','y4','y5','y6'), facLab = 'constr')
    model.2 <- model.cfa(LY=LY, RTE= RTE, AL= AL.2, RPS = RPS,
                         indLab=c('y1','y2','y3','y4','y5','y6'), facLab = 'constr')
    
  }else if(type =='one.meth'){
    loadings <- matrix(0,6,2)
    loadings[1:6,1] <- NA
    loadings[4:6,2] <- NA
    loadingVals <- matrix(0,6,2)
    loadingVals[1:6,1] <- .7
    loadingVals[4:6,2] <- meth.loadings
    LY = bind(loadings, loadingVals)
    
    latent.cov <- matrix(0,2,2)
    diag(latent.cov) <- NA
    RPS <- binds(latent.cov,1)
    
    error.cov <- matrix(0,6,6)
    diag(error.cov) <- 1
    RTE <- binds(error.cov)
    
    factor.mean <- c(NA,NA)
    AL.1 <- bind(factor.mean,c(0,'runif(1,-1,1)')) 
    AL.2 <- bind(factor.mean,c(mean.dif,'runif(1,-1,1)')) 
    model.1 <- model.cfa(LY=LY, RPS = RPS, RTE= RTE, AL= AL.1, indLab=c('y1','y2','y3','y4','y5','y6'), facLab = c('constr', 'meth'))
    model.2 <- model.cfa(LY=LY, RPS = RPS, RTE= RTE, AL= AL.2, indLab=c('y1','y2','y3','y4','y5','y6'), facLab = c('constr', 'meth'))
    
  }else if(type == 'two.meth'){
    loadings <- matrix(0,6,3)
    loadings[1:6,1] <- NA
    loadings[1:3,2] <- NA
    loadings[4:6,2] <- NA
    loadingVals <- matrix(0,6,3)
    loadingVals[1:6,1] <- .7
    loadingVals[1:3,2] <- meth.loadings
    loadingVals[4:6,3] <- meth.loadings
    LY = bind(loadings,loadingVals)
    
    latent.cov <- matrix(0,3,3)
    diag(latent.cov) <- NA
    RPS <- binds(latent.cov,1)
    
    error.cov <- matrix(0,6,6)
    diag(error.cov) <- 1
    RTE <- binds(error.cov)
    
    factor.mean <- c(NA,NA,NA)
    AL.1 <- bind(factor.mean,c(0,'runif(1,-1,1)','runif(1,-1,1)')) 
    AL.2 <- bind(factor.mean,c(mean.dif, 'runif(1,-1,1)','runif(1,-1,1)'))
    model.1 <- model.cfa(LY=LY, RPS = RPS, RTE= RTE, AL= AL.1, indLab=c('y1','y2','y3','y4','y5','y6'), facLab = c('constr', 'meth1','meth2'))
    model.2 <- model.cfa(LY=LY, RPS = RPS, RTE= RTE, AL= AL.2, indLab=c('y1','y2','y3','y4','y5','y6'), facLab = c('constr', 'meth1','meth2'))
  }else if(type == 'interact'){
    # group 1
    loadings <- matrix(0,6,2)
    loadings[1:6,1] <- NA
    loadings[4:6,2] <- NA
    loadingVals <- matrix(0,6,2)
    loadingVals[1:6,1] <- .7
    loadingVals[4:6,2] <- meth.loadings
    LY = bind(loadings, loadingVals)
    
    latent.cov <- matrix(0,2,2)
    diag(latent.cov) <- NA
    RPS <- binds(latent.cov,1)
    
    error.cov <- matrix(0,6,6)
    diag(error.cov) <- 1
    RTE <- binds(error.cov)
    
    factor.mean <- c(NA,NA)
    AL.1 <- bind(factor.mean,c(0,'runif(1,-1,1)')) 
    model.1 <- model.cfa(LY=LY, RPS = RPS, RTE= RTE, AL= AL.1, indLab=c('y1','y2','y3','y4','y5','y6'), facLab = c('constr', 'meth'))
    
    
    # group 2
    loadings.2 <- matrix(0,6,3)
    loadings.2[1:6,1] <- NA
    loadings.2[1:3,2] <- NA
    loadings.2[4:6,3] <- NA
    loadingVals.2 <- matrix(0,6,3)
    loadingVals.2[1:6,1] <- .7
    loadingVals.2[1:3,2] <- meth.loadings
    loadingVals.2[4:6,3] <- meth.loadings
    LY.2 = bind(loadings.2,loadingVals.2)
    
    latent.cov.2 <- matrix(0,3,3)
    diag(latent.cov.2) <- NA
    RPS.2 <- binds(latent.cov.2,1)
    
    factor.mean.2 <- c(NA,NA,NA)
    AL.2 <- bind(factor.mean.2,c(mean.dif, 'runif(1,-1,1)','runif(1,-1,1)'))
    
    model.2 <- model.cfa(LY=LY.2, RPS = RPS.2, RTE= RTE, AL= AL.2, indLab=c('y1','y2','y3','y4','y5','y6'))
  }
  
  print('simulation model created')
  
  # generate a new dataframe
  est <- rep(0,1000)
  se <- rep(0,1000)
  store <- data.frame(est,se)
  
  print('data simulation begins')
  
  # monte carlo simuation
  for (i in 1:1000){
    # data simulation
    group.1 <- generate(model.1, n = sample.size)
    group.2 <- generate(model.2, n = sample.size)
    
    group.1$group <- 1
    group.2$group <- 2
    all.data <- rbind(group.1,group.2)
    
    fit <- cfa(meth, data = all.data, group= 'group',
               group.equal= c('loadings','intercepts'),
               group.partial = c('meth =~ y4','meth =~ y5','meth =~ y6'),
               meanstructure = T)
    
    para <- parameterestimates(fit)
    constr <- para[which(para$lhs =='constr'& para$op== '~1' & para$group ==2),]
    constr <- constr[,c('est','se')]
    
    store[i,] <- constr
  }
  
  end.time <- Sys.time()
  print(round((end.time - start.time),2))
  return(store)
}

















