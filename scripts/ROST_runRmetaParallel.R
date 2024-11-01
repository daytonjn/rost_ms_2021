#Title: Spatial/Temporal Changes in Genetic Variation among the NW Atlantic Population of Roseate Terns
#Date: 2020/07/08
#Authors: Jacob Dayton, jacob.dayton@tufts.edu

#Purpose: Generate and simulate an rmetasim landscape for a given number of generations
#Input: A vector with the following elements: [1]locus mutation rate, [2] number years to simulate, [3] stages to sample individuals from, [4] 
#Outputs a dataframe containing various metrics used to assess change in results/fit of simulation through time

#R Environment, v.3.6.3 (2020-02-29)

runForwardSim_1iter <- function(x) {
  
  res.df<- as.data.frame(matrix(ncol = 6, nrow = x[2], dimnames = list(paste0("Year_", 1:x[2]),c("Na", "Ho", "He", "F", "NB", "F%") )))
  
  rland <- create.land.10stages.MF(x[1]) #create.land.10stages.MF(x[1]) OR create.land.10stages_Nisbet.MF(x[1])
  
  for (i in 1:x[2]){ #i keeps track of simulation year
    
    temp.sample <- landscape.sample(rland, ns=16, svec = c(x[3], x[4]))
    temp <- temp.sample %>% landscape.allelecount()
    res.df$Na[i] <- table(temp$loc) %>% mean() %>% round(digits = 2)
    res.df$Ho[i]<- temp.sample %>% landscape.obs.het() %>% mean()  %>% round(digits = 3)
    res.df$He[i] <- temp.sample %>% landscape.exp.het() %>% mean() %>% round(digits = 3)
    res.df$F[i] <- 1-(res.df$Ho[i]/res.df$He[i]) %>% round(digits = 3)
    
    temp.nc <- c(table(rland$individuals[,1])) #Record number of individuals in each stage
    res.df$NB[i] <- sum(temp.nc[5], temp.nc[10])
    res.df$`F%`[i] <- temp.nc[10]/res.df$NB[i] %>% round(digits = 3)
    
    
    rland <- rland %>% landscape.advance() %>% landscape.carry()  %>% landscape.extinct() %>% landscape.survive() %>% landscape.reproduce()
    rland$intparam$currentgen %>% print()
    
  }
  

  return(res.df)
}

runForwardSim_1iter_Nisbet <- function(x) {
  
  res.df<- as.data.frame(matrix(ncol = 6, nrow = x[2], dimnames = list(paste0("Year_", 1:x[2]),c("Na", "Ho", "He", "F", "NB", "F%") )))
  
  rland <- create.land.10stages_Nisbet.MF(x[1]) #create.land.10stages.MF(x[1]) OR create.land.10stages_Nisbet.MF(x[1])
  
  for (i in 1:x[2]){ #i keeps track of simulation year
    
    temp.sample <- landscape.sample(rland, ns=16, svec = c(x[3], x[4]))
    temp <- temp.sample %>% landscape.allelecount()
    res.df$Na[i] <- table(temp$loc) %>% mean() %>% round(digits = 2)
    res.df$Ho[i]<- temp.sample %>% landscape.obs.het() %>% mean()  %>% round(digits = 3)
    res.df$He[i] <- temp.sample %>% landscape.exp.het() %>% mean() %>% round(digits = 3)
    res.df$F[i] <- 1-(res.df$Ho[i]/res.df$He[i]) %>% round(digits = 3)
    
    temp.nc <- c(table(rland$individuals[,1])) #Record number of individuals in each stage
    res.df$NB[i] <- sum(temp.nc[5], temp.nc[10])+0.85*(temp.nc[4] + temp.nc[9])
    res.df$`F%`[i] <- (temp.nc[10]+0.85*temp.nc[9])/res.df$NB[i] %>% round(digits = 3) #REMEMBER TO EDIT IF WE NEED
    
    
    rland <- rland %>% landscape.advance() %>% landscape.carry()  %>% landscape.extinct() %>% landscape.survive() %>% landscape.reproduce()
    rland$intparam$currentgen %>% print()
    
  }
  
  
  return(res.df)
}
