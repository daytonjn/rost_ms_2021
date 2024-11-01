
#Create a function for simulated 'landscape'
#Later on, this function is called over each iteration of matrix
#mut_rate (min,max)

#Sample 1, 5

create.land.10stages_Nisbet.MF <- function(mut_rate){

  
#First set up matrices for local demographies

  S.epoch <- matrix(c(0,0,0,0,0,0,0,0,0,0,
                      0.376,0,0,0,0,0,0,0,0,0,
                      0,0.835,0,0,0,0,0,0,0,0,
                      0,0,0.835,0,0,0,0,0,0,0,
                      0,0,0,0.835,0.835,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0.376,0,0,0,0,
                      0,0,0,0,0,0,0.835,0,0,0,
                      0,0,0,0,0,0,0,0.835,0,0,
                      0,0,0,0,0,0,0,0,0.835,0.835),byrow = TRUE, nrow = 10)
  
  R.epoch <- matrix(c(0,0,0,0,0,0,0.011,0.28,0.47,0.55,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0.012,0.30,0.50,0.59,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0),byrow = TRUE, nrow = 10)
  
  M.epoch <- matrix(c(00,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0.02,0,0,0,0,0,0,0,0,
                      0,0,0.51,0,0,0,0,0,0,0,
                      0,0,0,0.85,0,0,0,0,0,0,
                      0,0,0,0,1,0,0,0,0,0), byrow = TRUE, nrow = 10)

 landscape.new.empty() %>%
 #h = # of different subpopulations within landscape, s = # stages, #cg = current generation, ce = current epoch (i.e., year), totgen = total generations to simulate
 #xdim = if habitats arrayed in rectangle, number of x grid cells
 landscape.new.intparam(h = 1, s = 10, totgen=20, ce = 0) %>% 
 
 #re = 1 = randomly pick a new epoch after an epoch completes, 0 = epochs are chosen in order,
 #rd, 0=demographies assigned in order, 1=randomly pick new demography
 #mp, 1=multiple paternity (default), 0 = entire families from a single mating
 #dd, density dependence... if dd=1, then 2 of each local demog. matrix MUST be defined: first using new.local.demo representing low density and at k=1 for demography at high population density
 landscape.new.switchparam(mp = 0)  %>%
 landscape.new.floatparam(s=0)  %>% #s=selfing rate

 landscape.new.local.demo(S.epoch, R.epoch, M.epoch)  %>%

 landscape.new.epoch(S = S.epoch, R = R.epoch, M = M.epoch, 
                             carry = 20000)  %>%

#Generate MS Loci and Incorporate Point Estimates of 1997-observed allele frequencies
#Use of '0' denotes an allele that was absent from 1997 sample and present in 2016
   
#MsSh18
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 4,
                             frequencies = c(0.625, 0.031, 0.219, 0.125))  %>%
#MsSh20
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, numalleles = 2,
                             frequencies = c(0.953, 0.047))  %>%
#AAC20
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 3,
                             frequencies = c(0, 0.344, 0.656))  %>%
#AAT20
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 3,
                             frequencies = c(0, 0.828, 0.172))  %>%
#RBG27
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 4,
                             frequencies = c(0, 0, 0.859, 0.141))  %>%
#RBG29
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 2,
                             frequencies = c(0, 1))  %>%
#K16
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 4,
                             frequencies = c(0, 0, 0.266, 0.734))  %>%

#aat27
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 2,
                             frequencies = c(0, 1))  %>%

#LZ2
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 2,
                             frequencies = c(0.188, 0.813))  %>%
#RBG18
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 3,
                             frequencies = c(0.391, 0.422, 0.188))  %>%
#K6
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 2,
                             frequencies = c(0.069, 0.931))  %>%
#Calbo2
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 14,
                             frequencies = c(0.065, 0.016, 0.081, 
                                             0.048, 0.113, 0.194,
                                             0.065, 0.032, 0.113, 
                                             0.145, 0.081, 0,
                                             0.048, 0))  %>%
#calbo1
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 4,
                             frequencies = c(0, 0, 0.266, 0.734))  %>%
#calbo5
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 2,
                             frequencies = c(0.703, 0.297))  %>%
#calbo35
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 7,
                             frequencies = c(0.016, 0.172, 0.109, 
                                             0.219, 0.359, 0.125, 0))  %>%
#calbo40
 landscape.new.locus( type = 1, ploidy = 2, 
                             mutationrate = mut_rate, transmission = 0, numalleles = 9,
                             frequencies = c(0.016, 0.172, 0.094,
                                             0.219, 0.109, 0.063, 
                                             0.156, 0.125, 0.047))  %>%
 landscape.new.individuals(c(2178, 1275, 835, 525, 2976,
                             2359, 1484, 1043, 705, 3938)) 

 
}

