library(here)
library(readxl)
library(stringr)
library(lubridate)
library(dplyr)
library(nimble)
library(MCMCvis)
library(coda)

#read in data 
#I don't have the other data files for Clearwater and Ellsworth - so this is Hoh only
#source(here("Mia_capstone",'read_data.r'))

#put all the processing steps in a function so you can apply them to any input file 
get.simple <- function(input){
  
#keep only what you need 
input <- input %>% select(source_file, `Common name`)
colnames(input) <- c("source_file","common_name")

###MAKE SURE TO CHECK FOR MORE NON-BIRDS IN OTHER FILES! 
#get rid of non-birds 
input <- input[input$common_name != "Abiotic Aircraft", ]
input <- input[input$common_name != "Abiotic Logging", ]
input <- input[input$common_name != "Abiotic Rain", ]
input <- input[input$common_name != "Abiotic Vehicle", ]
input <- input[input$common_name != "Abiotic Wind", ]
input <- input[input$common_name != "Biotic Anuran", ]
input <- input[input$common_name != "Biotic Insect", ]

#make dates in the file 
input$date <- make_date(year = substr(input$source_file,10,13),
                        month = substr(input$source_file,14,15), 
                        day =  substr(input$source_file,16,17))

#collapse to a record for species by day, with a count of the observations 
daily <- input %>% group_by(common_name,date) %>% tally() 

return(daily)
} 

#need to get these for Ellsworth and Clearwater 
H1.s <- as.data.frame(get.simple(H1))  #5-8 to 6-23 plus 7-1 
H2.s <- as.data.frame(get.simple(H2))  #5-8 to 6-23
H3.s <- as.data.frame(get.simple(H3))  #5-8 to 6-14
H4.s <- as.data.frame(get.simple(H4))  #5-8 to 6-13 with days missing 05/18,05/21,05/24,05/26,06/03,06/06,06/09,06/12
H5.s <- as.data.frame(get.simple(H5))  #5-8 to 6-23 with days missing incl stretch 5-20 to 6-1 and 06/14,06/16,06/18,06/20,06/22
H6.s <- as.data.frame(get.simple(H6))  #5-8 to 6-24
H7.s <- as.data.frame(get.simple(H7))  #5-8 to 6-25
H8.s <- as.data.frame(get.simple(H8))  #5-8 to 6-23
H9.s <- as.data.frame(get.simple(H9))  #5-8 to 6-16
H10.s <- as.data.frame(get.simple(H10))  #5-8 to 6-15
H11.s <- as.data.frame(get.simple(H11))  #5-8 to 6-22
H12.s <- as.data.frame(get.simple(H12))  #5-8 to 6-22
H13.s <- as.data.frame(get.simple(H13))  #5-13 to 6-23 with missing days 05/14,05/16,05/18
H14.s <- as.data.frame(get.simple(H14))  #5-8 to 6-24
H15.s <- as.data.frame(get.simple(H15))  #5-8 to 6-12 
H16.s <- as.data.frame(get.simple(H16))  #5-8 to 6-24 
H17.s <- as.data.frame(get.simple(H17))  #5-8 to 6-24 
H18.s <- as.data.frame(get.simple(H18))  #5-8 to 6-20
H19.s <- as.data.frame(get.simple(H19))  #5-8 to 6-14
H20.s <- as.data.frame(get.simple(H20))  #5-8 to 6-12 
H21.s <- as.data.frame(get.simple(H21))  #5-8 to 6-11
H22.s <- as.data.frame(get.simple(H22))  #5-8 to 6-08

#we can also automate above (for Hoh) 
for(i in 1:22){
  file <- get(paste0("H",i))
  name <-  paste0("H",i,".s")
  assign(name,as.data.frame(get.simple(file)))
}


#get a comprehensive list of dates
#may need to change this if the other sites have more dates we want to use 
dates <- sort(unique(H7.s$date))

#get a comprehensive list of species 
#may need to expand this when we add other sites  
all <- c(H1.s$common_name)
for(i in 2:22){
  new <- paste0("H",i,".s")
  result <- get(new)
  names <- result$common_name
  all <- c(all,names)
}
species <- sort(unique(all))

#create an array of species by date by point  
all.array <- array(NA,dim = c(length(species),length(dates),22))
for(i in 1:22){
  site <- get(paste0("H",i,".s"))
  for(s in 1:length(species)){
    for(t in 1:length(dates)){
      spp <- which(site$common_name == species[s]) 
      dt <- which(site$date == dates[t])
      inter <- intersect(spp,dt)
      if(length(inter > 0)){
        all.array[s,t,i] <- site$n[inter]
      }
    }
  }
}
#now we can make this array 0 (if NA) or 1 (if otherwise)
all.obs <- all.array
all.obs[is.na(all.array==TRUE)] <- 0
all.obs[which(all.array>0)] <- 1


#Look at how often different species are seen
#this gives us a count of the days * sites where a given species was seen
#apply(all.obs,c(1),sum)
#for WCSP = 1
#for RCKI = 12 
#for OSFL = 27 
#all other species > 50 


#we also need to create an effort matrix, which is dimensions dates by sites
#I am assuming here that if there are no detections of any species in a day, there was no effort on that day  
effort <- matrix(0,nrow = length(dates),ncol = 22)
for(i in 1:22){
  site <- get(paste0("H",i,".s"))
  for(t in 1:length(dates)){
    dt <- which(site$date == dates[t])
    if(length(dt > 0)){
      effort[t,i] <- 1 
    }
  }
}

######################################################################
#                                                                    #  
#                      Model                                         #
#                                                                    #    
######################################################################

##NIMBLE code 
occ1 <- nimbleCode( { 
  
#Likelihood
for(p in 1:n.points){
  for(s in 1:n.species){
    
    # State Process  
    z[s,p] ~ dbern(psi[s,p])
    
    # Observation Process  
    for(t in 1:n.days){
      
      all.obs[s,t,p] ~ dbern(z[s,p] * p.det[s,t,p] * effort[t,p])  #state * p * effort (0 or 1)
      
      #observation model
      logit(p.det[s,t,p]) <- int.p  #+ p.rand.point[p] + p.rand.species[s]
      
    }#t replicate days
        
        
    #occupancy model 
    logit(psi[s,p]) <- int.psi  #+ psi.rand.species[s]
  }#s species
}#p point
  
int.p ~ dnorm(0,1)
int.psi ~ dnorm(0,1)


#Random effects 
#for(p in 1:n.points){
#  p.rand.point[p] ~ dnorm(0,sd = sd.p.site)
#}
#for(s in 1:n.species){
#  p.rand.species[y] ~ dnorm(0,sd = sd.p.species)
#}
#for(s in 1:n.species){
#  psi.rand.species[s] ~ dnorm(0,sd = sd.psi.species)
#}
#sd.p.site ~ dunif(0,1)
#sd.p.species ~ dunif(0,1)
#sd.psi.species ~ dunif(0,1)

})

######################################################################
#                                                                    #  
#                      Data and Constants                            #
#                                                                    #    
######################################################################
  
# Bundle data
data <- list(all.obs = all.obs)
  
n.points = dim(all.obs)[3]
n.species = dim(all.obs)[1]
n.days = dim(all.obs)[2]
constants <- list(effort = effort, n.points = n.points, n.species = n.species, n.days = n.days)
  
######################################################################
#                                                                    #  
#                             Inits                                  #
#                                                                    #    
######################################################################
  
z.init <- matrix(0,n.species,n.points)
for(s in 1:n.species){
  for(p in 1:n.points){
    if(any(all.obs[s,,p]==1)){
      z.init[s,p] <- 1 
    }  
  }
}
inits <- list(z=z.init)
  
######################################################################
#                                                                    #  
#                         Run the model                              #
#                                                                    #    
######################################################################
  
# Parameters monitored
params <- c("int.p","int.psi") 

# MCMC settings
ni <- 150000
nt <- 1
nb <- 25000
nc <- 3

Rmodel1 <- nimbleModel(code = occ1, constants = constants, data = data,
                         check = FALSE, calculate = FALSE, inits = inits)
conf1 <- configureMCMC(Rmodel1, monitors = params, thin = nt, useConjugacy = FALSE)
Rmcmc1 <- buildMCMC(conf1)
Cmodel1 <- compileNimble(Rmodel1, showCompilerOutput = FALSE)
Cmcmc1 <- compileNimble(Rmcmc1, project = Rmodel1)
  
## Run MCMC ####
out <- runMCMC(Cmcmc1, niter = ni, nburnin = nb , nchains = nc, inits = inits,
                 setSeed = FALSE, progressBar = TRUE, samplesAsCodaMCMC = TRUE)
  
out.all <- rbind(out$chain1,out$chain2,out$chain3)
  
R.hat <- gelman.diag(out[,c(1,2)],multivariate=TRUE)$mpsrf

