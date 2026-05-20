library(here)
library(readxl)
library(stringr)
library(lubridate)
library(dplyr)
library(nimble)
library(MCMCvis)
library(coda)

#read in data 
#MIA: you can go to the file read_data.r and enter the rest of the data files there 
#MIA: you will have to put all the data files in the data folder 
source(here("Mia_capstone",'read_data.r'))

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

daily.f1 <- get.simple(f1)
daily.f2 <- get.simple(f2)
#MIA: enter a corresponding line for each file 

#MIA: 
#once all of the daily files are read in, we need to create 
#a species-by-date matrix with 1s when species was seen and 0s when species was not 


start.time <- Sys.time() 

# MCMC settings
ni <- 150000
nt <- 1
nb <- 25000
nc <- 3

  
  ######################################################################
  #                                                                    #  
  #                      Data and Constants                            #
  #                                                                    #    
  ######################################################################
  
  # Bundle data
  data <- list()
  
  constants <- list()
  
  ######################################################################
  #                                                                    #  
  #                             Inits                                  #
  #                                                                    #    
  ######################################################################
  
  z.init <- matrix(0,nrow=n.points,ncol=n.years)
  for(s in 1:n.points){
    for(y in 1:n.years){
      if(any(spp.i[s,y,]==1)){
        z.init[s,y] <- 1 
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
  params <- c("int.p","sd.p.site","sd.p.year","int.psi","beta.site.type","beta.year","beta.year2","w.sitetype","w.year","w.year2") 
  
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
  
  R.hat[i] <- gelman.diag(out[,c(2,3,4,5,6,7,8)],multivariate=TRUE)$mpsrf
  
  write.csv(out.all,paste("results/occ_run4.",spp.names[i], ".csv",sep=""))
  
  print(i)
  
}
(elapsed <- Sys.time() - start.time)

#print R.hat 
R.hat 




