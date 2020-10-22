# Install / Load needed libraries and functions
if(!require("CroptimizR")){
  devtools::install_github("SticsRPacks/CroptimizR@*release")
  library("CroptimizR")
}
library(dplyr)				 

source("R/AIC.R")
source("R/select_param_FwdReg_AIC.R")
source("R/main_function_guidelines.R")
source("R/create_AgMIP_outputs.R")



########## TO BE ADAPTED TO YOUR CASE ....

# Define your model and associated options
# i.e. model_options argument given to estim_param 
# ... that depends on your model wrapper 
model_function <- 
model_options <- 

  
# Select the observations for the parameter estimation
# i.e. set obs_list here
obs_list <- 

  
# Give information on the parameters to estimate : 

## Bounds for all parameters (obligatory and additional) (e.g. lb=c(p1=0, p2=0, ...); ub=c(p1=1, p2=5, ...))
param_info_tot=list(lb=c(),
                    ub=c())

## Names of obligatory parameters
oblig_param_list <- c()

## Names of additional candidate parameters
add_param_list <- c()


# Define parameter estimation algorithm options
optim_options=list(path_results = getwd(), # path where to store the results (graph and Rdata)
                   nb_rep = 4,             # Number of repetitions of the minimization
                   ranseed = 1234)         # set random seed so that each execution give the same results
                                           # If you want randomization, don't set it.

##########################################


main_function_guidelines(optim_options, oblig_param_list, add_param_list, 
                         param_info_tot, obs_list, 
                         model_function, model_options)