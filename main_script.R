# Install / Load needed libraries and functions
if(!require("CroptimizR")){
  devtools::install_github("SticsRPacks/CroptimizR@*release")
  library("CroptimizR")
}
library(dplyr)				 

source("R/AICc.R")
source("R/BIC.R")
source("R/select_param_FwdReg.R")
source("R/main_function_guidelines.R")
source("R/create_AgMIP_outputs.R")
source("R/complete_init_values.R")



########## TO BE ADAPTED TO YOUR CASE ....

# Define your model and associated options
# i.e. model_options argument given to estim_param that depends on your model wrapper 
model_function <- 
model_options <- 

  
# Select the observations for the parameter estimation
# i.e. set obs_list here
obs_list <- 

  
# Give information on the parameters to estimate : 

## Names of obligatory parameters
oblig_param_list <- c()

## Names of additional candidate parameters
add_param_list <- c()

param_names <- c(oblig_param_list,add_param_list)

## Information on all estimated parameters (obligatory and additional) 

### param_info_tot must contains:
###   - named vector of lower and upper bounds (lb and ub) THAT WILL BE USED FOR 
###     CONSTRAINING THE RANGE OF PARAMETERS DURING MINIMIZATION PROCESS. 
###     Bounds must be defined FOR ALL parameters, set -Inf and Inf if you don't 
###     need to constrain the bounds.
###      e.g.   param_info_tot <- list(lb=setNames(rep(-Inf,length(param_names)), param_names),
###                                    ub=setNames(rep(Inf,length(param_names)), param_names))
###             if you want to set -Inf and Inf as lower and upper bounds for all parameters
###      or 
###             param_info_tot <- list(lb=c(p1=0, p2=1, p3=0),
###                                    ub=c(p1=1, p2=10, p3=500))
###             if you want to set [0,1], [1.2, 10] and [50, 500] for parameters p1, p2 and p3
###   - initial values (init_values) if you want to provide some. Must be a data.frame, 
###     one column per parameter, one row per repetition of the minimization.
###     If provided, should contain one column for each candidate parameter (i.e. 
###     obligatory and additional ones). Use NA if you don't want to provide a value 
###     for some parameters and/or repetitions. It is not mandatory to define as 
###     many rows as number of repetitions (optim_options$nb_rep). Missing values 
###     will be automatically completed by random sampling within lb_initV and ub_initV.
###       e.g. param_info_tot$init_values <- data.frame(p1=c(0.2,0.4), p2=c(5, NA), p3=c(70, 90))
### 

param_info_tot <- list(lb=c(),
                       ub=c())


### Vectors of lower and upper bounds for initial values sampling 
### (only needed if param_info_tot$init_values is not provided or if values are not provided 
###  for all parameters AND repetitions). 
### Can be different or same as param_info_tot$lb and param_info_tot$ub, as you want.
### e.g. lb_initV <- c(p1=0, p2=3, p3=100)
###      ub_initV <- c(p1=1, p2=7, p3=300)
lb_initV <- c()
ub_initV <- c()																								 

# Define parameter estimation algorithm options
optim_options=list(path_results = getwd(), # path where to store the results (graph and Rdata)
                   nb_rep = 4,             # Number of repetitions of the minimization
                   ranseed = 1234,         # set random seed so that each execution give the same results
                                           # If you want randomization, don't set it.
                   maxeval=500)            # Maximal number of criterion evaluation
                                           # SET IT TO A LOW VALUE (e.g. 3) TO TEST THE SCRIPT to dramatically 
										   # reduce computational cost
                                           # SET IT TO A LARGE VALUE (> 500) FOR REAL APPLICATION OF THE PROTOCOL 
							
							
##########################################


main_function_guidelines(optim_options, oblig_param_list, add_param_list, 
                         param_info_tot, lb_initV, ub_initV, obs_list, 
                         model_function, model_options, info_crit_name="AICc")

main_function_guidelines(optim_options, oblig_param_list, add_param_list, 
                         param_info_tot, lb_initV, ub_initV, obs_list, 
                         model_function, model_options, info_crit_name="BIC")
						 
print(paste0("Results saved in ",optim_options$path_results))