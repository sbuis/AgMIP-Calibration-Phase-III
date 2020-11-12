# Install / Load needed libraries and functions
if(!require("CroptimizR")){
  devtools::install_github("SticsRPacks/CroptimizR@*release")
  library("CroptimizR")
}
library(SticsOnR)
library(SticsRFiles)
library(dplyr)

source("R/AICc.R")
source("R/BIC.R")
source("R/select_param_FwdReg.R")
source("R/main_function_guidelines.R")
source("R/create_AgMIP_outputs.R")
source("R/complete_init_values.R")

# Define your model and associated options
# i.e. model_options argument given to estim_param that depends on your model wrapper 
model_function <- stics_wrapper
javastics_path <- "D:\\Home\\sbuis\\Documents\\WORK\\STICS\\JavaSTICS-1.40-stics-8.50"
stics_path <- file.path(javastics_path,"bin/stics_modulo")
data_dir <- "D:\\Home\\sbuis\\Documents\\WORK\\AgMIP\\CalibrationProtocol\\2ndExercise_Guidelines_2020\\AustralianDataset\\SticsData"
stics_inputs_path <- file.path(data_dir,"TxtFiles")
model_options <- stics_wrapper_options(javastics_path=javastics_path,data_dir=stics_inputs_path,parallel=TRUE)

  
# Select the observations for the parameter estimation
# i.e. set obs_list here
sit_names <- c("Erad-2010-TOS1","Erad-2010-TOS2","Erad-2010-TOS3","Minn-2010-TOS1",
               "Minn-2010-TOS2","Minn-2010-TOS3","Lake-2010-TOS1","Lake-2010-TOS2",
               "Lake-2010-TOS3","Erad-2011-TOS1","Erad-2011-TOS2","Erad-2011-TOS3",
               "Minn-2011-TOS1","Minn-2011-TOS2","Minn-2011-TOS3","Spri-2010-TOS1",
               "Spri-2010-TOS2","Spri-2010-TOS3","Spri-2011-TOS1","Spri-2011-TOS2",
               "Spri-2011-TOS3","Lake-2011-TOS1","Lake-2011-TOS2","Lake-2011-TOS3")
obs_list <- get_obs(file.path(data_dir,"XmlFiles"))
obs_list <- filter_obs(obs_list, sit_names = sit_names,include=TRUE)
obs_list <- sapply(obs_list, function(x)  x %>% dplyr::select(-ian, -mo, -jo, -jul, -Plant), simplify = FALSE)

  
# Give information on the parameters to estimate : 

## Names of obligatory parameters
oblig_param_list <- c("stlevamf","stamflax", "stflodrp", "stlevdrp", "stdrpmat")

## Names of additional candidate parameters
add_param_list <- c("tdmax", "phobase", "sensiphot", "stressdev", "phosat", "tcxstop", "jvc", "jvcmini")

param_names <- c(oblig_param_list,add_param_list)

## Bound constraints
param_info_tot <- list(lb=c(stlevamf=1, stamflax=1, stflodrp=0, stlevdrp=1, stdrpmat=1, 
                             jvc=20, jvcmini=0, stressdev=0, tdmax=15, tcxstop=30, sensiphot=0, 
                             phobase=0, phosat=12),
                       ub=c(stlevamf=Inf, stamflax=Inf, stflodrp=Inf, stlevdrp=Inf, stdrpmat=Inf, 
                            jvc=Inf, jvcmini=Inf, stressdev=1, tdmax=30, tcxstop=Inf, sensiphot=1, 
                            phobase=12, phosat=24))

## Lower and upper bounds for initial values sampling 
lb_initV <- c(stlevamf=50, stamflax=50, stflodrp=0, stlevdrp=350, stdrpmat=100, 
              jvc=20, jvcmini=0, stressdev=0, tdmax=15, tcxstop=30, sensiphot=0, 
              phobase=0, phosat=12)
ub_initV <- c(stlevamf=700, stamflax=700, stflodrp=300, stlevdrp=2000, stdrpmat=1300, 
              jvc=70, jvcmini=20, stressdev=1, tdmax=30, tcxstop=50, sensiphot=1, 
              phobase=12, phosat=24)


# Define parameter estimation algorithm options
optim_options=list(path_results = getwd(), # path where to store the results (graph and Rdata)
                   nb_rep = 4)             # Number of repetitions of the minimization
                   ranseed = 1234)         # set random seed so that each execution give the same results
                                           # If you want randomization, don't set it.
optim_options$maxeval=1000

##########################################

main_function_guidelines(optim_options, oblig_param_list, add_param_list, 
                         param_info_tot, lb_initV, ub_initV, obs_list, 
                         model_function, model_options, info_crit_name="AICc")

main_function_guidelines(optim_options, oblig_param_list, add_param_list, 
                         param_info_tot, lb_initV, ub_initV, obs_list, 
                         model_function, model_options, info_crit_name="BIC")
						 
print(paste0("Results saved in ",optim_options$path_results))