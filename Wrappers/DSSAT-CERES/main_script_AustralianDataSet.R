# Install / Load needed libraries and functions
#  devtools::install_github("SticsRPacks/CroptimizR")


library("CroptimizR") 
#}

library("dplyr")		
library("readxl")
library("tidyr")
library("DSSAT")
library("CroPlotR")
library("lubridate")

source("R/AICc.R")
source("R/BIC.R")
source("R/select_param_FwdReg.R")
source("R/main_function_guidelines.R")
source("R/create_AgMIP_outputs.R")
source("R/complete_init_values.R")
source("DSSAT_wrapper.R")


########## TO BE ADAPTED TO YOUR CASE ....

# Define your model and associated options
# i.e. model_options argument given to estim_param that depends on your model wrapper 
model_function <- DSSAT_wrapper 

model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT47'   
model_options$DSSAT_exe <- 'DSCSM047.EXE'   
model_options$Crop <- "Wheat"
model_options$Genotype <- "Genotype" 
model_options$ecotype_filename <- "WHCER047.ECO"  
model_options$cultivar_filename <- "WHCER047.CUL" 

# Adapt to Australian or French dataset case ...
model_options$project_file <- 'AUST0124.WHX'  # e.g. 'CSIR1066.WHX', 'AUST0124.WHX'
model_options$ecotype <- "AUWH01"      # e.g. UKWH03 "AUWH01"
model_options$cultivar <- "AU0001"     # Apache "AU0001","FR1801"


# Select the observations for the parameter estimation
# i.e. set obs_list here
# obs_list must be a named list of data.frames or tibbles (similar to sim_list 
# returned by the model wrapper, see ? estim_param for more information).
# BE CAREFUL: data.frames/tibbles in obs_list MUST ONLY CONTAIN ONE COLUMN PER OBSERVED VARIABLE,
# and ONE COLUMN "Date".The presence of any other column will perturbe the computation of AICc and BIC.

####### set "F" for french dataset or "A" for Australian dataset ##########
data="A" # "A" or "F"

pathObsFile <- 
  
selected_stages <- NULL
if (data=="F") {
  obs_data <- read_excel(file.path(pathObsFile,"FrancetrainingZadoksdates.xlsx"),
                         sheet = "Apache")
} else if (data=="A") {
  filename <- file.path(pathObsFile,"training Zadoks dates_v2_DSSAT.txt")
print(filename)
    obs_data <- read.table(file = filename,sep = "\t",row.names = NULL,header = TRUE, stringsAsFactors=FALSE)
  
  ############### HERE SOME PARTICULAR STAGES ARE SELECTED, COMMENT THE LINE IF YOU WANT TO USE ALL OBSERVED STAGES  ####################
  selected_stages <- c("Zadok30","Zadok65","Zadok90")  
}

format="%d/%m/%Y"
obs_data_final <- obs_data %>% mutate(Orig = as.Date(paste(year(as.Date(sowDay, format=format)),"01","01",sep="-"))) %>% 
  rowwise() %>% 
  mutate(across(grep("Zadok",names(obs_data),value=TRUE), function(x) julian(as.Date(x,format=format), origin=Orig))) %>%
  mutate(Date = as.POSIXct(paste(Year,"12","31",sep="-"),format="%Y-%m-%d", tz="UTC")) %>%  
  filter(!is.na(Date)) %>%
  mutate(situation=TRNO)
if (is.null(selected_stages)) {
  selected_stages_idx <- grep("Zadok",names(obs_data_final))
} else {
  selected_stages_idx <- which(names(obs_data_final) %in% selected_stages)
}
obs_list <- obs_data_final %>%
  select(Date, situation, selected_stages_idx) %>%
  as_tibble() %>% group_split(situation)


names(obs_list) <- sapply(obs_list, function(x) x$situation[1])
obs_list <- lapply(obs_list, function(x) select(x,!situation))
obs_list <- lapply(obs_list,function(x) x[!duplicated(x$Date),])  # filter observations that have the same dates
obs_list <- lapply(obs_list,function(x) x %>% select_if(~!all(is.na(.))))  # filter NA
obs_list[sapply(obs_list,length)==1] <- NULL  # filter non observed situations


# If you want to take into account only a sub-part of the observed stages, e.g. GSTD==30 and GTD==55, proceed as in the following line
# obs_list <- lapply(obs_list, function(x) filter(x,GSTD==30 | GSTD==55))

#-------------
## for the Australian case :
#obs_data <- read_tsv("C:/JQ2020/AgMIPCalibration/training Zadoks dates_v2_DSSAT.txt")
#obs_data_final <- obs_data %>% gather(GSTD, Date, Zadok1:Zadok100) %>%
#  mutate(GSTD = as.numeric(gsub("Zadok", "", GSTD))) %>%
#  mutate(Date = as.POSIXct(Date,format="%d/%m/%Y", tz="UTC")) %>%
#  filter(!is.na(Date)) %>%
#  mutate(situation=TRNO)
#obs_list <- obs_data_final %>%
#  select(Date, GSTD, situation) %>%
#  group_split(situation)
#names(obs_list) <- sapply(obs_list, function(x) x$situation[1])
#obs_list <- lapply(obs_list, function(x) select(x,!situation))
#obs_list <- lapply(obs_list,function(x) x[!duplicated(x$Date),])  # filter observations that have the same dates

# If you want to take into account only a sub-part of the observed stages, e.g. GSTD==30 and GTD==55, proceed as in the following line
# obs_list <- lapply(obs_list, function(x) filter(x,GSTD==30 | GSTD==55))
#-----------

# Give information on the parameters to estimate : 

## Names of obligatory parameters

oblig_param_list <- 

## Names of additional candidate parameters
add_param_list <- 

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

param_info_tot <- 

### Vectors of lower and upper bounds for initial values sampling 
### (only needed if param_info_tot$init_values is not provided or if values are not provided 
###  for all parameters AND repetitions). 
### Can be different or same as param_info_tot$lb and param_info_tot$ub, as you want.
### e.g. lb_initV <- c(p1=0, p2=3, p3=100)
###      ub_initV <- c(p1=1, p2=7, p3=300)

lb_initV <- 
ub_initV <- 																						 

# Define parameter estimation algorithm options
optim_options=list(path_results = pathObsFile, # path where to store the results (graph and Rdata)
              #     nb_rep = 8,             # Number of repetitions of the minimization
                   ranseed = 1234,         # set random seed so that each execution give the same results
                   # If you want randomization, don't set it.
                   maxeval=500)            # 500 Maximal number of criterion evaluation
# SET IT TO A LOW VALUE (e.g. 3) TO TEST THE SCRIPT to dramatically 
# reduce computational cost
# SET IT TO A LARGE VALUE (> 500) FOR REAL APPLICATION OF THE PROTOCOL 
# Number of repetitions: if nb_rep includes only one value, all steps will use the same number of repetitions
#                        if nb_rep includes 2 values, the first step will use the first value as repetition number and ALL the others will use the second values
#                        otherwise, nb_rep can include as many values as number of steps if you want to define different values for each step
nb_rep <- c(20,5) 

##########################################
main_function_guidelines(optim_options, oblig_param_list, add_param_list, 
                         param_info_tot, lb_initV, ub_initV, obs_list, 
                         model_function, model_options, info_crit_name="AICc",nb_rep=nb_rep)
main_function_guidelines(optim_options, oblig_param_list, add_param_list, 
                         param_info_tot, lb_initV, ub_initV, obs_list, 
                         model_function, model_options, info_crit_name="BIC",nb_rep=nb_rep)

print(paste0("Results saved in ",optim_options$path_results))
