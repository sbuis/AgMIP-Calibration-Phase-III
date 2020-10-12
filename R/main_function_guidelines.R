#' @title Apply the AgMIP phase III protocol (Crop model calibration using phenology data)
#'
#' @param optim_options List of options of the parameter estimation method:
#'    o `path_results` The path where to store the results (optional, default=getwd())
#'    o `nb_rep` The number of repetition of the minimization
#'    o `ranseed`  Set random seed so that each execution give the same results
#       If you want randomization, don't set it.
#' 
#' @param oblig_param_list Vector of names of parameters that must be estimated 
#' (list of obligatory parameters)
#'
#' @param add_param_list Vector of names of additional parameters candidate to the 
#' estimation
#' 
#' @param param_info_tot Information on all the candidate parameters to estimate.
#' A list containing:
#'    - (named) vectors of upper and lower bounds (`ub` and `lb`) (-Inf and Inf can be used),
#'    - `init_values`, A data.frame containing initial values to test for the parameters 
#'
#' @param obs_list List of observed values to use for parameter estimation
#' A `named list` (names = situations names) of data.frame containing
#' one column named Date with the dates (Date or POSIXct format) of the different observations
#' and one column per observed variables with either the measured values or NA, if
#' the variable is not observed at the given date.
#' 
#' @param model_function Crop Model wrapper function to use.
#' 
#' @param model_options List of options for the Crop Model wrapper (see help of
#' the Crop Model wrapper function used).
#' 
#' @return A data.frame containing for each set of candidate parameters, the names of the parameters 
#' and the initial and final values of the parameters, the OLS criterion and of AIC
#' 
#' This data.frame is also saved in csv and Rdata formats (AgMIP_outputs.* files, 
#' saved in optim_options$path_results)

main_function_guidelines <- function(optim_options, oblig_param_list, add_param_list, 
                                     param_info_tot, obs_list, 
                                     model_function, model_options) {
  
  candidate_params <- oblig_param_list
  best_final_values <- setNames(data.frame(matrix(data=NA, ncol=length(candidate_params),nrow=0)),
                                candidate_params)
  prev_AIC <- Inf
  count<-1
  df_outputs<-NULL
  
  while(!is.null(candidate_params)) {
    
    param_info <- lapply(param_info_tot,function(x) x[candidate_params])
    param_names <- names(param_info$ub)
    
    print(paste("Estimated parameters:",paste(param_names,collapse=" ")))
    
    # initialize parameters with the values estimated for the best AIC obtained
    param_info$init_values<-bind_rows(init_values[candidate_params],best_final_values)
    
    optim_results <-     estim_param(obs_list=obs_list,
                                     model_function=model_function,
                                     model_options=model_options,
                                     optim_options=optim_options,
                                     crit_function=crit_ols,
                                     param_info=param_info)
    
    
    optim_results$aic <- AIC(obs_list, optim_results$min_crit_value, 
                             param_nb=length(param_info$ub))
    if (optim_results$aic<min(prev_AIC)) {
      best_final_values<-optim_results$final_values
    }
    
    # Save and move results
    save(optim_results, file = file.path(optim_options$path_results,
                                         paste0("optim_results_set",count,".Rdata")))
    file.copy(from="EstimatedVSinit.pdf",
              to=paste0("EstimatedVSinit_et",count,".pdf"))
    
    print(paste("Values for the estimated parameters:",optim_results$final_values,
                collapse=" "))
    print(paste0("AIC =",optim_results$aic))
    
    df_outputs<-bind_rows(df_outputs,create_AgMIP_outputs(candidate_params, obs_list, 
                                                          optim_results, model_function, 
                                                          model_options))
    
    candidate_params <- select_param_FwdReg_AIC(oblig_param_list, add_param_list, 
                                                candidate_params, optim_results$aic, 
                                                prev_AIC)
    prev_AIC <- c(prev_AIC, optim_results$aic)
    
    count <- count+1
  }
  
  # Save the results
  save(df_outputs, file = file.path(optim_options$path_results,
                                    paste0("AgMIP_outputs.Rdata")))
  
  df <- apply(df_outputs,2,as.character)
  write.csv(df, sep=";", file=file.path(optim_options$path_results,
                                        paste0("AgMIP_outputs.csv")))
 
  return(df) 
}
