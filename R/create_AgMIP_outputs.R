#' @title Fill a data.frame with the optim_resultsults required by the AgMIP calibration PhaseIII protocol
#'
#' @param candidate_params Vector of names of current candidate parameters 
#'
#' @param obs_list List of observed values to use for parameter estimation
#' 
#' @param optim_results Results of the parameter estimation, as given by estim_param, 
#' for the current candidate parameters
#' 
#' @param model_function Crop Model wrapper function to use.
#' 
#' @param model_options List of options for the Crop Model wrapper (see help of
#' the Crop Model wrapper function used).
#' 
#' @return A data.frame containing for each set of candidate parameters, the names of the parameters 
#' and the initial and final values of the parameters, the OLS criterion and of AIC
#' 

create_AgMIP_outputs <- function(candidate_params, obs_list, optim_results, model_function, 
                                 model_options) {
  
  # Compute initial value of criterion and AIC (for the "best" repetition)
  obs_list_ini <- obs_list
  model_results <- model_function(model_options = model_options, 
                                  param_values = optim_results$init_values[optim_results$ind_min_crit,], 
                                  sit_names = names(obs_list),
                                  sit_var_dates_mask = obs_list)    
  
  obs_sim_list <- CroptimizR:::intersect_sim_obs(model_results$sim_list, obs_list)
  init_crit <- crit_ols(obs_sim_list$sim_list, obs_sim_list$obs_list)
  init_aic <- AIC(obs_sim_list$obs_list, init_crit, param_nb=length(candidate_params))
  final_aic <- AIC(obs_list, optim_results$min_crit_value, param_nb=length(candidate_params))
  
  # Gather optim_resultsults
  df <- data.frame(param_names=NA, init_values=NA, 
                   ini_crit=init_crit, ini_aic=init_aic, 
                   final_values=NA, final_crit=optim_results$min_crit_value, 
                   final_aic=final_aic)
  df$init_values[1]=list(optim_results$init_values[optim_results$ind_min_crit,])
  df$final_values[1]=list(optim_results$final_values)
  df$param_names[1]=list(candidate_params)
  
  return(df)
  
}
