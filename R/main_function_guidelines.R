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
#'    - `ub` and `lb`: named vectors of upper and lower bounds, THAT WILL BE USED 
#'    FOR CONSTRAINING THE RANGE OF PARAMETERS DURING MINIMIZATION PROCESS. 
#'    Bounds must be defined FOR ALL parameters, set -Inf and Inf if you don't need 
#'    to constrain the bounds.
#'    - `init_values`, A data.frame containing initial values for the parameters.
#'    One column per parameter, one line per repetition of the minimization.
#'    Set it to NULL if you want all the initial values to be automatically sampled 
#'    (within bounds `lb_initV` and `ub_initV`).
#'    If you want to provide initial values for only a subpart of the parameters or repetitions, 
#'    set NA for parameters and/or repetitions for which you do not provide values.
#'
#' @param lb_initV Named vector of parameters' lower bounds to use for sampling initial values
#' 
#' @param ub_initV Named vector of parameters' upper bounds to use for sampling initial values
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
#' @param digits (optional) Number of digits to take into account for outputs printing format
#' 
#' @param info_crit_name Name of the information criterion to use for parameter selection ("AICc" or "BIC")
#' 
#' @param satisfy_par_const (optional) Function for inequality constraints between parameters
#' 
#' @param forced_param_values_tot (optional) Vector of values of the parameters to force 
#' when they are not estimated, if they have to take a value different from the model input files
#' 
#' @param nb_rep (optional, c(20,5) by default) Vector containing the number of repetition for the different steps. 
#' If nb_rep includes only one value, all steps will use the same number of repetitions
#'  If nb_rep includes 2 values, the first step will use the first value as repetition number and ALL the others will use the second values
#'  If nb_rep includes n values, the first n steps will use the first n values as repetition number and the next ones will use the nth value.
#'  
#' @return A data.frame containing for each set of candidate parameters, the names of the parameters 
#' and the initial and final values of the parameters, the OLS criterion and of the information criterion used.
#' 
#' This data.frame is also saved in csv and Rdata formats (AgMIP_outputs.* files, 
#' saved in optim_options$path_results)
#' 
#' @details checkpoint_AICc_set#.Rdata (resp. checkpoint_BIC_set#.Rdata) files will be stored at the end of each step. 
#' Renaming one of them in checkpoint_AICc.Rdata (resp. checkpoint_BIC.Rdata) and calling again main_function_guidelines
#' will make the process restart from the next step (i.e. with the next candidate).
#' 

main_function_guidelines <- function(optim_options, oblig_param_list, add_param_list, 
                                     param_info_tot, lb_initV=NULL, ub_initV=NULL, 
                                     obs_list, model_function, model_options,
                                     info_crit_name, digits=3, satisfy_par_const=NULL, 
                                     forced_param_values_tot=NULL, nb_rep=c(20,5)) {
  path <- getwd()
  crit_type <- info_crit_name

  if (file.exists(paste0("checkpoint_",info_crit_name,".Rdata"))) {
    
    load(paste0("checkpoint_",info_crit_name,".Rdata"))
    
  } else {
  
  # Checks
  if (is.null(param_info_tot$lb) || is.null(param_info_tot$ub)) {
    stop("param_info_tot$lb and param_info_tot$ub must be defined!")
  } else if (any(sort(names(param_info_tot$lb))!=sort(names(param_info_tot$ub))) ||
             !all(c(oblig_param_list, add_param_list) %in% names(param_info_tot$lb))) {
    stop("param_info_tot$lb and param_info_tot$ub must be defined for all candidate parameters (i.e. obligatory and additional ones)")
  }
  if (!is.null(param_info_tot$init_values)) {
    if (!all(c(oblig_param_list, add_param_list) %in% names(param_info_tot$init_values))) {
      stop("If param_info_tot$init_values is provided, it must have a column for each candidate parameter 
      (i.e. obligatory and additional ones). Fill this column with NA if you don't want to define initial values for this parameter.")
    }
  }
  if ((is.null(lb_initV) || is.null(ub_initV)) && 
      (nrow(param_info_tot$init_values)<model_options$nb_rep || any(is.na(param_info_tot$init_values)))) {
    stop("Init_values must contain initial values for all parameters and repetitions if lb_initV and ub_initV are not provided.")
  }
  if (info_crit_name=="AICc") {
    info_crit_func <- AICc
  } else if (info_crit_name=="BIC") {
    info_crit_func <- BIC
  } else {
    stop("info_crit_name must be AICc or BIC.")
  }
  
  
  
  candidate_params <- oblig_param_list
  best_final_values <- setNames(data.frame(matrix(data=NA, ncol=length(candidate_params),nrow=0)),
                                candidate_params)
  prev_info_crit <- Inf
  count<-1
  df_outputs<-NULL
  
  }
  
  while(!is.null(candidate_params)) {
    
    print(paste("Estimated parameters:",paste(candidate_params,collapse=" ")))

    optim_options$nb_rep <- nb_rep[min(length(nb_rep),count)]
    param_info <- lapply(param_info_tot,function(x) x[candidate_params])
    
    param_info$init_values <- complete_init_values(param_info_tot$init_values, 
                                        optim_options$nb_rep, 
                                        lb_initV, ub_initV, 
                                        optim_options$ranseed,
                                        satisfy_par_const)
    
    # initialize already estimated parameters with the values leading to the best criterion obtained so far
    if (nrow(best_final_values)>0 && length(best_final_values)>0) {
#      param_info$init_values <- setNames(data.frame(matrix(data=NA, ncol=length(candidate_params),
#                                                           nrow=optim_options$nb_rep)),
#                                         candidate_params)
      param_info$init_values[names(best_final_values)] <- best_final_values[rep(1,optim_options$nb_rep),]
#      new_candidate <- candidate_params[length(candidate_params)]
#      param_info$init_values[, new_candidate] <- param_info_tot$init_values[,new_candidate]
    }
    

    # Handle forced values of parameters (filter candidate from forced_param_values if there are some)
    forced_param_values <- forced_param_values_tot
    inter_forc_cand <- names(forced_param_values) %in% candidate_params
    if (any(inter_forc_cand)) {
      forced_param_values <- forced_param_values_tot[-which(inter_forc_cand)]
    }
    
    optim_results <-     estim_param(obs_list=obs_list,
                                     model_function=model_function,
                                     model_options=model_options,
                                     optim_options=optim_options,
                                     crit_function=crit_ols,
                                     param_info=param_info,
                                     satisfy_par_const=satisfy_par_const, 
                                     forced_param_values=forced_param_values)
    
    optim_results$info_crit <- info_crit_func(obs_list, optim_results$min_crit_value, 
                             param_nb=length(candidate_params))
    if (optim_results$info_crit<min(prev_info_crit)) {
      best_final_values <- tibble::tibble(!!!optim_results$final_values)
    }
    
    # Save and move results
    save(optim_results, file = file.path(optim_options$path_results,
                                         paste0("optim_results_",info_crit_name,"_set",count,".Rdata")))
    file.rename(from=file.path(optim_options$path_results,"EstimatedVSinit.pdf"), to=file.path(optim_options$path_results,paste0("EstimatedVSinit_",info_crit_name,"_set",count,".pdf")))

    # Plot simulations versus observations
  	var_names <- setdiff(unique(unlist(lapply(obs_list, names))),"Date")
    model_results <- model_function(model_options = model_options, 
                                    param_values = unlist(c(forced_param_values, 
                                                            optim_results$final_values)),
                                    sit_names = names(obs_list),
                                    sit_var_dates_mask = obs_list,
									var_names = var_names)    
    p<- plot(model_results$sim_list,obs=obs_list, type = "scatter", all_situations = TRUE)
    
    if (exists('plot_save', where='package:CroPlotR', mode='function')) {
      plot_save(plot = p, path = optim_options$path_results,suffix = paste0("_scatter_",info_crit_name,"_set",count))
    } else if (exists('save_plot_pdf', where='package:CroPlotR', mode='function')) {
      save_plot_png(plot = p, path = optim_options$path_results,suffix = paste0("_scatter_",info_crit_name,"_set",count))
    }
    
    print(paste("Values for the estimated parameters:",paste(optim_results$final_values,
                collapse=" ")))
    print(paste0(info_crit_name,"=",optim_results$info_crit))
    
    df_outputs<-bind_rows(df_outputs,create_AgMIP_outputs(candidate_params, obs_list, 
                                                          optim_results, model_function, 
                                                          model_options, info_crit_func, 
                                                          info_crit_name, digits,
                                                          forced_param_values=forced_param_values))
    
    candidate_params <- select_param_FwdReg(oblig_param_list, add_param_list, 
                                            candidate_params, optim_results$info_crit, 
                                            prev_info_crit)
    prev_info_crit <- c(prev_info_crit, optim_results$info_crit)
    
    count <- count+1
    
    #save(list = ls(all.names = TRUE), file = file.path(path,"checkpoint.Rdata"), envir = 
    #       environment())
    save(crit_type, candidate_params, prev_info_crit, best_final_values, count, file = file.path(path,paste0("checkpoint_",info_crit_name,"_set",count-1,".Rdata")), envir = 
           environment())
    
  }

  
  # Save the results
  save(df_outputs, file = file.path(optim_options$path_results,
                                    paste0("phase3_",info_crit_name,"_Table4.Rdata")))
  
  df <- as.data.frame(sapply(df_outputs, as.character, simplify = FALSE))
  write.table(df, sep=";", file=file.path(optim_options$path_results,
                                          paste0("phase3_",info_crit_name,"_Table4.csv")), row.names=FALSE)
 
  return(df) 
}
