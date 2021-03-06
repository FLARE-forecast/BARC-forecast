
create_obs_matrix_test <- function(cleaned_observations_file_long, obs_config, start_datetime_local, end_datetime_local, local_tzone, modeled_depths){
  
  full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")
  
  d <- readr::read_csv(cleaned_observations_file_long,
                       col_types = readr::cols())
  
  obs_list <- list()
  for(i in 1:length(obs_config$state_names_obs)){
    print(paste0("Extracting ",obs_config$target_variable[i]))
    
    obs_tmp <- array(NA,dim = c(length(full_time_local),length(modeled_depths)))
    
    for(k in 1:length(full_time_local)){
      for(j in 1:length(modeled_depths)){
        d1 <- d %>%
          dplyr::filter(variable == obs_config$target_variable[i],
                        date == lubridate::as_date(full_time_local[k]),
                        (is.na(hour) | hour == lubridate::hour(full_time_local[k])),
                        abs(depth-as.numeric(modeled_depths[j])) < obs_config$distance_threshold[i])
        
        if(nrow(d1) == 1){
          obs_tmp[k,j] <- d1$value
        }
      }
    }
    
    obs_list[[i]] <- obs_tmp
  }
  
  ####################################################
  #### STEP 7: CREATE THE Z ARRAY (OBSERVATIONS x TIME)
  ####################################################
  
  obs <- array(NA, dim = c(length(obs_config$state_names_obs), length(full_time_local), length(modeled_depths)))
  for(i in 1:nrow(obs_config)){
    obs[i , , ] <-  obs_list[[i]]
  }
  
  
  
  return(obs)
}
