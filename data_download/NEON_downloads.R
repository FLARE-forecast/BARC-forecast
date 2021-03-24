# Function to extract and clean up the NEON data from NEON
download_neon_files <- function(siteID_neon, products){

        # Download newest products
        neonstore::neon_download(product = products, site = siteID_neon)
        
        # Store the NEON met data products
        neonstore::neon_store("SECPRE_30min-basic")
        neonstore::neon_store("2DWSD_30min-basic")
        neonstore::neon_store("SLRNR_30min-basic")
        neonstore::neon_store("SAAT_30min-basic")
        neonstore::neon_store("RH_30min-basic")
        neonstore::neon_store("BP_30min-basic")
        
        # Store the NEON buoy data products
        neonstore::neon_store("TSD_30_min-basic")
        
        # Tidy up the met data
        # Airtemp
        airtemp <- neonstore::neon_table(table = "SAAT_30min-basic", site = "BARC") %>%
          select(endDateTime, tempSingleMean)%>%
          mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
          select(-endDateTime)%>%
          group_by(time) %>%
          summarize_at(c("tempSingleMean"), mean, na.rm = TRUE)%>%
          arrange(time)%>%
          mutate(time = time - 5*3600)
        
        # Radiation
        radiation <- neonstore::neon_table(table = "SLRNR_30min-basic", site = "BARC") %>%
          select(endDateTime, inSWMean, inLWMean) %>%
          mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
          select(-endDateTime)%>%
          group_by(time) %>%
          summarize_at(c("inSWMean", "inLWMean"), mean, na.rm = TRUE)%>%
          arrange(time)%>%
          mutate(time = time - 5*3600)
        
        # Humidity
        humidity <- neonstore::neon_table(table = "RH_30min-basic", site = "BARC") %>% 
          select(endDateTime, RHMean)%>%
          mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
          select(-endDateTime)%>%
          group_by(time) %>%
          summarize_at(c("RHMean"), mean, na.rm = TRUE)%>%
          arrange(time)%>%
          mutate(time = time - 5*3600)
        
        # Precipitation
        precip  <- neonstore::neon_table(table = "SECPRE_30min-basic", site = "OSBS") %>%
          select(endDateTime, secPrecipBulk) %>%
          mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
          select(-endDateTime)%>%
          group_by(time) %>%
          summarize_at(c("secPrecipBulk"), sum, na.rm = TRUE)%>%
          arrange(time)%>%
          mutate(time = time - 5*3600)
        
        # Wind Speed
        windspeed <- neonstore::neon_table(table = "2DWSD_30min-basic", site = "BARC")%>%  
          select(endDateTime, windSpeedMean)%>%
          mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
          select(-endDateTime)%>%
          group_by(time) %>%
          summarize_at(c("windSpeedMean"), sum, na.rm = TRUE)%>%
          arrange(time)%>%
          mutate(time = time - 5*3600)
        
        # Pressure
        pressure <- neonstore::neon_table(table = "BP_30min-basic", site = "BARC") %>%
          select(endDateTime, staPresMean)%>%
          mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
          select(-endDateTime)%>%
          group_by(time) %>%
          summarize_at(c("staPresMean"), mean, na.rm = TRUE)%>%
          arrange(time)%>%
          mutate(time = time - 5*3600)
        
        
        met_target <- full_join(radiation, airtemp, by = "time")%>%
          full_join(., humidity, by = "time")%>%
          full_join(., windspeed, by = "time")%>%
          full_join(., precip, by = "time")%>%
          full_join(., pressure, by = "time")%>%
          rename(ShortWave = inSWMean, LongWave = inLWMean, AirTemp = tempSingleMean,
                 RelHum = RHMean, WindSpeed = windSpeedMean, Rain = secPrecipBulk, Pressure = staPresMean)%>%
          mutate(Rain = Rain*0.024)%>%
          mutate(Pressure = Pressure*1000)%>%
          mutate(ShortWave = ifelse(ShortWave<=0,0,ShortWave))%>%
          filter(time >= "2018-08-06")
        
        met_target <- as.data.frame(met_target)
        write_csv(met_target, "./data/met_data_w_gaps.csv")
        
        # Water temperature by depth
        # ----------------------------------------------------------------------------------------
        water_temp <- neonstore::neon_table(table = "TSD_30_min-basic", site = "BARC")%>% 
          select(endDateTime, thermistorDepth, tsdWaterTempMean) %>%
          arrange(endDateTime, thermistorDepth)%>%
          rename(depth = thermistorDepth)%>%
          rename(value = tsdWaterTempMean)%>%
          rename(timestamp = endDateTime)%>%
          mutate(variable = "temperature",
                 method = "thermistor",
                 value = ifelse(is.nan(value), NA, value))%>%
          select(timestamp, depth, value, variable, method)%>%
          mutate(timestamp = timestamp - 5*3600)
        
        write_csv(water_temp, "./data/temp_data.csv")

}