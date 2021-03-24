saved_file <- "/Users/ryanmcclure/Documents/CRAM-forecast/glm/CRAMPTON_works_H_2021_03_20_2021_03_22_F_10_20210323T154031.nc"
qaqc_data_location <- "/Users/ryanmcclure/Documents/CRAM-forecast/qaqc_data/"

#file_name <- saved_file
flare::plotting_general(file_name = saved_file,
                        qaqc_location = qaqc_data_location)

visualization_location <- file.path(lake_directory,"visualization")
source(paste0(visualization_location,"/manager_plot.R"))

manager_plot(file_name = saved_file,
             qaqc_location = qaqc_data_location,
             focal_depths = c(1, 5, 8))
