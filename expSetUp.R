# Experimentation Set Up

### Proxy connection to CRAN ###
# proxy_url <- "http://www-cache-bol.reith.bbc.co.uk:80"
Sys.setenv(http_proxy = "", https_proxy = "", ftp_proxy = "")

# Function to install package if not present. If present, load.
getPackage <- function(pkg) { 
  if (!pkg %in% installed.packages()) { 
    install.packages(pkg) 
  } else { 
      library(pkg, character.only = TRUE)
      paste(pkg, character.only=TRUE)
  }
}

getPackage("ggplot2")
getPackage("readxl")
getPackage("devtools")
getPackage("ggstatsplot")
getPackage("dplyr")
getPackage("robust")
getPackage("readxl")
getPackage("scales")
getPackage("tinytex")
getPackage("tidyr")
getPackage("kableExtra")
getPackage("userfriendlyscience")

## Run remove_outliers function
### This function will remove outliers greater than than 3 standard deviations away from the mean ###
remove_outliers <- function (df) {
  y <- df[2][df[2] > 0] #remove any zero values
  # print(paste0("mean = ", mean(y)))
  # print(paste0("3sd = ",3*sd(y)))
  # print(paste0("3sd + mean = ", 3*sd(y) + mean(y)))
  dfNoOutliers<- df%>% filter(df[2]< 3*sd(y) + mean(y)) #remove any outliers
  valsremaining <- length(dfNoOutliers)/length(df)
  valsremaining
  
  if (valsremaining < 0.95){
    stop ("This function will remove more than 5% percent of your data. You need to remove outliers manually.")}
  
  else if (valsremaining < 0.99){
    warning("This calculation has removed between 1% and 5% of your data.") 
    outlier_statement <- paste0(valsremaining*100, "% has been removed")
  }
  else{
    outlier_statement <- "Less than 1% of data has been removed"
  }
  return(dfNoOutliers)
}


## Uplift calculator function
### This section of the script calculators individual uplifts between variants ###
uplift_calculator <- function(Control_Metric, Variant_Metric){
  uplift <- (Variant_Metric/Control_Metric)-1
  uplift <- percent(uplift)
  if (uplift > 0){
    sprintf("The variant beat the control by %s", uplift, Control_Metric, Variant_Metric)
  }else{
    sprintf("The variant performed worse than the control by %s", uplift, Control_Metric, Variant_Metric)}
  return(uplift)
}

# Dump
## Approach 1
# Loop remove outliers and create DF for each metric 
# for (i in Metrics){
#   Exp <- Experiment %>% select(user_id, i, experiment_group)
#   Exp2 <- remove_outliers(Exp)
#   nam <- paste("experiment", i, sep = "_")
#   assign(nam, Exp2)
# }
# 
# # Loop uplift calculations
# 
# df_list = mget(ls(pattern = "experiment_"))
# for (i in 1:length(df_list)) {
#   
#   t <- names(df_list[[i]])
#   t <- t[[2]]
#   df <- as.data.frame(df_list[i])
#   names(df)[length(names(df))]<-"experiment_group" 
#   
#   for(group in experimentGroups) {
#     name <- paste0(group,"_", t)
#     
#     assign(name, mean(df %>% filter(experiment_group == group) %>% pull(2)))
#   }
# }