library(tidyverse)
library(ggplot2)

remove_outliers <- function (df) {
  y <- df[2][df[2] > 0] #remove any zero values
  
  dfNoOutliers<- df%>% filter(df[2]< 3*sd(y) + mean(y)) #remove any outliers
  valsremaining <<- nrow(dfNoOutliers)/nrow(df)
  print(1-valsremaining)
  print(paste0(100*round(1-valsremaining,6), "% of data removed"))
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

experiment_data <- read.csv("vb_exp_r_output_editorial_new_trending.csv")
experiment_data$clicks %>% head()

#Find how many users have each number of metrics
#full_data<-
experiment_data %>% 
  spread(key= age_range, value = completes)
  select(completes) %>%
  group_by(completes) %>%
  count() %>%
  ungroup() %>%
  mutate(perc_of_total = round(100 * n / sum(n)))

#How about if we remove outliers
outliers_removed<-remove_outliers(experiment_data %>%select(hashed_id, completes))
#cleaned_data<-
  outliers_removed %>% select(completes) %>%
  group_by(completes) %>%
  count() %>%
  ungroup() %>%
  mutate(perc_of_total = round(100 * n / sum(n)))




