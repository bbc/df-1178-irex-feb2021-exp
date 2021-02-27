# Experimentation Significance Calculator
# Updated 26/02/2021

# What does this script do? 
## 1. Calculates the total values for each experiment group per metric provided
## 2. Calculates whether differences seen between groups are significant
## 3. Produces summary table and plots to visualise results


# This script replaces the previous 'significance calculator for total metrics.R' script

# Set Up -
#rm(list=ls())

# This script runs packages and custom functions required for experiment analysis
#source("~/Documents/GitHub/data-force-analysis-snippets/R/experimentation/expSetUp.R")

# Set Working Directory
#setwd("~/Documents/GitHub/data-force-analysis-snippets/R/experimentation/")

# Set name of folder where you want to save plots and summary data
results_folder <- getwd() #"~/Documents/plot_folder"
setwd(results_folder)
############################ Define functions #####################
######### Get Packages #########
# Function to install package if not present. If present, load.
getPackage <- function(pkg) { 
   if (!pkg %in% installed.packages()) { 
      install.packages(pkg) 
   } else { 
      library(pkg, character.only = TRUE)
      paste(pkg, character.only=TRUE)
   }
}
########## Get packages ###########
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

######### Remove Outliers #########

#This function will remove outliers greater than than 3 standard deviations away from the mean ###
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

######### Get uplift #########
get_uplift<- function(var_totals){
   #create empty df to fill
   uplift_values<- data.frame(variable = character(), comparison = character(), uplift = numeric())
   uplift_values
   
   for(var_col in 2:ncol(var_totals)) {
      for(comp_col in 2:ncol(var_totals)){
         uplift <- data.frame( variable = colnames(var_totals)[var_col],
                               comparison = colnames(var_totals)[comp_col],
                               uplift = round(((as.numeric(var_totals[1,var_col]) - as.numeric(var_totals[1,comp_col]))/ as.numeric(var_totals[1,comp_col])) * 100, 1))
         
         uplift_values <- uplift_values %>% bind_rows(uplift)%>% filter(variable !=comparison)
      }
   }
   return(uplift_values)
}

######### get p-values for significance #########
get_p_values<-function(metric_col){
   pValues<- pairwise.t.test(experiment[,metric_col],
                             experiment$exp_group,
                             p.adj = "bonf")
   pValueDF<-data.frame(pValues$p.value)
   pValueDF<- pValueDF %>% 
      tibble::rownames_to_column(var = "variable")%>%
      gather(key = comparison, value = p_value, 2:4) %>%
      #mutate_all(~replace_na(., 0))%>%
      mutate(significance =  ifelse(p_value < 0.05  & p_value >= 0.0, "Significant", "Not Significant"))%>%
      filter(variable != comparison & p_value != 'NA')
   return(pValueDF)
}

######### Create statistical results table #########
create_stats_results<- function(pValueDF, uplift_values, metric_col){
   stats_results<- 
      pValueDF %>% 
      left_join(uplift_values, 
                by = c("variable"="variable", 
                       "comparison"="comparison"))%>%
      mutate(metric = colnames(experiment)[metric_col],
             performance = ifelse( uplift > 0, 
                                   paste0(variable, " outperformed ", comparison),
                                   paste0(comparison, " outperformed ", variable) )
      )%>%
      select(metric, everything())
   return(stats_results)
}

######### Sum the total values of the metric for each variant #########
get_variant_totals<-function(exp_cleaned, metric_col){
   totals<- exp_cleaned %>%
      mutate(metric = colnames(experiment)[metric_col]) %>%
      group_by(metric, exp_group) %>%
      summarise(total = sum(metric_value)) %>%
      spread(exp_group, total)
   
   return(totals)
}


######### Remove outliers, calcualte uplift and statistical significance for each metric #########
analyse<-function (experiment,age,analysis_number){
   for(col in 3:ncol(experiment)){
      
      exp_cleaned<-remove_outliers(experiment %>% select(hashed_id, colnames(experiment)[col], exp_group))
      names(exp_cleaned)[2]<- 'metric_value'
      
      if (col ==3 & analysis_number == 1){
         var_totals <- get_variant_totals(exp_cleaned, col)
         uplift_values<-get_uplift(var_totals)
         pValueDF<-get_p_values(col)
         stats_results<-create_stats_results(pValueDF, uplift_values, col)
         
         var_totals_combined<-data.frame(age_range = age) %>% bind_cols(var_totals)
         stats_results_combined<-data.frame(age_range = age) %>% bind_cols(stats_results)
         
      }
      else{
         var_totals<-get_variant_totals(exp_cleaned, col)
         uplift_values<-get_uplift(var_totals)
         pValueDF<-get_p_values(col)
         stats_results<-create_stats_results(pValueDF, uplift_values, col)
         
         var_totals_combined<- var_totals_combined %>% bind_rows(data.frame(age_range = age) %>% bind_cols(var_totals))
         stats_results_combined<- stats_results_combined %>% bind_rows(data.frame(age_range = age) %>% bind_cols(stats_results))
      }
      
   }
   stats_results_combined <<- stats_results_combined
   var_totals_combined <<- var_totals_combined
   # print(var_totals_combined)
   # print(stats_results_combined)
}

############################ Read in data #####################
# STEP TO IMPROVE: Could load data directly from Redshift
#Format hashed_id, exp_group, metric 1, metric 2... etc
experiment_data <- read.csv("vb_exp_r_output_editorial_new_trending.csv") %>% 
   filter(age_range != 'under 13'& age_range != '14-16' & age_range !='') #remove ages not needed
head(experiment_data)

#Check the split across age groups
experiment_data %>%
   group_by(age_range)%>%
   count() %>% 
   ungroup()%>%
   mutate(percent= round(prop.table(n) * 100,0))

ages<- data.frame(age_range = experiment_data$age_range %>%unique())%>%arrange(age_range)


##Tell the user their ages and metrics
metrics <- names(experiment_data)
metrics <- metrics[!metrics %in% c("hashed_id", "exp_group","age_range")]
metrics
ages

#Loop over analysis for each age range
for(row in 1:nrow(ages)) {
   print(ages$age_range[row])
   experiment <- experiment_data %>%filter(age_range == ages$age_range[row]) %>%select(-age_range)#select just the age range needed
   experiment <- experiment %>% select(hashed_id, exp_group, everything()) #order columns
   analyse(experiment,ages$age_range[row], row)
}
#Repeat for all 16+ with no age split
experiment <- experiment_data %>%select(-age_range)#select just the age range needed
experiment <- experiment %>% select(hashed_id, exp_group, everything()) #order columns
analyse(experiment,'all 16+', 2) #This 2 is just any value not = 1



#write.csv(total, paste0("summary_data.csv"), row.names = FALSE)

# Summary --------------------------------------------------------------
# Nice summary table showing means for each metric by experiment group, uplift and significance
# This is also in the RMarkdown file, so commented our here unless needed.
