# Experimentation Significance Calculator
# Updated 28/02/2021
# This script replaces the previous 'significance calculator for total metrics.R' script

############################ What does this script do? ############################
## 1. Clear environment, set working directory, rail name (this will go into the final file name), read in data file (you need to specify this file name)
## 2. Defines all the functions needed
## 3. Removed child age groups if they've made it in.
## 4. Analyse the data. 
   #4a. Take the users in the age range required and remove that age_range column.
   #4b. Remove outliers from that data
   #4c. Calcualte the totals for each variant.
   #4d. Calcualte the uplift for each variant pair. 
   #4e. Find the p-value and give significance for each variant pair comparison
   #4f. Repeat this for each metric(starts/completes) and each age range.
#5. Repeat step 4 for all ages with no age split.
#6. Write results to file

### Output ###
#1. df stats_results_combined - this gives all the variants and their significance and uplift
#2. dt var_totals_combined - this is the total starts/completes per variant and age group - this is outliwers removed so will be different from the Redshift values.
#3. The stats_results_combined df as a .csv file.

############################ SET UP ############################
rm(list=ls()) #Clear Environment
getwd() #"~/Documents/plot_folder"
#setwd("~/Documents/GitHub/data-force-analysis-snippets/R/experimentation/")


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
get_p_values<-function(exp_data, metric_col){
   pValues<- pairwise.t.test(exp_data[,metric_col],
                             exp_data$exp_group,
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
create_stats_results<- function(exp_df, pValueDF, uplift_values, metric_col){
   stats_results<- 
      pValueDF %>% 
      left_join(uplift_values, 
                by = c("variable"="variable", 
                       "comparison"="comparison"))%>%
      mutate(metric = colnames(exp_df)[metric_col],
             performance = ifelse( uplift > 0, 
                                   paste0(variable, " outperformed ", comparison),
                                   paste0(comparison, " outperformed ", variable) )
      )%>%
      select(metric, everything())
   return(stats_results)
}

######### Sum the total values of the metric for each variant #########
get_variant_totals<-function(exp_data, exp_cleaned, metric_col){
   totals<- exp_cleaned %>%
      mutate(metric = colnames(exp_data)[metric_col]) %>%
      group_by(metric, exp_group) %>%
      summarise(total = sum(metric_value),.groups = 'drop') %>%
      spread(exp_group, total)
   return(totals)
}


######### Remove outliers, calcualte uplift and statistical significance for each metric #########
analyse<-function (experiment,age,analysis_number){
   for(col in 3:ncol(experiment)){
      exp_cleaned<-remove_outliers(experiment %>% select(hashed_id, colnames(experiment)[col], exp_group))
      names(exp_cleaned)[2]<- 'metric_value'
      
      if (col ==3 & analysis_number == 1){
         var_totals <- get_variant_totals(exp_data = experiment,exp_cleaned, col)
         uplift_values<-get_uplift(var_totals)
         pValueDF<-get_p_values(exp_data = experiment, col)
         stats_results<-create_stats_results(exp_df = experiment,pValueDF, uplift_values, col)

         var_totals_combined<-data.frame(age_range = age) %>% bind_cols(var_totals)
         stats_results_combined<-data.frame(age_range = age) %>% bind_cols(stats_results)
        
      }
      else{

         var_totals<-get_variant_totals(exp_data = experiment,exp_cleaned, col)
         uplift_values<-get_uplift(var_totals)
         pValueDF<-get_p_values(exp_data = experiment,col)
         stats_results<-create_stats_results(exp_df = experiment,pValueDF, uplift_values, col)

         var_totals_combined<- var_totals_combined %>% bind_rows(data.frame(age_range = age) %>% bind_cols(var_totals))
         stats_results_combined<- stats_results_combined %>% bind_rows(data.frame(age_range = age) %>% bind_cols(stats_results))
      }
      
   }
   stats_results_combined <<- stats_results_combined
   var_totals_combined <<- var_totals_combined

   return(stats_results_combined)
}


analyse_rail<- function(rail_name, data_path)
{
   print(rail_name)
   
   #Read in the data 
   experiment_data <<- read.csv(paste0(data_path,".csv"))
   
   #Run a few checks 
   #1.Check what metrics there are
   metrics <- names(experiment_data)
   metrics <<- metrics[!metrics %in% c("hashed_id", "exp_group","age_range")]
   print(metrics)
   
   #2.Check the split across age groups
   print(
      experiment_data %>%
      group_by(age_range)%>%
      count() %>% 
      ungroup()%>%
      mutate(percent= round(prop.table(n) * 100,0))
   )
   ages<- data.frame(age_range = experiment_data$age_range %>%unique())%>%arrange(age_range)
   
   
   #Loop over analysis for each age range
   for(row in 1:nrow(ages)) {
      print(ages$age_range[row])
      experiment <- experiment_data %>%filter(age_range == ages$age_range[row]) %>%select(-age_range)#select just the age range needed
      experiment <- experiment %>% select(hashed_id, exp_group, everything()) #order columns
      analyse(experiment,ages$age_range[row], row)

   }
   #Repeat for all 16+ with no age split
   print("all 16+")
   experiment <- experiment_data %>%select(-age_range)#select just the age range needed
   experiment <- experiment %>% select(hashed_id, exp_group, everything()) #order columns
   analyse(experiment,'all 16+', 2) #This 2 is just any value not = 1
   head(stats_results_combined) #Show the resu;ts
   #Write to file
   write.csv(stats_results_combined, paste0("stats_results_",rail_name,".csv"), row.names = FALSE)
   print(paste0("file stats_results_",rail_name,".csv written") )
   
   
}


################### Run analysis for each homepage rail ################### 
#data should be of the format hashed_id, exp_group, metric 1, metric 2... etc and be saved in a .csv file
analyse_rail(rail_name = "binge-worthy", data_path =  "vb_exp_r_output_binge-worthy")
analyse_rail(rail_name = "editorial-new-trending", data_path =  "vb_exp_r_output_editorial_new_trending")
