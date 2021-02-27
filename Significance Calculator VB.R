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
      mutate(significance =  ifelse(p_value < 0.05  & p_value > 0.0, "Significant", "Not Significant"))%>%
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


############################ Read in data #####################
# STEP TO IMPROVE: Could load data directly from Redshift
#Format hashed_id, exp_group, metric 1, metric 2... etc
experiment <- read.csv("vb_exp_r_output_editorial_new_trending.csv")
head(experiment)

#Select the age range(s) you need and remove that column
experiment$age_range %>%unique()
experiment<- experiment %>% filter(age_range != 'under 13') %>% select(-age_range)
experiment<-experiment[1:1000,]

# Rearrange columns:
experiment <- experiment %>% 
    select(hashed_id, exp_group, everything())
head(experiment)

# Set Vectors of experimentGroups and metrics 
experimentGroups <- unique(experiment$exp_group)
metrics <- names(experiment)
metrics <- metrics[!metrics %in% c("hashed_id", "exp_group")]
metrics

setwd(results_folder)
get_variant_totals<-function(exp_cleaned, metric_col){
   totals<- exp_cleaned %>%
      mutate(metric = colnames(experiment)[metric_col]) %>%
      group_by(metric, exp_group) %>%
      summarise(total = sum(metric_value)) %>%
      spread(exp_group, total)
   
   return(totals)
}



##remove outliers
for(col in 3:ncol(experiment)){
   print(col)
exp_cleaned<-remove_outliers(experiment %>% select(hashed_id, colnames(experiment)[col], exp_group))
names(exp_cleaned)[2]<- 'metric_value'

if (col ==3){
   var_totals <- get_variant_totals(exp_cleaned, col)
   uplift_values<-get_uplift(var_totals)
   pValueDF<-get_p_values(col)
   stats_results<-create_stats_results(pValueDF, uplift_values, col)
   
   var_totals_combined<-var_totals
   stats_results_combined<-stats_results

}
else{
   var_totals<-get_variant_totals(exp_cleaned, col)
   uplift_values<-get_uplift(var_totals)
   pValueDF<-get_p_values(col)
   stats_results<-create_stats_results(pValueDF, uplift_values, col)
   
   var_totals_combined<- var_totals_combined %>% bind_rows(var_totals)
   stats_results_combined<- stats_results_combined %>% bind_rows(stats_results)
}

}
var_totals_combined
stats_results_combined

# ######## get totals for each exp group (starts) and remove outliers
# exp_cleaned<-remove_outliers(experiment %>% select(hashed_id, colnames(experiment)[3], exp_group))
# names(exp_cleaned)[2]<- 'metric_value' 
# var_totals<- exp_cleaned %>% 
#    mutate(metric = colnames(experiment)[3]) %>%
#    group_by(metric, exp_group) %>%
#    summarise(total = sum(metric_value))  %>%
#             spread(exp_group, total)
# var_totals


uplift_values<-get_uplift(var_totals)
# #create empty df to fill
# uplift_values<- data.frame(variable = character(), comparison = character(), uplift = numeric())
# uplift_values
# 
# for(var_col in 2:ncol(var_totals)) {
# for(comp_col in 2:ncol(var_totals)){
#    uplift <- data.frame( variable = colnames(var_totals)[var_col],
#                          comparison = colnames(var_totals)[comp_col],
#                          uplift = round(((as.numeric(var_totals[1,var_col]) - as.numeric(var_totals[1,comp_col]))/ as.numeric(var_totals[1,comp_col])) * 100, 1))
#    
#    uplift_values <- uplift_values %>% bind_rows(uplift) %>% filter(variable !=comparison)
# }
# }

uplift_values


pValueDF<-get_p_values(4)

# pValues<- pairwise.t.test(experiment[,3],
#                             experiment$exp_group,
#                             p.adj = "bonf")
# pValueDF<-data.frame(pValues$p.value)
# pValueDF<- pValueDF %>% 
#    tibble::rownames_to_column(var = "variable")%>%
#    gather(key = comparison, value = p_value, 2:4) %>%
#    #mutate_all(~replace_na(., 0))%>%
#    mutate(significance =  ifelse(p_value < 0.05  & p_value > 0.0, "Significant", "Not Significant")
#           )%>%
#     filter(variable != comparison & p_value != 'NA')
# pValueDF


stats_results<-create_stats_results(pValueDF, uplift_values, 4)
# stats_results<- 
#    pValueDF %>% 
#    left_join(uplift_values, 
#                        by = c("variable"="variable", 
#                               "comparison"="comparison"))%>%
#    mutate(metric = colnames(experiment)[3],
#           performance = ifelse( uplift > 0, 
#                            paste0(variable, " outperformed ", comparison),
#                            paste0(comparison, " outperformed ", variable) )
#           )%>%
#    select(metric, everything())
# stats_results




# Calculations  ----------------------------------------------------------- 

# Create summary table of mean comparisons
if (length(experimentGroups)==2) {
   master_summary <- master %>% 
      group_by(Metric, exp_group) %>% 
      summarise(total=sum(value)) %>% 
      spread(exp_group, total) %>% 
      mutate(`Variant Uplift` = paste0(round(((var1/control)-1)*100,1), "%"),
             `Statement` = uplift_calculator(control, var1))
} else if (length(experimentGroups)==3) {
   master_summary <- master %>% 
      group_by(Metric, exp_group) %>% 
      summarise(total=sum(value)) %>% 
      spread(exp_group, total) %>% 
      mutate(control_vs_var1_uplift = paste0(round(((var1/control)-1)*100,1), "%")) %>% 
      mutate(uplift_statement_1 = uplift_calculator(control, var1)) %>% 
      mutate(control_vs_var2_uplift = paste0(round(((var2/control)-1)*100,1), "%")) %>% 
      mutate(uplift_statement_2 = uplift_calculator(control, var2)) %>% 
      mutate(var1_vs_var2_comparison = paste0(round(((var2/var1)-1)*100,1), "%")) %>% 
      mutate(uplift_statement_3 = uplift_calculator(var1, var2))
} else if (length(experimentGroups)==4) {
   master_summary <- master %>% 
      group_by(Metric, exp_group) %>% 
      summarise(total=sum(value)) %>% 
      spread(exp_group, total) %>% 
      mutate(control_vs_var1_uplift = paste0(round(((var1/control)-1)*100,1), "%"),
             uplift_statement_1 = uplift_calculator(control, var1), 
             control_vs_var2_uplift = paste0(round(((var2/control)-1)*100,1), "%"),
             uplift_statement_2 = uplift_calculator(control, var2),
             control_vs_var3_uplift = paste0(round(((var3/control)-1)*100,1), "%"),
             uplift_statement_3 = uplift_calculator(control, var3), 
             var1_vs_var2_comparison = paste0(round(((var2/var1)-1)*100,1), "%"),
             uplift_statement_4 = uplift_calculator(var1, var2),
             var1_vs_var3_comparison = paste0(round(((var3/var1)-1)*100,1), "%"),
             uplift_statement_5 = uplift_calculator(var1, var3),
             var2_vs_var3_comparison = paste0(round(((var3/var2)-1)*100,1), "%"),
             uplift_statement_6 = uplift_calculator(var2, var3))
} else {
   print("Number of variants is >4. Code doesn't support this number of variants.")
}


total <- merge(master_summary, master_pvalue ,by="Metric")
total
write.csv(total, paste0("summary_data.csv"), row.names = FALSE)

# Summary --------------------------------------------------------------
# Nice summary table showing means for each metric by experiment group, uplift and significance
# This is also in the RMarkdown file, so commented our here unless needed.


total <- read.csv("summary_data.csv")

total <- total %>%
   mutate(Significance = ifelse(`p.value` <= 0.05, "Significant", "Non-Significant")) %>%
   mutate(control = round(control),
          var1 = round(var1))

total  %>%
   dplyr::mutate(Significance = kableExtra::cell_spec(Significance,
                                                      color = ifelse(Significance == "Significant", "green", "black"),
                                                      bold = ifelse(Significance == "Significant", TRUE, FALSE),
                                                      background = ifelse(Significance == "Significant", "white", "white"))) %>%
   knitr::kable(escape = FALSE) %>%
   kableExtra::kable_styling(full_width = F)





pairwise.t.test(
   experiment$starts,
   experiment$exp_group,
   p.adj = "bonf"
)




