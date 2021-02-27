# experimentation Significance Calculator
# Updated 26/02/2021

# What does this script do? 
## 1. Calculates the total values for each experiment group per metric provided
## 2. Calculates whether differences seen between groups are significant
## 3. Produces summary table and plots to visualise results


# This script replaces the previous 'significance calculator for total metrics.R' script

# Set Up -----------------------------------------------------------------
#rm(list=ls())

# This script runs packages and custom functions required for experiment analysis
#source("~/Documents/GitHub/data-force-analysis-snippets/R/experimentation/expSetUp.R")

# Set Working Directory
#setwd("~/Documents/GitHub/data-force-analysis-snippets/R/experimentation/")

# Set name of folder where you want to save plots and summary data
results_folder <- getwd() #"~/Documents/plot_folder"

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

# ##remove outliers
# for(col in 3:ncol(experiment)){
#    print(col)
# exp_cleaned<-remove_outliers(experiment %>% select(hashed_id, colnames(experiment)[col], exp_group))
# names(exp_cleaned)[2]<- 'metric_value' 
# 
# if (col ==3){
# var_totals<- exp_cleaned %>% 
#    mutate(metric_name = colnames(experiment)[col]) %>%
#    group_by(metric_name, exp_group) %>%
#    summarise(total = sum(metric_value)) %>%
#    spread(exp_group, total)
# }
# else{
#    var_totals<- var_totals %>% bind_rows(
#       exp_cleaned %>% 
#          mutate(metric_name = colnames(experiment)[col]) %>%
#          group_by(metric_name, exp_group) %>%
#          summarise(total = sum(metric_value)) %>%
#          spread(exp_group, total)
#    )
# }
# }
# var_totals

######## get totals for each exp group (starts) and remove outliers
exp_cleaned<-remove_outliers(experiment %>% select(hashed_id, colnames(experiment)[3], exp_group))
names(exp_cleaned)[2]<- 'metric_value' 
var_totals<- exp_cleaned %>% 
   mutate(metric_name = colnames(experiment)[3]) %>%
   group_by(metric_name, exp_group) %>%
   summarise(total = sum(metric_value))  %>%
            spread(exp_group, total)
var_totals

######### Get uplift
#create empty df to fill
uplift_values<- data.frame(variable = character(), comparison = character(), uplift = numeric())
uplift_values

for(var_col in 2:ncol(var_totals)) {
for(comp_col in 2:ncol(var_totals)){
   uplift <- data.frame( variable = colnames(var_totals)[var_col],
                         comparison = colnames(var_totals)[comp_col],
                         uplift = round(((as.numeric(var_totals[1,var_col]) - as.numeric(var_totals[1,comp_col]))/ as.numeric(var_totals[1,comp_col])) * 100, 1))
   
   uplift_values <- uplift_values %>% bind_rows(uplift)
}
}
uplift_values
uplift_values<- uplift_values %>%filter(variable !=comparison)
#%>%
 #  spread(key = comparison, value = uplift) 
uplift_values

#get p-values for significance
pValues<- pairwise.t.test(experiment$starts,
                            experiment$exp_group,
                            p.adj = "bonf")
pValueDF<-data.frame(pValues$p.value)
pValueDF<- pValueDF %>% 
   tibble::rownames_to_column(var = "variable")%>%
   gather(key = comparison, value = p_value, 2:4) %>%
   #mutate_all(~replace_na(., 0))%>%
   mutate(significance =  ifelse(p_value < 0.05  & p_value > 0.0, "Significant", "Not Significant")
          )
pValueDF

stats_results<- pValueDF %>% 
   filter(variable != comparison & p_value != 'NA')%>%
   left_join(uplift_values, 
                       by = c("variable"="variable", 
                              "comparison"="comparison"))

#put into nice table



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




