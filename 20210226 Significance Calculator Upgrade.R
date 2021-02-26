# Experimentation Significance Calculator
# Updated 26/02/2021

# What does this script do? 
## 1. Calculates the total values for each experiment group per metric provided
## 2. Calculates whether differences seen between groups are significant
## 3. Produces summary table and plots to visualise results


# This script replaces the previous 'significance calculator for total metrics.R' script

## What has changed?
## Set up code (library load and custom functions) placed in separate script and ran via source()
## Automated removal of outliers per metric instead of needing to do it manually
## Automated total and uplift calculations per metric and per control v variant combination (can take up to 3 groups including control) - previously this was manual
## Automated generation of ggstatplot::ggbetweenplot plots per metric
## Automated extraction of p-values
## New formatted summary table holding totals and uplifts per metric and significance

# Set Up -----------------------------------------------------------------
#rm(list=ls())

# This script runs packages and custom functions required for experiment analysis
#source("~/Documents/GitHub/data-force-analysis-snippets/R/Experimentation/expSetUp.R")

# Set Working Directory
#setwd("~/Documents/GitHub/data-force-analysis-snippets/R/Experimentation/")

# Set name of folder where you want to save plots and summary data
results_folder <- getwd() #"~/Documents/plot_folder"

# Read in data 
   # STEP TO IMPROVE: Could load data directly from Redshift
#Format hashed_id, exp_group, metric 1, metric 2... etc
Experiment <- read.csv("vb_exp_r_output_editorial_new_trending.csv")
head(Experiment)
Experiment$age_range %>%unique()
Experiment<- Experiment %>% filter(age_range != 'under 13') %>% select(-age_range)
Experiment<-Experiment[1:1000,]
# Rearrange columns:
Experiment <- Experiment %>% 
    select(hashed_id, exp_group, everything())
head(Experiment)

# Set Vectors of experimentGroups and Metrics 
experimentGroups <- unique(Experiment$exp_group)
Metrics <- names(Experiment)
Metrics <- Metrics[!Metrics %in% c("hashed_id", "exp_group")]

setwd(results_folder)

# Data Cleaning -----------------------------------------------------------
# Removes outliers, gets ggstatsplot and p-value for each metric
for (i in 3:ncol(Experiment)){ #ncol(Experiment
   #for (i in 3:3){
   metric_name <- colnames(Experiment)[i]
   Exp <- Experiment %>% select(hashed_id, colnames(Experiment)[i], exp_group)
   Exp2 <- remove_outliers(Exp)
   Exp2 <- Exp2 %>% rename(value=colnames(Experiment)[i])
   Exp2$Metric <- colnames(Experiment)[i]
   nam <- paste("experiment", colnames(Experiment)[i], sep = "_")
   assign(nam, Exp2)
   
   ## Run Stats Plot
         ggbetween_plot <- ggstatsplot::ggbetweenstats(
            data = Exp2,
            x = exp_group,
            y = value, 
            mean.label.size = 2.5,
            type = "parametric",
            k = 3,
            pairwise.comparisons = TRUE,
            pairwise.annotation = "p.value",
            pairwise.display = "all",
            p.adjust.method = "bonferroni",
            title = "AB/N Test",
            messages = TRUE)
   
      ggsave(paste0("ggstatsplot_",colnames(Experiment)[i],".png"), height = 7, width = 7)
   
   
   ## Extract P-Values from plot
   
   p_value <- ggbetween_plot$labels$subtitle
   pval <- list(p_value)
   pval <- type.convert(pval)
   pval <- pval[[1]]
   
   if (length(experimentGroups)==2){
    pval <- as.character(pval)[10]  
   } else {
    pval <- as.character(pval)[12]
   }
    pval <- as.numeric(pval)
    print(pval)

    
   df <- data.frame(colnames(Experiment)[i],pval)
   names(df) <- c("Metric","p-value")
    
   assign(paste0("p_value_", colnames(Experiment)[i]), df)
   
   rm(ggbetween_plot)
   
}

# Bind and save clean full dataset 
df_list <- mget(ls(pattern = "experiment_"))
master <- do.call("rbind", df_list)
rm(list=ls(pattern = "experiment_"))

# Bind and save p-values
df_list_pval <- mget(ls(pattern = "p_value_"))
master_pvalue <- do.call("rbind", df_list_pval)
rownames(master_pvalue) <- NULL
rm(list=ls(pattern = "p_value_"))
master_pvalue

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
   Experiment$starts,
   Experiment$exp_group,
   p.adj = "bonf"
)




