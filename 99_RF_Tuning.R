
library(tidymodels)
library(tidyverse) 
library(dials)      #for grid_regular() and parameter tuning, dunno why it isnt auto loaded
library(ranger)     
library(parsnip) 
library(doParallel) 

library(caret)
library(dplyr)
library(party)
library(tibble)
library(pROC) 
library(flextable) 

# grab and prep data----
hum_forest <-read_csv("Data/hum_forest_tuning.csv")


#making Model A subset
hum_A <- hum_forest %>%
  select(gender_prolific:ed_level_z, h_sincere_z:altruism_z, tasks_sum_severity)  %>%
  drop_na() 


#Model A: profanity use
hum_A_prof_use <- hum_A %>%
  select(gender_prolific, age_z, ed_level_z,     #demographics
         avg_word_count_z, sr_avg_z, sr_rt_avg_z, hum_rt_avg_z, #response features
         h_sincere_z:altruism_z,                 #personality
         any_profanity_fac)                     #dependent variable 
#ensure any profanity fac is a factor
hum_A_prof_use <- hum_A_prof_use %>%
  mutate(any_profanity_fac = as.factor(any_profanity_fac))

#Model A: profanity severity
hum_A_prof_sev <- hum_A %>%
  select(gender_prolific, age_z, ed_level_z,     #demographics
         avg_word_count_z, sr_avg_z, sr_rt_avg_z, hum_rt_avg_z, #response features
         h_sincere_z:altruism_z,                 #personality
         tasks_sum_severity)                     #dependent variable 

#Model A: agro use
hum_A_agro_use <- hum_A %>%
  select(gender_prolific, age_z, ed_level_z,     #demographics
         avg_word_count_z, sr_avg_z, sr_rt_avg_z, hum_rt_avg_z, #response features
         h_sincere_z:altruism_z,                 #personality
         binary_agro_use)                     #dependent variable 
#ensure any agro use is a factor
hum_A_agro_use <- hum_A_agro_use %>%
  mutate(binary_agro_use = as.factor(binary_agro_use))

#Model A: agro sum
hum_A_agro_sum <- hum_A %>%
  select(gender_prolific, age_z, ed_level_z,     #demographics
         avg_word_count_z, sr_avg_z, sr_rt_avg_z, hum_rt_avg_z, #response features
         h_sincere_z:altruism_z,                 #personality
         sum_agro)                     #dependent variable



#------------------------------------------
#making Model B subset
hum_B <- hum_forest %>%
  select(gender_prolific:ed_level_z, machiavellian_sd4_z:meanness_tripm_z, tasks_sum_severity) %>%
  drop_na()


#Model B: profanity use
hum_B_prof_use <- hum_B %>%
  select(gender_prolific, age_z, ed_level_z,     #demographics
         avg_word_count_z, sr_avg_z, sr_rt_avg_z, hum_rt_avg_z,#response features
         machiavellian_sd4_z:meanness_tripm_z,                 #personality
         any_profanity_fac)                     #dependent variable 
#ensure any profanity fac is a factor
hum_B_prof_use <- hum_B_prof_use %>%
  mutate(any_profanity_fac = as.factor(any_profanity_fac))

#Model B: profanity severity
hum_B_prof_sev <- hum_B %>%
  select(gender_prolific, age_z, ed_level_z,     #demographics
         avg_word_count_z, sr_avg_z, sr_rt_avg_z, hum_rt_avg_z,#response features
         machiavellian_sd4_z:meanness_tripm_z,                 #personality
         tasks_sum_severity)                     #dependent variable 

#Model B: agro use
hum_B_agro_use <- hum_B %>%
  select(gender_prolific, age_z, ed_level_z,     #demographics
         avg_word_count_z, sr_avg_z, sr_rt_avg_z, hum_rt_avg_z,#response features
         machiavellian_sd4_z:meanness_tripm_z,                 #personality
         binary_agro_use)                     #dependent variable 
#ensure any agro use is a factor
hum_B_agro_use <- hum_B_agro_use %>%
  mutate(binary_agro_use = as.factor(binary_agro_use))

#Model B: agro sum
hum_B_agro_sum <- hum_B %>%
  select(gender_prolific, age_z, ed_level_z,     #demographics
         avg_word_count_z, sr_avg_z, sr_rt_avg_z, hum_rt_avg_z,#response features
         machiavellian_sd4_z:meanness_tripm_z,                 #personality
         sum_agro)                     #dependent variable 


#------------------------------------------

#making Model C subset
hum_C <- hum_forest %>%
  select(gender_prolific:o_hexaco_z, altruism_z, #do we want altruism?
         machiavellian_sd4_z:humor_background_z, tasks_sum_severity)   %>%
  drop_na()

#Model C: profanity use
hum_C_prof_use <- hum_C %>%
  select(gender_prolific, age_z, ed_level_z,     #demographics
         avg_word_count_z, sr_avg_z, sr_rt_avg_z, hum_rt_avg_z,#response features
         h_hexaco_z:o_hexaco_z, altruism_z,      #HEXACO personality
         machiavellian_sd4_z:humor_background_z, #DARK TETRAD personality + hsq + heiss
         any_profanity_fac)                     #dependent variable 

#ensure any profanity fac is a factor
hum_C_prof_use <- hum_C_prof_use %>%
  mutate(any_profanity_fac = as.factor(any_profanity_fac))

#Model C: profanity severity
hum_C_prof_sev <- hum_C %>%
  select(gender_prolific, age_z, ed_level_z,     #demographics
         avg_word_count_z, sr_avg_z, sr_rt_avg_z, hum_rt_avg_z,#response features
         h_hexaco_z:o_hexaco_z, altruism_z,      #HEXACO personality
         machiavellian_sd4_z:humor_background_z, #DARK TETRAD personality + hsq + heiss
         tasks_sum_severity)                     #dependent variable 

#Model C: agro use
hum_C_agro_use <- hum_C %>%
  select(gender_prolific, age_z, ed_level_z,     #demographics
         avg_word_count_z, sr_avg_z, sr_rt_avg_z, hum_rt_avg_z,#response features
         h_hexaco_z:o_hexaco_z, altruism_z,      #HEXACO personality
         machiavellian_sd4_z:humor_background_z, #DARK TETRAD personality + hsq + heiss
         binary_agro_use)                     #dependent variable 
#ensure any agro use is a factor
hum_C_agro_use <- hum_C_agro_use %>%
  mutate(binary_agro_use = as.factor(binary_agro_use))

#Model C: agro sum
hum_C_agro_sum <- hum_C %>%
  select(gender_prolific, age_z, ed_level_z,     #demographics
         avg_word_count_z, sr_avg_z, sr_rt_avg_z, hum_rt_avg_z,#response features
         h_hexaco_z:o_hexaco_z, altruism_z,      #HEXACO personality
         machiavellian_sd4_z:humor_background_z, #DARK TETRAD personality + hsq + heiss
         sum_agro)                     #dependent variable 


#TUNING


#-----------------------------#
# CREATING FUNCTION: CLASSIFICATION RF
tune_classification_cforest <- function(df, outcome_var, mtry_values) {
  #prep formula
  formula <- as.formula(paste(outcome_var, "~ ."))
  
  #train control for cross-validation
  control <- trainControl(
    method = "cv",
    number = 5, #x validation is just easier than training and testing sets
    classProbs = TRUE,                #enable probability prediction
    summaryFunction = twoClassSummary #use AUC for evaluation
  )
  
  #set up grid for tuning
  tune_grid <- expand.grid(
    mtry = mtry_values                
  )
  
  #train model using caret
  model <- train(
    formula,
    data = df,
    method = "cforest",
    metric = "ROC",                   
    trControl = control,
    tuneGrid = tune_grid
  )
  
  return(model)
}


#-----------------------------#
# FUNCTION: REGRESSION RF     #
#-----------------------------#

tune_regression_cforest <- function(df, outcome_var, mtry_values) {
  #prep formula
  formula <- as.formula(paste(outcome_var, "~ ."))
  
  #train control for cross-validation
  control <- trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
  
  #setting up grid for tuning
  tune_grid <- expand.grid(
    mtry = mtry_values                
  )
  
  #traning model using caret
  model <- train(
    formula,
    data = df,
    method = "cforest",
    metric = "RMSE",                  
    trControl = control,
    tuneGrid = tune_grid
  )
  
  return(model)
}


#-----------------------------#
# TUNING ALL MODELS

#list of predictor sets and their respective outcomes
predictor_sets <- list(
  "hum_A" = list(
    "any_profanity_fac" = hum_A_prof_use,
    "tasks_sum_severity" = hum_A_prof_sev,
    "binary_agro_use" = hum_A_agro_use,
    "sum_agro" = hum_A_agro_sum
  ),
  "hum_B" = list(
    "any_profanity_fac" = hum_B_prof_use,
    "tasks_sum_severity" = hum_B_prof_sev,
    "binary_agro_use" = hum_B_agro_use,
    "sum_agro" = hum_B_agro_sum
  ),
  "hum_C" = list(
    "any_profanity_fac" = hum_C_prof_use,
    "tasks_sum_severity" = hum_C_prof_sev,
    "binary_agro_use" = hum_C_agro_use,
    "sum_agro" = hum_C_agro_sum
  )
)

#list of classification outcomes
classification_outcomes <- c("any_profanity_fac", "binary_agro_use")

#list of regression outcomes
regression_outcomes <- c("tasks_sum_severity", "sum_agro")

#mtry values for tuning
mtry_values <- c(3, 4, 5, 6, 7, 9, 11)

#creating a list to store results
classification_results <- list()
regression_results <- list()

#-----------------------------#
# RUNNING CLASSIFICATION MODELS

for (predictor_name in names(predictor_sets)) {
  predictor_models <- predictor_sets[[predictor_name]]
  
  for (outcome in classification_outcomes) {
    #select the appropriate dataframe
    df <- predictor_models[[outcome]]
    
    # Ensure outcome is a factor with valid levels
    df <- df %>%
      mutate(!!sym(outcome) := as.factor(make.names(!!sym(outcome))))
    
    cat("Tuning classification model for:", predictor_name, "-", outcome, "\n")
    model <- tune_classification_cforest(df, outcome, mtry_values)
    classification_results[[paste(predictor_name, outcome, sep = "_")]] <- model
  }
}

#-----------------------------#
# RUNNING REGRESSION MODELS 

for (predictor_name in names(predictor_sets)) {
  predictor_models <- predictor_sets[[predictor_name]]
  
  for (outcome in regression_outcomes) {
    #select the appropriate dataframe
    df <- predictor_models[[outcome]]
    
    cat("Tuning regression model for:", predictor_name, "-", outcome, "\n")
    model <- tune_regression_cforest(df, outcome, mtry_values)
    regression_results[[paste(predictor_name, outcome, sep = "_")]] <- model
  }
}

#-----------------------------#
# SAVING RESULTS 

#combo all results
all_results <- list(
  "classification" = classification_results,
  "regression" = regression_results
)

all_results

# Save results
saveRDS(all_results, "cforest_tuned_models.rds")


#---------------------------------
#tableifying


#getting best mtry and AUC/R^2 for classification and regression models
extract_best_results <- function(results_list, metric_name) {
  results <- lapply(results_list, function(model) {
    best_row <- model$results[which.max(model$results[[metric_name]]), ]
    return(best_row)
  })
  return(results)
}

#get classification and regression results
classification_results <- all_results$classification
regression_results <- all_results$regression

#for classification, use ROC; for regression, use R-squared
best_classification <- extract_best_results(classification_results, "ROC")
best_regression <- extract_best_results(regression_results, "Rsquared")

#initialise table
outcomes <- c("any_profanity_fac", "tasks_sum_severity", "binary_agro_use", "sum_agro")
models <- c("hum_A", "hum_B", "hum_C")

results_table <- matrix("", nrow = length(models), ncol = length(outcomes))
rownames(results_table) <- paste0("Model ", LETTERS[1:length(models)], ": (", models, ")")
colnames(results_table) <- outcomes

#fill classification results
for (model in models) {
  for (outcome in c("any_profanity_fac", "binary_agro_use")) {
    key <- paste(model, outcome, sep = "_")
    if (!is.null(best_classification[[key]])) {
      results_table[paste0("Model ", toupper(substr(model, 5, 5)), ": (", model, ")"), outcome] <- 
        paste0("mtry=", best_classification[[key]]$mtry, 
               ", AUC=", round(best_classification[[key]]$ROC, 3))
    }
  }
}

#fill regression results using R2 
for (model in models) {
  for (outcome in c("tasks_sum_severity", "sum_agro")) {
    key <- paste(model, outcome, sep = "_")
    if (!is.null(best_regression[[key]])) {
      results_table[paste0("Model ", toupper(substr(model, 5, 5)), ": (", model, ")"), outcome] <- 
        paste0("mtry=", best_regression[[key]]$mtry, 
               ", R2=", round(best_regression[[key]]$Rsquared, 3))
    }
  }
}

#convert matrix to df 
results_table_df <- as.data.frame(results_table, stringsAsFactors = FALSE)
results_table_df



#
#END
#