#packages
library(dplyr)
#Initial data processing
data <- read.csv("merged_test_datasets.csv", header=TRUE)
data$Date_Time <- as.POSIXct(data$Date_Time, format = '%Y-%m-%d %H:%M:%S', tz="UTC")
data <- data %>% select(-is_breach_actual_time)
data$Vertical_Velocity[is.na(data$Vertical_Velocity)] <- 0

##Initialise Threshold variables and values

thresholds <- list(
  Vertical_Velocity = c(-2.5, -1.0, -0.75, -0.5, -0.25),
  Depth = c(10, 20, 50, 75, 90),
  Depth_1_cpa = c(15, 10, 5, 2.5),
  vv_1_cpa = c(0.5, 0.4, 0.3, 0.2)
)

# Create list with all possible variable combinations

combinations <- list(
  c("Depth"),
  c("Vertical_Velocity"),
  c("Depth_1_cpa"),
  c("vv_1_cpa"),
  c("Depth", "Vertical_Velocity"),
  c("Depth", "Depth_1_cpa"),
  c("Depth", "vv_1_cpa"),
  c("Vertical_Velocity", "vv_1_cpa"),
  c("Vertical_Velocity", "Depth_1_cpa"),
  c("vv_1_cpa", "Depth_1_cpa"),
  c("Depth", "Vertical_Velocity", "vv_1_cpa"),
  c("Depth", "Vertical_Velocity", "Depth_1_cpa"),
  c("Depth", "vv_1_cpa", "Depth_1_cpa"),
  c("Vertical_Velocity", "vv_1_cpa", "Depth_1_cpa"),
  c("Depth", "Vertical_Velocity", "vv_1_cpa", "Depth_1_cpa")
)

##Generate all possible combination of value and variable combinations
all_threshold_combinations <- expand.grid(lapply(thresholds, function(x) c(NA, x)))

#Create an empty dataframe to store results
results <- data.frame(
  Combination = character(nrow(all_threshold_combinations)),
  True_Positive = numeric(nrow(all_threshold_combinations)),
  False_Positive = numeric(nrow(all_threshold_combinations)),
  True_Negative = numeric(nrow(all_threshold_combinations)),
  False_Negative = numeric(nrow(all_threshold_combinations))
)

# Create a list to store subset data for each combination
subset_data_list <- list()

#iterate through each combination
for (i in 1:nrow(all_threshold_combinations)) {
  threshold_combination <- all_threshold_combinations[i, ]
  print(paste("Combination:", paste(names(threshold_combination), collapse = ", ")))
  
  #Construct threshold conditions
  threshold_conditions <- character(length(threshold_combination))
  for (j in seq_along(threshold_combination)) {
    var <- names(threshold_combination)[j]
    threshold <- threshold_combination[[j]]
    
    if (is.na(threshold)) {
      next #skip empty strings
    } else {
      threshold_conditions[j] <- paste0("data$", var, ifelse(grepl("cpa", var), " >= ", " <= "), threshold)
      }
  }
  
  #Remove empty strings from threshold_conditions
  threshold_conditions <- threshold_conditions[threshold_conditions != ""]
  
  if (length(threshold_conditions) == 0) {
    print("No valid threshold conditions found.")
    next
  }
  
  print(paste("Threshold Conditions:", paste (threshold_conditions, collapse = " & ")))
  
  threshold_expression <- paste(threshold_conditions, collapse = " & ")
  
  subset_data <- data %>% filter(eval(parse(text = threshold_expression)))
  
  subset_data_list[[i]] <- subset_data
  names(subset_data_list)[i] <- paste(threshold_conditions, collapse = "&")
  
  print(nrow(subset_data))
  
  print(head(subset_data))
  
  #Calculate TP, TN, FP, & FN
  true_positive <- sum(subset_data$is_breach ==1)
  false_positive <- sum(subset_data$is_breach ==0)
  true_negative <- sum(data$is_breach == 0) - false_positive
  false_negative <- sum(data$is_breach == 1) - true_positive
  
  #Store results
  results[i, "Combination"] <- paste(threshold_conditions, collapse = " & ")
  results[i, "True_Positive"] <- true_positive
  results[i, "False_Positive"] <- false_positive
  results[i, "True_Negative"] <- true_negative
  results[i, "False_Negative"] <- false_negative
}
###remove first row - first row is empty
results <- results[-1, , drop = FALSE]
rownames(results) <- NULL
subset_data_list

##Calculation of performance metrics
#Accuracy
results$Accuracy <- (results$True_Positive + results$True_Negative) /
  (results$True_Positive + results$False_Positive +
     results$True_Negative + results$False_Negative)
results$Accuracy <- ifelse(is.nan(results$Accuracy), 0, results$Accuracy)

##Precision
results$Precision <- results$True_Positive / (results$True_Positive + results$False_Positive)
results$Precision <- ifelse(is.nan(results$Precision), 0, results$Precision)

#Recall
results$Recall <- results$True_Positive / (results$True_Positive + results$False_Negative)
results$Recall <- ifelse(is.nan(results$Recall), 0, results$Recall)

#F1 Score
results$F1_Score <- 2 * (results$Precision * results$Recall) / (results$Precision +results$Recall)
results$F1_Score <- ifelse(is.nan(results$F1_Score), 0, results$F1_Score)

