#creating training vectors with interaction of features
#idea is to make these distinct features, since all these are categorical, its not that i am placing such weights on features, 
#but just creating new distinct features

roll = train_originalData$ROLE_ROLLUP_1 + 1000*train_originalData$ROLE_ROLLUP_2

title_code = train_originalData$ROLE_TITLE + 1000*train_originalData$ROLE_CODE

family = train_originalData$ROLE_FAMILY + 1000*train_originalData$ROLE_FAMILY_DESC

title_code_family = title_code + 1000*family

engineered_training_five_features = data.frame(ACTION = train_originalData$ACTION, ROLE_ROLLUP = roll, TITLE_CODE = title_code, FAMILY = family, 
                                               RESOURCE = train_originalData$RESOURCE, 
                                               DEPTNAME = train_originalData$ROLE_DEPTNAME)

cv_log = cross_validate(engineered_training_five_features, 10, "logistic")

cv_rf = cross_validate(engineered_training_five_features, 7, "rf")


engineered_training_four_features = data.frame(ACTION = train_originalData$ACTION, ROLE_ROLLUP = roll, TITLE_CODE_FAMILY = title_code_family, 
                                               RESOURCE = train_originalData$RESOURCE, 
                                               DEPTNAME = train_originalData$ROLE_DEPTNAME)


categorized_engineered_training_five_features <- engineered_training_five_features

categorized_engineered_training_five_features[sapply(categorized_engineered_training_five_features, is.integer)] <- lapply(categorized_engineered_training_five_features[sapply(categorized_engineered_training_five_features, is.integer)], 
                                                                                   as.factor)

cv_rf_categorized = cross_validate(categorized_engineered_training_five_features, 7, "logistic")

cv_rf_categorized2 = randomForest(categorized_engineered_training_five_features[,-1], as.factor(train_originalData$ACTION))
  
interaction_features <- function(dataset_without_action, avoid_single_features)
{
  interaction_dataset <- data.frame(col1 = 1:dim(dataset_without_action)[1])
  total_cols = dim(dataset_without_action)[2]
  col_names = colnames(dataset_without_action)
  count = 1
  
  for(i in 1:(total_cols-1))
  {
    for(j in (i+avoid_single_features):total_cols)
      #if avoid_single_features is 0, so we get even single features, for ex we also get RESOURCE * RESOURCE as a feature (since these are categorical, taking just resource, or resource^2 means the same thing)
    {
      count = count + 1
      col = (dataset_without_action[, i] / 10^3) * dataset_without_action[, j]
      interaction_dataset = cbind(interaction_dataset, col)
      colnames(interaction_dataset)[count] = paste(as.character(col_names[i]), as.character(col_names[j]), sep = "_")
    }
  }
  
  return(interaction_dataset[,-1]) #removed first col which was just for initializing data frame with how many rows
  
}

interaction_testing = interaction_features(train_originalData[,-1], 1)

add_action_to_dataset <- function(dataset_containing_action, dataset_not_containing_action)
{
  ACTION = dataset_containing_action$ACTION
  
  ACTION = cbind(ACTION, dataset_not_containing_action)
  
  return(ACTION)
}

interaction_testing_with_action_added <- add_action_to_dataset(train_originalData, interaction_testing)
interaction_testing_with_action_added <- interaction_testing_with_action_added [,-2]

interaction_testing_with_action_added[1:3,1:5]

cv_interaction_rf <- cross_validate(interaction_testing_with_action_added, 5, "rf")
