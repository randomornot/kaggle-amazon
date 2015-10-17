get_correlation_between_two_features <- function(dataset, feature_1, feature_2)
{
  df_for_corr = xtabs(~ (feature_1) + (feature_2), data = as.character(dataset))
  return(assocstats(df_for_corr))
}


cross_validate = function(training_dataset_to_use, k, model_name)
{
  x = sample(k, dim(training_dataset_to_use)[1], replace = TRUE)
  auc_vec = vector("numeric", length = k)
  
  if(model_name == "logistic")
  {
    for(i in 1:k)
    {
      training_set = training_dataset_to_use[x != i, ]
      test_set = training_dataset_to_use[ x == i, ]
      
      logistic_mod = glm(ACTION ~ ., family = "binomial", data = training_set)
      predicted_test = predict.glm(logistic_mod, test_set[, -1])
      
      pred_roc = prediction(predicted_test, test_set[, 1])
      
      auc.perf = performance(pred_roc, measure = "auc")
      
      auc_vec[i] = as.vector(auc.perf@y.values)
      print(auc_vec[i])
    }
    
  }
  
  else if(model_name == "rf")
  {
    for(i in 1:k)
    {
      training_set = training_dataset_to_use[x != i, ]
      test_set = training_dataset_to_use[ x == i, ]
      
      rf_mod <- randomForest(ACTION ~ ., data = training_set )
      predicted_test = predict(rf_mod, test_set[,-1])
      
      pred_roc = prediction(predicted_test, test_set[, 1])
      
      auc.perf = performance(pred_roc, measure = "auc")
      
      auc_vec[i] = as.vector(auc.perf@y.values) 
      print(auc_vec[i])
    }
  
  }
    
    else if(model_name == "svm")
    {
      for(i in 1:k)
      {
        training_set = training_dataset_to_use[x != i, ]
        test_set = training_dataset_to_use[ x == i, ]
        
        svm_mod <- svm(ACTION ~ ., data = training_set )
        predicted_test = predict(svm_mod, test_set[,-1])
        
        pred_roc = prediction(predicted_test, test_set[, 1])
        
        auc.perf = performance(pred_roc, measure = "auc")
        
        auc_vec[i] = as.vector(auc.perf@y.values) 
        #print(auc_vec[i])
      }
    }
  
  return(mean(sapply(auc_vec, sum)))

}



