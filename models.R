test_data = read.csv("..//test.csv")

logistic_model = glm(ACTION ~ .,family = "binomial", data = train_originalData)
predicted_test = predict.glm(logistic_model, test_data)

predicted_Var = ifelse(predicted_test < 0.5, 0, 1)

head(DF)

logistic_model_3features = glm(ACTION ~ ROLE_ROLLUP_1+ROLE_TITLE+ROLE_CODE, family = "binomial", data = train_originalData)
summary(logistic_model_3features)
predicted_test_3features = predict.glm(logistic_model_3features, test_data)
predicted_var_3features = ifelse(predicted_test_3features < 2.7, 0, 1)

summary(predicted_var_3features)
table(predicted_var_3features)

solution_3features = data.frame(Id = test_data$id, Action = predicted_var_3features)
summary(solution_3features)
#write.csv(solution_3features, "solution(first) logistic with 3 features.csv")


solution_3features_predicted_test3features <- data.frame(Id = test_data$id, Action = predicted_test_3features)
write.csv(solution_3features_predicted_test3features, "solution(second) logistic values.csv")


logistic_model_2features = glm(ACTION ~ ROLE_ROLLUP_1+ROLE_TITLE, family = "binomial", data = train_originalData)
predicted_test_2features = predict.glm(logistic_model_2features, test_data)

solution_2features = data.frame(Id = test_data$id, Action = predicted_test_2features)
write.csv(solution_2features, "solution(third) logistic values.csv")


logistic_rollUp1_code = glm(ACTION ~ ROLE_ROLLUP_1 + ROLE_CODE, family="binomial", data = train_originalData)
predicted_rollUp1_code = predict.glm(logistic_rollUp1_code, test_data)

solution_rollUp1_code = data.frame(Id = test_data$id, Action = predicted_rollUp1_code)
write.csv(solution_rollUp1_code, "solution(fourth) logistic values.csv")

############################################################



rf_allData = randomForest(ACTION ~ ., data = train_originalData)
predicted_rfAllData = predict(rf_allData, test_data)

solution_rfAllData = data.frame(Id = test_data$id, Action = predicted_rfAllData)
write.csv(solution_rfAllData, "soultion (fifth) random forest (first).csv")


rf_rollUp1_code_title_family <- randomForest(ACTION ~ ROLE_ROLLUP_1 + ROLE_CODE + ROLE_FAMILY + ROLE_TITLE, data = train_originalData )
predicted_rfSubset1 = predict(rf_rollUp1_code_title_family, test_data)

solution_rfSubset1 = data.frame(Id = test_data$id, Action = predicted_rfSubset1)
write.csv(solution_rfSubset1, "soultion (sixth) random forest (second).csv")


rf_model_interactions_1 <- randomForest(ACTION ~ ROLE_ROLLUP_1*ROLE_ROLLUP_2 + RESOURCE + MGR_ID + ROLE_TITLE + ROLE_CODE, data = train_originalData )
predicted_rfSubset2 = predict(rf_model_interactions_1, test_data)



solution_rfSubset2 = data.frame(Id = test_data$id, Action = predicted_rfSubset2)
write.csv(solution_rfSubset2, "soultion (seventh) random forest (3rd).csv")

############################################################

adabag_allData.boosting = boosting(ACTION ~ ., data = train_originalData, boos = TRUE, mfinal = 10, control = rpart.control(minsplit = 0))
#above not working

adabag_allData.bagging = bagging(ACTION ~ . ,data = train_originalData, mfinal = 10, control = rpart.control(cp = -1))
  

############################################################

svm_allData = svm(ACTION ~ ., data = train_originalData)
predicted_svmAllData = predict(svm_allData, test_data)

solution_svmAllData = data.frame(Id = test_data$id, Action = predicted_svmAllData)
write.csv(solution_svmAllData, "solution (eighth) svm (first).csv")
