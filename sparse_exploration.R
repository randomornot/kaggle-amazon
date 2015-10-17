#models with sparse data


train_data_factor_columns <- train_originalData

train_data_factor_columns[sapply(train_data_factor_columns, is.integer)] <- lapply(train_data_factor_columns[sapply(train_data_factor_columns, is.integer)], 
                                                                                   as.factor)

test_Data_factor_columns <- test_data

test_Data_factor_columns[sapply(test_Data_factor_columns, is.integer)] <- lapply(test_Data_factor_columns[sapply(test_Data_factor_columns, is.integer)], 
                                                                                   as.factor)


sparse_data_3 = model.matrix(~ ROLE_ROLLUP_1 + ROLE_ROLLUP_2 + ROLE_DEPTNAME + ROLE_TITLE, data = train_data_factor_columns) 
sparse_data_3 = sparse_data_3[, -1]
sparse_training_data = as.data.frame(sparse_data_3)
sparse_training_data$ACTION <- train_originalData$ACTION

sparse_data_for_test = model.matrix( ~ ROLE_ROLLUP_1 + ROLE_ROLLUP_2 + ROLE_DEPTNAME + ROLE_TITLE, data = test_Data_factor_columns)
sparse_data_for_test = sparse_data_for_test[, -1]

col_names_sparse_train = colnames(sparse_data_3)
col_names_sparse_test = colnames(sparse_data_for_test)

col_names_not_in_train = setdiff(col_names_sparse_test, col_names_sparse_train)
col_names_not_in_train_2 = setdiff(col_names_sparse_train, col_names_sparse_test)

sparse_data_for_test = sparse_data_for_test[, -c("ROLE_ROLLUP_1118376", "ROLE_ROLLUP_2118127")]

logistic_sparse <- glm(ACTION ~ .,family = "binomial", data = sparse_training_data)
predicted_sparse = predict.glm(logistic_sparse, test_Data_factor_columns)


#####

sparse_data_4 = model.matrix(~ ROLE_ROLLUP_2 + ROLE_DEPTNAME + ROLE_TITLE, data = train_data_factor_columns) 
sparse_data_4 = sparse_data_3[, -1]
sparse_training_data = as.data.frame(sparse_data_4)
sparse_training_data$ACTION <- train_originalData$ACTION



######

cross_validate(train_originalData, 7, "svm")

