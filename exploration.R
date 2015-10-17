getwd()
train_originalData = read.csv("..//train.csv")
summary(train_originalData)
library("dplyr")
library(vcd)
library(randomForest)
library(ROCR)
library(adabag)
library(e1071)


count(train_originalData, c("ACTION", "RESOURCE"))
summarise_each(train_originalData, funs(count))
head(train_originalData)
table(train_originalData)
count(train_originalData, RESOURCE, sort=T)
count(train_originalData, MGR_ID, sort=T)
count(train_originalData, ROLE_ROLLUP_1, sort=T)
count(train_originalData, ROLE_ROLLUP_2, sort = T)
count(train_originalData, ROLE_DEPTNAME, sort=T)
count(train_originalData, ROLE_TITLE, sort=T)
count(train_originalData, ROLE_FAMILY_DESC, sort=T)
count(train_originalData, ROLE_FAMILY, sort=T)
count(train_originalData, ROLE_CODE, sort=T)

hist(train_originalData$RESOURCE)
hist(train_originalData$ROLE_DEPTNAME)

hist(train_originalData$ROLE_FAMILY_DESC)
hist(train_originalData$ROLE_FAMILY)

hist(train_originalData$ROLE_CODE)
hist(train_originalData$ROLE_TITLE)

hist(train_originalData$ROLE_ROLLUP_1)
hist(train_originalData$ROLE_ROLLUP_2)

hist(train_originalData$MGR_ID)

train_data_factor_columns <- train_originalData

train_data_factor_columns[sapply(train_data_factor_columns, is.integer)] <- lapply(train_data_factor_columns[sapply(train_data_factor_columns, is.integer)], 
                                       as.factor)


chisq.test(train_originalData$ROLE_DEPTNAME, train_originalData$ROLE_TITLE)
chisq.test(as.factor(train_originalData$ACTION), as.factor(train_originalData$MGR_ID))
?assocstats

statsdf = xtabs(~ ROLE_DEPTNAME + ROLE_TITLE, data = train_originalData)
summary(assocstats(statsdf))


corr_df <- xtabs(~ ROLE_ROLLUP_1 + ROLE_ROLLUP_2 , data = train_originalData)
summary(assocstats(corr_df))




corr_df_2 <- xtabs(~ ROLE_DEPTNAME + ROLE_ROLLUP_2 , data = train_originalData)
summary(assocstats(corr_df_2))

corr_df_3 <- xtabs( ~ ROLE_ROLLUP_1 +ROLE_TITLE, data = train_originalData)
summary(assocstats(corr_df_3))


corr_df_4 <- xtabs( ~ ROLE_TITLE +ROLE_CODE, data = train_originalData)
summary(assocstats(corr_df_4))


corr_df_5 <- xtabs( ~ ROLE_ROLLUP_1 +ROLE_DEPTNAME, data = train_originalData)
summary(assocstats(corr_df_5))


corr_df_6 <- xtabs( ~ ROLE_CODE +ROLE_ROLLUP_2, data = train_originalData)
summary(assocstats(corr_df_6))

corr_df_7 <- xtabs(~ ROLE_FAMILY + ROLE_FAMILY_DESC, data = train_originalData)
summary(assocstats(corr_df_7))


corr_df_8 <- xtabs(~ ROLE_FAMILY + RESOURCE, data = train_originalData)
summary(assocstats(corr_df_8))

corr_df_9 =  xtabs(~ ROLE_FAMILY_DESC + RESOURCE, data = train_originalData)
summary(assocstats(corr_df_9))


corr_df_10 <- xtabs( ~ ROLE_DEPTNAME +ROLE_TITLE, data = train_originalData)
summary(assocstats(corr_df_10))

corr_df_11 <- xtabs( ~ ROLE_DEPTNAME +RESOURCE, data = train_originalData)
summary(assocstats(corr_df_11))

corr_df_12 <- xtabs(~ ROLE_FAMILY_DESC + ROLE_TITLE, data = train_originalData)
summary(assocstats(corr_df_12))

corr_df_13 <- xtabs(~ ROLE_FAMILY_DESC + ROLE_CODE, data = train_originalData)
summary(assocstats(corr_df_13))

corr_df_14 <- xtabs( ~ ROLE_CODE +RESOURCE, data = train_originalData)
summary(assocstats(corr_df_14))

corr_df_15 <- xtabs( ~ ROLE_ROLLUP_1 +RESOURCE, data = train_originalData)
summary(assocstats(corr_df_15))

corr_df_16 <- xtabs(~ ROLE_FAMILY + MGR_ID, data = train_originalData)
summary(assocstats(corr_df_16))

corr_df_17 <- xtabs( ~ ROLE_CODE +MGR_ID, data = train_originalData)
summary(assocstats(corr_df_17))

corr_df_18 <- xtabs( ~ ROLE_ROLLUP_1 +MGR_ID, data = train_originalData)
summary(assocstats(corr_df_18))


corr_df_19 <- xtabs( ~ ROLE_DEPTNAME +MGR_ID, data = train_originalData)
summary(assocstats(corr_df_19))

corr_df_20 <- xtabs( ~ RESOURCE +MGR_ID, data = train_originalData)
summary(assocstats(corr_df_20))
