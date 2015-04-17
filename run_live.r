set.seed(415)
#need this library for working our hours from time stamps
library("chron")


#clear existing elemts - comment this out if needed
rm(list = ls())

#flags for file reading
t_te=0
t_tr=0
t_sa=0
t_f=0

d_kaggle="/home/pawel/kaggle/kaggle-cyclists/"                                    # working directory
d_test_file="/home/pawel/kaggle/datasets/kaggle-cyclists/test.csv"                # test file
d_train_file="/home/pawel/kaggle/datasets/kaggle-cyclists/train.csv"              # train file
d_sample_file="/home/pawel/kaggle/datasets/kaggle-cyclists/sampleSubmission.csv"  # sample file


# set the working directory to the required directory
setwd(d_kaggle)

#load needed functions
source("code/funcs.r")

# it is not allowed to upload dataset into repository due to license limitation
# therefore, we have to load them from the local repository, sorry
# files can be obtained from http://www.kaggle.com/c/bike-sharing-demand/data

#load testing file
f_test=load_testing_file( P_file_name = d_test_file, P_test =t_te )

#load training file
f_train=load_testing_file( P_file_name = d_train_file, P_test =t_tr )

#load sample file
f_sample=load_testing_file( P_file_name = d_sample_file, P_test = t_sa )


#training data
f_train_formatted=feature_engineering(P_train_set = f_train,P_orig_set = f_train)
#testing data
f_test_formatted=feature_engineering(P_train_set = f_train,P_orig_set = f_test)
#response data
response_count = as.integer(factor(f_train[ ,"count"]))
response_casual = as.integer(factor(f_train[ ,"casual"]))
response_registered = as.integer(factor(f_train[ ,"registered"]))

f_train_formatted=variable_transformation(P_train_set=f_train_formatted)
f_train_formatted = remove_column(P_train_set = f_train_formatted, P_column_name = "holiday") # remove them from training below
f_test_formatted = remove_column(P_train_set = f_test_formatted, P_column_name = "holiday") # remove them from training below
tm_df = split_into_years(P_train_df=f_train_formatted,P_test_df=f_test_formatted)
# df_t_t = split_for_training(P_training_set=f_train_formatted)
# 
# f_train_formatted=df_t_t$train
# f_test_formatted=df_t_t$test
# 
library(randomForest)
n_trees = 500
n_mtry = 5
b_importance = TRUE
f_fit_casual_1<- randomForest(casual 
                              ~ season 
                              #+ holiday
                              + workingday
                              + weather  
                              + temp
                              + atemp 
                              + humidity
                              + windspeed 
                              + day 
                              + month 
                              + year 
                              #+ hour
                              , data=tm_df$train_1
                              , ntree=n_trees
                              , mtry=n_mtry
                              , importance=b_importance
)
f_fit_casual_2<- randomForest(casual 
                              ~ season 
                              #+ holiday
                              + workingday
                              + weather  
                              + temp
                              + atemp 
                              + humidity
                              + windspeed 
                              + day 
                              + month 
                              #+ year 
                              + hour
                              , data=tm_df$train_2
                              , ntree=n_trees
                              , mtry=n_mtry
                              , importance=b_importance
)
f_fit_registered_1<- randomForest(registered  
                                  ~ season 
                                  #+ holiday
                                  + workingday
                                  + weather  
                                  + temp
                                  + atemp 
                                  + humidity
                                  + windspeed 
                                  + day 
                                  + month 
                                  #+ year 
                                  + hour
                                  , data=tm_df$train_1
                                  , ntree=n_trees
                                  , mtry=n_mtry
                                  , importance=b_importance                                )

f_fit_registered_2<- randomForest(registered  
                                  ~ season 
                                  #+ holiday
                                  + workingday
                                  + weather  
                                  + temp
                                  + atemp 
                                  + humidity
                                  + windspeed 
                                  + day 
                                  + month 
                                  #+ year 
                                  + hour
                                  , data=tm_df$train_2
                                  , ntree=n_trees
                                  , mtry=n_mtry
                                  , importance=b_importance                                )


f_predict_casual_1=predict(f_fit_casual_1,tm_df$test_1)
f_predict_casual_2=predict(f_fit_casual_2,tm_df$test_2)
f_predict_registered_1=predict(f_fit_registered_1,tm_df$test_1)
f_predict_registered_2=predict(f_fit_registered_2,tm_df$test_2)
time_1 = tm_df$test_1$datetime
time_2 = tm_df$test_2$datetime


df_tmp_1 = data.frame(datetime = tm_df$test_1$datetime, count = round(exp(f_predict_casual_1)-1 + exp(f_predict_registered_1)-1))
df_tmp_2 = data.frame(datetime = tm_df$test_2$datetime, count = round(exp(f_predict_casual_2)-1 + exp(f_predict_registered_2)-1))

res=rbind(df_tmp_1,df_tmp_2)

write.csv(res, file = "output.csv",row.names=FALSE)

# #f_test_formatted$count_predict = round(  exp(f_predict_casual)+exp(f_predict_registered)  ,0)
