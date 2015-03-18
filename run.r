
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


#add day column
 f_train=add_column_into_frame( P_data_frame = f_train
                              , P_column_vector = weekdays(as.Date(f_train$datetime))
                              , P_column_name = "day")

#add hour column
 f_train=add_column_into_frame( P_data_frame = f_train
                              , P_column_vector = hours(f_train[ ,c("datetime")])
                              , P_column_name = "hour")

#add day column
 f_test=add_column_into_frame( P_data_frame = f_test
                             , P_column_vector = weekdays(as.Date(f_test$datetime))
                             , P_column_name = "day")

#add hour column
 f_test=add_column_into_frame( P_data_frame = f_test
                             , P_column_vector = hours(f_test[ ,c("datetime")])
                             , P_column_name = "hour")


#training data
f_train_formatted = f_train[ ,c("season","holiday","workingday","day","hour","temp")]
#response data
response = as.integer(factor(f_train[ ,"count"]))
#test data
f_test_formatted = f_test[ ,c("season","holiday","workingday","day","hour","temp")]

fit=fit_with_cubist(P_train = f_train_formatted, P_response = response)
predict=predict_with_cubist(P_fit = fit,P_test = f_test_formatted)

