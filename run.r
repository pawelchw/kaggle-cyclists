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
#training data

f_test_formatted=feature_engineering(P_train_set = f_train,P_orig_set = f_test)
#response data
response_count = as.integer(factor(f_train[ ,"count"]))
response_casual = as.integer(factor(f_train[ ,"casual"]))
response_registered = as.integer(factor(f_train[ ,"registered"]))


#the following is giving me 0.4360
f_train_formatted=variable_transformation(P_train_set=f_train_formatted)
f_train_formatted = remove_column(P_train_set = f_train_formatted, P_column_name = "holiday") # remove them from training below
f_test_formatted = remove_column(P_train_set = f_test_formatted, P_column_name = "holiday") # remove them from training below

#f_train_formatted_pca = prcomp(f_test_formatted,center = TRUE,scale. = TRUE) 
# f_train_formatted_pca = prcomp(f_train_formatted[, !colnames(f_train_formatted) %in% c("datetime","count","registered","casual")]
#                                ,center = TRUE
#                                ,scale. = TRUE)
# f_test_formatted_pca = predict(f_train_formatted_pca
#                                , newdata = f_test_formatted[,!colnames(f_test_formatted) %in% ("datetime")])
# 
# f_train_pca=as.data.frame(f_train_formatted_pca$x[,1:12])
# f_test_pca=as.data.frame(f_test_formatted_pca[,1:12])
# f_train_pca$casual = f_train_formatted$casual
# f_train_pca$registered = f_train_formatted$registered
#t_train_and_test = merge( x= f_train_formatted,y = f_test_formatted,by = "datetime",all = TRUE)
# t_train_and_test = rbind(f_train_formatted[,colnames(f_test_formatted)], f_test_formatted)
# 
t_train_and_test=f_train_formatted
t_df_temp = three_sigma_test( t_train_and_test, "temp")
f_train_formatted= clear_outliers ( P_train_set = f_train_formatted 
                                    , P_decision_matrix = t_df_temp
                                    , P_range =3
                                    , P_column = "temp"
) 

t_df_atp = three_sigma_test( t_train_and_test, "atemp")
f_train_formatted= clear_outliers ( P_train_set = f_train_formatted 
                                    , P_decision_matrix = t_df_atp
                                    , P_range =3
                                    , P_column = "atemp"
) 

t_df_hum = three_sigma_test( t_train_and_test, "humidity")
f_train_formatted= clear_outliers ( P_train_set = f_train_formatted 
                      , P_decision_matrix = t_df_hum
                      , P_range =3
                      , P_column = "humidity"
                      )

t_df_wsp = three_sigma_test( t_train_and_test, "windspeed")
f_train_formatted= clear_outliers ( P_train_set = f_train_formatted 
                                    , P_decision_matrix = t_df_wsp
                                    , P_range =3
                                    , P_column = "windspeed"
) 

f_train_formatted$season = normalise_column(P_training_set = f_train_formatted
                                              ,P_column_name = "season")
f_train_formatted$weather = normalise_column(P_training_set = f_train_formatted
                                              ,P_column_name = "weather")
f_train_formatted$temp = normalise_column(P_training_set = f_train_formatted
                                              ,P_column_name = "temp")
f_train_formatted$atemp = normalise_column(P_training_set = f_train_formatted
                                              ,P_column_name = "atemp")
f_train_formatted$humidity = normalise_column(P_training_set = f_train_formatted
                                               ,P_column_name = "humidity")
f_train_formatted$windspeed = normalise_column(P_training_set = f_train_formatted
                                               ,P_column_name = "windspeed")
f_train_formatted$day = normalise_column(P_training_set = f_train_formatted
                                         ,P_column_name = "day")
f_train_formatted$month = normalise_column(P_training_set = f_train_formatted
                                         ,P_column_name = "month")
f_train_formatted$year = normalise_column(P_training_set = f_train_formatted
                                         ,P_column_name = "year")
f_train_formatted$hour = normalise_column(P_training_set = f_train_formatted
                                               ,P_column_name = "hour")


f_test_formatted$season = normalise_column(P_training_set = f_test_formatted
                                              ,P_column_name = "season")
f_test_formatted$weather = normalise_column(P_training_set = f_test_formatted
                                           ,P_column_name = "weather")
f_test_formatted$temp = normalise_column(P_training_set = f_test_formatted
                                          ,P_column_name = "temp")
f_test_formatted$atemp = normalise_column(P_training_set = f_test_formatted
                                               ,P_column_name = "atemp")
f_test_formatted$humidity = normalise_column(P_training_set = f_test_formatted
                                               ,P_column_name = "humidity")
f_test_formatted$windspeed = normalise_column(P_training_set = f_test_formatted
                                               ,P_column_name = "windspeed")
f_test_formatted$day = normalise_column(P_training_set = f_test_formatted
                                        ,P_column_name = "day")
f_test_formatted$month = normalise_column(P_training_set = f_test_formatted
                                        ,P_column_name = "month")
f_test_formatted$year = normalise_column(P_training_set = f_test_formatted
                                          ,P_column_name = "year")
f_test_formatted$hour = normalise_column(P_training_set = f_test_formatted
                                              ,P_column_name = "hour")

#shall we clear more columns ?
# 
library(randomForest)
#variables
n_trees = 500
n_mtry = 5
b_importance = TRUE
#fit and predict casual
# f_fit_casual<- randomForest(casual 
#                             ~ PC1 
#                             + PC2
#                             + PC3
#                             + PC4  
#                             + PC5
#                             + PC6 
#                             + PC7
#                             + PC8 
#                             + PC9 
#                             + PC10 
#                             + PC11 
#                             + PC12
#                             , data=f_train_pca#f_train_formatted
#                             , ntree=n_trees
#                             , mtry=n_mtry
#                             , importance=b_importance
# )
# f_fit_registered<- randomForest(registered  
#                                 ~ PC1 
#                                 + PC2
#                                 + PC3
#                                 + PC4  
#                                 + PC5
#                                 + PC6 
#                                 + PC7
#                                 + PC8 
#                                 + PC9 
#                                 + PC10 
#                                 + PC11 
#                                 + PC12
#                                 , data=f_train_pca#f_train_formatted
#                                 , ntree=n_trees
#                                 , mtry=n_mtry
#                                 , importance=b_importance                                )
#  f_predict_casual=predict(f_fit_casual,f_test_pca)
#  f_predict_registered=predict(f_fit_registered,f_test_pca)
#  f_output = data.frame(datetime=f_test$datetime)
#  f_output$count <- round(exp(f_predict_casual)+exp(f_predict_registered),0)
#  write.csv(f_output, file = "output.csv",row.names=FALSE)




f_fit_casual<- randomForest(casual 
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
                            + hour
                            , data=f_train_formatted
                            , ntree=n_trees
                            , mtry=n_mtry
                            , importance=b_importance
)
f_fit_registered<- randomForest(registered  
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
                                + hour
                                , data=f_train_formatted
                                , ntree=n_trees
                                , mtry=n_mtry
                                , importance=b_importance                                )
f_fit_count<- randomForest(count  
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
                                + hour
                                , data=f_train_formatted
                                , ntree=n_trees
                                , mtry=n_mtry
                                , importance=b_importance                                )
 f_predict_casual=predict(f_fit_casual,f_test_formatted)
 f_predict_registered=predict(f_fit_registered,f_test_formatted)
 f_predict_count=predict(f_fit_count,f_test_formatted)
 f_output = data.frame(datetime=f_test$datetime)

  #df_res=data.frame(  s=round(  exp(f_predict_casual)+exp(f_predict_registered)  ,0)
  #                    , c=round(exp(f_predict_count),0) )
#   
#   df_res$res = rep(0,dim(df_res)[1])    
#   
#   for (i in 1:dim(df_res)[1])
#   {
#     if (df_res$s[i] >= df_res$c[i])
#     {
#       df_res$res[i]=df_res$s[i]
#     }
#     else
#     {
#       df_res$res[i]=df_res$c[i]
#     }
#   }
 #f_output$count = df_res$res
f_output$count = round(  exp(f_predict_casual)+exp(f_predict_registered)-2  ,0)
 write.csv(f_output, file = "output.csv",row.names=FALSE)

