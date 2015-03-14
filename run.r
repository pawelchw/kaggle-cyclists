#clear existing elemts - comment this out if needed
rm(list = ls())

#flags for file reading
f_te=0
f_tr=0
f_sa=0
f_f=0

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
 f_test=load_testing_file( P_file_name = d_test_file, P_test =f_te )

#load training file
 f_train=load_testing_file( P_file_name = d_train_file, P_test =f_tr )

#load sample file
 f_sample=load_testing_file( P_file_name = d_sample_file, P_test = f_sa )

