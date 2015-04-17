#datetime - hourly date + timestamp  
#season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
#holiday - whether the day is considered a holiday
#workingday - whether the day is neither a weekend nor holiday
#weather - 1: Clear, Few clouds, Partly cloudy, Partly cloudy 
#weather -2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
#weather -3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds 
#weather -4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
#temp - temperature in Celsius
#atemp - "feels like" temperature in Celsius
#humidity - relative humidity
#windspeed - wind speed
#casual - number of non-registered user rentals initiated
#registered - number of registered user rentals initiated
#count - number of total rentals

# this function reads a csv file from a working directory
   load_testing_file <- function( P_file_name  # file name to search for in the working directory
                                , P_test # used as flag indicating
                                         #  1 - correct file location
                                         # -1 - unable to locate the file
                                 )
   {
   #find out if file exists
      if ( file.exists( P_file_name ) )
      {
         csv.test <- read.csv( P_file_name )
         P_test = 1
         return ( csv.test  )
      }
      else
      {
        print(  paste("file ", P_file_name, " doesn-t exist") )
        P_test = -1
        return ( -1 )
      }
   }
   
# this function adds a column into a dataframe
   add_column_into_frame <- function( P_data_frame    # dataframe to be extended
                                    , P_train_column         # we need this for lookup generation
                                    , P_orig_name     # original column name to point to original column
                                    , P_column_vector # vector to be added
                                    , P_column_name   # name of the new column
                                    )
   {
     #check if string/characters
     if (typeof(P_column_vector) %in% c("character","string"))
     {
        l_df = prep_lookup( P_column_vector = P_train_column
                          , P_column_name = P_column_name)  
        P_data_frame[, P_column_name] = match(P_column_vector,l_df$P_column_name)
     }
     # just add it
     else
     {
        P_data_frame[, P_column_name] = as.numeric(P_column_vector)
     }
     return (P_data_frame)
   }
   
   
# this function prepares lookup table for unique string values
   prep_lookup <- function( P_column_vector # vector of strings; each string will get a unique no
                          , P_column_name # name used inside data frame
                          )
   {
     #apparently, this is the fastest method of converting a vector of strings
     #into unique values. see
     # http://stackoverflow.com/questions/3879522/finding-unique-values-from-a-list
     # for details
     
     df = data.frame( P_column_name = unique(unlist(P_column_vector, use.names = FALSE)))
     # extend by adding unique numeric value
     df[, "id"]=seq(from = 1,to = dim(df)[1])
     return (df)
   }
   
   
# this functionperforms data modification of different sort before
# any training is applied
   
  feature_engineering <-function( P_train_set # we need this for lookups
                                , P_orig_set  # we will be changing some features here
                                )
  {
    
    #add day column
    P_orig_set=add_column_into_frame( P_data_frame = P_orig_set
                                    , P_train_column = weekdays(as.Date(P_train_set$datetime))
                                    , P_orig_name = "datetime"
                                    , P_column_vector = weekdays(as.Date(P_orig_set$datetime))
                                    , P_column_name = "day")
    #add month column
    P_orig_set=add_column_into_frame( P_data_frame = P_orig_set
                                    , P_train_column = months(as.Date(P_train_set$datetime))
                                    , P_orig_name = "datetime"
                                    , P_column_vector = months(as.Date(P_orig_set$datetime))
                                    , P_column_name = "month")
    
    #add year column
    P_orig_set=add_column_into_frame( P_data_frame = P_orig_set
                                    , P_train_column = years(as.Date(P_train_set$datetime))
                                    , P_orig_name = "datetime"
                                    , P_column_vector = years(as.Date(P_orig_set$datetime))
                                    , P_column_name = "year")
    #add hour column
    P_orig_set=add_column_into_frame( P_data_frame = P_orig_set
                                    , P_train_column = hours(P_train_set[ ,c("datetime")])
                                    , P_orig_name = "datetime"
                                    , P_column_vector = hours(P_orig_set[ ,c("datetime")])
                                    , P_column_name = "hour")
    
    return(P_orig_set)
  }
   
   variable_transformation <- function (P_train_set)
   {
     df_ts=P_train_set
     # we need to get do some cleaning here 
     df_ts=df_ts[df_ts$weather <4,]
     df_ts$casual=log(df_ts$casual+1)
     df_ts$registered=log(df_ts$registered+1)
     #df_ts$count=log(df_ts[(df_ts$count>0),]$count)
     #df_ts[(df_ts$temp>0),]$temp=log(df_ts[(df_ts$temp>0),]$temp)
     #df_ts[(df_ts$atemp>0),]$atemp=log(df_ts[(df_ts$atemp>0),]$atemp)
  
     
     return(df_ts)
   }
   remove_column <- function ( P_train_set
                             , P_column_name)
   {
     df_ts = P_train_set
     col_cn = P_column_name
     return(df_ts[ !(colnames(df_ts) %in% c(col_cn))])
   }
# return some counts indicating 3 sigma rule applicaiton
# in general we want to clear some data for training - and we want to know score distribution
   
   three_sigma_test <- function( P_train_set
                              , P_column)
     
      { #these columns give high
     P_train_set=f_train_formatted
     P_column_list = c("year","hour","season", P_column)
     P_gr_year = P_train_set$year
     P_gr_hour = P_train_set$hour
     P_gr_season = P_train_set$season
     #we want to aggregate to get mean per year and hour
     t_agg_mu=aggregate( P_train_set[, P_column_list]
                         , by=list(gr_year = P_gr_year, gr_hour =P_gr_hour, gr_season=P_gr_season)
                         , FUN=mean
     )
     
     #same here but we want standard deviation
     t_agg_sd=aggregate( P_train_set[, P_column_list]
                         , by=list(gr_year = P_gr_year, gr_hour =P_gr_hour, gr_season=P_gr_season)
                         , FUN=sd
     )
     
     #order by year hour to have better insight
     t_agg_mu=t_agg_mu[with(t_agg_mu, order(gr_year, gr_hour, gr_season)),]
     t_agg_sd=t_agg_sd[with(t_agg_sd, order(gr_year, gr_hour, gr_season)),]
     
     
     t_res_df = data.frame( year = t_agg_mu$gr_year
                            , hour=t_agg_mu$gr_hour
                            , season = t_agg_mu$season
                            , mu_col = t_agg_mu[,c(P_column)]
                            , sd_col = t_agg_sd[,c(P_column)] 
                            , orig = rep(x = 0,dim(t_agg_mu)[1])
                            , one_sd = rep(x = 0,dim(t_agg_mu)[1])
                            , two_sd = rep(x = 0,dim(t_agg_mu)[1])
                            , three_sd = rep(x = 0,dim(t_agg_mu)[1])
     )
     for ( i in 1:dim(t_res_df)[1])
     {
       t_res_df$orig[i] = dim(P_train_set[ #find out how many have we got for..
         ( P_train_set$year ==  t_res_df$year[i] &
             P_train_set$hour ==  t_res_df$hour[i] &
             P_train_set$season ==  t_res_df$season[i]
         )                            
         , ]
       )[1]
       t_res_df$one_sd[i] = dim(P_train_set[ #find out how many have we got for..
         ( P_train_set[,c(P_column)] >  t_res_df$mu_col[i] - 1*t_res_df$sd_col[i] &
             P_train_set[,c(P_column)] <  t_res_df$mu_col[i] + 1*t_res_df$sd_col[i] &
             P_train_set$year ==  t_res_df$year[i] &
             P_train_set$hour ==  t_res_df$hour[i] &
             P_train_set$season ==  t_res_df$season[i]
         )                            
         , ]
       )[1]
       t_res_df$two_sd[i] = dim(P_train_set[ #find out how many have we got for..
         ( P_train_set[,c(P_column)] >  t_res_df$mu_col[i] - 2*t_res_df$sd_col[i] &
             P_train_set[,c(P_column)] <  t_res_df$mu_col[i] + 2*t_res_df$sd_col[i] &
             P_train_set$year ==  t_res_df$year[i] &
             P_train_set$hour ==  t_res_df$hour[i] &
             P_train_set$season ==  t_res_df$season[i]
         )                            
         , ]
       )[1]
       t_res_df$three_sd[i] = dim(P_train_set[ #find out how many have we got for..
         ( P_train_set[,c(P_column)] >  t_res_df$mu_col[i] - 3*t_res_df$sd_col[i] &
             P_train_set[,c(P_column)] <  t_res_df$mu_col[i] + 3*t_res_df$sd_col[i] &
             P_train_set$year ==  t_res_df$year[i] &
             P_train_set$hour ==  t_res_df$hour[i] &
             P_train_set$season ==  t_res_df$season[i]
         )                            
         , ]
       )[1]     
       
     }
        return (t_res_df)
      }
   
#funciton to clear outliers
  clear_outliers <- function ( P_train_set # we are doing it for training right?
                             , P_decision_matrix # were we have calculated means and std deviations
                             , P_range
                             , P_column
  )
  {
    df_ts = P_train_set
    df_ts$to_delete = rep(FALSE, dim(df_ts)[1]) 
    df_dm = P_decision_matrix
    i_rn = P_range
   #go through the decision matrix and strip out the rows that are outside the range
    for ( i in 1:dim(df_dm)[1])
    {
    
      if ( dim(
               df_ts[ #find out how many have we got for..
               ( df_ts$year ==  df_dm$year[i] & df_ts$hour ==  df_dm$hour[i] & df_ts$season ==  df_dm$season[i] &
                  (
                   df_ts[,c(P_column)] <  df_dm$mu_col[i] - i_rn*df_dm$sd_col[i] 
                   |
                   df_ts[,c(P_column)] >  df_dm$mu_col[i] + i_rn*df_dm$sd_col[i] 
                   )
               )                            
              , ])[1] > 0)
      {
        df_ts[ #find out how many have we got for..
          ( df_ts$year ==  df_dm$year[i] & df_ts$hour ==  df_dm$hour[i] & df_ts$season ==  df_dm$season[i] &
              (
                df_ts[,c(P_column)] <  df_dm$mu_col[i] - i_rn*df_dm$sd_col[i] 
                |
                df_ts[,c(P_column)] >  df_dm$mu_col[i] + i_rn*df_dm$sd_col[i] 
              )
          )                            
          , ]$to_delete = TRUE
      }
     }
    return(df_ts[(df_ts$to_delete == FALSE),])
  }
  
#function to normalise
  normalise_column <- function( P_training_set, P_column_name)
  {
    df_ts = P_training_set
    s_cn = P_column_name
    t_col = df_ts[,c(s_cn)]
    #i_min = min(t_col)
    #i_max = max(t_col)
    i_mean = mean(t_col)
    i_sd = sd(t_col)
    #t_col_n = ( (t_col- i_min)/(i_max- i_min))
    t_col_n = ( (t_col- i_mean)/i_sd)
    return(t_col_n)
  }
   
# function to split training dataset for trainin and validating purposes
# kaggle allows only for 5 submissions per day, therefore it is better 
# to have own version of testing/validating
  split_for_training <- function(P_training_set)
  {
    df_training=P_training_set
    #10 percent sample from the training set
    df_tmp = sample( seq(from = 1,to =dim(df_training)[1] )
                     , size =floor(0.1*dim(df_training)[1])
                     , replace = FALSE)
    
    
    df =list( train = df_training[-c(df_tmp),], test = df_training[df_tmp,])
    

    
    return (df)
  }
   
   split_into_years <- function(P_train_df ,P_test_df)
   {
     i_year= 1
     
     f_train_1=P_train_df[(P_train_df$year == 1), ]
     f_train_2=P_train_df[(P_train_df$year == 2), ]
     f_test_1=P_test_df[(P_test_df$year == 1), ]
     f_test_2=P_test_df[(P_test_df$year == 2), ]
     dim(f_train_1)
     dim(f_train_2)
     dim(f_test_1)
     dim(f_test_2)
     
     t_df_temp = three_sigma_test( f_train_1, "temp")
     f_train_1= clear_outliers ( P_train_set = f_train_1 , P_decision_matrix = t_df_temp, P_range =3 , P_column = "temp") 
     
     t_df_atp = three_sigma_test( f_train_1, "atemp")
     f_train_1= clear_outliers ( P_train_set = f_train_1, P_decision_matrix = t_df_atp,  P_range =3, P_column = "atemp") 
     
     t_df_hum = three_sigma_test( f_train_1, "humidity")
     f_train_1= clear_outliers ( P_train_set = f_train_1 , P_decision_matrix = t_df_hum, P_range =3 , P_column = "humidity")
     
     t_df_wsp = three_sigma_test( f_train_1, "windspeed")
     f_train_1= clear_outliers ( P_train_set = f_train_1 , P_decision_matrix = t_df_wsp, P_range =3  , P_column = "windspeed") 
     
     f_train_1$season = normalise_column(P_training_set = f_train_1 , P_column_name = "season")
     f_train_1$weather = normalise_column(P_training_set = f_train_1, P_column_name = "weather")
     f_train_1$temp = normalise_column(P_training_set = f_train_1, P_column_name = "temp")
     f_train_1$atemp = normalise_column(P_training_set = f_train_1, P_column_name = "atemp")
     f_train_1$humidity = normalise_column(P_training_set = f_train_1, P_column_name = "humidity")
     f_train_1$windspeed = normalise_column(P_training_set = f_train_1, P_column_name = "windspeed")
     f_train_1$day = normalise_column(P_training_set = f_train_1, P_column_name = "day")
     f_train_1$month = normalise_column(P_training_set = f_train_1, P_column_name = "month")
     #f_train_1$year = normalise_column(P_training_set = f_train_1, P_column_name = "year")
     f_train_1$hour = normalise_column(P_training_set = f_train_1, P_column_name = "hour")
     
     
     f_test_1$season = normalise_column(P_training_set = f_test_1 , P_column_name = "season")
     f_test_1$weather = normalise_column(P_training_set = f_test_1, P_column_name = "weather")
     f_test_1$temp = normalise_column(P_training_set = f_test_1, P_column_name = "temp")
     f_test_1$atemp = normalise_column(P_training_set = f_test_1, P_column_name = "atemp")
     f_test_1$humidity = normalise_column(P_training_set = f_test_1, P_column_name = "humidity")
     f_test_1$windspeed = normalise_column(P_training_set = f_test_1, P_column_name = "windspeed")
     f_test_1$day = normalise_column(P_training_set = f_test_1, P_column_name = "day")
     f_test_1$month = normalise_column(P_training_set = f_test_1, P_column_name = "month")
     #f_test_1$year = normalise_column(P_training_set = f_test_1, P_column_name = "year")
     f_test_1$hour = normalise_column(P_training_set = f_test_1, P_column_name = "hour")
     
     
     t_df_temp = three_sigma_test( f_train_2, "temp")
     f_train_2= clear_outliers ( P_train_set = f_train_2 , P_decision_matrix = t_df_temp, P_range =3 , P_column = "temp") 
     
     t_df_atp = three_sigma_test( f_train_2, "atemp")
     f_train_2= clear_outliers ( P_train_set = f_train_2, P_decision_matrix = t_df_atp,  P_range =3, P_column = "atemp") 
     
     t_df_hum = three_sigma_test( f_train_2, "humidity")
     f_train_2= clear_outliers ( P_train_set = f_train_2 , P_decision_matrix = t_df_hum, P_range =3 , P_column = "humidity")
     
     t_df_wsp = three_sigma_test( f_train_2, "windspeed")
     f_train_2= clear_outliers ( P_train_set = f_train_2 , P_decision_matrix = t_df_wsp, P_range =3  , P_column = "windspeed") 
     
     f_train_2$season = normalise_column(P_training_set = f_train_2 , P_column_name = "season")
     f_train_2$weather = normalise_column(P_training_set = f_train_2, P_column_name = "weather")
     f_train_2$temp = normalise_column(P_training_set = f_train_2, P_column_name = "temp")
     f_train_2$atemp = normalise_column(P_training_set = f_train_2, P_column_name = "atemp")
     f_train_2$humidity = normalise_column(P_training_set = f_train_2, P_column_name = "humidity")
     f_train_2$windspeed = normalise_column(P_training_set = f_train_2, P_column_name = "windspeed")
     f_train_2$day = normalise_column(P_training_set = f_train_2, P_column_name = "day")
     f_train_2$month = normalise_column(P_training_set = f_train_2, P_column_name = "month")
     f_train_2$year = normalise_column(P_training_set = f_train_2, P_column_name = "year")
     #f_train_2$hour = normalise_column(P_training_set = f_train_2, P_column_name = "hour")
     
     
     f_test_2$season = normalise_column(P_training_set = f_test_2 , P_column_name = "season")
     f_test_2$weather = normalise_column(P_training_set = f_test_2, P_column_name = "weather")
     f_test_2$temp = normalise_column(P_training_set = f_test_2, P_column_name = "temp")
     f_test_2$atemp = normalise_column(P_training_set = f_test_2, P_column_name = "atemp")
     f_test_2$humidity = normalise_column(P_training_set = f_test_2, P_column_name = "humidity")
     f_test_2$windspeed = normalise_column(P_training_set = f_test_2, P_column_name = "windspeed")
     f_test_2$day = normalise_column(P_training_set = f_test_2, P_column_name = "day")
     f_test_2$month = normalise_column(P_training_set = f_test_2, P_column_name = "month")
     f_test_2$year = normalise_column(P_training_set = f_test_2, P_column_name = "year")
     #f_test_2$hour = normalise_column(P_training_set = f_test_2, P_column_name = "hour")
     
     return ( list (train_1=f_train_1,train_2=f_train_2,test_1=f_test_1,test_2=f_test_2))
   }   