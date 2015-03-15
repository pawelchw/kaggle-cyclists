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
   add_column_into_frame <- function( P_data_frame # dataframe to be extended
                                    , P_column_vector # vector to be added
                                    , P_column_name # name of the new column
                                    )
   {
     #check if string/characters
     if (typeof(P_column_vector) %in% c("character","string"))
     {
        l_df = prep_lookup( P_column_vector = P_column_vector
                          , P_column_name = P_column_name)  
        P_data_frame[, P_column_name] = match(P_column_vector,l_df$P_column_name)
     }
     # just add it
     else
     {
        P_data_frame[, P_column_name] = P_column_vector 
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
     
     # create data frame with a columd day of unique days
     df = data.frame( P_column_name = unique(unlist(P_column_vector, use.names = FALSE)))
     # extend by adding unique numeric value
     df[, "id"]=seq(from = 1,to = dim(df)[1])
     return (df)
   }
   
   
# we need to transform strings into ids
