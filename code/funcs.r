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
     P_data_frame[, P_column_name] = P_column_vector
     return (P_data_frame)
   }