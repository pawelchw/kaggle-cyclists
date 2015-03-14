# this function reads a csv file from a working directory
   load_testing_file <- function( P_file_name  # file name to search for in the working directory
                                 )
   {
  
   #find out if file exists
      if (file.exists( P_file_name ))
      {
         csv.test <- read.csv("../test.csv")
         return (1)
      }
      else
      {
        print("file"+P_file_name+"doesn-t exist")
        return (-1)
      }
}