
# generateUUID
generate_UUID <- function(N)
{
  if (require('uuid')) {
    UUID <- character()
    for(i in 1:N) {
      UUID[i] <- UUIDgenerate()
    }
    return(UUID)
  } else {
    warning('library dplR missing')
  }
}

# fetch all data on disk and compile to one dataframe
loadData <- function() {
  # read all the files into a list
  files <- list.files("./data/output", full.names = TRUE)
  
  if (length(files) == 0 ) {
    # create empty data frame with correct columns
    field_list <- c(names(as.data.frame(locations)),names(taxon_list))
    result_data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(result_data) <- field_list
  } else {
    result_data <- lapply(files, function(x) read.csv(x)) 
    
    # Concatenate all data together into one data.frame
    result_data <- do.call(rbind, result_data)
  }
  
  result_data
}


