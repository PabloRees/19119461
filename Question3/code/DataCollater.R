#ref https://github.com/zander-prinsloo/20065124/blob/main/Question2/code/fn_Collate_Data.R

collateData <- function(path, keyword){

    library(tidyverse)

    # function imports all the files of extension `ext` within the folder described by `path`
    # and then collates them into one data set

    # Which files should be imported? - all with specified extension
    data_files <- list.files(path = path, full.names = T, pattern = keyword)

    # Import them into a list
    data_list <- list() # create empty list object
    data_list <- lapply(
        data_files, # full names of data files to import
        read_csv, # import each as tibble
        col_names = T, # keep column names
        col_types = cols() # suppress messages
        # skip_empty_rows = T,
        # quoted_na = "string"
    )

    all_character <- function(df){

        if ('loser_seed' %in% colnames(df)){
        df <- df %>%
            mutate(loser_seed = as.character(loser_seed),
                   winner_seed = as.character(winner_seed))
        }


        df
    }

    data_list <- lapply(X = data_list, FUN = all_character)

    # Check whether each data set has the same column names
    for(i in 1:length(data_list)){
        if(!identical(colnames(data_list[[1]]), colnames(data_list[[i]])))
            stop(paste("The column names of 1 and", i, "are not the same"))
    }

    # Bind the data sets by rows
    final_data <- data_list %>%
        bind_rows()


    # Output final data set
    final_data



}