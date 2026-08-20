load_node_data_434 <- function(directory) {
    files <- list.files(path = directory, pattern = "*434.csv")
    print(files)
    file_df <- data.frame()
    for (f in files) {
        file <- f
        node_id <- strsplit(file, split = "_")[[1]][1]
        single_file_df <- data.frame(
            file_name = file,
            node_id = node_id
        )
        file_df <- rbind(file_df, single_file_df)
    }

    files <- file_df$file_name
    print(paste("Loading", length(files), "node 434 MHz detection file(s) from:", directory))

    # combine full path with file name
    file_df$file <- lapply(file_df$file, function(x) {
        return(paste(directory, x, sep = ""))
    })
    # read all of the files into a data frame
    full_df <- rbindlist(apply(file_df, 1, function(f) {
        file_data <- fread(f$file)
        file_data <- file_data %>% mutate(node_id = f$node_id)
        return(file_data)
    }))

    print(paste("Finished Reading Files!", nrow(full_df), "total node 434 MHz detection records found."))
    return(full_df)
}

load_node_data_2p4 <- function(directory) {
    files <- list.files(path = directory, pattern = "*2p4.csv")
    print(files)
    file_df <- data.frame()
    for (f in files) {
        file <- f
        node_id <- strsplit(file, split = "_")[[1]][1]
        single_file_df <- data.frame(
            file_name = file,
            node_id = node_id
        )
        file_df <- rbind(file_df, single_file_df)
    }

    files <- file_df$file_name
    print(paste("Loading", length(files), "node 2.4 GHz detection file(s) from:", directory))

    # combine full path with file name
    file_df$file <- lapply(file_df$file, function(x) {
        return(paste(directory, x, sep = ""))
    })
    # read all of the files into a data frame
    full_df <- rbindlist(apply(file_df, 1, function(f) {
        file_data <- fread(f$file)
        file_data <- file_data %>% mutate(node_id = f$node_id)
        return(file_data)
    }))

    print(paste("Finished Reading Files!", nrow(full_df), "total node 2.4 GHz detection records found."))
    return(full_df)
}
