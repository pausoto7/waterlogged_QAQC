#Bind_hobo_utils

# READ METADATA FILE, CONVERT datetime to correct format, check that removal is after deployment
QAQC_metadata <- function(metadata_path) {
  tryCatch({
    # 1) File check
    if (!file.exists(metadata_path)) {
      stop("File not found: ", metadata_path)
    }
    
    # 2) Read CSV (quietly, no factors)
    metadata <- read.csv(metadata_path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

    # 3) Required columns present?
    required_cols <- c("site_station_code", "timestamp_deploy", "timestamp_remove")
    missing_cols  <- setdiff(required_cols, names(metadata))
    if (length(missing_cols)) {
      stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
    }
    # 4) Parse timestamps (expect mdy_hm; auto-UTC)
    parse_dt <- function(x) {
      x_chr <- as.character(x)
      p1 <- try(lubridate::mdy_hm(x_chr, tz = "UTC"), silent = TRUE)
      if (!inherits(p1, "try-error") && !all(is.na(p1))) return(p1)
      
      # fallback to common variants
      p2 <- suppressWarnings(
        lubridate::parse_date_time(
          x_chr,
          orders = c("mdy HM", "mdy HMS", "mdY HM", "mdY HMS", "ymd HM", "ymd HMS"),
          tz = "UTC"
        )
      )
      if (all(is.na(p2))) {
        stop("Failed to parse timestamp column. Expected mdy_hm like '4/30/2025 13:45' in UTC.")
      }
      p2
    }
    
    metadata$timestamp_deploy <- parse_dt(metadata$timestamp_deploy)
    metadata$timestamp_remove <- parse_dt(metadata$timestamp_remove)
    
    # 5) sanity checks (fail hard)
    if (any(metadata$timestamp_deploy > metadata$timestamp_remove, na.rm = TRUE)) {
      bad_rows <- which(metadata$timestamp_deploy > metadata$timestamp_remove)
      stop("Deploy time is after remove time on row(s): ",
           paste(bad_rows, collapse = ", "))
    }
    
    # 6) Light type hygiene (common with logger data)
    if ("sn" %in% names(metadata)) {
      metadata$sn <- as.character(metadata$sn)  # preserve leading zeros if any
    }
    
    metadata
    
  }, error = function(e) {
    stop("Error reading/parsing metadata: ", e$message)
  })
}

QAQC_loggertype <- function(logger_type) {
  
  # Define accepted values (always lowercase)
  valid_types <- c("u20", "u26", "tidbit")
  
  # Clean input, trim and put to lowercase
  logger_type <- tolower(trimws(as.character(logger_type)))
  
  # Check validity
  if (is.na(logger_type) || !logger_type %in% valid_types) {
    stop(
      "QC check failed: invalid 'logger_type' value: '", logger_type, "'.\n",
      "Valid options are: ", paste(valid_types, collapse = ", ")
    )
  }
  
  # Return cleaned string (standardized case/spacing)
  return(logger_type)
}


qc_measurement_type <- function(measurement_type) {
  # Define accepted values (always lowercase)
  valid_types <- c("waterlevel", "barometric", "conductivity", "dissolvedoxygen")
  
  # Handle input: if factor, convert to character; trim whitespace and lowercase
  measurement_type <- tolower(trimws(as.character(measurement_type)))
  
  # Identify invalid or missing entries
  bad_rows <- which(is.na(measurement_type) | !measurement_type %in% valid_types)
  
  # Stop if any invalid
  if (length(bad_rows) > 0) {
    stop(
      "QC check failed: invalid 'measurement_type' value(s) found at index(es): ",
      paste(bad_rows, collapse = ", "),
      "\nValid options are: ",
      paste(valid_types, collapse = ", ")
    )
  }
  
  return(measurement_type)
}


## SUMMARIZE DATA FILES ####
extract_alldata_from_file <- function(file) {
  data <- read.csv(file, header = FALSE, sep = ",", dec = ".",
                   stringsAsFactors = FALSE)
  file_name <- sub(".*/", "", file, perl = T) # get file name

  tz <- as.character(data[2,2]) # get timezone
  tz <- sub(".*(Date Time.*), ", "", tz)
  logger_info <- as.character(data[2,3])
  dat1_type <- sub("\\,.*", "", logger_info) # extract everything before comma
  dat1_unit <- sub("\\s*\\(.*", "", logger_info) # extract everything before the left bracket
  dat1_unit <- sub(".*, ", "", dat1_unit) # extract everything after the comma
  logger_sn <- regmatches(logger_info, regexpr("\\d{8}", logger_info)) # extract 8 digit serial number

  dat2_info <- as.character(data[2,4])
  dat2_type <- sub("\\,.*", "", dat2_info) # extract everything before comma
  dat2_unit <- sub("\\s*\\(.*", "", dat2_info) # extract everything before the left bracket
  dat2_unit <- sub(".*, ", "", dat2_unit) # extract everything after the comma

  
  # use serial number for data link
  # add as first column
  data$V1 <- logger_sn
  data <- data[-c(1:2),] # drop header rows
  data <- data[,c(1:4)] # drop empty columns
  
  # drop empty rows
  data$V3 <- as.numeric(data$V3)
  data <- data[complete.cases(data), ]
  
  #write new headers
  colnames(data) <- c("sn", "timestamp", paste(dat1_type,dat1_unit), paste(dat2_type, dat2_unit))
  
  # add metadata to df
  data <- data %>%
    mutate("file_name" = file_name, 
           "timezone" = tz, 
           "data1_type" = dat1_type, 
           "data1_unit" = dat1_unit, 
           "data2_type" = dat2_type, 
           "dat2_unit" = dat2_unit)

  return(data)
}


