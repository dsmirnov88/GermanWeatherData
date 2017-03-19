#' @title Retrieve Weather Data from DWD Server
#' @description Function retreiving historical weather data based on a provided weather station ID from the server of the 'Deutscher Wetterdienst'
#' @usage retrieve_dwd_data(station_id, vars, proxy_opts, verbose, ...)
#' @param station_id ID of the DWD weather station for which the data should be retrieved (as character or numeric)
#' @param vars List of data elements; one or multiple of c("LUFTTEMPERATUR", "LUFTTEMPERATUR_MAXIMUM", "LUFTTEMPERATUR_MINIMUM", "SONNENSCHEINDAUER", "REL_FEUCHTE", "WINDGESCHWINDIGKEIT", "NIEDERSCHLAGSHOEHE", "NIEDERSCHLAGSHOEHE_IND")
#' @param start_date Optional start date for the weather data time series in the format "%Y-%m-%d"
#' @param end_date Optional end date for the weather data time series in the format "%Y-%m-%d"
#' @param proxy_opts Optional list object containing proxy inormation required for online connection
#' @param verbose Optional parameter triggeringa console notification of the current query (if set to TRUE)
#' @return Returns a data frame with the weather data
#' @seealso \link[GermanWeatherData]{getWeatherData}, \link[GermanWeatherData]{retreive_dwd_stations}

retrieve_dwd_data <- function(station_id,
                              vars = c("LUFTTEMPERATUR", "LUFTTEMPERATUR_MAXIMUM", "LUFTTEMPERATUR_MINIMUM",
                                       "SONNENSCHEINDAUER", "REL_FEUCHTE", "WINDGESCHWINDIGKEIT",
                                       "NIEDERSCHLAGSHOEHE", "NIEDERSCHLAGSHOEHE_IND"),
                              start_date = NULL, end_date = NULL,
                              proxy_opts = NULL, verbose = FALSE)
{
        if(verbose)
                cat("\n", "Retrieving weather data for station-ID ", station_id, "...", "\n", sep = "")


        #--- Retrieve recent climate data ---

        #Build url containing recent climate data
        recent_climate_data_url <- paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/",
                                         "/tageswerte_KL_", station_id, "_akt.zip", sep = "")

        #Create temporary file and download zip-file containing recent climate data
        tempf <- tempfile(); tempd <- paste(tempdir(), "\\wd_temp", sep = ""); suppressWarnings(dir.create(tempd))
        download.file(recent_climate_data_url,tempf)

        #Detect the file you want to read
        fileNames <- unzip(tempf, exdir = tempd)
        extractFile <- fileNames[which(grepl("produkt_klima_Tageswerte", fileNames) == TRUE)]
        extractFile <- substr(extractFile,
                              gregexpr(pattern = "produkt_klima_Tageswerte", extractFile),
                              nchar(extractFile))

        #Unzip and extract the data file
        recent_climate_data <- read.table(file = paste(tempd, "\\", extractFile, sep = ""),
                                          header = T,sep = ";",dec = ".")

        #Keep only the spcified variables
        recent_climate_data <- recent_climate_data[,c("STATIONS_ID", "MESS_DATUM", vars)]

        #Clean up
        unlink(tempf); unlink(tempd, recursive = T)
        rm(tempf, tempd, fileNames, extractFile)
        gc()

        #------------------------------------



        #--- Retrieve historical climate data ---

        #Build url of the ftp site ontaining  historical cliamte data
        histor_climate_data_url <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/"

        #Screen the page for the entire content
        pageContent <- unlist(
                strsplit(
                        if(!is.null(proxy_opts))
                           {
                                   RCurl::getURL(histor_climate_data_url,
                                                 ftp.use.epsv = FALSE,
                                                 dirlistonly = TRUE,
                                                 .opts = proxy_opts)
                           } else {
                                   RCurl::getURL(histor_climate_data_url,
                                                 ftp.use.epsv = FALSE,
                                                 dirlistonly = TRUE)
                           },
                        "\n")
        )

        #Search for the element containing the file we are looking for
        pageContent <- pageContent[which(grepl(paste("tageswerte_", station_id, sep = ""), pageContent) == TRUE)[1]]

        #Extract the file name
        fileNameStart <- gregexpr(pattern = paste("tageswerte_", station_id, sep = ""),pageContent)[[1]][1]
        fileName <- substr(pageContent, start = fileNameStart, stop = fileNameStart+43-1)

        #Update the url string to download the file
        histor_climate_data_url <- paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/",
                                         fileName, sep = "")

        #Create temporary file and download zip-file containing recent climate data
        tempf <- tempfile(); tempd <- paste(tempdir(), "\\wd_temp", sep = ""); suppressWarnings(dir.create(tempd))
        download.file(histor_climate_data_url, tempf)

        #Detect the file you want to extract
        fileNames <- unzip(tempf, exdir = tempd)
        extractFile <- fileNames[which(grepl("produkt_klima_Tageswerte", fileNames) == TRUE)]
        extractFile <- substr(extractFile,
                              gregexpr(pattern = "produkt_klima_Tageswerte", extractFile),
                              nchar(extractFile))

        #Unzip and extract the data file
        historical_climate_data <- read.table(file = paste(tempd, "\\", extractFile, sep = ""),
                                              header = T,sep = ";",dec = ".")

        #Keep only the spcified variables
        historical_climate_data <- historical_climate_data[,c("STATIONS_ID", "MESS_DATUM", vars)]

        #Clean up
        unlink(tempf); unlink(tempd, recursive = T)
        rm(tempf, tempd, fileNames, extractFile, fileNameStart, fileName)
        gc()

        #------------------------------------



        #--- Merge the data and generate clean it ---

        #Delete false observations
        if(length(which(is.na(historical_climate_data$MESS_DATUM))) > 0)
                historical_climate_data <- historical_climate_data[-which(is.na(historical_climate_data$MESS_DATUM)),]
        if(length(which(is.na(recent_climate_data$MESS_DATUM))) > 0)
                recent_climate_data <- recent_climate_data[-which(is.na(recent_climate_data$MESS_DATUM)),]

        #Merge data
        climate_data <- rbind(historical_climate_data, recent_climate_data)

        #Create binary indicator for reighn and snow
        if("NIEDERSCHLAGSHOEHE_IND" %in% vars)
        {
                climate_data$REIGN <- 0; climate_data$SNOW <- 0
                climate_data$REIGN <- sapply(X = climate_data$NIEDERSCHLAGSHOEHE_IND,
                                             FUN = function(x){if(x %in% c(1, 6, 8)) return(1) else return(0)})
                climate_data$SNOW <- sapply(X = climate_data$NIEDERSCHLAGSHOEHE_IND,
                                            FUN = function(x){if(x %in% c(7, 8)) return(1) else return(0)})
                climate_data$NIEDERSCHLAGSHOEHE_IND <- NULL
        }

        #Replace all "-999" values with NA
        climate_data[climate_data == -999] <- NA

        #Format measurement date field as numeric
        climate_data$MESS_DATUM <- as.numeric(climate_data$MESS_DATUM)

        #Remove duplicate entries
        duplicates <- duplicated(climate_data$MESS_DATUM)
        if(length(which(duplicates == TRUE)) > 0)
                climate_data <- climate_data[-which(duplicates == TRUE),]

        #Remove observations outside specified date range
        if(!is.null(start_date))
        {
                #Re-format date element
                start_date <- as.numeric(gsub('-','',as.character(start_date)))

                #Check and remove observations before the requested start date
                if(length(which(climate_data$MESS_DATUM < start_date)) > 0)
                        climate_data <- climate_data[-which(climate_data$MESS_DATUM < start_date),]
        }

        if(!is.null(end_date))
        {
                #Re-format date element
                end_date <- as.numeric(gsub('-','',as.character(end_date)))

                #Check and remove observations before the requested start date
                if(length(which(climate_data$MESS_DATUM > end_date)) > 0)
                        climate_data <- climate_data[-which(climate_data$MESS_DATUM > end_date),]
        }

        #--------------------------------------------

        #Return the weather data
        return(climate_data)
}

