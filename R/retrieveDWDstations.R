#' @title Get list of DWD weather stations
#' @description Function retreiving and filtering a list of weather stations from the 'Deutscher Wetterdienst' server
#' @usage retrieve_dwd_stations(recording_start = NULL, recording_end = NULL, station_type = c("FF", "KL", "RR", "SO", "TU"), proxy_opts = NULL, ...)
#' @param recording_start Optional date parameter determining when data collection must have started, to filter out irrelevant weather stations
#' @param recording_end Optional date parameter determining until when data collection must have been performed, to filter out irrelevant weather stations
#' @param station_type Optional vector of string values, defining which data types should be recorded at the weather station (used for filtering the list)
#' @param proxy_opts Optional list object containing proxy settings
#' @param verbose Optional parameter triggeringa console notification of the current query (if set to TRUE)
#' @return Returns a list object containing data frames with historical weather data for each provided location
#' @seealso \link[GermanWeatherData]{getWeatherData}, \link[GermanWeatherData]{find_nearest_weather_station}

retrieve_dwd_stations <- function(recording_start = as.Date("2012-01-01", format = "%Y-%m-%d"),
                                  recording_end = as.Date(paste(as.numeric(substr(as.character(Sys.Date()), 1, 4))-1, "-12-31", sep = ""), format = "%Y-%m-%d"),
                                  station_type = c("FF", "KL", "RR", "SO", "TU"),
                                  proxy_opts = NULL,
                                  verbose = FALSE)
{
        #Set the DWD station list URL
        station_list_url <- "https://www.dwd.de/DE/leistungen/klimadatendeutschland/statliste/statlex_html.html?view=nasPublication"

        #Print user info
        if(verbose)
                cat("\n", "Retrieving list of weather stations from DWD server...", "\n", sep = "")

        #Retreive the station list
        if(!is.null(proxy_opts))
        {
                station_list <- RCurl::getURL(station_list_url, .opts = proxy_opts)
        } else {
                station_list <- RCurl::getURL(station_list_url)
        }

        #Convert station list into data frame
        station_list <- as.data.frame(XML::readHTMLTable(doc = station_list, header = T, as.data.frame = T)[[1]])

        #Clean up
        gc()

        #Format the data frame and drop not required columns
        station_list <- station_list[2:nrow(station_list),c(2,3,5,6,10,11)]
        colnames(station_list) <- c("STATION_ID", "TYPE", "LATITUDE", "LONGITUDE", "RECORDING_START", "RECORDING_END")

        #TYPE-Field definitions:
           # AE := Stationen mit aerologischen Beobachtungen
           # EB := Stationen mit taeglichen Daten der Erdbodentemperatur
           # FF := Stationen mit stuendlichen Winddaten
           # KL := Stationen mit Klimadaten
           # MI / MN := Stationen mit automatischen Messungen (10-Minuten-Aufloesung)
           # PE / PS := Stationen mit phaenologischen Beobachtungen
           # RR := Stationen mit taeglichen Niederschlagsdaten
           # SO := Stationen mit stuendlichen Daten der Sonnenscheindauer
           # SY := Stationen mit stuendlichen, automatischen Messungen
           # TU := Stationen mit stuendlichen Daten der Temperatur und der relativen Feuchte

        #Convert geo-coordinate data fields to numeric format
        station_list$LATITUDE <- as.numeric(as.character(station_list$LATITUDE))
        station_list$LONGITUDE <- as.numeric(as.character(station_list$LONGITUDE))

        #Convert the recording columns to date format
        station_list$RECORDING_START <- as.Date(as.character(station_list$RECORDING_START), format = "%d.%m.%Y")
        station_list$RECORDING_END <- as.Date(as.character(station_list$RECORDING_END), format = "%d.%m.%Y")

        #Eliminate stations that do not fall in the required time interval
        if(length(which(is.na(station_list$RECORDING_START))) > 0)
                station_list <- station_list[-as.vector(which(is.na(station_list$RECORDING_START))),]
        if(length(which(is.na(station_list$RECORDING_END))) > 0)
                station_list <- station_list[-as.vector(which(is.na(station_list$RECORDING_END))),]

        if(length(which(station_list$RECORDING_START > recording_start)) > 0)
                station_list <- station_list[-as.vector(which(station_list$RECORDING_START > recording_start)),]
        if(length(which(station_list$RECORDING_END < recording_end)) > 0)
                station_list <- station_list[-as.vector(which(station_list$RECORDING_END < recording_end)),]

        #Keep only stations with all required data types
        if(!is.null(station_type))
        {
                #Get list of relevant weather stations
                station_list <- station_list[as.vector(which(station_list$TYPE %in% station_type)),]
                station_list$STATION_ID <- factor(station_list$STATION_ID)
                station_list <- station_list[as.vector(which(station_list$STATION_ID %in%
                                                                     names(table(station_list$STATION_ID)[table(station_list$STATION_ID) == length(station_type)]))),]
        }

        #Drop all columns that are not further required
        station_list <- station_list[,c("STATION_ID", "LATITUDE", "LONGITUDE")]
        station_list <- station_list[!duplicated(station_list),]

        #Return the final list
        return(station_list)

}
