#' @title Get Historical Weather Data
#' @description Function retreiving historical weather data based on a provided address from the server of the 'Deutscher Wetterdienst'
#' @usage getWeatherData(address, timeseries_start_date = NULL, timeseries_end_date = NULL, weather_data = c("temperature", "humidity", "sunshine", "wind", "precipitation"),
#' proxy_url, proxy_port, proxy_password = NULL, proxy_username, verbose = FALSE)
#' @param address Character value or vector of character strings (for query of multiple locations) containing the address of the location for which the data should be retreived in the format "street name, street number, ZIP code, city, country"
#' @param timeseries_start_date Optional start date for the weather data time series in the format "%Y-%m-%d"
#' @param timeseries_end_date Optional end date for the weather data time series in the format "%Y-%m-%d"
#' @param weather_data Optional vector of character values controlling which data elements are required (impacts the choice of weather station); one of c("temperature", "humidity", "sunshine", "wind", "precipitation")
#' @param proxy_url Optional character argument for the proxy URL
#' @param proxy_port Optional numeric argument for the proxy port
#' @param proxy_password Optional character argument for the password (if not supplied, no proxy settings are set)
#' @param proxy_username Optional character argument containing the username
#' @param verbose Optional parameter triggeringa console notification of the current query (if set to TRUE)
#' @return Returns a list object containing data frames with historical weather data for each provided location
#' @seealso \link[GermanWeatherData]{retrieve_dwd_stations}, \link[GermanWeatherData]{retrieve_dwd_data}
#' @export

getWeatherData <- function(address,
                           timeseries_start_date = NULL, timeseries_end_date = NULL,
                           weather_data = c("temperature", "humidity", "sunshine", "wind", "precipitation"),
                           proxy_url = "proxy.whu.edu", proxy_port = 3128, proxy_password = NULL,
                           proxy_username = paste(Sys.getenv("USERDOMAIN"), Sys.getenv("USERNAME"), sep = "\\"),
                           verbose = FALSE, ...)
{
        #Set default values for the start and end date of the weather time series, if none were supplied to the function
        if(is.null(timeseries_start_date))
        {
                timeseries_start_date = as.Date("2012-01-01", format = "%Y-%m-%d")
        } else {
                timeseries_start_date <- as.Date(timeseries_start_date, format = "%Y-%m-%d")
        }
        if(is.null(timeseries_end_date))
        {
                timeseries_end_date = as.Date(paste(as.numeric(substr(as.character(Sys.Date()), 1, 4))-1, "-12-31", sep = ""), format = "%Y-%m-%d")

                if(verbose)
                        cat("\n", "No time series dates specified.", "\n",
                            "The default value of 5 full years will be set...", "\n", sep = "")
        } else {
                timeseries_end_date <- as.Date(timeseries_end_date, format = "%Y-%m-%d")
        }



        #Set proxy setting and build proxy-options object
        if(!(TRUE %in% c(is.null(proxy_url), is.null(proxy_port), is.null(proxy_username), is.null(proxy_password))))
        {
                httr::set_config(
                        use_proxy(url=proxy_url, port=proxy_port,
                                  username=proxy_username,
                                  password = proxy_password)
                )

                proxy_opts <- list(
                        proxy         = proxy_url,
                        proxyusername = proxy_username,
                        proxypassword = proxy_password,
                        proxyport     = proxy_port)
        } else {
                proxy_opts <- NULL
        }



        #Translate the weather data variables for underlying functions
        if(FALSE %in% (weather_data %in% c("temperature", "humidity", "sunshine", "wind", "precipitation")))
        {
                weather_data <- c("temperature", "humidity", "sunshine", "wind", "precipitation")

                vars <- c("LUFTTEMPERATUR", "LUFTTEMPERATUR_MAXIMUM", "LUFTTEMPERATUR_MINIMUM", "SONNENSCHEINDAUER",
                          "REL_FEUCHTE", "WINDGESCHWINDIGKEIT","NIEDERSCHLAGSHOEHE", "NIEDERSCHLAGSHOEHE_IND")

                station_type <- c("FF", "KL", "RR", "SO", "TU")
        } else {
                vars <- vector(); station_type <- vector()

                if("temperature" %in% weather_data)
                {
                        vars <- append(vars, c("LUFTTEMPERATUR", "LUFTTEMPERATUR_MAXIMUM", "LUFTTEMPERATUR_MINIMUM"))
                        station_type <- append(station_type, c("KL", "TU"))
                }

                if("humidity" %in% weather_data)
                {
                        vars <- append(vars, "REL_FEUCHTE")
                        station_type <- append(station_type, c("KL", "TU"))
                }

                if("sunshine" %in% weather_data)
                {
                        vars <- append(vars, "SONNENSCHEINDAUER")
                        station_type <- append(station_type, "SO")
                }

                if("wind" %in% weather_data)
                {
                        vars <- append(vars, "WINDGESCHWINDIGKEIT")
                        station_type <- append(station_type, "FF")
                }

                if("precipitation" %in% weather_data)
                {
                        vars <- append(vars, c("NIEDERSCHLAGSHOEHE", "NIEDERSCHLAGSHOEHE_IND"))
                        station_type <- append(station_type, "RR")
                }

                station_type <- unique(station_type)
        }


        #Retrieve the list of relevant weather stations
        weather_stations <- retrieve_dwd_stations(recording_start = timeseries_start_date, recording_end = timeseries_end_date,
                                          station_type = station_type, proxy_opts = proxy_opts, verbose = verbose)

        #Prepare list object for the results
        weather_data_result <- list()

        #Prepare table storing nearest weather stations
        nws <- data.frame("ADDRESS" = character(), "WEATHER_STATION_ID" = character(), "DISTANCE" = numeric())

        #Retrieve data for each of the addresses in the list
        for(i in 1:length(address))
        {
                #Set current address
                current_address <- address[i]

                #Determine the nearest weather station from the list
                nearest_weather_station <- find_nearest_weather_station(address = current_address, weather_stations = weather_stations,
                                                                        proxy_opts = proxy_opts, verbose = verbose, ...)

                #Build station ID (length must be 5 digits)
                station_id <- nearest_weather_station$station_id
                if(nchar(station_id) != 5)
                {
                        if(nchar(station_id) > 5)
                        {
                                stop("Error: The length of the weather station ID can not be bigger 5 characters!")
                        } else {
                                station_id <- paste(
                                        paste(rep("0", (5-nchar(station_id))), collapse = "", sep = ""),
                                        station_id, collapse = "", sep = "")
                        }
                }

                #Get and store nearest weather station
                nws <- rbind(nws, data.frame("ADDRESS" = current_address, "WEATHER_STATION_ID" = station_id,
                                             "DISTANCE" = nearest_weather_station$distance))

                #Retrieve weather data
                weather_data_result[[paste("STATION_", station_id, sep = "")]] <- retrieve_dwd_data(station_id = station_id,
                                                                                                    vars = vars, proxy_opts = proxy_opts, verbose = verbose,
                                                                                                    start_date = timeseries_start_date, end_date = timeseries_end_date)
        }

        #Add the list of matched weather stations to the result list
        weather_data_result[["weather_stations"]] <- nws

        #Clean up
        gc()

        #Return result object
        return(weather_data_result)
}
