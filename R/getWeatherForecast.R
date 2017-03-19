#' @title Download weather forecast
#' @description Function for getting a weather forecast from an open weather API. Powered by Weather Underground (wunderground.com).
#' @param address Character value or vector of character strings (for query of multiple locations) containing the address of the location for which the data should be retreived in the format "street name, street number, ZIP code, city, country"
#' @param api_key Weather Underground API key as character
#' @param max_requests Numeric value indicating the maximum number of daily requests (in free version of the WU API == 500)
#' @param export_query Logical value indicating if the downloaded data should be exported to a .csv file
#' @param destination_path Destination path of the export .csv-file (default is set to desktop)
#' @param proxy_url Optional character argument for the proxy URL
#' @param proxy_port Optional numeric argument for the proxy port
#' @param proxy_password Optional character argument for the password (if not supplied, no proxy settings are set)
#' @param proxy_username Optional character argument containing the username
#' @param verbose Optional parameter triggeringa console notification of the current query (if set to TRUE)
#' @param ... List of optional parameters to underlying functions
#' @seealso \url{https://www.wunderground.com/weather/api/d/docs}
#' @export

getWeatherForecast <- function(address,
                               api_key = "a98e65d6eb8f07eb", max_requests = 500,
                               export_query = FALSE, destination_path = file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\"),
                               proxy_url = "proxy.whu.edu", proxy_port = 3128, proxy_password = NULL,
                               proxy_username = paste(Sys.getenv("USERDOMAIN"), Sys.getenv("USERNAME"), sep = "\\"),
                               verbose = FALSE, ...)
{
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

        #Declare result list
        weather_fc <- list()

        #Check if maximum number of requests is not violated
        if(length(address) > max_requests)
                stop("\n", paste ("Maximum number of ", max_requests, " requests is exceeded!", sep = ""), "\n")


        #Retrieve data for each of the addresses in the list
        for(j in 1:length(address))
        {
                #Build in pause to avoid IP-blocking (in case of multiple repeated requests)
                if(j > 1)
                        Sys.sleep(0.5)

                #Set current address
                current_address <- address[j]

                #Get coordinates of the target store
                coordinates <- geoCode(address = current_address, proxy_opts = proxy_opts, verbose = verbose, ...)

                #Convert address into coordinates to ensure unique match for the weather query
                search_term <- paste(coordinates[1], ",", coordinates[2], sep = "")

                #Build URL:
                weather_forecast_url <- URLencode(paste("http://api.wunderground.com/api/",api_key, "/forecast10day/q/",
                                                        search_term, ".json", sep = ""))
                #User info:
                if(verbose)
                {
                        cat("\n", "Retrieving weather 10-days forecast for address:", current_address, sep = "")
                        cat("\n", "Query URL: ", weather_forecast_url, "\n", sep = "")
                }

                #Prepare timestamp
                my_timestamp <- Sys.time()

                #Check if the settings for proxy need to be adjusted
                if(!is.null(proxy_opts))
                {
                        APIquery <- RCurl::getURL(weather_forecast_url, .opts = proxy_opts)
                } else {
                        APIquery <- RCurl::getURL(weather_forecast_url)
                }

                #Extract the data from the website
                weather_forecast_10d <- RJSONIO::fromJSON(APIquery,simplify = FALSE)

                #Prepare data frae to store the results
                weather_fc_df <- data.frame("PROGNOSE_ERSTELLUNGSZEIT" = character(), "ADRESSE" = character(),
                                            "LUFTTEMPERATUR" = numeric(),
                                            "LUFTTEMPERATUR_MAXIMUM" = numeric(),"LUFTTEMPERATUR_MINIMUM" = numeric(),
                                            "SONNENSCHEINDAUER" = numeric(),
                                            "REL_FEUCHTE" = numeric(), "WINDGESCHWINDIGKEIT" = numeric(),
                                            "NIEDERSCHLAGSHOEHE" = numeric(), "REIGN" = numeric(), "SNOW" = numeric())

                #Store results in a data frame
                for(i in 1:9)
                {
                        weather_fc_df <- rbind(weather_fc_df,
                                               data.frame("PROGNOSE_ERSTELLUNGSZEIT" = as.character(my_timestamp),
                                                          "ADRESSE" = current_address,
                                                          "LUFTTEMPERATUR" = 0.5 * as.numeric(weather_forecast_10d$forecast$simpleforecast$forecastday[[(i+1)]]$high$celsius) +
                                                                  0.5 * as.numeric(weather_forecast_10d$forecast$simpleforecast$forecastday[[(i+1)]]$low$celsius),
                                                          "LUFTTEMPERATUR_MAXIMUM" = as.numeric(weather_forecast_10d$forecast$simpleforecast$forecastday[[(i+1)]]$high$celsius),
                                                          "LUFTTEMPERATUR_MINIMUM" = as.numeric(weather_forecast_10d$forecast$simpleforecast$forecastday[[(i+1)]]$low$celsius),
                                                          "SONNENSCHEINDAUER" = NA,
                                                          "REL_HUMIDITY" = as.numeric(weather_forecast_10d$forecast$simpleforecast$forecastday[[(i+1)]]$avehumidity),
                                                          "WINDSPEED" = round(as.numeric(weather_forecast_10d$forecast$simpleforecast$forecastday[[(i+1)]]$avewind$kph)*0.277777778,1),
                                                          "PRECIPITATION" = as.numeric(weather_forecast_10d$forecast$simpleforecast$forecastday[[(i+1)]]$qpf_allday$mm) +
                                                                  as.numeric(weather_forecast_10d$forecast$simpleforecast$forecastday[[(i+1)]]$snow_allday$cm),
                                                          "REGHT" = if(as.numeric(weather_forecast_10d$forecast$simpleforecast$forecastday[[(i+1)]]$qpf_allday$mm) > 0) 1 else 0,
                                                          "SNOW" = if(as.numeric(weather_forecast_10d$forecast$simpleforecast$forecastday[[(i+1)]]$snow_allday$cm) > 0) 1 else 0
                                                          )
                                               )
                }

                #Build index of the request
                if(nchar(as.character(j)) != nchar(as.character(max_requests)))
                {
                                counter <- paste(
                                        paste(rep("0",
                                                  (nchar(as.character(max_requests))-nchar(as.character(j)))),
                                              collapse = "", sep = ""),
                                        as.character(j), collapse = "", sep = "")
                }

                weather_fc[[paste("WEATHER_FC_", counter, sep = "")]] <- weather_fc_df
        }

        #Write the results to a destination file if requested
        if(export_query)
        {
                #Read all tables from the list object
                for(l in 1:length(weather_fc))
                {
                        if(l == 1)
                        {
                                joint_table <- weather_fc[[l]]
                        } else {
                                joint_table <- rbind(joint_table, weather_fc[[l]])
                        }
                }

                #Check if a file with forecasts already exists
                if(file.exists(paste(destination_path, "\\", "weather_forecast.csv",sep="")))
                {
                        #Read the old forecast file
                        old_forecasts <- read.csv(file = paste(destination_path, "\\", "weather_forecast.csv", sep=""),
                                                  header = T, na="NA")

                        #Append the new data
                        new_forecast <- rbind(old_forecasts, joint_table)

                } else {
                        #Create output table
                        new_forecast <- joint_table
                }

                #Write the forecast
                write.csv(x = new_forecast, file = paste(destination_path, "\\", "weather_forecast.csv",sep=""),
                          na = "NA", row.names = F)
        }


        #Return the result
        return(weather_fc)

}
