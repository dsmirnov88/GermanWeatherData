#0. Load / Install all required R packages
library(GermanWeatherData);

#1a. Set the path to te temporary directory
my_path <- paste(Sys.getenv("TEMP"), "\\", "weather_fc_download", sep = "")

#1b. Load all parameters
address <- readRDS(file = paste(my_path, "\\address_list", sep = ""))
api_key <- readRDS(file = paste(my_path, "\\weather_forecast_api_key", sep = ""))
max_requests <- readRDS(file = paste(my_path, "\\max_api_requests", sep = ""))
destination_path <- readRDS(file = paste(my_path, "\\output_file_path", sep = ""))
proxy_opts <- readRDS(file = paste(my_path, "\\proxy_options", sep = ""))


#2a. Prepare data frae to store the results
weather_fc_df <- data.frame("PROGNOSE_ERSTELLUNGSZEIT" = character(), "ADRESSE" = character(),
                            "LUFTTEMPERATUR" = numeric(),
                            "LUFTTEMPERATUR_MAXIMUM" = numeric(),"LUFTTEMPERATUR_MINIMUM" = numeric(),
                            "SONNENSCHEINDAUER" = numeric(),
                            "REL_FEUCHTE" = numeric(), "WINDGESCHWINDIGKEIT" = numeric(),
                            "NIEDERSCHLAGSHOEHE" = numeric(), "REIGN" = numeric(), "SNOW" = numeric())


#2b. Retrieve data for each of the addresses in the list
for(j in 1:length(address))
{
        #Build in pause to avoid IP-blocking (in case of multiple repeated requests)
        if(j > 1)
                Sys.sleep(0.5)

        #Set current address
        current_address <- address[j]

        #Get coordinates of the target store
        coordinates <- GermanWeatherData::geoCode(address = current_address, proxy_opts = proxy_opts)

        #Convert address into coordinates to ensure unique match for the weather query
        search_term <- paste(coordinates[1], ",", coordinates[2], sep = "")

        #Build URL:
        weather_forecast_url <- URLencode(paste("http://api.wunderground.com/api/",api_key, "/forecast10day/q/",
                                                search_term, ".json", sep = ""))

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

}

#3. Write the results to a destination file if requested
#Check if a file with forecasts already exists
if(file.exists(paste(destination_path, "\\", "weather_forecast.csv",sep="")))
{
        #Read the old forecast file
        old_forecasts <- read.csv(file = paste(destination_path, "\\", "weather_forecast.csv", sep=""),
                                  header = T, na="NA")

        #Append the new data
        new_forecast <- rbind(old_forecasts, weather_fc_df)

} else {
        #Create output table
        new_forecast <- weather_fc_df
}

#Write the forecast
write.csv(x = new_forecast, file = paste(destination_path, "\\", "weather_forecast.csv",sep=""),
          na = "NA", row.names = F)
