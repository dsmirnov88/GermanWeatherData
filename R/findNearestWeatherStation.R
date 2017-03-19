#' @title Find nearest weather station
#' @description Function calculating the distance of the address to all supplied DWD weather stations and selects the closest one
#' @usage retrieve_dwd_stations(recording_start = NULL, recording_end = NULL, station_type = c("FF", "KL", "RR", "SO", "TU"), proxy_opts = NULL, ...)
#' @param address Character value or vector of character strings (for query of multiple locations) containing the address of the location for which the data should be retreived in the format "street name, street number, ZIP code, city, country"
#' @param weather_stations Data frame containing a list of DWD weather stations, previously retrieved from the DWD server (see function \link[GermanWeatherData]{retrieve_dwd_stations})
#' @param ... List of optional parameters to underlying functions
#' @return Returns the station ID of the closest weather station as a numeric value
#' @seealso \link[GermanWeatherData]{geoCode}, \link[GermanWeatherData]{linear_coordinates_distance}

find_nearest_weather_station <- function(address, weather_stations, ...)
{

        #Get coordinates of the target store
        coordinates <- geoCode(address = address, ...)

        #Calculate the distance for every seather station
        weather_stations$DISTANCE <- mapply(FUN = linear_coordinates_distance,
                                            from_latitude = coordinates[1], from_longitude = coordinates[2],
                                            to_latitude = weather_stations$LATITUDE, to_longitude = weather_stations$LONGITUDE)

        #Find the Station-ID of the nearest weather station from the generated list; then query the name
        favorable_station_ID <- weather_stations$STATION_ID[weather_stations$DISTANCE == min(weather_stations$DISTANCE)][1]

        #Return ID of nearest weather station
        return(list(station_id = as.character(favorable_station_ID),
                    distance = weather_stations[weather_stations$STATION_ID == favorable_station_ID, 4]))

}



#' @title Calculate Euclidean Distance of Two Locations
#' @description Function calculating linear distance between two GPS coordinates expressed as decimal numbers
#' @usage linear_coordinates_distance(from_latitude, from_longitude, to_latitude, to_longitude)
#' @param from_latitude Latitude of the origin (provided as a numeric value)
#' @param from_longitude Longitude of the origin (provided as a numeric value)
#' @param to_latitude Latitude of the destination (provided as a numeric value)
#' @param to_longitude Longitude of the destination (provided as a numeric value)
#' @return Returns the distance of the two locations in km (as a numeric value)
#' @seealso \link[GermanWeatherData]{geoCode}, \link[GermanWeatherData]{find_nearest_weather_station}

linear_coordinates_distance <- function(from_latitude, from_longitude, to_latitude, to_longitude)
{

        step1 <- cos((90-from_latitude)*pi/180) * cos((90-to_latitude)*pi/180)

        step2 <- sin((90-from_latitude)*pi/180) * sin((90-to_latitude)*pi/180) * cos((from_longitude-to_longitude)*pi/180)

        step3 <- acos(step1+step2) * 6371


        #Returns distance between the two points, in km
        return(step3)

}
