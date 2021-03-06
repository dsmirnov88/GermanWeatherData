% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findNearestWeatherStation.R
\name{find_nearest_weather_station}
\alias{find_nearest_weather_station}
\title{Find nearest weather station}
\usage{
retrieve_dwd_stations(recording_start = NULL, recording_end = NULL, station_type = c("FF", "KL", "RR", "SO", "TU"), proxy_opts = NULL, ...)
}
\arguments{
\item{address}{Character value or vector of character strings (for query of multiple locations) containing the address of the location for which the data should be retreived in the format "street name, street number, ZIP code, city, country"}

\item{weather_stations}{Data frame containing a list of DWD weather stations, previously retrieved from the DWD server (see function \link[GermanWeatherData]{retrieve_dwd_stations})}

\item{...}{List of optional parameters to underlying functions}
}
\value{
Returns the station ID of the closest weather station as a numeric value
}
\description{
Function calculating the distance of the address to all supplied DWD weather stations and selects the closest one
}
\seealso{
\link[GermanWeatherData]{geoCode}, \link[GermanWeatherData]{linear_coordinates_distance}
}

