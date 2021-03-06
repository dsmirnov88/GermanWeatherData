% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieveDWDdata.R
\name{retrieve_dwd_data}
\alias{retrieve_dwd_data}
\title{Retrieve Weather Data from DWD Server}
\usage{
retrieve_dwd_data(station_id, vars, proxy_opts, verbose, ...)
}
\arguments{
\item{station_id}{ID of the DWD weather station for which the data should be retrieved (as character or numeric)}

\item{vars}{List of data elements; one or multiple of c("LUFTTEMPERATUR", "LUFTTEMPERATUR_MAXIMUM", "LUFTTEMPERATUR_MINIMUM", "SONNENSCHEINDAUER", "REL_FEUCHTE", "WINDGESCHWINDIGKEIT", "NIEDERSCHLAGSHOEHE", "NIEDERSCHLAGSHOEHE_IND")}

\item{start_date}{Optional start date for the weather data time series in the format "%Y-%m-%d"}

\item{end_date}{Optional end date for the weather data time series in the format "%Y-%m-%d"}

\item{proxy_opts}{Optional list object containing proxy inormation required for online connection}

\item{verbose}{Optional parameter triggeringa console notification of the current query (if set to TRUE)}
}
\value{
Returns a data frame with the weather data
}
\description{
Function retreiving historical weather data based on a provided weather station ID from the server of the 'Deutscher Wetterdienst'
}
\seealso{
\link[GermanWeatherData]{getWeatherData}, \link[GermanWeatherData]{retreive_dwd_stations}
}

