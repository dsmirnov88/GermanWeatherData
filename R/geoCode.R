#' @title GEO Code
#' @description Function retreiving the GEO coordinates of an address, utilizing the Google Maps API
#' @usage geoCode <- function(address, ...)
#' @param address Character value containing the address in te form "Street Nr., ZIP, City, Country"
#' @param proxy_opts Optional list object containing proxy inormation required for online connection
#' @param google_maps_api String containing the root of Google Maps' API
#' @param return.call String parameter to the API, defining the return type (default = "json")
#' @param verbose Optional parameter triggeringa console notification of the current query (if set to TRUE)
#' @return Returns a simle vector containing two elements - latitude and longitude
#' @seealso \link[GermanWeatherData]{getWeatherData},  \link[GermanWeatherData]{getWeatherForecast}
#' @export

geoCode <- function(address, proxy_opts = NULL,
                    google_maps_api = "http://maps.google.com/maps/api/geocode/",
                    return.call = "json", verbose=FALSE)
{
        #User info:
        if(verbose)
                cat("\n", "Searching GEO coordinates for: ", address,"\n", sep = "")

        #Convert address string into ASCII (UTF-8)
        address <- iconv(x = address, to = "ASCII//TRANSLIT")

        #Construct the URL to be passed to the API
        u <- URLencode(paste(google_maps_api, return.call, "?address=", address, "&sensor=false", sep = ""))

        #User info:
        if(verbose)
                cat("URL-Query string: ", u,"\n", sep = "")

        #Check if the settings for proxy need to be adjusted
        if (!is.null(proxy_opts))
                doc <- RCurl::getURL(u, .opts = proxy_opts)
        else
                doc <- RCurl::getURL(u)

        #Extract the data from the website
        x <- RJSONIO::fromJSON(doc,simplify = FALSE)

        #Get the required information out of the downloaded content
        if(x$status=="OK") {
                lat <- x$results[[1]]$geometry$location$lat
                lng <- x$results[[1]]$geometry$location$lng

                #Additional arguments that can be returned
                #location_type  <- x$results[[1]]$geometry$location_type
                #formatted_address  <- x$results[[1]]$formatted_address
                #return(c(lat, lng, location_type, formatted_address))

                return(c(lat, lng))
                Sys.sleep(0.5)
        } else {
                #Warn user that something went wrong
                cat("\n", "The address '", address, "' could not be found! 'NA' will be returned.", "\n", sep = "")

                return(c(NA,NA))
        }
}
