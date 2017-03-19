#' @title Schedule Weather Forecast Download
#' @description Function scheduling an automatic retreival of weather forecast for a list of addresses, utilizing the Weather Underground API
#' @param taskname Name of the task for the Windows Task Scheduler
#' @param frequency One of c("MINUTE", "DAILY", "WEEKLY","ONCE")
#' @param starttime Character in the format "%H:%M"
#' @param weekdays One of weekdays = c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"), if weekly scheduling is required
#' @param minutes_step Numeric value indicating the frequency of the task in minutes (only if frequency == "MINUTE")
#' @param address Character value or vector of character strings (for query of multiple locations) containing the address of the location for which the data should be retreived in the format "street name, street number, ZIP code, city, country"
#' @param api_key Weather Underground API key as character
#' @param max_requests Numeric value indicating the maximum number of daily requests (in free version of the WU API == 500)
#' @param destination_path Destination path of the export .csv-file (default is set to desktop)
#' @param proxy_url Optional character argument for the proxy URL
#' @param proxy_port Optional numeric argument for the proxy port
#' @param proxy_password Optional character argument for the password (if not supplied, no proxy settings are set)
#' @param proxy_username Optional character argument containing the username
#' @seealso \link[GermanWeatherData]{getWeatherForecast}, \url{https://www.wunderground.com/weather/api/d/docs}
#' @export

scheduleWeatherForecastDownload <- function(taskname, frequency = c("MINUTE", "DAILY", "WEEKLY","ONCE"),
                                            starttime = "09:00", weekdays = c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"),
                                            minutes_step = 1,
                                            address,
                                            api_key = "a98e65d6eb8f07eb", max_requests = 500,
                                            destination_path = file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\"),
                                            proxy_url = "proxy.whu.edu", proxy_port = 3128, proxy_password = NULL,
                                            proxy_username = paste(Sys.getenv("USERDOMAIN"), Sys.getenv("USERNAME"), sep = "\\"))
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

        #Check if task-scheduler is installed (otherwise install from github, using devtools)
        if(!require("taskscheduleR"))
        {
                #Set up devtools
                if(!require("devtools"))
                {
                        install.packages("devtools")
                        library(devtools)
                }

                #Download and install from github
                devtools::install_github(repo = "jwijffels/taskscheduleR")
                library(taskscheduleR)
        }

        #Generate path to where all the files should be stored
        script_path <- file.path(paste(Sys.getenv("TEMP"), "\\", "weather_fc_download", sep = ""))
        dir.create(file.path(script_path), showWarnings = FALSE)

        #Detect location of the script file
        temp_script_file <- system.file("schedulingScript", "getWeatherForecastScript.R", package = "GermanWeatherData")
        if(temp_script_file == "")
        {
                #Assuming that if te package is not installed, the file is located in the working directory
                temp_script_file <- gsub("/", "\\",
                                         paste(getwd(), "/schedulingScript/getWeatherForecastScript.R", sep=""),
                                         fixed = T)
        } else {
                temp_script_file <- gsub("/", "\\", temp_script_file, fixed = T)
        }

        #Copy over the script file
        file.copy(from=temp_script_file, to=script_path,
                  recursive = F, copy.mode = T, overwrite = T)

        #Export all required information
        saveRDS(address, file = paste(script_path, "\\address_list", sep = ""))
        saveRDS(api_key, file = paste(script_path, "\\weather_forecast_api_key", sep = ""))
        saveRDS(max_requests, file = paste(script_path, "\\max_api_requests", sep = ""))
        saveRDS(destination_path, file = paste(script_path, "\\output_file_path", sep = ""))
        saveRDS(proxy_opts, file = paste(script_path, "\\proxy_options", sep = ""))


        #Validate the input
        if(length(frequency) != 1)
                stop("Please select a task frequency! One of: 'MINUTE', 'DAILY', 'WEEKLY', 'ONCE'.")


        #Schedule the script:
        if(frequency == "MINUTE")
        {
                if(!is.numeric(minutes_step))
                        stop("The specified minute-step parameter must be a numeric value!")

                taskscheduler_create(taskname = taskname, rscript = paste(script_path, "\\getWeatherForecastScript.R", sep = ""),
                                     schedule = "MINUTE", starttime = starttime, modifier = minutes_step)
        }
        if(frequency == "DAILY")
        {
                if(format(Sys.time(), "%H:%M") > starttime)
                        startdate <- format(Sys.Date()+1, "%d/%m/%Y") else
                                startdate <- format(Sys.Date(), "%d/%m/%Y")

                taskscheduler_create(taskname = taskname, rscript = paste(script_path, "\\getWeatherForecastScript.R", sep = ""),
                                     schedule = "DAILY", starttime = starttime, startdate = startdate)
        }
        if(frequency == "WEEKLY")
        {
                if(length(weekdays) != 1)
                        warning("The task can be scheduled only for one weekday at a time! The default value 'MON' was applied.")

                taskscheduler_create(taskname = taskname, rscript = paste(script_path, "\\getWeatherForecastScript.R", sep = ""),
                                     schedule = "WEEKLY", starttime = starttime, days = weekdays)
        }
        if(frequency == "ONCE")
        {
                if(format(Sys.time(), "%H:%M") > starttime)
                        stop("The specified execution time is in the past!")

                        taskscheduler_create(taskname = taskname, rscript = paste(script_path, "\\getWeatherForecastScript.R", sep = ""),
                                             schedule = "ONCE", starttime = starttime)
        }

        return("Task successfully scheduled...")

}



#' @title Delete Scheduled Weather Forecast Download
#' @description Function deleting a scheduled automatic retreival of weather forecast from Weather Underground
#' @param taskname Name of the task for the Windows Task Scheduler
#' @seealso \link[GermanWeatherData]{scheduleWeatherForecastDownload}, \link[taskscheduleR]{taskscheduler_delete}
#' @export

deleteWeatherForecastDownload <- function(taskname)
{
        #Delete the temporary directory
        script_path <- file.path(paste(Sys.getenv("TEMP"), "\\", "weather_fc_download", sep = ""))
        unlink(script_path, recursive = T)

        #Delete the scheduled task
        taskscheduleR::taskscheduler_delete(taskname = taskname)

        return("See status message above...")
}
