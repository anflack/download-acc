library('move')
library('foreach')
library('dplyr')
library('data.table')
library('lubridate')

#Select year example
#The last parameter with the name data is the result of the previous app
#   -> Should be removed if no data should be provided from previous app

rFunction = function(username, password, study, animals=NULL, timestamp_start=NULL, timestamp_end=NULL, ...) {
  credentials <- movebankLogin(username, password)
  arguments <- list()
  
  logger.info("you have (possibly by default) selected to combine your duplicated values to retain the first value of each. Note that this is the fastest option, but might loose some information.")
  
  arguments[["study"]] = study
  arguments[["login"]] = credentials
  
  
  if (exists("timestamp_start")&& !is.null(timestamp_start)) {
    logger.info(paste0("timestamp_start is set and will be used: ", timestamp_start))
    timestamp_start= paste0(str_replace_all(timestamp_start,"[ +/()-]", ""),"0000000")
    arguments["timestamp_start"] = timestamp_start
  } else {
    logger.info("timestamp_start not set.")
  }
  
  if (exists("timestamp_end") && !is.null(timestamp_end)) {
    logger.info(paste0("timestamp_end is set and will be used: ", timestamp_end))
    timestamp_end= paste0(str_replace_all(timestamp_end,"[ +/()-]", ""),"0000000")
    arguments["timestamp_end"] = timestamp_end
  } else {
    logger.info("timestamp_end not set.")
  }
  
  if (length(animals) == 0) 
  {
    logger.info("no animals set, using full study")
    animals <- as.character(unique(do.call(getMovebankAnimals, list(study,credentials))$local_identifier))
  }
  
  logger.info(paste(length(animals), "animals:", paste(animals,collapse=", ")))
  
  all <- list()
  all <- foreach(animal = animals) %do% {
    arguments["animalName"] = animal
    logger.info(animal)
    
    downData = getMovebankNonLocationData(sensorID=2365683,arguments)
    
    result <- downData
  }
  result = list(result,result)
}