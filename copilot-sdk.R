library(jsonlite)
source("logger.R")
source("RFunction.R")

inputFileName = NULL #important to set to NULL for movebank-download
outputFileName = "output.rds"

args <- list()
#################################################################
########################### Arguments ###########################
# The data parameter will be added automatically if input data is available
# The name of the field in the vector must be exaclty the same as in the r function signature
# Example:
# rFunction = function(username, password)
# The paramter must look like:
#    args[["username"]] = "any-username"
#    args[["password"]] = "any-password"

# Add your arguments of your r function here

args[["username"]] = "aflack"
args[["password"]] = "3Iguan4b"
args[["study"]] = 	1176017658 # needs to be study ID!!!
args[["animals"]] = c("Rici ABA65 (eobs 9677)","Apollo AAR45 (eobs 9670)" ,"Lina ABA63 (ebos 9679)")
args[["timestamp_start"]] = NULL
args[["timestamp_end"]] = NULL


#################################################################
#################################################################
inputData <- NULL
if(!is.null(inputFileName) && inputFileName != "" && file.exists(inputFileName)) {
  cat("Loading file from", inputFileName, "\n")
  inputData <- readRDS(file = inputFileName)
} else {
  cat("Skip loading: no input File", "\n")
}

# Add the data paramter if input data is available
if (!is.null(inputData)) {
  args[["data"]] <- inputData
}

result <- tryCatch(do.call(rFunction, args),
error = function(e) {})

if(!is.null(outputFileName) && outputFileName != "" && !is.null(result)) {
  cat("Storing file to", outputFileName, "\n")
  saveRDS(result, file = outputFileName)
} else {
  cat("Skip store result: no output File or result is missing", "\n")
}