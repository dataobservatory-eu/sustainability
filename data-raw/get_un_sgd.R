library(rjson)
library(jsonlite)
url <- 'https://data.un.org/ws/rest/data/DF_UNData_UNFCC'
url <- 'https://unstats.un.org/SDGAPI/v1/sdg/GeoArea/Tree'
url2 <- 'https://unstats.un.org/SDGAPI/v1/sdg/Series/SI_POV_DAY1/GeoArea/DE/DataSlice'

url3 <- 'https://unstats.un.org/SDGAPI/v1/sdg/Goal/DataCSV'

url(url3)
url3
read.csv(url3)
close (url3)

result <- fromJSON(url3)

this <- jsonlite::fromJSON(url)

#initiate the df
SDGdata<- data.frame()
# call to get the # elements with the years filter
page1 <- fromJSON("https://unstats.un.org/SDGAPI/v1/sdg/Indicator/Data?timePeriod=2004&timePeriod=2007&timePeriod=2011", flatten = TRUE)
perpage <- ceiling(page1$totalElements/10)
ptm <- proc.time()
for(i in 1:10){
  SDGpage <- fromJSON(paste0("https://unstats.un.org/SDGAPI/v1/sdg/Indicator/Data?timePeriod=2004&timePeriod=2007&timePeriod=2011&pageSize=",perpage,"&page=",i), flatten = TRUE)
  message("Retrieving page ", i, " :", (proc.time() - ptm) [3], " seconds")
  SDGdata <- rbind(SDGdata,SDGpage$data[,1:16])
}

fromJSON ("https://unstats.un.org/SDGAPI/v1/sdg/Indicator/SD_MDP_CSMP/Data?timePeriod=2004&timePeriod=2007&timePeriod=2011&pageSize=")
