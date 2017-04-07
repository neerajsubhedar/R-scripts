project.initialize()


summary(zipcode)
glimpse(zipcode)



getLatLong.zip <- function(enter.zipcode = "06105",radius.mi = "100mi"){
  attach(zipcode)
  lat.long <- zipcode[zip == enter.zipcode,c("latitude","longitude")]
  lat.long.mi <- paste0(lat.long$latitude,",",lat.long$longitude,",",radius.mi)
  return(lat.long.mi)
}

getZipCode <- function(){
  zip.code <- readline(prompt = "Enter zipcode (should be between 00210 and 99950): ")
  return(as.character(zip.code))
}

getMiles <- function(){
  miles <- readline(prompt = "Enter the radius, in miles, to search for tweets: ")
  return(as.character(miles))
}

get.zip <- getZipCode()
get.mi <- getMiles()
getLatLong.zip(enter.zipcode = get.zip,radius.mi = get.mi)