# get the IDs of interest
inventory <- read.csv("us_temp_data/dly_inventory.txt", header=FALSE)
get_id <- function(loc){
  loc <- strsplit(loc, ",\\s*")[[1]]
  city <- loc[1]
  state <- loc[2]
  idx <- grepl(city, inventory$V1)
  leftover <- inventory[idx, ]
  n <- length(leftover)
  if (n == 0){
    return('city not found at all')
  }
  correct <- -1
  for (i in 1:n){
    combos <- strsplit(leftover[i], ' ')[[1]]
    combos <- combos[combos != ""]
    if (combos[5] == state){
      # print(combos)
      return(combos[1])
    }
  }
  if (correct == -1){
    return('state not found for this city')
  }
}
loc <- 'CLEVELAND, OH'
loc <- 'ANCHORAGE, AK'

# test cases
get_id('INDIANAPOLIS, IN')
get_id('LOS ANGELES, CA')
get_id('TOLEDO, OH')       
get_id('NEW YORK, NY')
get_id('ALBANY, NY')
get_id("SPRINGFIELD, IL")
get_id("MIAMI, FL")
get_id("FISHERS, IN")
get_id("EVANSVILLE, IN")
get_id("CHICAGO, IL")
get_id("PHILADELPHIA, PA")
loc <- "PHILADELPHIA, PA"
city_info <- matrix(NA, nrow=length(top_100_cities), ncol=3)
k <- 1
for (name in top_100_cities){
  city_info[k, 1] <- name
  city_info[k, 2] <- get_id(name)
  k <- k + 1
}

# initial read-in of data
# temp_data <- read.csv('us_temp_data/dly-temp-normal.csv')
names(temp_data)
relevant_cols <- c("GHCN_ID", "DLY.TMAX.NORMAL", "DLY.TMIN.NORMAL")
relevant_rows <- temp_data$GHCN_ID %in% city_info[,2]
relevant_data <- temp_data[relevant_rows, relevant_cols]
names(relevant_data) <- c("GHCN_ID", "TMAX", "TMIN")
write.csv(relevant_data, 'relevant_data.csv')
# relevant_data <- read.csv('relevant_data.csv')

for (i in 1:nrow(city_info)){
  id <- city_info[i,2]
  temp <- relevant_data[relevant_data$GHCN_ID == id, ]
  val <- mean(abs(temp$TMAX - temp$TMIN))
  city_info[i, 3] <- as.double(round(val, 2))
}

temp_data[temp_data$GHCN_ID == 'USW00004853', relevant_cols]

sorted_cities <- as.data.frame(city_info[order(as.double(city_info[, 3])),])
names(sorted_cities) <- c('city', 'id', 'avg_fluct')
View(sorted_cities)
View(head(sorted_cities[,c('city','avg_fluct')], n=10))
View(tail(sorted_cities, n=10))
