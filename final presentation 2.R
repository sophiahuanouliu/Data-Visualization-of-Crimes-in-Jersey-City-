library(ggplot2)
library(maps)
library(ggmap)
crime <- read.csv("ucr-2015.csv", stringsAsFactors = F, header = T)
crime$location <- paste(crime$Inc_Address, " Jersey City, NJ", sep = ",")
place <- crime$location
place <- as.data.frame(place, stringsAsFactors = F)
## we have to get the goecode of addresses by two lines of codes, because google map API limits the request up to 2,500
## per computer per IP everday.
LatLon1 <- c(lapply(place[1:2037, ], geocode))
LatLon2 <-  c(lapply(place[2038:4438, ], geocode))
LatLon1DF <- do.call(rbind.data.frame, LatLon1[1:2037])
LatLon2DF <- do.call(rbind.data.frame, LatLon2)
LatLonDF <- rbind(LatLon1DF, LatLon2DF)
jc.crime <- cbind.data.frame(crime[1:4438, ], LatLonDF, stringsAsFactors = F)
jc.crime <- subset(jc.table, (lon > -74.12 & lon < -74.02 & lat > 40.68 & lat < 40.74), 
                   select = c(Incident_Date, UCR, lon, lat))
## Converting the date to a recognizable format
jc.crime$Date <- strptime(jc.crime$Incident_Date, format = '%m/%d/%Y %H:%M:%S')
## Getting the day and hour of each crime
jc.crime$Day <- weekdays(jc.crime$Date)
jc.crime$Hour <- jc.crime$Date$hour
## Sorting the weekdays
dailyCrimes <- as.data.frame(table(jc.crime$Day, jc.crime$Hour))
names(dailyCrimes) <- c('Day', 'Hour', 'Freq')
dailyCrimes$Hour <- as.numeric(as.character(dailyCrimes$Hour))
dailyCrimes$Day <- factor(dailyCrimes$Day, ordered = TRUE, 
                          levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

## Plotting the number of crimes each day (line graph)
png(filename = 'dailyCrimes.png', width = 800, height = 600, units = 'px')
ggplot(dailyCrimes, aes(x = Hour, y = Freq)) + 
  geom_line(aes(group = Day, color = Day), lwd = 1.5) + 
  xlab('Hour') + ylab('Number of crimes') +
  theme(plot.title = element_text(face="bold", size = 22, hjust = 0.5), legend.key.size = unit(1.5, "cm")) +
  ggtitle("Simple Plot of Daily Crime Occurence")
dev.off()

## Plotting the number of crimes each day (heat map)
png(filename = 'heatmap.png', width = 800, height = 600, units = 'px')
ggplot(dailyCrimes, aes(x = Hour, y = Day)) + geom_tile(aes(fill = Freq)) + 
  scale_fill_gradient(name = 'Total Crimes', low = 'green', high = 'red') + 
  theme(axis.title.y = element_blank(), 
        plot.background = element_rect(fill = 'white', color = 'white'),
        panel.background = element_rect(fill = 'grey95', color = 'white'),
        plot.title = element_text(face="bold", size = 22, hjust = 0.5),
        axis.text=element_text(size=14),
        legend.key.size = unit(1.5, "cm")) +
  scale_x_discrete(limits=c(0, 3, 6, 9, 12, 15, 18, 21, 24)) +
  scale_y_discrete(limits = rev(levels(dailyCrimes$Day))) +
  xlab("Hour of the Day") +
  ggtitle("Heat Map of Crimes in Jersey City by Time")
dev.off()
# Round the lat and lon
jc.crime$Longitude <- round(as.numeric(jc.crime$lon), 2)
jc.crime$Latitude <- round(as.numeric(jc.crime$lat), 2)
# Get Jersey City map
jc <- get_map("Jersey City", zoom = 12)
png(filename = "Jersey City.png", width = 1200, height = 900, units = "px")
ggmap(jc)
dev.off()

## Get crime locations
locationCrimes <- as.data.frame(table(jc.crime$Longitude, jc.crime$Latitude))
names(locationCrimes) <- c('long', 'lat', 'Frequency')
locationCrimes$long <- as.numeric(as.character(locationCrimes$long))
locationCrimes$lat <- as.numeric(as.character(locationCrimes$lat))
locationCrimes <- subset(locationCrimes, Frequency > 0)

## Plotting the location heatmap
png(filename = "Jersey City map.png", width = 1100, height = 950, units = "px")
locationCrimes$Frequency <- as.factor(locationCrimes$Frequency)
ggmap(jc) +
theme(axis.title=element_text(size=14,face="bold"),
      plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5),
      axis.text=element_text(size=14), legend.key.size = unit(2, "cm"),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13)) +
scale_x_continuous(limits = c( -74.12 , -74.02 )) +
scale_y_continuous(limits = c( 40.675 , 40.75 )) +
geom_tile(data = locationCrimes, aes(x = long, y = lat, alpha = Frequency), fill = "red") + 
scale_alpha_discrete(guide = guide_legend(title = "Crime Frequency of 2015"),
                     breaks = c(1, 20, 41, 68, 88, 123, 134),
                     labels = c("Safe", 25, 50, 75, 100, 125, "Dangerous")) +
ggtitle("Heat Map of Crimes in \n Jersey City by Location") +
labs(x = "Latitude", y = "Longtitude")
# guide_legend(title = "Crime Frequency of 2015")
dev.off()
