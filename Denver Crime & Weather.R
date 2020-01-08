DenverCrime <- read.csv("crime.csv", header=TRUE, stringsAsFactors = F)
CrimeCodes <- read.csv("offense_codes.csv", header= T, stringsAsFactors = F)

colnames(DenverCrime)

# Remove all columns from the coding data that shouldn't be joined in the Crime Data
CrimeCodesTrimmed <- subset(CrimeCodes,  
                            select=c(OFFENSE_CODE, OFFENSE_CODE_EXTENSION,
                                     OFFENSE_TYPE_NAME, OFFENSE_CATEGORY_NAME))

# Combine Crime Code & Extension in both the Code set & Crime set to join them by full code
DenverCrime$OFFENSE_CODE_FULL <- paste(DenverCrime$OFFENSE_CODE, 
                                       DenverCrime$OFFENSE_CODE_EXTENSION, sep="-")
CrimeCodesTrimmed$OFFENSE_CODE_FULL <- paste(CrimeCodesTrimmed$OFFENSE_CODE, 
                                             CrimeCodesTrimmed$OFFENSE_CODE_EXTENSION, 
                                             sep="-")

# Remove the original offense code & extension variables
DenverCrime <- subset(DenverCrime, 
                      select = -c(OFFENSE_CODE, OFFENSE_CODE_EXTENSION))
CrimeCodesTrimmed <- subset(CrimeCodesTrimmed, 
                            select = -c(OFFENSE_CODE, OFFENSE_CODE_EXTENSION))

# Join the Crime set & Code set by the full offense code for crime descriptions
DenverCrimeCoded <- left_join(DenverCrime, CrimeCodesTrimmed, by = "OFFENSE_CODE_FULL")

# Re-format Dates
DenverCrimeCoded$FIRST_OCCURRENCE_DATE <- substr(DenverCrimeCoded$FIRST_OCCURRENCE_DATE, 0, 10)
DenverCrimeCoded$FIRST_OCCURRENCE_DATE <- as.Date(DenverCrimeCoded$FIRST_OCCURRENCE_DATE, 
                                                  format = "%m/%d/%Y")

# Remove Traffic Accidents
DenverCrimeCoded <- subset(DenverCrimeCoded, IS_CRIME == 1)

colnames(DenverCrimeCoded)

DenverCrimeCoded <- subset(DenverCrimeCoded, 
                           select= c(INCIDENT_ID, FIRST_OCCURRENCE_DATE, DISTRICT_ID, 
                                     OFFENSE_CODE_FULL, OFFENSE_TYPE_NAME, OFFENSE_CATEGORY_NAME)) 

# Remove Traffic Violations
DenverCrimeCoded <- DenverCrimeCoded[-grep("traffic", DenverCrimeCoded$OFFENSE_TYPE_NAME, 
                                           ignore.case = T),]

write.csv(DenverCrimeCoded, file="DenverCrimeCleanedAndCoded.csv", row.names = F)

class(DenverCrimeCoded$FIRST_OCCURRENCE_DATE)

length(unique(DenverCrimeCoded$FIRST_OCCURRENCE_DATE))

options(scipen=999)
# Load the weather data by year & trim to only variables needed
Weather2014 <- read.csv("1927937 - Weather 2014.csv", header=T, stringsAsFactors = F)
Weather2015 <- read.csv("1949974 - Weather 2015.csv", header=T, stringsAsFactors = F)
Weather2016 <- read.csv("1927946 - Weather 2016.csv", header=T, stringsAsFactors = F)
Weather2017 <- read.csv("1927949 - Weather 2017.csv", header=T, stringsAsFactors = F)
Weather2018 <- read.csv("1927956 - Weather 2018.csv", header=T, stringsAsFactors = F)
Weather2019 <- read.csv("1949998 - Weather 2019.csv", header=T, stringsAsFactors = F)

colnames(Weather2014)

# The main variables being analyzed are windspeed, precipitation, snowfall, snow depth, average
# temperature, max temperature, and minimum temperature
# Trim down to the variables to be analyzed
Weather2014Trim <- subset(Weather2014, select = c(STATION, DATE, AWND, PRCP, SNOW, SNWD, TAVG, TMAX, TMIN))
Weather2015Trim <- subset(Weather2015, select = c(STATION, DATE, AWND, PRCP, SNOW, SNWD, TAVG, TMAX, TMIN))
Weather2016Trim <- subset(Weather2016, select = c(STATION, DATE, AWND, PRCP, SNOW, SNWD, TAVG, TMAX, TMIN))
Weather2017Trim <- subset(Weather2017, select = c(STATION, DATE, AWND, PRCP, SNOW, SNWD, TAVG, TMAX, TMIN))
Weather2018Trim <- subset(Weather2018, select = c(STATION, DATE, AWND, PRCP, SNOW, SNWD, TAVG, TMAX, TMIN))
Weather2019Trim <- subset(Weather2019, select = c(STATION, DATE, AWND, PRCP, SNOW, SNWD, TAVG, TMAX, TMIN))

# Look at the data from each of these sets to make sure there are no missing/abberant values

summary(Weather2014Trim)

# No real extreme values

summary(Weather2015Trim)

# AWND - Average Wind Speed has a max of 320.11?
# This is an impossible value for this variable
which(Weather2015Trim$AWND == 320.11)
Weather2015Trim[54137,]
# Remove that row as the value is obviously incorrect
Weather2015Trim <- Weather2015Trim[-54137,]
summary(Weather2015Trim)
# Average Wind Speed again has a max of 99.99...
# This is a high value, but not necessarily incorrect.  I will check
# Other values for wind speed on this date
which(Weather2015Trim$AWND == 99.99)
Weather2015Trim[53880,]
Weather2015Trim$DATE <- as.Date(Weather2015Trim$DATE)
Jan2015 <- subset(Weather2015Trim, Weather2015Trim$DATE == "2015-01-04", select = c(STATION, DATE, AWND))
na.omit(Jan2015)
#          STATION       DATE  AWND
# 31482 USW00093067 2015-01-04  6.49
# 53880 USC00058995 2015-01-04 99.99
# 78399 USW00003017 2015-01-04  6.71
# The other readings for this date are much lower, which leads me
# to believe this is another erroneous value
Weather2015Trim <- Weather2015Trim[-53880,]
summary(Weather2015Trim)

summary(Weather2016Trim)

# No extreme values

summary(Weather2017Trim)

# No extreme values

summary(Weather2018Trim)

# No extreme values

summary(Weather2019Trim)
# Average wind speed of 250.09 - an impossible value

which(Weather2019Trim$AWND == 250.09)

Weather2019Trim[17846,]

Apr2019 <- subset(Weather2019Trim, Weather2019Trim$DATE == "2019-04-07", select = c(STATION, DATE, AWND))
na.omit(Apr2019)
# Especially compared to other windspeeds in the area on the same date
Weather2019Trim <- Weather2019Trim[-17846,]


AllWeather <- Reduce(function(x,y) merge(x,y,all=TRUE), 
                     list(Weather2014Trim, Weather2015Trim, Weather2016Trim, Weather2017Trim,
                          Weather2018Trim, Weather2019Trim))

AllWeather$DATE <- as.Date(AllWeather$DATE)

WeatherByDate <- data.frame("Date" = seq.Date(from=as.Date("2014/01/02"), 
                                              to=as.Date("2019/05/31"), by = "days"),
                            "AvgWindSpeed" = aggregate(AWND ~ DATE, AllWeather, mean),
                            "Precipitation" = aggregate(PRCP ~ DATE, AllWeather, mean),
                            "Snowfall" = aggregate(SNOW ~ DATE, AllWeather, mean),
                            "SnowDepth" = aggregate(SNWD ~ DATE, AllWeather, mean),
                            "AvgTemp" = aggregate(TAVG ~ DATE, AllWeather, mean),
                            "AvgMaxTemp" = aggregate(TMAX ~ DATE, AllWeather, mean),
                            "AvgMinTemp" = aggregate(TMIN ~ DATE, AllWeather, mean))

WeatherByDate <- subset(WeatherByDate, select=c(Date, AvgWindSpeed.AWND, Precipitation.PRCP,
                                                Snowfall.SNOW, SnowDepth.SNWD, AvgTemp.TAVG,
                                                AvgMaxTemp.TMAX, AvgMinTemp.TMIN))  

names(WeatherByDate)[names(WeatherByDate) == "AvgWindSpeed.AWND"] <- "AvgWindSpeed"
names(WeatherByDate)[names(WeatherByDate) == "Precipitation.PRCP"] <- "Precipitation"
names(WeatherByDate)[names(WeatherByDate) == "Snowfall.SNOW"] <- "Snowfall"
names(WeatherByDate)[names(WeatherByDate) == "SnowDepth.SNWD"] <- "SnowDepth"
names(WeatherByDate)[names(WeatherByDate) == "AvgTemp.TAVG"] <- "AvgTemp"
names(WeatherByDate)[names(WeatherByDate) == "AvgMaxTemp.TMAX"] <- "AvgMaxTemp"
names(WeatherByDate)[names(WeatherByDate) == "AvgMinTemp.TMIN"] <- "AvgMinTemp"

WeatherByDate$Date <- as.Date(WeatherByDate$Date)

write.csv(WeatherByDate, file='WeatherByDateClean.csv', row.names = F)

WeatherByDate <- read.csv('WeatherByDateClean.csv', stringsAsFactors = F)

DenverViolentCrime <- read.csv("DenverViolentCrime.csv", header = T, stringsAsFactors = F)
WeatherByDate <- read.csv("WeatherByDateClean.csv", header = T, stringsAsFactors = F)

DenverViolentCrime$FIRST_OCCURRENCE_DATE <- as.Date(DenverViolentCrime$FIRST_OCCURRENCE_DATE, 
                                                  format = "%Y-%m-%d")
WeatherByDate$Date <- as.Date(WeatherByDate$Date, 
                              format = "%Y-%m-%d")



class(DenverViolentCrime$FIRST_OCCURRENCE_DATE)
class(WeatherByDate$Date)

DenverCrimeWeather <- merge(DenverViolentCrime, WeatherByDate, 
                            by.x="FIRST_OCCURRENCE_DATE", by.y = "Date")

names(DenverCrimeWeather)[names(DenverCrimeWeather) == "FIRST_OCCURRENCE_DATE"] <- "Date"
names(DenverCrimeWeather)[names(DenverCrimeWeather) == "DISTRICT_ID"] <- "District"
names(DenverCrimeWeather)[names(DenverCrimeWeather) == "PRECINCT_ID"] <- "Precinct"
names(DenverCrimeWeather)[names(DenverCrimeWeather) == "OFFENSE_TYPE_NAME"] <- "OffenseName"

DenverCrimeWeather <- subset(DenverCrimeWeather, select = -c(Neighborhood, OFFENSE_CODE_FULL, OFFENSE_CATEGORY_NAME))

length(unique(DenverCrimeWeather$Precinct))

write.csv(DenverCrimeWeather, file="DenverViolentCrimeWeather.csv", row.names = F)

DenverCrimeWeather <- read.csv("DenverViolentCrimeWeather.csv", stringsAsFactors = F)
colnames(DenverCrimeWeather)

# Distribution of years
CrimeByYearPlot <- ggplot(data = DenverCrimeWeather) + 
  geom_bar(mapping = aes(x = year(Date), y = ..prop.., group = 1), 
           stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  ggtitle("Distribution of Years") +
  xlab("Year") +
  ylab("Proportion")

CrimeByYearPlot

Months = c("January","February","March","April","May","June","July","August","September",
           "October","November","December")

CrimeByMonthPlot <- ggplot(data = DenverCrimeWeather) +
  geom_bar(mapping = aes(x = month(Date), y = ..prop.., group=1),
           stat = 'count') +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() + 
  ggtitle("Distribution of Crimes by Month") +
  xlab("Month") +
  ylab("Proportion")

CrimeByMonthPlot

CrimeByDistrictPlot <- ggplot(data = DenverCrimeWeather) + 
  geom_bar(mapping = aes(x=District, y = ..prop.., group =1), stat = 'count') +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() + 
  ggtitle("Distribution of Districts") + 
  xlab("District") +
  ylab("Proportion")

CrimeByDistrictPlot

DenverCrimeWeather$PRECINCT_ID <- as.factor(DenverCrimeWeather$PRECINCT_ID)

CrimeByPrecinctPlot <- ggplot(data = DenverCrimeWeather) +
  geom_bar(mapping = aes(x=Precinct, y = ..prop.., group =1), stat = 'count') +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Distribution of Precincts") + 
  xlab("District") +
  ylab("Proportion")

CrimeByPrecinctPlot


unique(DenverCrimeWeather$NEIGHBORHOOD_ID)

DenverCrimeWeather$NEIGHBORHOOD_ID <- as.factor(DenverCrimeWeather$NEIGHBORHOOD_ID)

CrimeByNeighborhoodPlot <- ggplot(data = DenverCrimeWeather) +
  geom_bar(mapping = aes(x=NEIGHBORHOOD_ID, y = ..prop.., group =1), stat = 'count') +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() + 
  ggtitle("Distribution of Neighborhoods") + 
  xlab("District") +
  ylab("Proportion")


DenverCrimeWeather$OFFENSE_TYPE_NAME <- as.factor(DenverCrimeWeather$OFFENSE_TYPE_NAME)

OffenseNamePlot <- ggplot(data = DenverCrimeWeather) + 
  geom_bar(mapping = aes(x = OFFENSE_TYPE_NAME, y = ..prop.., group = 1), stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  ggtitle("Distribution of Offense Name") +
  xlab("Offense Name") +
  ylab("Proportion")

summary(DenverCrimeWeather$AvgWindSpeed)

AvgWindSpeedPlot <- ggplot(data = DenverCrimeWeather) + 
  geom_area(mapping = aes(x = AvgWindSpeed, y = ..density..), stat = "bin", binwidth=0.5) +
  scale_x_continuous(limits = c(0, 23)) +
  ggtitle("Distribution of Average Wind Speed (in MPH)")

summary(DenverCrimeWeather$Precipitation)

PrecipitationPlot <- ggplot(data = DenverCrimeWeather) + 
  geom_area(mapping = aes(x = Precipitation, y = ..density..), stat = "bin", binwidth=.1) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  ggtitle("Distribution of Average Precipitation (in inches)")

summary(DenverCrimeWeather$Snowfall)

SnowfallPlot <- ggplot(data = DenverCrimeWeather) + 
  geom_area(mapping = aes(x = Snowfall, y = ..density..), stat = "bin", binwidth=0.1) +
  scale_x_continuous(limits = c(-1, 14)) +
  ggtitle("Distribution of Average Snowfall (in inches)")

summary(DenverCrimeWeather$SnowDepth)

AvgWindSpeedPlot <- ggplot(data = DenverCrimeWeather) + 
  geom_area(mapping = aes(x = AvgWindSpeed, y = ..density..), stat = "bin", binwidth=0.5) +
  scale_x_continuous(limits = c(0, 25)) +
  ggtitle("Distribution of Average Snow Depth (in inches)")

summary(DenverCrimeWeather$AvgTemp)

AvgTempPlot <- ggplot(data = DenverCrimeWeather) + 
  geom_area(mapping = aes(x = AvgTemp, y = ..density..), stat = "bin", binwidth=1) +
  scale_x_continuous(limits = c(-10, 75)) +
  ggtitle("Distribution of Average Temperature (in Fahrenheit)")

summary(DenverCrimeWeather$AvgMaxTemp)

AvgMaxTempPlot <- ggplot(data = DenverCrimeWeather) + 
  geom_area(mapping = aes(x = AvgMaxTemp, y = ..density..), stat = "bin", binwidth=1) +
  scale_x_continuous(limits = c(5, 100)) +
  ggtitle("Distribution of Average Maximum Temperature (in Fahrenheit)")

summary(DenverCrimeWeather$AvgMinTemp)

AvgMinTempPlot <- ggplot(data = DenverCrimeWeather) + 
  geom_area(mapping = aes(x = AvgMinTemp, y = ..density..), stat = "bin", binwidth=1) +
  scale_x_continuous(limits = c(-20, 65)) +
  ggtitle("Distribution of Average Minimum Temperature (in Fahrenheit)")

colnames(DenverCrimeWeather)

OffenseName_X_AvgWindSpeed <- ggplot(DenverCrimeWeather) + 
  aes(x=OFFENSE_TYPE_NAME=='Rape', fill = factor(AvgWindSpeed)) +
  geom_bar(position = "fill") +
  ggtitle("Distribution of Offense Type Name by Average Windspeed") +
  labs(y = "Proportion")

OffenseName_X_AvgWindSpeed 

WeatherByDate <- read.csv('WeatherByDateClean.csv', stringsAsFactors = F)
DenverCrimeWeather <- read.csv("DenverCrimeWeather.csv", header = T)

DateDensity <- as.data.frame(table(DenverCrimeWeather$Date))

colnames(DateDensity)

DateWeatherCrimes <- merge(DateDensity, WeatherByDate,
                           by.x="Var1", by.y = "Date")

colnames(DateWeatherCrimes)

names(DateWeatherCrimes)[names(DateWeatherCrimes) == "Var1"] <- "Date"
names(DateWeatherCrimes)[names(DateWeatherCrimes) == "Freq"] <- "TotalCrimes"

TotalCrimesByPrecip <- ggplot(DateWeatherCrimes, aes(x=TotalCrimes, y=Precipitation)) +
  geom_point()

TotalCrimesByPrecip

ggplot(DateWeatherCrimes, aes(x=TotalCrimes, y=Snowfall)) + geom_point()

ggplot(DateWeatherCrimes, aes(x=TotalCrimes, y=AvgMaxTemp)) + geom_point()

ggplot(DateWeatherCrimes, aes(x=TotalCrimes, y=AvgTemp)) + geom_point()

ggplot(DateWeatherCrimes, aes(x=TotalCrimes, y=AvgMinTemp)) + geom_point()

ggplot(DateWeatherCrimes, aes(x=TotalCrimes, y=AvgWindSpeed)) + geom_point()

ggplot(DateWeatherCrimes, aes(x=TotalCrimes, y=SnowDepth)) + geom_point()

max(DateWeatherCrimes$TotalCrimes)

which(DateWeatherCrimes$TotalCrimes == 250)
DateWeatherCrimes[1096,]

DenverCrimeWeather <- read.csv("DenverCrimeWeather.csv", header = T, stringsAsFactors = F)

colnames(DenverCrimeWeather)

DenverCrimeWeather$Date <- as.Date(DenverCrimeWeather$Date)
DenverCrimeWeather$District <- as.factor(DenverCrimeWeather$District)
DenverCrimeWeather$OffenseName <- as.factor(DenverCrimeWeather$OffenseName)
DenverCrimeWeather$OffenseCategory <- as.factor(DenverCrimeWeather$OffenseCategory)

# Offense Category by Precipitation
CatbyRain <- ggplot(DenverCrimeWeather,aes(x=OffenseCategory, y=Precipitation, 
                                           group = District, colour=factor(District))) +
  geom_jitter(width=0.25, alpha=0.5) +
  geom_hline(yintercept=0) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
  labs(title = "Offense Category by Precipitation (in Inches)",
       x = "Offense Category",
       y = "Precipitation (in Inches)")

CatbyRain

# Offense Category by Snowfall
CatbySnow <- ggplot(DenverCrimeWeather,aes(x=OffenseCategory, y=Snowfall, 
                                           group = District, colour=factor(District))) +
  geom_jitter(width=0.25, alpha=0.5) +
  geom_hline(yintercept=0) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
  labs(title = "Offense Category by Snowfall (in Inches)",
       x = "Offense Category",
       y = "Snowfall (in Inches")

CatbySnow

# Offense Category by SnowDepth
CatbySnowDepth <- ggplot(DenverCrimeWeather,aes(x=OffenseCategory, y=SnowDepth, 
                                                group = District, colour=factor(District))) +
  geom_jitter(width=0.25, alpha=0.5) +
  geom_hline(yintercept=0) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
  labs(title = "Offense Category by SnowDepth (in Inches)",
       x = "Offense Category",
       y = "SnowDepth (in Inches")

CatbySnowDepth

# Offense Category by AvgWindSpeed
CatbyAvgWindSpeed <- ggplot(DenverCrimeWeather,aes(x=OffenseCategory, y=AvgWindSpeed, 
                                                   group = District, colour=factor(District))) +
  geom_jitter(width=0.25, alpha=0.5) +
  geom_hline(yintercept=0) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
  labs(title = "Offense Category by AvgWindSpeed (in Inches)",
       x = "Offense Category",
       y = "AvgWindSpeed (in Inches")

CatbyAvgWindSpeed

# Offense Category by Average Max Temp
CatbyAvgMaxTemp <- ggplot(DenverCrimeWeather,aes(x=OffenseCategory, y=AvgMaxTemp, 
                                                 group = District, colour=factor(District))) +
  geom_jitter(width=0.25, alpha=0.5) +
  geom_hline(yintercept=0) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
  labs(title = "Offense Category by AvgMaxTemp (in Inches)",
       x = "Offense Category",
       y = "AvgMaxTemp (in Inches")

CatbyAvgMaxTemp

# Offense Category by Average Max Temp
CatbyAvgMinTemp <- ggplot(DenverCrimeWeather,aes(x=OffenseCategory, y=AvgMinTemp, 
                                                 group = District, colour=factor(District))) +
  geom_jitter(width=0.25, alpha=0.5) +
  geom_hline(yintercept=0) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
  labs(title = "Offense Category by AvgMinTemp (in Inches)",
       x = "Offense Category",
       y = "AvgMinTemp (in Inches")

CatbyAvgMinTemp                   

# Offense Category by Avg Temp
CatbyAvgTemp <- ggplot(DenverCrimeWeather,aes(x=OffenseCategory, y=AvgTemp, 
                                              group = District, colour=factor(District))) +
  geom_jitter(width=0.25, alpha=0.5) +
  geom_hline(yintercept=0) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
  labs(title = "Offense Category by AvgTemp (in Inches)",
       x = "Offense Category",
       y = "AvgTemp (in Inches")

CatbyAvgTemp

class(DenverCrimeWeather$Precipitation)

# Precipitation ANOVA
CatbyRainANOVA <- aov(Precipitation ~ OffenseCategory, DenverCrimeWeather)
summary(CatbyRainANOVA)

# Snowfall ANOVA
CatbySnowfallANOVA <- aov(Snowfall ~ OffenseCategory, DenverCrimeWeather)
summary(CatbySnowfallANOVA)
# Snowfall Tukey
SnowfallTukey <- HSD.test(CatbySnowfallANOVA, trt = 'OffenseCategory')
SnowfallTukey

# SnowDepth ANOVA
CatbySnowDepthANOVA <- aov(SnowDepth ~ OffenseCategory, DenverCrimeWeather)
summary(CatbySnowDepthANOVA)
# SnowDepth Tukey
SnowDepthTukey <- TukeyHSD(CatbySnowDepthANOVA, DenverCrimeWeather$SnowDepth, conf.level = 0.95)
summary(SnowDepthTukey)



# AvgWindSpeed ANOVA
CatbyAvgWindSpeedANOVA <- aov(AvgWindSpeed ~ OffenseCategory, DenverCrimeWeather)
summary(CatbyAvgWindSpeedANOVA)

# AvgTemp ANOVA
CatbyAvgTempANOVA <- aov(AvgTemp ~ OffenseCategory, DenverCrimeWeather)
summary(CatbyAvgTempANOVA)

# AvgMaxTemp ANOVA
CatbyAvgMaxTempANOVA <- aov(AvgMaxTemp ~ OffenseCategory, DenverCrimeWeather)
summary(CatbyAvgMaxTempANOVA)

# AvgMinTemp ANOVA
CatbyAvgMinTempANOVA <- aov(AvgMinTemp ~ OffenseCategory, DenverCrimeWeather)
summary(CatbyAvgMinTempANOVA)
       