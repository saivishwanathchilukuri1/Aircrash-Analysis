#importing all required libraries
install.packages("PerformanceAnalytics")
install.packages("plotly")
install.packages("tidytext")
install.packages("timetk")
install.packages("tm")
library(tidyverse)  # to manipulate data
library(dplyr)      # to manipulate data
library(ggplot2)    # to plot graph
library(lubridate)  # to manipulate as date
library(gridExtra)  # to arrange multiple grid based plots on a page
library(readr)    # to read functions to import xls file / to read flat/tabular text files
library(stringr)  # to use regx and other string functions
library(tidyverse)  # to manipulate data
library(RColorBrewer)# to have nice color palettes
library(tm)         # to perform text mining operations (for wordcloud here)
library(caret)      # to spilt data and and select featured data
library(plotly)
library(timetk)
library(tidytext)
library(corrplot)
library(ggpubr)
library(PerformanceAnalytics)
library(plotrix)

#loading dataset
master_air_crash_df <-
  read_csv(
    "C:\\StFX\\Bigdata\\Project2\\Airplane_Crashes_and_Fatalities_Since_1908_new.csv"
  )

air_crash_df = master_air_crash_df

#data cleaning

#date conversion
air_crash_df$Crash_Date <- strptime(air_crash_df$Date, "%m/%d/%Y")
air_crash_df$Crash_Date <- as.Date(air_crash_df$Crash_Date)

dates <- air_crash_df$Crash_Date
air_crash_df$Day <- day(dates)
air_crash_df$Month <- month(dates)
air_crash_df$Year <- year(dates)

str(air_crash_df)

air_crash_df$Location <- sapply(air_crash_df$Location, as.character)
air_crash_df$LocationCountry <-
  gsub(".*,", "", air_crash_df$Location)

#remove white space at beginning
air_crash_df$LocationCountry <-
  str_trim(air_crash_df$LocationCountry, side = "both")

#Convert string back to factors
air_crash_df$LocationCountry <-
  sapply(air_crash_df$LocationCountry, as.factor)

#convert columns  char to int
air_crash_df$Month <- sapply(air_crash_df$Month, as.numeric)
air_crash_df$Day <- sapply(air_crash_df$Day, as.numeric)
air_crash_df$Year <- sapply(air_crash_df$Year, as.character)
air_crash_df$Aboard <- sapply(air_crash_df$Aboard, as.numeric)
air_crash_df$`Aboard Crew` <-
  sapply(air_crash_df$`Aboard Crew`, as.numeric)
air_crash_df$`Aboard Passangers` <-
  sapply(air_crash_df$`Aboard Passangers`, as.numeric)
air_crash_df$Fatalities <-
  sapply(air_crash_df$Fatalities, as.numeric)
air_crash_df$`Fatalities Passangers` <-
  sapply(air_crash_df$`Fatalities Passangers`, as.numeric)
air_crash_df$`Fatalities Crew` <-
  sapply(air_crash_df$`Fatalities Crew`, as.numeric)


#start of 1 crash trend yearly and monthly

years <- as.data.frame(table(air_crash_df$Year))
years_graph <- years %>% 
  ggplot(aes(y = Freq, x = Var1,color = Freq, group = 1))  +
  geom_line(size = 1,
            linetype = 1,
            color = "Navy") +
  geom_smooth() +
  geom_point(size = 3, shape = 20) +
  xlab("Years") + ylab("Crashes") +
  scale_x_discrete(breaks = seq(from = 1908, to = 2019, by = 10)) +
  ggtitle("Figure 3. Total number of crashes per year") +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))

months <- as.data.frame(table(air_crash_df$Month))

months_graph <- months %>% 
  ggplot(aes(Var1, Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.3) +
  xlab("Months") + ylab("Crashes") +
  ggtitle("Total number of crashes per month") +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))

grid.arrange(years_graph,
             months_graph,
             nrow = 2,
             heights = 2:1)
#end of 1 crash trend yearly and monthly

#start of 2 fatality percent
Fatalities <- air_crash_df %>%
  group_by(Year) %>%
  summarise(
    total_fatalities = sum(Fatalities, na.rm = TRUE),
    total_passengers = sum(Aboard, na.rm = TRUE)
  )

Fatalities %>% 
  ggplot(aes(
  y = (total_fatalities / total_passengers) * 100,
  x = Year,
  color = (total_fatalities / total_passengers) * 100,
  group = 10
))  +
  geom_line(size = 1,
            linetype = 1) +
  geom_point(size = 3, shape = 20) +
  geom_smooth() +
  xlab("Years") + ylab("% Fatalities") +
  scale_x_discrete(breaks = seq(from = 1908, to = 2019, by = 10)) +
  ggtitle(Figure 4."Percent of fatalities per year") +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of 2 fatality percent

#start of 3 crash locations
Location_Crash <-   air_crash_df %>%
  group_by(LocationCountry) %>%
  summarise(total_fatalities = sum(Fatalities, na.rm = TRUE)) %>%
  arrange(desc(total_fatalities))

Location_Crash <- Location_Crash[1:10, ]

Location_Crash %>% 
  ggplot(aes(
         x = reorder(LocationCountry,-total_fatalities),
         y = total_fatalities,
         fill =  LocationCountry
       )) +
  geom_bar(stat = "identity", width = 0.5) +
  xlab("Countries") + ylab("Number of fatalities") +
  ggtitle("Figure 5. Top 10 Countries with Maximum Fatalities") +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))

pie3D(
  Location_Crash$total_fatalities,
  labels = Location_Crash$LocationCountry,
  main = "Figure 6. Reason of cancelled flights"
)
#end of 3 crash locations

# start of 4 aircraft operatiors
operator <- air_crash_df %>%
  group_by(Operator) %>%
  summarise(Freq = n()) %>%
  arrange(desc(Freq))

ggplot(operator[1:10, ], aes(
  x = reorder(factor(Operator), Freq),
  y = Freq,
  fill = Freq
)) +
  geom_bar(stat = "identity", width = 0.09) +
  xlab("Aircraft Operators") + ylab("Crashes") +
  ggtitle("Figure 7. Top 10 Aircraft Operator causing Aircrash") +
  coord_flip() +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
# end of 4 aircraft operatiors

# strat of 5 aircraft type
type <- air_crash_df %>%
  group_by(`AC Type`) %>%
  summarise(Freq = n()) %>%
  arrange(desc(Freq))

view(type)

ggplot(type[1:10, ], aes(
  x = reorder(factor(`AC Type`), Freq),
  y = Freq,
  fill = Freq
)) +
  geom_bar(stat = "identity", width = 0.09) +
  xlab("Types") + ylab("Crashes") +
  ggtitle("Figure 8. Top 10 Aircraft Type causing Aircrash") +
  coord_flip() +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
# end of 5 aircraft type

#start of 6 Total number of crashes per year from 1970 to 2019
years <- as.data.frame(table(air_crash_df$Year))
years <- years[60:109, ]

ggplot(years, aes(y = Freq, x = Var1, color = Freq, group = 1))  +
  geom_line(size = 1,
            linetype = 1) +
  geom_point(size = 3, shape = 20) +
  geom_smooth() +
  xlab("Years") + ylab("Crashes") +
  scale_x_discrete(breaks = seq(from = 1970, to = 2019, by = 10)) +
  ggtitle("Figure 9. Total number of crashes per year from 1970 to 2019") +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of 6 Total number of crashes per year from 1970 to 2019

#start of 7 total no of fetalities and passengers
total_fetalities_passengers <- air_crash_df %>%
  group_by(Year) %>%
  summarize(
    total_fetalities = sum(Fatalities, na.rm = TRUE),
    total_passengers = sum(Aboard, na.rm = TRUE)
  )
total_fetalities_passengers <- total_fetalities_passengers[60:109, ]
plot_ly(x = total_fetalities_passengers$Year) %>%
  add_lines(
    y = total_fetalities_passengers$total_passengers,
    color = I("red"),
    name = "Total Passengers"
  ) %>%
  add_lines(
    y = total_fetalities_passengers$total_fetalities,
    color = I("green"),
    name = "Total Fetalities"
  )
#end of 7 total no of fetalities and passengers

#start of 8 count of accidents by year of Aeroflot
crash_of_aeroflot =  air_crash_df %>%
  group_by(Year) %>%
  filter(Operator == "Aeroflot") %>%
  summarise(Freq = n())

ggplot(crash_of_aeroflot, aes(y = Freq, x = Year, group = 1))  +
  geom_line(size = 1,
            linetype = 1,
            color = "Navy") +
  geom_point(size = 3, shape = 20) +
  geom_smooth() +
  xlab("Years") + ylab("Count") +
  scale_x_discrete(breaks = seq(from = 1934, to = 2019, by = 10)) +
  ggtitle("Figure 10. Count of Accidents by year (Aeroflot)") +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of 8 count of accidents by year of Aeroflot

#start of 9 time series
ts_year_df <- air_crash_df %>% count(Year)
ts_month_df <- air_crash_df %>% count(Month)
ts_day_df <- air_crash_df %>% count(Day)
ts_year_df %>%
  plot_time_series(as.numeric(Year), n)
ts_month_df %>%
  plot_time_series(as.numeric(Month), n)
ts_day_df %>%
  plot_time_series(as.numeric(Day), n)
#end of 9 time series


#start of 11 other data mining
#all important words
datamining <- air_crash_df %>%
  unnest_tokens(word, Summary) %>%
  filter(!word %in% stop_words$word |
           word %in% c("crashed", "aircraft", "failure")) %>%
  dplyr::count(word, sort = TRUE)

ggplot(datamining[1:20, ], aes(
  x = reorder(word,-n),
  y = n,
  alpha = n
)) +
  geom_bar(stat = "identity", fill = "green", width = 0.5) +
  xlab("crash words") + ylab("Count") +
  ggtitle("Figure 11.Common air crash words") +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))

#only sentences
datamining_sentences <- air_crash_df %>%
  unnest_tokens(sentences, Summary, token = "sentences") %>%
  filter(!sentences %in% stop_words$sentences) %>%
  dplyr::count(sentences, sort = TRUE)

ggplot(datamining_sentences[1:20, ],
       aes(
         x = reorder(sentences,-n),
         y = n,
         alpha = n
       )) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  xlab("Count") + ylab("Crash Phrases") +
  ggtitle("Figure 12. Common air crash words") +
  coord_flip() +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of 11 other data mining

#start of 12 correlation matrix
air_crash_df$Year <- sapply(air_crash_df$Year, as.numeric)
selected_columns_df <- air_crash_df %>%
  select(
    Month,
    Day,
    Year,
    Aboard,
    `Aboard Passangers`,
    `Aboard Crew`,
    Fatalities,
    `Fatalities Passangers`,
    `Fatalities Crew`
  )

selected_columns_df = na.omit(selected_columns_df)

selected_columns_df.cor = cor(selected_columns_df, method = c("spearman"))
corrplot(selected_columns_df.cor)+title("Figure 1.Correlation Matrix ")
#end of 12 correlation matrix

#start of 13 hourly crash
df_time <- air_crash_df %>%
  separate(Time, c("hours", "minutes"), sep = "(:)")
df_time = na.omit(df_time)
df_time_freq <-  df_time %>%
  group_by(hours) %>%
  filter(hours < 25) %>%
  dplyr::summarise(Freq = n())
ggplot(df_time_freq,
       aes(x = hours, y = Freq,
           alpha = Freq)) +
  geom_bar(stat = "identity", fill = "maroon", width = 0.5) +
  xlab("Hour") + ylab("Number of crashes") +
  ggtitle("Figure 13.Air crashes per hour") +
  coord_flip() +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of 13 hourly crash

#start of 14 normal distribution
selected_columns_df <- air_crash_df %>%
  select(Month, Day, Year, Aboard, Fatalities)
selected_columns_df = na.omit(selected_columns_df)

#month
y <- dnorm(air_crash_df$Month, mean = 2.5, sd = 0.5)
plot(air_crash_df$Month, y) +
y <- pnorm(air_crash_df$Month, mean = 2.5, sd = 2)
plot(air_crash_df$Month, y)
#end of 14 normal distribution

#start of 15 corelation between aboard and fatalities
ggscatter(
  air_crash_df,
  x = "Aboard",
  y = "Fatalities",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "Aboard",
  ylab = "Fatalities"
)

ggqqplot(air_crash_df$Aboard, ylab = "Aboard")
ggqqplot(air_crash_df$Fatalities, ylab = "Fatalities")
#end of 15 corelation between aboard and fatalities

#start of 16 Use chart.Correlation(): Draw scatter plots
selected_columns_df <- air_crash_df %>%
  select(Aboard,  Fatalities)
selected_columns_df = na.omit(selected_columns_df)
selected_columns_df$Aboard <-
  sapply(selected_columns_df$Aboard, as.numeric)
selected_columns_df$Fatalities <-
  sapply(selected_columns_df$Fatalities, as.numeric)

chart.Correlation(selected_columns_df, histogram = TRUE, pch = 19)+title("Figure 2. Corr b/w Aboard and Fatalities")   
#end of 16 Use chart.Correlation(): Draw scatter plots

#start of 17 crashes per day
day <- as.data.frame(table(air_crash_df$Day))
ggplot(day, aes(y = Freq, x = Var1, group = 1))  +
  geom_line(size = 1,
            linetype = 1,
            color = "Navy") +
  geom_point(size = 3, shape = 20) +
  geom_smooth() +
  xlab("Day") + ylab("Crashes") +
  scale_x_discrete(breaks = seq(from = 1, to = 31, by = 1)) +
  ggtitle("Figure 14. Total number of crashes per day") +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of 17 crashes per day

#start of 17 time series Crashes on each day of the week
air_crash_df$Day_Name <- weekdays(as.Date(air_crash_df$Crash_Date))
crashes_day_df <- air_crash_df %>%
  group_by(Day_Name) %>%
  dplyr::summarize(CRASHES_TOTAL_DAY = n())
crashes_day_df$Day_Name <-
  factor(
    crashes_day_df$Day_Name,
    levels = c(
      "Sunday",
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday"
    )
  )
ggplot(crashes_day_df,
       aes(x = Day_Name, y = CRASHES_TOTAL_DAY,
           alpha = CRASHES_TOTAL_DAY)) +
  geom_bar(stat = "identity", fill = "maroon", width = 0.5) +
  xlab("Day") + ylab("Total crashes") +
  ggtitle("Figure 15. Crashes on each day of the week") +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of 17 time series Crashes on each day of the week