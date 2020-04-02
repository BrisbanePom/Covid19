#Import Covid Time Series data from JH github and chart Australian states plus others on a log scale based on date 100 cases reported


library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(scales)



#Import raw data from Github
urlfile="https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
dfCovidRaw <- read_csv(url(urlfile))




#Transform data - country + Aus State level
dfCovid1 <- dfCovidRaw %>% 
  select(-Lat,-Long) %>% 
  filter(`Country/Region` %in% c("Australia", "Italy", "Japan", "Korea, South", "New Zealand", "US", "Spain") |
           (`Country/Region` == "United Kingdom" & is.na(`Province/State`))) %>%
  rename(Country = `Country/Region`,
         State = `Province/State`) %>% 
  gather(DateX,CumulativeCases,-State, -Country) %>%
  mutate(Date = parse_date_time(DateX,"mdy")) %>% 
  select(Country, State, Date, CumulativeCases)  
  
#Summarise for Australia as whole
dfCovid1a <- dfCovid1 %>% 
    group_by(Country, Date) %>% 
    summarise(CumulativeCases = sum(CumulativeCases)) %>% 
    rename(Region = Country)
    

#Summarise for Australian states
dfCovid1b <- dfCovid1 %>% 
  filter(Country == "Australia") %>% 
  group_by(State, Date) %>% 
  summarise(CumulativeCases = sum(CumulativeCases)) %>% 
  rename(Region = State)

#Set comparison base point
CompareBase = 100

#Join Australian states back to country data
dfCovid1c <- dfCovid1a %>% 
              bind_rows(dfCovid1b) %>% 
              arrange(Region,Date) %>%
              group_by(Region,Date) %>% 
              summarise(CumulativeCases = sum(CumulativeCases)) %>% 
              arrange(Region,Date) %>% 
              
              mutate(CasesBaseLine = if_else(CumulativeCases >= CompareBase,1,0)) %>% 
              group_by(Region, CasesBaseLine) %>% 
              mutate(DaysSinceBaseCases = row_number()-1) %>% 
              ungroup()
  

#Take reference point - date at which base number of cases exceeded
dfCovid2a <- dfCovid1c %>% 
    filter(DaysSinceBaseCases == 0 & CasesBaseLine == 1) %>% 
    select(Region, Date) %>% 
    rename(RefDate = Date)

#Set Zero date based on Australia
ZeroDate <- dfCovid2a %>% 
              filter(Region == "Australia")
ZeroDate <- as.character(ZeroDate[["RefDate"]])

MaxDate <- dfCovid1c %>% 
              filter(Region == "Australia") %>% 
              group_by(Region) %>% 
              summarise(MaxDate = max(Date))
MaxDate <- as.character(MaxDate[["MaxDate"]])


#Merge back on reference point
dfCovid2 <- dfCovid1c %>% 
              left_join(dfCovid2a, by = "Region") %>% 
              mutate(DaysSinceBaseCases = as.integer(difftime(Date, RefDate, units = c("days"))),
                     AusTimeShift = as.integer(difftime(MaxDate, ZeroDate, units = c("days"))),
                     PlotDate = DaysSinceBaseCases - AusTimeShift,
                     LineAlpha = if_else(Region %in% c("Australia","South Australia"),1,0.8))

  



#Plots
#Country plots
dfCovid2 %>% 
    filter(Region %in% c("Australia", "Italy", "Japan", "Korea, South", "New Zealand", "US", "Spain", "United Kingdom")) %>% 
    ggplot(aes(x=PlotDate, y=CumulativeCases, group=Region, color=Region)) +
      geom_line(aes(alpha = LineAlpha)) +
      geom_point(aes(alpha = LineAlpha), size=1) +
      scale_alpha(guide = 'none', range = c(0.4, 1)) +
      scale_y_log10(labels = comma) +
      coord_cartesian(xlim = c(-25,20), ylim = c(10,150000)) +
      labs(x = "Relative Date (Australia on 01Apr2020 = 0)", y = "Reported Cases (log scale)") +
      ggtitle("Growth in reported Covid-19 cases since Country first reported 100 cases")
      
  
#Aus State Plots
dfCovid2 %>% 
  filter(!(Region %in% c("Italy", "Japan", "Korea, South", "New Zealand", "US", "Spain","United Kingdom",
                         "Australian Capital Territory", "Tasmania", "Northern Territory"))) %>% 
  ggplot( aes(x=PlotDate, y=CumulativeCases, group=Region, color=Region)) +
  geom_line(aes(alpha = LineAlpha)) +
  geom_point(aes(alpha = LineAlpha)) +
  scale_alpha(guide = 'none', range = c(0.4, 1)) +
  scale_y_log10(labels = comma) +
  coord_cartesian(xlim = c(-30,0), ylim = c(10,10000)) +
  labs(x = "Relative Date (All Australia on 01Apr2020 = 0)", y = "Reported Cases (log scale") +
  ggtitle("Growth in reported Covid-19 cases since State first reported 100 cases")
    