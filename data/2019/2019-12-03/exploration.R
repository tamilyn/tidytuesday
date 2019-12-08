
# Load Libraries ----------------------------------------------------------

library(here)
library(tidyverse)


# Read in raw Data --------------------------------------------------------

df <- read_csv(here("data", "2019", "2019-12-03", "tickets.csv"))


# meter expired/meter expired cc/over time limit
# parking or stop prohibited/ loading zone

# Brier score
library(lubridate)
df1 <- df %>%
  mutate(violation_category = case_when(
    str_detect(violation_desc,'METER|OVER TIME') ~ 'time violation',
    #str_detect(violation_desc, 'PROHBITED|PROHIBITED|LOADING ZONE|LOADNG ZONE|ZONE') ~ 'prohibited',
    TRUE ~ 'other'),
    mon = month(issue_datetime),
    yr = year(issue_datetime),
    issuing_agency = as.factor(issuing_agency),
    hr = hour(issue_datetime),
    shift = case_when((hr %in% c(22,23,0,1,2,3,4,5)) ~ 'overnight',
                      (hr %in% 5:16) ~ 'day',
                      (hr %in% 17:22) ~  'evening'),
    mdate = lubridate::ymd(str_c(yr,"-",mon,"-01")))

count(df1, shift, hr) %>% View("Shifts")

library(SmartEDA)

df1a <- df1 %>% filter(violation_category == 'time violation')
dfaa <- df1a %>% select(violation_category, issue_datetime, mdate, shift)
df1b <- df1 %>% filter(violation_category != 'time violation')

plot2 <- ExpCatViz(df1,target=NULL,col ="slateblue4",clim=10,margin=2,Page = c(2,1),sample=4)
plot2[[1]]


plot2 <- ExpCatViz(df1a,target=NULL,col ="slateblue4",clim=10,margin=2,Page = c(2,1),sample=4)
plot2[[1]]


df2 <- df1 %>%
  filter(violation_category=='other') %>% count(violation_desc, sort = TRUE)

df3 <- df1 %>%
  filter(violation_category == 'time violation')

df4 <- df3 %>% count(mdate, mon)

df5 <- df1 %>% count(mdate,violation_category)

ggplot(df5) +
  geom_col(aes(x=mdate,y=n,fill=as.factor(violation_category)),
           position = "dodge") +
  scale_x_date(date_labels = "%b")

# by violation type
# by time of day - overnight - 10pm-6am, day 6am-6pm, even 6p-10pm
# by issuing officer - police or ppa

library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=df1$lon, lat=df1$lat, popup="The birthplace of R")
m
