#leaflet example
# Load Libraries ----------------------------------------------------------

library(here)
library(tidyverse)
library(leaflet)

# Read in raw Data --------------------------------------------------------

df <- read_csv(here("data", "2019", "2019-12-03", "tickets.csv"))

total_fines <- summarise(df, total = sum(fine)) %>%
  pull(total) %>%
  format(big.mark = ",")

df %>%
  mutate(y = floor_date(issue_datetime, unit = 'year'),
         yx = year(issue_datetime)) %>%
  distinct(y)

total_monthly_fines <- df %>%
  mutate(mon = floor_date(issue_datetime, unit = 'month')) %>%
  group_by(mon, violation_desc) %>%
  summarise(total = sum(fine)) %>%
  ungroup() %>%
  mutate(fines = total/1000000.0)
  #pull(total) %>%
  #format(big.mark = ",")

range(total_monthly_fines$total)
ggplot(total_monthly_fines) +
  geom_col(aes(mon, y = fines)) +
  scale_y_continuous(limits=c(0, max(total_monthly_fines$fines)))


ggplot(df) +
  geom_col(aes(x = floor_date(issue_datetime, unit = 'month'),
               y = fine,
               color = as.factor(violation_desc))) +
  scale_y_continuous(limits=c(0, max(total_monthly_fines$total)))


#by issuing agency
#by zipcode

total_fines

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



# m <- leaflet(df1) %>%
#   addTiles() %>%  # Add default OpenStreetMap map tiles
#   addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
# m
#19114
dfz <- df1 %>% filter(zip_code == 19114)
df2 <- df1 %>% filter(mon == 3, issuing_agency != 'POLICE',
                      zip_code != 19114,
                      violation_category == 'time violation') %>%
  mutate(msg = str_c("", violation_category, " ", shift, " Fine:", fine, " Agency ", issuing_agency, " ", issue_datetime,
                      " ", violation_desc, " ", zip_code),
         color = case_when(shift == 'day' ~ 'yellow',
                           shift == 'evening' ~ 'blue',
                           shift == 'overnight' ~ 'black'))

df2all <- df1 %>%
  mutate(msg = str_c("", violation_category, " ", shift, " Fine:", fine, " Agency ", issuing_agency, " ", issue_datetime,
                     " ", violation_desc, " ", zip_code),
         color = case_when(shift == 'day' ~ 'yellow',
                           shift == 'evening' ~ 'blue',
                           shift == 'overnight' ~ 'black'))
nrow(df2)
topzip <- df2 %>% count(zip_code, sort = TRUE) %>% top_n(10) %>% pull(zip_code)
df3 <- df2 %>% filter(zip_code %in% topzip)

df4 <- df3 %>%
  group_by(zip_code) %>%
  summarize(num_tickets = n(),
            total_fine = sum(fine))

df4a <- df2all %>%
  group_by(zip_code, violation_category) %>%
  summarize(num_tickets = n(),
            total_fine = sum(fine))

df4b <- df2all %>%
  group_by(violation_category) %>%
  summarize(num_tickets = n(),
            total_fine = sum(fine),
            total_str = format(total_fine, big.mark = ","))

df4c <- df2all %>%
  group_by(zip_code) %>%
  summarize(num_tickets = n(),
            total_fine = sum(fine))

df4d <- df2all %>%
  group_by(violation_desc) %>%
  summarize(num_tickets = n(),
            total_fine = sum(fine),
            total_str = format(total_fine, big.mark = ",")) %>%
  mutate(fine_in_millions = round(total_fine/1000000.0,2)) %>%
  arrange(desc(total_fine)) %>%
  slice(1:30)
  #op_n(50)
ggplot(df4d) +
  geom_col(aes(x = reorder(violation_desc, -fine_in_millions),
               y = fine_in_millions)) +
  scale_y_continuous(limits=c(0,11.0)) +
  coord_flip()

df4e <- df2all %>%
  group_by(violation_category) %>%
  summarize(num_tickets = n(),
            total_fine = sum(fine),
            total_str = format(total_fine, big.mark = ",")) %>%
  mutate(fine_in_millions = round(total_fine/1000000.0,2)) %>%
  arrange(desc(total_fine)) %>%
  slice(1:8)
#op_n(50)
ggplot(df4e) +
  geom_col(aes(x = reorder(violation_category, -fine_in_millions),
           y = fine_in_millions)) +
  #scale_y_continuous(limits=c(0,40.0)) +
  coord_flip()



df4f <- df2all %>%
  group_by(shift) %>%
  summarize(num_tickets = n(),
            total_fine = sum(fine),
            total_str = format(total_fine, big.mark = ",")) %>%
  mutate(fine_in_millions = round(total_fine/1000000.0,2)) %>%
  arrange(desc(total_fine)) %>%
  slice(1:8)


m <- leaflet(df2 %>% filter(zip_code ==19104)) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(popup=~msg, color = ~color)
m
