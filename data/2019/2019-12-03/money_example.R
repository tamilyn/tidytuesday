
library(here)
library(tidyverse)
library(lubridate)
library(patchwork)
library(treemapify)

# Read in raw Data --------------------------------------------------------

#df <- read_csv(here("data", "2019", "2019-12-03", "tickets.csv"))
#df <- read_csv(here("data/2019/2019-12-03/tickets.csv"))
df <- read_csv("data/2019/2019-12-03/tickets.csv")

# add categories
df1 <- df %>%
  mutate(violation_category = case_when(
    str_detect(violation_desc,'METER|OVER TIME') ~ 'EXPIRED TIME',
    str_detect(violation_desc, 'PROHBITED|PROHIBITED') ~ 'PROHIBITED',
    str_detect(violation_desc, 'ZONE') ~ 'ZONE',
    str_detect(violation_desc, 'INSPECTION') ~ 'INSPECTION',
    str_detect(violation_desc, 'RESERVED') ~ 'RESERVED',
    str_detect(violation_desc, 'FIRE HYDRANT') ~ 'FIRE HYDRANT',
    TRUE ~ 'OTHER'),
    violation_category = as.factor(violation_category),
    mon = month(issue_datetime),
    yr = year(issue_datetime),
    issuing_agency = case_when(issuing_agency == 'POLICE' ~ 'POLICE',
                               issuing_agency == 'PPA' ~ 'PPA',
                               TRUE ~ 'OTHER'
                                 ),
    issuing_agency = as.factor(issuing_agency),
    hr = hour(issue_datetime),
    shift = case_when((hr %in% c(22,23,0,1,2,3,4,5)) ~ 'OVERNIGHT',
                      (hr %in% 5:16) ~ 'DAY',
                      (hr %in% 17:22) ~  'EVENING'),
    mdate = lubridate::floor_date(issue_datetime, unit='month'))

total_fines <- summarise(df1, total = sum(fine)) %>%
  pull(total) %>%
  format(big.mark = ",")

total_fines

df_cat <- df1 %>%
  group_by(violation_category) %>%
  summarise(total = sum(fine), num_tickets = n()) %>%
  ungroup() %>%
  mutate(fines = round(total/1000000.0,0)) %>%
  mutate(catlab = str_c(violation_category,
                        "\n$",
  format(fines,big.mark = ","), " Million"))

df_agency <- df1 %>%
  group_by(issuing_agency) %>%
  summarise(total = sum(fine), num_tickets = n()) %>%
  ungroup() %>%
  mutate(fines = round(total/1000000.0,0)) %>%
  mutate(catlab = str_c(issuing_agency,
                        "\n$",
                        format(fines,big.mark = ","), " Million"))


# plot
plot_tm <- ggplot(df_cat, aes(area = fines, fill = catlab,
                   label = catlab)) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE) +
  ggtitle("By Ticket Type")

# monthly by violation category
# monthly by issuing_agency

plot_agency <- ggplot(df_agency, aes(area = fines, fill = catlab,
                              label = catlab)) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE) +
  ggtitle("By Issuing Agency")

###
c1 <- "https://www.opendataphilly.org/dataset/parking-violations"
caption <-
  str_c(c1,"\n",
        "From https://github.com/tamilyn/tidytuesday/tree/master/data/2019/2019-12-03")
plot_tm / plot_agency +
  plot_annotation(
    theme = theme(plot.title =
                    element_text(family="Palatino", face="bold", size=20),
                  plot.subtitle =
                    element_text(family="Palatino", face="bold", size=14)),
    title = "Philadelphia Parking Fines 2017",
    subtitle = str_c("Total fines: $", total_fines),
    caption = caption)
