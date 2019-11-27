library(tidyverse)
library(ggchicklet)
library(hrbrthemes)

loans <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")

#cleanup names
dd_all <- loans %>%
  mutate(agency_name = case_when(str_detect(agency_name,"Account Control Technology|ACT") ~ "ACT",
                                 str_detect(agency_name,"FMS|FMS Investment Corp") ~ "FMS",
                                 str_detect(agency_name,"GC Services|GC Services LP") ~ "GC Services",
                                 str_detect(agency_name,"^Pioneer") ~ "Pioneer",
                                 str_detect(agency_name,"^Windham") ~ "Windham",
                                 str_detect(agency_name,"Immediate Credit Recovery|Immediate Credit Recovry Inc") ~ "Immediate Credit",
                                TRUE ~ str_replace_all(agency_name, "\\*|,|\\.", ""))) %>%
  arrange(agency_name) %>%
  mutate(agency_name = as.factor(agency_name))

dd_l = tidyr::pivot_longer(dd_all, consolidation:wage_garnishments) %>%
  mutate(name_p = case_when(name == 'voluntary_payments' ~ "Voluntary Payments",
         TRUE ~ name))

d8 <- dd_l %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  group_by(agency_name, name) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(agency_name = fct_reorder(agency_name, value, sum, .desc=FALSE),
         millions = value/1000000) %>%
  arrange(millions)


top_agencies <- d8 %>%
  group_by(agency_name) %>%
  summarize(total_mills = sum(millions)) %>%
  ungroup() %>%
  arrange(total_mills) %>%
  top_n(10) %>%
  pull(agency_name)

d9 <- d8 %>% filter(agency_name %in% top_agencies)

max9 = max(d9$millions)
title = "Student Loan Debt Repayments\nTop 10 Agencies"
subtitle = "In most cases, wage garnishments are larger \nthan voluntary payments, \nwhich are dwarfed by rehabilitation and \nconsolidation payments."
caption = "Data from https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-11-26."
ggplot(d9,aes(x=agency_name, y=millions,fill=name) ) +
  geom_chicklet(width = 0.75) +
  scale_y_continuous(limits= c(0, max9*1.3)) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "consolidation" = "#dcdcdc",
      "rehabilitation" = "#d8cb98",
      "voluntary_payments" = "#a4ad6f",
      "wage_garnishments" = "#ae4544"
    ) #,
  #  breaks = setdiff(unique(d2$name), "Other")
  ) +
  guides(
    fill = guide_legend(nrow = 1)
  ) +
  coord_flip() +
  labs(
    x = NULL, y = NULL, fill = NULL,
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  theme(axis.text.x = element_text(color = "gray60", size = 10)) +
  theme(legend.position = "top")
