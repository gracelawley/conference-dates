library(readr)
library(dplyr)
library(glue)
library(stringr)
library(gt)


conf_data <- read_delim("conferences.csv", delim = ";") %>%
  # collapse/format conference name
  mutate(name = glue("**{name_abrev}**: {name_full}")) %>%
  select(-contains("name_")) %>%

  # format urls
  mutate(conf_url = if_else(conf_url == "TBA", "*Website (TBA)*", paste0("[Website](", conf_url, ")"))) %>%
  mutate(call_url = if_else(call_url == "TBA", "*Call for Papers (TBA)*", paste0("[Call for Papers](", call_url, ")"))) %>%
  mutate(urls = paste0(conf_url, "  \n", call_url)) %>%

  # format submission deadline date
  mutate(deadline = deadline %>%
           str_replace(., "\\(", "  \n*") %>%
           str_replace(., "\\)", "*")) %>%

  # reorder cols
  select(deadline, name, conf_dates, location, urls)


conf_table <- conf_data %>%
  gt() %>%
  fmt_markdown(columns = TRUE) %>%
  cols_label(
    conf_dates = "Dates",
    urls = "Links",
    name = "Conference",
    deadline = "Deadline",
    location = "Location"
  ) %>%
  cols_align("left") %>%
  cols_width(
    vars(name) ~ px(300),
    vars(deadline) ~ px(150),
    vars(conf_dates) ~ px(150),
    everything() ~ px(200)
  ) %>%
  tab_header(
    title = "Upcoming Conferences, 2020-2021",
    subtitle = paste0("Computational Linguistics, Autism Spectrum Disorder, Natural Language Processing, Corpus Linguistics, Speech & Language Disorders")) %>%
  tab_source_note(paste0("Last Updated: ", Sys.Date())) %>%
  tab_options(heading.align = "left")


gtsave(conf_table, "table.html")
