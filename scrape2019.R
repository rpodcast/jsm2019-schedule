library(tidyverse)
library(RSelenium)

jsm_link <- "https://ww2.amstat.org/meetings/jsm/2019/onlineprogram/index.cfm"


remDr <- remoteDriver(port = 4445L)
remDr$open()

remDr$navigate(jsm_link)
remDr$getTitle()

remDr$getCurrentUrl()

# find the search button
webElem <- remDr$findElement(using = "name", value = "btnSubmit")

# click the search button
webElem$clickElement()

# save the page source
pageSource <- remDr$getPageSource()[[1]]
cat(pageSource, file = "data/jsm2019.html")
# close session

remDr$close()

# close docker container
# docker stop $(docker ps -q)

# read data ---------------------------------------------------------
library(rvest)
library(glue)
jsm2019_html <- read_html("data/jsm2019.html")

# scrape session schedule -------------------------------------------

dates_times <- jsm2019_html %>%
  html_nodes("td tr:nth-child(1) td:nth-child(2) b") %>%
  html_text() %>%
  trim() %>%
  str_replace_all("\n", " ") %>%
  str_trim()

locations <- jsm2019_html %>%
  html_nodes("td~ td+ td b") %>%
  html_text()

sessions_types <- jsm2019_html %>%
  html_nodes("tr:nth-child(2) b") %>%
  html_text()

ids <- jsm2019_html %>%
  html_nodes("#bottom br+ a") %>%
  html_text() %>%
  str_remove("!") %>%
  str_remove("!$") %>%
  str_remove("\\*") %>%
  str_trim()

slugs <- jsm2019_html %>%
  html_nodes("#bottom br+ a") %>%
  html_attr("href")

sponsors <- jsm2019_html %>%
  html_nodes("tr:nth-child(3) td") %>%
  html_text() %>%
  str_trim() %>%
  str_replace_all("  ,|   ,| ,", ",")

jsm_sessions_raw <- tibble(
  date_time = dates_times,
  location = locations,
  id = ids,
  session_type = sessions_types,
  slug = slugs,
  sponsor = sponsors
)

jsm_sessions <- jsm_sessions_raw %>%
  # separate columns
  separate(date_time, into = c("day", "date", "time"), sep = ", ") %>%
  separate(time, into = c("beg_time", "end_time"), sep = " - ", remove = FALSE) %>%
  separate(session_type, into = c("session", "type"), sep = " \u2014 ") %>%
  # manual fixes
  mutate(
    # has fee
    has_fee = str_detect(session, "(ADDED FEE)"),
    # data error
    end_time = ifelse(id == "581", "3:50 PM", end_time),
    # compose URLs
    url = ifelse(
      type == "Professional Development Continuing Education Course", slug,
      glue("http://ww2.amstat.org/meetings/jsm/2019/onlineprogram/{slug}")
    ),
    # reduce type levels
    type = case_when(
      str_detect(type, "Roundtable")               ~ "Roundtable",
      str_detect(type, "Professional Development") ~ "Professional Development",
      str_detect(type, "Other")                    ~ "Other",
      TRUE                                         ~ type
    ),
    # civilian to military time
    beg_time_round = format(strptime(beg_time, "%I:%M %p"), format = "%H:%M:%S") %>% str_remove("\\:.+") %>% as.numeric(),
    end_time_round = format(strptime(end_time, "%I:%M %p"), format = "%H:%M:%S") %>% str_remove("\\:.+") %>% as.numeric(),
    end_time_round = ifelse(str_detect(end_time, "\\:[1-5]"), end_time_round+1, end_time_round),
    # for convenience
    end_time_round = ifelse(id == "216596", 23, end_time_round)
  ) %>%
  select(-slug)

write_csv(jsm_sessions, path = "data/jsm2019_sessions.csv")

# scrape talk info -------------------------------------------------

titles <- jsm2019_html %>%
  html_nodes("tr+ tr td+ td a") %>%
  html_text()

urls <- jsm2019_html %>%
  html_nodes("tr+ tr td+ td a") %>%
  html_attr("href")

jsm_talks_raw <- tibble(
  title = titles,
  url = glue("http://ww2.amstat.org/meetings/jsm/2019/onlineprogram/{urls}")
)

jsm_talks <- jsm_talks_raw %>%
  mutate(has_fee = str_detect(title, "(ADDED FEE)"))

write_csv(jsm_talks, "data/jsm2019_talks.csv")

