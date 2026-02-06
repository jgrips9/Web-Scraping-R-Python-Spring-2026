#Organized
install.packages("tidyverse")
install.packages("rvest")
install.packages("libridate")
install.packages("here")
install.packages("data.table")

library(tidyverse)
library(rvest)
library(lubridate)
library(here)
library(data.table)

chart = "hot-100"
edition_request <- "1992-12-05"

# make the url
scrape_url <- paste(
  "https://www.billboard.com/charts/",
  chart,
  "/",
  edition_request,
  sep = ""
)

# get initial scrape
scrape_first <- read_html(scrape_url)


# pull date from scrape
chart_date <- scrape_first |> 
  html_element("div#chart-date-picker") |> 
  html_attr("data-date")

# pull year from scrape
chart_year <- year(chart_date)


scrape_tibble <- scrape_first |> 
  html_elements("ul.o-chart-results-list-row") |> 
  html_text2() |> 
  as_tibble()

# clean up the tibble
scrape_clean <- scrape_tibble |> 
  mutate(
    data_cleaned = str_remove_all(value, " NEW\n|NEW"),
    data_cleaned = str_remove_all(data_cleaned, " RE- ENTRY\n|RE- ENTRY")
  ) |> 
  select(data_cleaned, value) |> 
  separate(
    col = data_cleaned,
    sep = "\n",
    into = c(
      "current_week",
      "title",
      "performer",
      "last_week",
      "peak_pos",
      "wks_on_chart"
    )
  ) |> 
  select(-value) |> 
  mutate(chart_week = chart_date) |> 
  select(chart_week, everything())


#Now do so for an entire year
years = seq(2025, 2025)
months = c("01", "02", "03")
days = c("01", "06", "13", "20", "27")

df <- data.frame(matrix(ncol = 7, nrow = 0))

#Some loops.
for(year in years){
  for(month in months){
    for(day in days){
      date = paste(year, "-", month, "-", day, sep = "")
      chart = "hot-100"
      scrape_url <- paste(
        "https://www.billboard.com/charts/",
        chart,
        "/",
        date,
        sep = ""
      )
      
      tryCatch({
        # get initial scrape
        scrape_first <- read_html(scrape_url)
        
        # pull date from scrape
        chart_date <- scrape_first |> 
          html_element("div#chart-date-picker") |> 
          html_attr("data-date")
        
        # pull year from scrape
        chart_year <- year(chart_date)
        
        # process results into a tibble
        scrape_tibble <- scrape_first |> 
          html_elements("ul.o-chart-results-list-row") |> 
          html_text2() |> 
          as_tibble()
        
        # clean up the tibble
        scrape_clean <- scrape_tibble |> 
          mutate(
            data_cleaned = str_remove_all(value, " NEW\n|NEW"),
            data_cleaned = str_remove_all(data_cleaned, " RE- ENTRY\n|RE- ENTRY")
          ) |> 
          select(data_cleaned, value) |> 
          separate(
            col = data_cleaned,
            sep = "\n",
            into = c(
              "current_week",
              "title",
              "performer",
              "last_week",
              "peak_pos",
              "wks_on_chart"
            )
          ) |> 
          select(-value) |> 
          mutate(chart_week = chart_date) |> 
          select(chart_week, everything())
        
        #Append dataset.
        df = rbind(df, scrape_clean)
        df = unique(df)
      }, error=function(e) {
        message('An Error Occurred')})
      
    }
  }
}

write.csv(df, file = "hot100_2025_Jan-Mar.csv")