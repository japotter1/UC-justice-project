### Analyzing media mentions of "teen takeovers" in Chicago
# Jada Potter; 6/22/26

# Note: for this to work, you must copy the news pdfs from Box into the folder "../news_pdfs/"
# These are in .gitignore to save space


##### PART 1: setup


library(tidyverse)
library(ggplot2)

# First, we make a function that takes a file name input and extracts the date (in character form because sapply doesn't work with dates)
# Return NA if no valid date in the name
# remove_videos: return NA if "_VIDEO" is found immediately after the date
  # (set to true because we are currently excluding these, but can be changed later on)

extract_article_date = function(title, remove_videos = TRUE) {
  # Remove articles specified as videos if remove_videos is TRUE
  if (remove_videos & str_detect(title, "VIDEO_")) { return(NA) }
  # This regular expression extracts matches dates from the start of the string in yyyy-mm-dd format
  date = str_extract(title, "^\\d{4}-\\d{2}-\\d{2}")
  return(date)
}


##### PART 2: get a list of news sources, each one containing a vector of dates


news_sources = list()

# Loop through each news source in "../news_pdfs"
for (source in list.files("../news_pdfs")) {

  # For each news source...
  # Get all article names
  article_dates = list.files(paste0("../news_pdfs/", source)) %>%
    # Convert to date using the function written above
    sapply(extract_article_date, USE.NAMES = FALSE) %>%
    # Get rid of NA
    na.omit() %>%
    # Now we can convert to an actual date
    as.Date(format = "%Y-%m-%d")

  # Create new sub-list (named using the source name) that includes article dates
  news_sources[[source]] = article_dates
  
  # Clean up: remove temp variables
  rm(source, article_dates)
}

# Create combined vectors: all sources, all Dan Proft sources, and all network sources
news_sources[["ALL_SOURCES"]] = Reduce(c, news_sources)

news_sources[["ALL_DAN_PROFT"]] = c(news_sources[["chambana_sun"]],
                                    news_sources[["chicago_city_wire"]],
                                    news_sources[["dupage_policy_journal"]],
                                    news_sources[["north_cook_news"]],
                                    news_sources[["prairie_state_wire"]],
                                    news_sources[["se_illinois_news"]],
                                    news_sources[["south_cook_news"]])
news_sources[["ALL_NETWORK"]]   = c(news_sources[["abc7"]],
                                    news_sources[["cbs_chicago"]],
                                    news_sources[["fox32"]],
                                    news_sources[["nbc_chicago"]],
                                    news_sources[["wgn_radio"]],
                                    news_sources[["wgn_tv"]])


##### PART 3: descriptive stats


# Sum of squares function (unless this isn't needed)
ss = function(x) {
  x = as.numeric(x)
  return(sum((x - mean(x))^2))
}

# Create table of summary stats using summary function
summary_stats = news_sources %>%
  lapply(summary) %>%
  as.data.frame() %>%
  t() %>% as.data.frame() %>%  # transpose and then convert back to data frame because it makes the data frame backwards for some reason
  rownames_to_column("source")

# Add standard deviations and sums of squares
standard_devs = news_sources %>%
  lapply(sd) %>%
  as.data.frame(row.names = c("SD")) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column("source")

summary_stats = summary_stats %>% left_join(standard_devs, by = "source")

sums_of_squares = news_sources %>%
  lapply(ss) %>%
  as.data.frame(row.names = c("SS")) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column("source")

summary_stats = summary_stats %>% left_join(sums_of_squares, by = "source")

# Export stats
write.csv(summary_stats, "../output/summary_stats.csv")


##### PART 4: histograms

# List of years for labeling (because R doesn't know how to do this for some reason)
year_labels = data.frame(
  numeric = c(as.numeric(as.Date("2022-01-01")),
              as.numeric(as.Date("2023-01-01")),
              as.numeric(as.Date("2024-01-01")),
              as.numeric(as.Date("2025-01-01")),
              as.numeric(as.Date("2026-01-01")),
              as.numeric(as.Date("2027-01-01"))),
  year = c(2022, 2023, 2024, 2025, 2026, 2027)
)

# Colors for quarters are from colorbrewer2.org
hist(news_sources$ALL_SOURCES, breaks = "quarters", freq = T, main = "Histogram of ALL_SOURCES", xlab = "Year", xaxt='n', col = c('#8dd3c7', '#ffffb3', '#bebada', '#fb8072'))
axis(1, at = year_labels$numeric, labels = year_labels$year)
legend(x = 19144, y = 100, legend=c('Q1', 'Q2', 'Q3', 'Q4'), 
       fill = c('#bebada', '#fb8072', '#8dd3c7', '#ffffb3')
)


