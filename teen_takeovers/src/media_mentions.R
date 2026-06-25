### Analyzing media mentions of "teen takeovers" in Chicago
# Jada Potter; 6/23/26

# Note: for this to work, you must copy the news pdfs from Box into the folder "../news_pdfs/"
# These are in .gitignore to save space


##### PART 1: setup


library(tidyverse)
library(ggplot2)
library(zoo)

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


# Create table of summary stats using summary function
summary_stats = news_sources %>%
  lapply(summary) %>%
  as.data.frame() %>%
  t() %>% as.data.frame() %>%  ## transpose and then convert back to data frame because it makes the data frame backwards for some reason
  rownames_to_column("source")

# Add standard deviations
standard_devs = news_sources %>%
  lapply(sd) %>%
  as.data.frame(row.names = c("SD")) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column("source")

# Join them together and specify that these are summaries of dates (so can be merged with other summary tables later in Part 6)
summary_stats = summary_stats %>%
  left_join(standard_devs, by = "source") %>%
  rename_with(~paste0(., "_date"), c(Min.:SD))

rm(standard_devs)  ## Cleanup!


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

## Can make more histograms here for different sources, but moving on for now


##### PART 5: make tables by month and quarter - sorry this is a little messy!


# Convert raw vectors of dates to tables showing number of mentions each month
news_sources_bymonth = news_sources %>%
  lapply(as.yearmon) %>%
  lapply(table) %>%
  lapply(as.data.frame, stringsAsFactors = F) %>%
  lapply(rename, month = Var1) %>%
  lapply(mutate, month = as.yearmon(month))  # Have to convert back to yearmon because as.data.frame makes it a character for some reason

# Same as above, but for quarters
news_sources_byqtr = news_sources %>%
  lapply(as.yearqtr) %>%
  lapply(table) %>%
  lapply(as.data.frame, stringsAsFactors = F) %>%
  lapply(rename, qtr = Var1) %>%
  lapply(mutate, qtr = as.yearqtr(qtr))  # Have to convert back to yearqtr because as.data.frame makes it a character for some reason

# Change name of Freq columns to just be the name of the news source so we can merge them later
# (doing this in a for loop cause I can't figure it out with lapply)
# Yes, this is inefficient, if you want to make it better feel free to
for (i in seq_along(news_sources_bymonth)) {
  cname = names(news_sources_bymonth)[i]
  news_sources_bymonth[[i]] = news_sources_bymonth[[i]] %>% rename(!!cname := Freq)  ## Idk why need to use !! and := it just works, had to google it, dplyr is weird
  news_sources_byqtr[[i]] = news_sources_byqtr[[i]] %>% rename(!!cname := Freq)
  rm(i, cname)
}

# Reduce down to single data frames for months and for quarters
news_sources_bymonth_df = Reduce(full_join, news_sources_bymonth)
news_sources_byqtr_df = Reduce(full_join, news_sources_byqtr)

# Get min and max months/quarters in order to make full tables with all possible dates
start_month = min(news_sources_bymonth_df$month)
end_month = max(news_sources_bymonth_df$month)
start_qtr = min(news_sources_byqtr_df$qtr)
end_qtr = max(news_sources_byqtr_df$qtr)

# Make full table of all possible months and merge in "news_sources_bymonth_df"
mentions_by_month = seq.Date(from = start_month, to = end_month, by = "month") %>%
  as.yearmon() %>%
  data.frame(month = .) %>%
  left_join(news_sources_bymonth_df)

# Same as above, but for quarters
mentions_by_qtr = seq.Date(from = start_qtr, to = end_qtr, by = "quarter") %>%
  as.yearqtr() %>%
  data.frame(qtr = .) %>%
  left_join(news_sources_byqtr_df)


##### PART 6: more descriptive stats! - month and quarter tables


# By month
summary_bymonth = mentions_by_month %>%
  select(-month) %>%
  sapply(summary) %>%
  as.data.frame() %>%
  t() %>% as.data.frame() %>%
  rownames_to_column("source")

summary_bymonth_sd = mentions_by_month %>%
  select(-month) %>%
  sapply(sd, na.rm = T) %>%
  as.data.frame() %>%
  rename("SD" = ".") %>%
  rownames_to_column("source")

summary_bymonth = summary_bymonth %>%
  left_join(summary_bymonth_sd, by = "source") %>%
  rename_with(~paste0(., "_permonth"), c(Min.:SD))

rm(summary_bymonth_sd)  ## Cleanup!

# By quarter
summary_byqtr = mentions_by_qtr %>%
  select(-qtr) %>%
  sapply(summary) %>%
  as.data.frame() %>%
  t() %>% as.data.frame() %>%
  rownames_to_column("source")

summary_byqtr_sd = mentions_by_qtr %>%
  select(-qtr) %>%
  sapply(sd, na.rm = T) %>%
  as.data.frame() %>%
  rename("SD" = ".") %>%
  rownames_to_column("source")

summary_byqtr = summary_byqtr %>%
  left_join(summary_byqtr_sd, by = "source") %>%
  rename_with(~paste0(., "_perqtr"), c(Min.:SD))

rm(summary_byqtr_sd)  ## Cleanup!

# News source counts
summary_cts = mentions_by_qtr %>%
  select(-qtr) %>%
  summarize_all(.funs = sum, na.rm = T) %>%
  t() %>% as.data.frame() %>%
  rename("total_articles" = "V1") %>%
  rownames_to_column("source")

# Summary stats by source totals
summary_sourcetotals = summary_cts %>%
  filter(!grepl("ALL_", source)) %>%
  summary()

# View summary stats of total articles per source
summary_sourcetotals

# Combine and export stats
summary_ALL = left_join(summary_cts, summary_stats, by = "source") %>%
  left_join(summary_bymonth, by = "source") %>%
  left_join(summary_byqtr, by = "source")

write.csv(summary_ALL, "../output/summary_stats.csv")


##### PART 7: line graphs (much more doable now that we have counts per month and per quarter)


# Because ggplot2 is weird, we have to reshape the data into "long" format
long_mentions_by_month = mentions_by_month %>%
  pivot_longer(names_to = "source", values_to = "n_mentions", cols = c("abc7":"ALL_NETWORK")) %>%
  mutate_all(~replace(., is.na(.), 0))  ## Changing NA to 0 because it makes more sense in line graphs

long_mentions_by_qtr = mentions_by_qtr %>%
  pivot_longer(names_to = "source", values_to = "n_mentions", cols = c("abc7":"ALL_NETWORK")) %>%
  mutate_all(~replace(., is.na(.), 0))  ## Changing NA to 0 because it makes more sense in line graphs

# Create Date class column (works better with ggplot2)
long_mentions_by_month = long_mentions_by_month %>% mutate(d_month = as.Date(month))
long_mentions_by_qtr = long_mentions_by_qtr %>% mutate(d_qtr = as.Date(qtr))

# Vectors of network names to select in different graphs
vec_network = c("abc7", "cbs_chicago", "fox32", "nbc_chicago", "wgn_radio", "wgn_tv", "ALL_NETWORK")
vec_dan_proft = c("chambana_sun", "chicago_city_wire", "dupage_policy_journal", "north_cook_news", "prairie_state_wire", "se_illinois_news", "south_cook_news", "ALL_DAN_PROFT")
vec_all_sources = c("ALL_NETWORK", "ALL_DAN_PROFT", "ALL_SOURCES")

# Vectors of months and quarters for labeling
vec_months = c(
  "Jan '22" = as.Date("2022-01-01"),
  "Apr '22" = as.Date("2022-04-01"),
  "Jul '22" = as.Date("2022-07-01"),
  "Oct '22" = as.Date("2022-10-01"),
  "Jan '23" = as.Date("2023-01-01"),
  "Apr '23" = as.Date("2023-04-01"),
  "Jul '23" = as.Date("2023-07-01"),
  "Oct '23" = as.Date("2023-10-01"),
  "Jan '24" = as.Date("2024-01-01"),
  "Apr '24" = as.Date("2024-04-01"),
  "Jul '24" = as.Date("2024-07-01"),
  "Oct '24" = as.Date("2024-10-01"),
  "Jan '25" = as.Date("2025-01-01"),
  "Apr '25" = as.Date("2025-04-01"),
  "Jul '25" = as.Date("2025-07-01"),
  "Oct '25" = as.Date("2025-10-01"),
  "Jan '26" = as.Date("2026-01-01"),
  "Apr '26" = as.Date("2026-04-01"),
  "Jul '26" = as.Date("2026-07-01"),
  "Oct '26" = as.Date("2026-10-01")
  )

vec_qtrs = c(
  "Q1 '22" = as.Date("2022-01-01"),
  "Q2 '22" = as.Date("2022-04-01"),
  "Q3 '22" = as.Date("2022-07-01"),
  "Q4 '22" = as.Date("2022-10-01"),
  "Q1 '23" = as.Date("2023-01-01"),
  "Q2 '23" = as.Date("2023-04-01"),
  "Q3 '23" = as.Date("2023-07-01"),
  "Q4 '23" = as.Date("2023-10-01"),
  "Q1 '24" = as.Date("2024-01-01"),
  "Q2 '24" = as.Date("2024-04-01"),
  "Q3 '24" = as.Date("2024-07-01"),
  "Q4 '24" = as.Date("2024-10-01"),
  "Q1 '25" = as.Date("2025-01-01"),
  "Q2 '25" = as.Date("2025-04-01"),
  "Q3 '25" = as.Date("2025-07-01"),
  "Q4 '25" = as.Date("2025-10-01"),
  "Q1 '26" = as.Date("2026-01-01"),
  "Q2 '26" = as.Date("2026-04-01"),
  "Q3 '26" = as.Date("2026-07-01"),
  "Q4 '26" = as.Date("2026-10-01")
)

## Graphs:

# Figure 1a: Network source mentions by month
long_mentions_by_month %>%
  filter(source %in% vec_network) %>%
  ggplot(aes(x = d_month, y = n_mentions, color = source)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = vec_months) +
  labs(
    title = "Mentions of 'teen takeovers' per month by network source",
    x = "Month",
    y = "Number of mentions",
    color = "Source"
  )

ggsave("../output/fig1a_network_by_month.png", scale = 2.5)

# Figure 1b: Network source mentions by quarter
long_mentions_by_qtr %>%
  filter(source %in% vec_network) %>%
  ggplot(aes(x = d_qtr, y = n_mentions, color = source)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = vec_months) +
  labs(
    title = "Mentions of 'teen takeovers' per quarter by network source",
    x = "Quarter",
    y = "Number of mentions",
    color = "Source"
  )

ggsave("../output/fig1b_network_by_qtr.png", scale = 2.5)

# Figure 2a: Dan Proft source mentions by month
long_mentions_by_month %>%
  filter(source %in% vec_dan_proft) %>%
  ggplot(aes(x = d_month, y = n_mentions, color = source)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = vec_months) +
  labs(
    title = "Mentions of 'teen takeovers' per month by Dan-Proft funded source",
    x = "Month",
    y = "Number of mentions",
    color = "Source"
  )

ggsave("../output/fig2a_danproft_by_month.png", scale = 2.5)

# Figure 2b: Dan Proft source mentions by quarter
long_mentions_by_qtr %>%
  filter(source %in% vec_dan_proft) %>%
  ggplot(aes(x = d_qtr, y = n_mentions, color = source)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = vec_qtrs) +
  labs(
    title = "Mentions of 'teen takeovers' per quarter by Dan-Proft funded source",
    x = "Quarter",
    y = "Number of mentions",
    color = "Source"
  )

ggsave("../output/fig2b_danproft_by_qtr.png", scale = 2.5)

# Figure 3a: Groups of sources mentions by month
long_mentions_by_month %>%
  filter(source %in% vec_all_sources) %>%
  ggplot(aes(x = d_month, y = n_mentions, color = source)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = vec_months) +
  labs(
    title = "Mentions of 'teen takeovers' per month by source type",
    x = "Month",
    y = "Number of mentions",
    color = "Source"
  )

ggsave("../output/fig3a_sourcetype_by_month.png", scale = 2.5)

# Figure 3b: Groups of sources mentions by quarter
long_mentions_by_qtr %>%
  filter(source %in% vec_all_sources) %>%
  ggplot(aes(x = d_qtr, y = n_mentions, color = source)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = vec_qtrs) +
  labs(
    title = "Mentions of 'teen takeovers' per quarter by source type",
    x = "Quarter",
    y = "Number of mentions",
    color = "Source"
  )

ggsave("../output/fig3b_sourcetype_by_qtr.png", scale = 2.5)

