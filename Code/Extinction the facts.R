#' Extinction the facts Wikipedia data

#' housekeeping
rm(list = ls())
graphics.off()

#' load the packages
#' might need to install R tools from the site for these
library(plyr)
library(tidyverse)
library(pageviews)
library(ggplot2)
# install.packages("devtools")
# devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

#Extinction - The Facts (EXT) air dates:
EXT_dates <- as.POSIXlt(c("2020-09-13"),
                        format = "%Y-%m-%d",
                        tz = "UTC")

EXT_dates_1 <- as.POSIXlt(c("2020-09-14"),
                          format = "%Y-%m-%d",
                          tz = "UTC")


####' GET DATA FOR MULTIPLE WIKIPEDIA ARTICLES ----
#' create a function to extract data for multiple articles
get_wiki <-
  function(x) {
    article_pageviews(
      project = "en.wikipedia",
      article = x      ,
      start = as.Date('2020-01-01'),
      end = as.Date("2020-12-31"),
      user_type = "user",
      platform = c("mobile-web") # only mobile access here
    )
  }


## EXTINCTION THE FACTS Themes ###
EXT <- c(
  "Extinction",
  "Climate_change",
  "Food_security",
  "Water_security",
  "Coronavirus_disease_2019",
  "Emerging_infectious_disease",
  "Epidemic",
  "Biodiversity_loss",
  "Habitat_destruction",
  "Overexploitation",
  "Land_development",
  "Soil_retrogression_and_degradation",
  "Nature_conservation",
  "Flood",
  "Wildlife_trade",
  "Poaching",
  "Overfishing",
  "Pollution",
  "Polychlorinated_biphenyl",
  "Heat_wave",
  "Roadkill",
  "Fur_farming",
  "Bushmeat",
  "Earth_Summit",
  "Sustainability",
  "Chlorofluorocarbon",
  "Pandemic",
  "Decline_in_insect_populations",
  "Paris_Agreement",
  "Deforestation",
  "Ozone",
  "Palm_oil",
  "Sustainable_fishery",
  "Ecosystem_service",
  "Extinction_risk_from_climate_change",
  "Beef",
  "Soybean",
  "Cocoa_bean",
  "Fishing_tackle"
)


#extract the article views based on the vector of names
output <- EXT %>% get_wiki
levels(as.factor(output$article))

#' plot to see if it worked (log10 scale), this will take a while if you download many articles
#' Notice that 'Extinction from Climate Change' 'Nature Conservation' and 'Ecosystem Service' seem to have
#' had a major jump half way through 2020 due to changes to their Wiki pages, 
#' might need to omit these along with the COVID stuff 
dplyr::filter(output, access == "mobile-web") %>%
  ggplot(., aes(x = date, y = log10(views))) + geom_point() +
  facet_wrap(~ article)

#' pull out one to plot
dplyr::filter(output, article == "Polychlorinated_biphenyl") %>%
  ggplot(., aes(x = date, y = views)) + geom_point()

#' export the data
write.csv(output, file = "Extinction_wiki_ts.csv", row.names = F)

#' extract the values on the air dates
#' need to make sure dates are in the same format
output$date <- as.POSIXlt(output$date,
                          format = "%Y-%m-%d",
                          tz = "UTC")


#' we use the air dates we defined earlier here
broadcast_views <- output[output$date %in% EXT_dates,]

broadcast_plus1_views <- output[output$date %in% EXT_dates_1,]

#' add the views from broadcast_plus1_views to the broadcast_views data using article names
broadcast_views$views_1 <-
  broadcast_plus1_views$views[match(broadcast_views$article, broadcast_plus1_views$article)]

broadcast_views$max <-
  if_else(
    broadcast_views$views > broadcast_views$views_1,
    broadcast_views$views,
    broadcast_views$views_1
  )

#' subset time series to extract median values
output$article <- as.factor(output$article)

output_subset <- output %>% group_by(article) %>%
  filter(date >= as.Date('2020-01-01') &
           date <= as.Date('2020-06-30'))

#' should be no data past 30th June 2020
tail(output_subset)

#' get the median value for views over this period
baseline_median <- output_subset %>% group_by(article) %>% summarise(median_views = median(views))
baseline_median %>% print(n=nrow(.))

#' add the median from baseline_median to the broadcast_views data using article names
broadcast_views$baseline <-
  baseline_median$median_views[match(broadcast_views$article, baseline_median$article)]

broadcast_views_summary <- broadcast_views %>% select(article, views, views_1, max, baseline)

#' create a function to determine the overlap of anomalies with certain dates
anom_func <-  function(x) {
  res = AnomalyDetectionTs((data.frame(x[6:7])),
                           # 6 and 7 are the date and views column
                           max_anoms = 0.1,
                           # play around with this value
                           direction = 'pos',
                           # plot = FALSE
  )
  # determine if the anomalies are between 2 dates
  # day of episode and day after
  anomalies <-
    ifelse(
      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
      == strptime(as.Date("2020-09-13"), format = "%Y-%m-%d") |
        strptime((res$anoms$timestamp), format = "%Y-%m-%d")
      == strptime(as.Date("2020-09-14"), format = "%Y-%m-%d"),
      1,
      0
    )
  # find the sum total of the ones that are
  sum(anomalies)
}

#' group the data by article and apply the function to each one
EXT_anom <- output %>%
  group_by(article) %>% nest() %>%
  mutate(count_anom = map(data, ~ anom_func(.)))

#' now pull out the number of anomalies by article
EXT_anom$count_anom
EXT_anom_df <- EXT_anom %>%
  unnest(cols = c(count_anom)) %>%
  dplyr::select(article, count_anom)
EXT_anom_df  %>% print(n=nrow(.))

#' add the anomalies from EXT_anom_df to the broadcast_views data using article names
broadcast_views_summary$anom <-
  EXT_anom_df$count_anom[match(broadcast_views_summary$article, EXT_anom_df$article)]

#' get the difference between the max and the baseline to check the episode effect
broadcast_views_summary$diff <- broadcast_views_summary$max - broadcast_views_summary$baseline

#' row numbers from earlier keep coming up so I delete them here 
rownames(broadcast_views_summary) <- NULL
broadcast_views_summary

#' reshape for plotting
plot_data <- broadcast_views_summary %>% 
  pivot_longer(
    cols = c(`baseline`, `diff`), 
    names_to = "type", 
    values_to = "hits"
  )

ggplot(plot_data, aes(x = type, y = hits, colour = type)) + geom_boxplot() + theme_minimal()
