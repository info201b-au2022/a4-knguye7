library(tidyverse)
library(stringr)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

summary_info <- list()

# The total number of observations in each county from the years 1970-2018
summary_info$num_observations <- nrow(trends)
#print(summary_info$num_observations)

# State with the highest number of incarcerations
summary_info$state_highest <- trends %>%
  select(state, total_jail_pop, year) %>%
  filter(year == max(year)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  pull(state)

#print(summary_info$state_highest)

# Highest number of black individuals incarcerated in a county in 2018
summary_info$highest_black_jail <- trends %>%
  select(black_jail_pop, year) %>%
  filter(year == max(year)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  pull(black_jail_pop)

#print(summary_info$highest_black_jail)

# Highest number of white individuals incarcerated in a county in 2018
summary_info$highest_white_jail <- trends %>%
  select(white_jail_pop, year) %>%
  filter(year == max(year)) %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = T)) %>%
  pull(white_jail_pop)
#print(summary_info$highest_white_jail)

#----------------------------------------------------------------------------#

## Section 3  ---- 
library(dplyr)
library(ggplot2)
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function returns a data frame that is suitable for visualization.
get_year_jail_pop <- function() {
  df_prison_growth <- trends %>%
    select(year, total_jail_pop) %>%
    drop_na() %>%
  return(df_prison_growth)
}

# This function returns the chart
plot_jail_pop_for_us <- function() {
  ggplot_prison_growth <-
    ggplot(data = get_year_jail_pop(), aes(x = year, y = total_jail_pop)) + 
    geom_bar(stat = "identity") +
    xlab ("Year") + 
    ylab("Total Jail Population") +
    ggtitle("Figure 1. Growth of the U.S. Prison Population from 1970-2018")
  return(ggplot_prison_growth)
}
plot(plot_jail_pop_for_us())

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

get_year_jail_pop_by_states <- function(states) {
  df_by_states <- trends %>%
    select(year, total_jail_pop, state) %>%
    drop_na() %>%
    filter(str_detect(state, c("WA", "OR", "FL", "CA", "NY")))
  return(df_by_states)
}


plot_jail_pop_by_states <- function(states) {
  line_graph_growth <-
    ggplot(data = get_year_jail_pop_by_states(), aes(x = year, y = total_jail_pop, color = state)) +
      geom_line() +
      xlab("Year") +
      ylab("Total Jail Population") +
      ggtitle("Figure 2. Growth of Prison Population by State")
  return(line_graph_growth)
}
plot(plot_jail_pop_by_states())


# plot_jail_pop_by_states <- function(states) {
#   line_graph_growth <-
#     ggplot(get_year_jail_pop_by_states(), aes(x = state, y = total_jail_pop, color = state)) +
#     geom_line()
#   return(line_graph_growth)
# }
# 
# plot(plot_jail_pop_by_states())
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# The highest number of white individuals incarcerated in a county in the state of California in 2018
summary_info$highest_white_ca <- trends %>%
  select(white_jail_pop, year, state) %>%
  drop_na() %>%
  filter(year == max(year)) %>%
  filter(str_detect(state, "CA")) %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = T)) %>%
  pull(white_jail_pop)

# print(summary_info$highest_white_ca)

# The highest number of black individuals incarcerated in a county in the state of California in 2018
summary_info$highest_black_ca <- trends %>%
  select(black_jail_pop, year, state) %>%
  drop_na() %>%
  filter(year == max(year)) %>%
  filter(str_detect(state, "CA")) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  pull(black_jail_pop)

# print(summary_info$highest_black_ca)

# Pie chart of highest number of white and black incarcerations in a county in CA (2018)
  done_pie_chart <-
    slices <- c(2579, 5024)
    lbls <- c("White", "Black")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels
    lbls <- paste(lbls,"%",sep="") # ad % to labels
    pie(slices,labels = lbls, col=rainbow(length(lbls)),
        main="Figure 3. County with Highest Black and White 
      Incarcerations in California (2018)")

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Step 1: 
library(tidyverse)
#install.packages("maps")
library(maps)
#install.packages("mapproj")
library(mapproj)
#install.packages("patchwork")
library(patchwork)

trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


# trends$date <- as.Date(trends$date)

# Step 2:
trends_mod <- trends %>%
  filter(year == max(year))

# Step 3:

trends_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by="polyname")
  
# Step 4:
map_data <- trends_shape %>%
  left_join(trends_mod, by="fips") %>%
  filter(state == "WA")

# Step 5:
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# Step 6:
trends_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color="gray", linewidth = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop)), na.value = "white", low = "yellow", high = "red") + 
  blank_theme + 
  ggtitle("Figure 4. Proportion of Black Individuals Incarcerated of counties in WA")
  
plot(trends_map)
#----------------------------------------------------------------------------#

## Load data frame ---- 


