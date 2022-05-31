# Storytelling with Data Community Project | May 2022 - Embracing Constraints
# Data from 2019 through May 31, 2022 using using Strava plus a few weeks of manually collected data from early 2019

# Goals for the visual
# 1. Leverage a prior project with Strava API to limit time spent on collecting data
# 2. Simple color scheme -- black, white and a single color
# 3. Focus on a simple plot
# 4. Time box this to a couple of hours, or less


# load packages
library(tidyverse)
library(rStrava)
library(httr)
library(ggtext)
library(showtext)
library(lubridate)
library(jsonlite)

# add fonts
font_add_google(name = "IBM Plex Mono", family = "IBM Plex Mono")
font_add_google(name = "Permanent Marker", family = "Permanent Marker")

# turn on showtext
showtext_auto()

# Strava API instructions created using
# https://bldavies.com/blog/accessing-strava-api/

# credentials -------------------------------------------------------------
client_id <- "INSERT ID"
secret <- "INSERT SECRET"

# OAuth application -------------------------------------------------------
app <- oauth_app("strava", client_id, secret)
endpoint <- oauth_endpoint(
  request = NULL,
  authorize = "https://www.strava.com/oauth/authorize",
  access = "https://www.strava.com/oauth/token"
)

# OAuth access token ------------------------------------------------------
token <- oauth2.0_token(endpoint, app, as_header = FALSE,
                        scope = "activity:read_all")

# get old manual data pre-Strava ------------------------------------------
App_df <- read_csv("./data/10K App Data.csv")


# get Activity List ---------------------------------------------------
strava_df_list <- list()
i <- 1
done <- FALSE
while (!done) {
  req <- GET(
    url = "https://www.strava.com/api/v3/athlete/activities",
    config = token,
    query = list(per_page = 200, page = i)
  )
  strava_df_list[[i]] <- fromJSON(content(req, as = "text"), flatten = TRUE)
  if (length(content(req)) < 200) {
    done <- TRUE
  } else {
    i <- i + 1
  }
}

# combine Activity into a df and wrangle ----------------------------------------------
strava_df <- rbind_pages(strava_df_list) %>%
  mutate(distance_miles = distance * 0.00062137119224) %>%
  filter(type == "Run") %>%
  mutate(year = lubridate::year(start_date_local),
         month = lubridate::month(start_date_local),
         day = lubridate::day(start_date_local)) %>%
  select(distance_miles, year, month, day, workout_type)

# combine dfs and wrangle ------------------------------------------------------------
data <- bind_rows(strava_df,App_df) %>%
  select(month, year, distance_miles, workout_type) %>%
  group_by(year) %>%
  summarise(total = sum(distance_miles))
  
# combine dfs and wrangle for total data thru May 31 ------------------------------------------------------------
asofmay <- bind_rows(strava_df,App_df) %>%
  select(month, year, distance_miles, workout_type) %>%
  filter(month < 6) %>%
  group_by(year) %>%
  summarise(total = sum(distance_miles))

# create plot
ggplot() +
  geom_col(data = data, aes(x = year, y = total), fill = "#000000") +
  geom_line(data = asofmay, aes(x = year, y = total), color = "#00b0ff") +
  geom_point(data = asofmay, aes(x = year, y = total), color = "#00b0ff", size = 7) +
  geom_text(data = asofmay, aes(x = year, y = total, label=round(total, digits=0)), size = 2.3, family = "IBM Plex Mono", color = "#ffffff", fontface = "bold") +
  scale_y_continuous(expand = c(0,0), limits = c(0,2200)) +
  annotate(geom = "text", x = 2022, y = 1050, label = "YOY Comparison\ntotal miles run\nthrough May 31", size = 3.3, family = "Permanent Marker", color = "#00b0ff") +
  theme_minimal() +
  theme(text = element_text(size = 8, family = "IBM Plex Mono", color = "#000000"),
    plot.title = element_text(family = "IBM Plex Mono", size = 20, hjust = 0.5, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(family = "IBM Plex Mono", size = 10, hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 8, family = "IBM Plex Mono", color = "#000000"),
    axis.title.y = element_text(size = 8, family = "IBM Plex Mono", color = "#000000", angle=90),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#000000", size = 0.25, linetype = "dotted"),
    axis.line.x.bottom = element_line(color = "#000000", size = 0.5),
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
    plot.background = element_rect(color = "#ffffff", fill = "#ffffff")) +
  labs(title = "RUN MORE, RUN MORE CONSISTENTLY",
       subtitle = "Trying to improve as a runner every year, one mile at a time.",
       caption = "\n#SWDChallenge | Data: Mostly Strava | Design: Ryan Hart",
       y = "Total Miles Run\n")

# save plot
ggsave(paste0("RunMore_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

