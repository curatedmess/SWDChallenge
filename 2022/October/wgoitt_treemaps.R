# Story Telling with Data | October 2022 - Challenge prompt is Treemaps
# Title - "What's Going On in These Treemaps?"
# Data source is What’s Going On in This Picture? from the New York Times
# Images from 2022 - January 10, 2022 to October 17, 2022

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(ggtext)
library(showtext)
library(stringr)
library(scales)
library(magick)
library(imager)
library(treemapify)

# add font ----------------------------------------------------------------
font_add_google(name = "Arvo", family = "Arvo")
font_add_google(name = "Work Sans", family = "Work Sans")
font <- "Arvo"
font2 <- "Work Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()

# create a list of images using NYT URLs ----------------------------------
img_urls <- c("https://static01.nyt.com/images/2022/10/13/learning/VTS10-17-22LN2/VTS10-17-22LN2-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/09/29/learning/VTS10-02-22LN/VTS10-02-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/09/23/learning/VTS09-26-22LN/VTS09-26-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/09/15/learning/VTS09-19-22LN/VTS09-19-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/09/08/learning/VTS09-12-22LN/VTS09-12-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/05/20/learning/VTS05-23-22LN-JPG/VTS05-23-22LN-JPG-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/05/16/learning/VTS05-16-22LN/VTS05-16-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/05/05/learning/VTS05-09-22LN/VTS05-09-22LN-superJumbo.png",
          "https://static01.nyt.com/images/2022/04/29/learning/VTS05-02-22LN/VTS05-02-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/04/15/learning/VTS04-25-22LN/VTS04-25-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/04/07/learning/VTS04-11-22LN/VTS04-11-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/03/31/learning/VTS03-28-22LN/VTS03-28-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/03/24/learning/VTS03-28-22LN/VTS03-28-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/03/16/learning/VTS03-21-22LN/VTS03-21-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/03/11/learning/VTS03-14-22LN/VTS03-14-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/03/04/learning/VTS03-07-22LN/VTS03-07-22LN-superJumbo.png",
          "https://static01.nyt.com/images/2022/02/25/learning/VTS02-28-22LN/VTS02-28-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/02/07/learning/VTS02-14-22LN/VTS02-14-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2017/02/24/blogs/24-lens-POYi-slide-MZJO/24-lens-POYi-slide-MZJO-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/01/27/learning/VTS01-31-22LN/VTS01-31-22LN-superJumbo.jpg",
          "https://static01.nyt.com/images/2022/01/21/learning/VTS01-24-22LN/VTS01-24-22LN-superJumbo.png",
          "https://static01.nyt.com/images/2021/12/29/learning/VTS01-10-22LN/VTS01-10-22LN-superJumbo.jpg")

# reduce number of colors for each image in list of URLs ------------------
# set number of colors ----------------------------------------------------
n_colors <- 25

# loop to get colors for each image in list -------------------------------
# code adopted from https://www.r-bloggers.com/2019/01/extracting-colours-from-your-images-with-image-quantization/
lapply(img_urls, function(x) {
  
    image_read(x) %>% 
    image_quantize(max = n_colors) %>% 
    magick2cimg() %>%
    RGBtoHSV() %>% 
    as.data.frame(wide = "c") %>% 
    mutate(hex = hsv(rescale(c.1, from = c(0, 360)), c.2, c.3), hue = c.1, sat = c.2, value = c.3) %>%
    count(hex, hue, sat, value, sort = TRUE) %>% 
    mutate(freq = round(n / sum(n), 3)) %>%  
    mutate(url = rep(paste0(x)), nrow(n_colors)) %>% 
    mutate(week = rep(paste0(sub(".*VTS*(.*?) *LN-.*", "\\1", x))), nrow(n_colors))
  
}) -> data

# create data frame -------------------------------------------------------
df <- do.call(rbind,  data)


# clean up dates ----------------------------------------------------------
df$week[df$week == "10-02-22"] <- "10-03-22"
df$week[df$week == "01-24-22"] <- "01-23-22"
df$week[df$url == "https://static01.nyt.com/images/2017/02/24/blogs/24-lens-POYi-slide-MZJO/24-lens-POYi-slide-MZJO-superJumbo.jpg"] <- "02-07-22"
df$week[df$url == "https://static01.nyt.com/images/2022/10/13/learning/VTS10-17-22LN2/VTS10-17-22LN2-superJumbo.jpg"] <- "10-17-22"
df$week[df$url == "https://static01.nyt.com/images/2022/03/31/learning/VTS03-28-22LN/VTS03-28-22LN-superJumbo.jpg"] <- "04-04-22"

# Update the formatting of week data --------------------------------------
df$week <- lubridate::mdy(df$week)

# add long date format for strip label ------------------------------------
df <- mutate(df, full_date = format(df$week, "%B %d, %Y"))

# create plot -------------------------------------------------------------
df %>%
    mutate(full_date = factor(full_date, levels = rev(c("October 17, 2022", "October 03, 2022", "September 26, 2022", "September 19, 2022",	"September 12, 2022",	"May 23, 2022",	"May 16, 2022",	"May 09, 2022",	"May 02, 2022",	"April 25, 2022", "April 11, 2022",	"April 04, 2022", "March 28, 2022",	"March 21, 2022",	"March 14, 2022",	"March 07, 2022",	"February 28, 2022",	"February 14, 2022",	"February 07, 2022", "January 31, 2022",	"January 23, 2022",	"January 10, 2022")))) %>%
    ggplot(aes(area = freq, fill = hex)) +
    geom_treemap(color = "#F2F2F2", size = 1.5) +
    geom_treemap_text(aes(color = value > 0.6, label = scales::percent(freq, 1)), place = "topleft", family = font2) +
    scale_fill_identity() +
    scale_color_manual(values = c("#FFFFFF", "#000000")) +
    facet_wrap(~ full_date) +
    theme_void() +
    theme(plot.title = element_text(family = font, size = 26, hjust = 0, face = "bold"),
          plot.title.position = "plot",
          plot.subtitle = element_markdown(family = font2, size = 11, hjust = 0, lineheight = 1.3),
          plot.caption = element_text(family = font2, hjust = 0.5, size = 9,),
          plot.caption.position = "plot",
          strip.text = element_text(family = font, size = 8, margin = margin(t = 2, b = 2)),
          legend.position = "Null",
          plot.margin = unit(c(1, 1, 1, 1), "cm"),
          plot.background = element_rect(color = NA, fill = "#F2F2F2"),
          panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
    labs(title = "What's Going On in These Treemaps?",
          subtitle = "A collection of treemap plots illustrating the distribution of color in 22 digital photographs<br>from <i><b>What's Going On in This Picture?</b></i>, a student learning series by The New York Times.<br><br><span style=font-size:9.0pt;'>The total number of colors for each image has been reduced to simplify visualization (n = 25).</span><br>",
          caption = "\n\nData: New York Times | Design: Ryan Hart | #SWDchallenge")

# save plot ---------------------------------------------------------------
ggsave(paste0("wgoitt_treemaps_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 8)


