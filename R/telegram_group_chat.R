library(tidyverse)    # CRAN v1.3.0
library(lubridate)    # CRAN v1.7.9
library(jsonlite)     # CRAN v1.7.0
library(textfeatures) # CRAN v0.3.3
library(emo)          # [github::hadley/emo] v0.0.0.9000

source("R/count_utils.R")

#######################################################################
#                                                                     #
#              Import data Telegram chat from JSON file               #
#                                                                     #
#######################################################################

tg <- fromJSON("data-raw/result.json", flatten = TRUE)
tg_df <- tg$messages

glimpse(tg_df)
View(tg_df)

#######################################################################
#                                                                     #
#                          Data Preparation                           #
#                                                                     #
#######################################################################

tg_df <- tg_df %>% 
  filter(is.na(from) | from != "Sri")

dim(tg_df)

tg_df %>% 
  distinct(from) %>% 
  count()

#################### Function to tidy the message #####################

tidy_text <- function(text){
  if(is.list(unlist(text))){
    x <- unlist(unlist(text))
    paste(x[names(x) != "type"], collapse = " ")
  } else if(length(unlist(text)) > 1){
    x <- unlist(text)
    paste(x[names(x) != "type"], collapse = " ")
  } else {
    unlist(text)
  }
  
}

#######################################################################
#                                                                     #
#                         Feature Engineering                         #
#                                                                     #
#######################################################################

tg_clean_df <- tg_df %>%
  mutate(type = if_else(type != "message", action, type),
         text = map(text, tidy_text) %>% unlist(),
         datetime = as_datetime(date),
         date = as_date(date),
         hour = hour(datetime),
         day = wday(date, week_start = 1, label = TRUE),
         month = month(date, label = TRUE),
         year = year(date),
         media = case_when(is.na(file) & !is.na(photo) ~ "photo",
                           str_detect(file, "File not") & str_detect(mime_type, "pdf") ~ "pdf",
                           str_detect(file, "File not") & str_detect(mime_type, "video") & media_type == "animation" ~ "animation",
                           str_detect(file, "File not") & str_detect(mime_type, "video") & media_type == "video_file" ~ "video",
                           str_detect(file, "File not") & str_detect(mime_type, "application") ~ "others",
                           str_detect(file, "stickers") ~ "sticker",
                           str_detect(mime_type, "video") ~ "video",
                           str_detect(mime_type, "text") ~ "file",
                           str_detect(mime_type, "audio") ~ "audio",
                           str_detect(mime_type, "jpeg") | str_detect(mime_type, "jpg") | str_detect(mime_type, "png") ~ "image",
                           str_detect(mime_type, "image") ~ str_remove_all(mime_type, "image/"),
                           TRUE ~ mime_type
         ),
         is_reply = !is.na(reply_to_message_id),
         any_emoji = emo::ji_detect(text) | !is.na(sticker_emoji),
         is_emoji_only = !is.na(sticker_emoji),
         emoji = emo::ji_extract_all(text),
         n_char = n_chars(text),
         n_url = n_urls(text)
  ) %>% 
  select(id:from, datetime,  media, sticker_emoji, emoji, hour:is_emoji_only, n_char:n_url)

glimpse(tg_clean_df)
View(tg_clean_df)
