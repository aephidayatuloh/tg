library(dplyr)
library(ggplot2)
library(scales)
library(av)
library(gganimate)
library(gghighlight)
library(wordcloud2)


########################################################################
#                                                                      #
#                  Monthly no of chat and new member                   #
#                                                                      #
########################################################################

nchats <- tg_clean_df %>% 
  mutate(date = as_date(format(date, "%Y%m01"))) %>% 
  group_by(date) %>% 
  summarise(n_chats = n())

newmember <- tg_clean_df %>% 
  mutate(date = as_date(format(date, "%Y%m01"))) %>% 
  filter(type == 'invite_members') %>% 
  group_by(date) %>% 
  summarise(n_member = n())

ndata <- left_join(nchats, newmember, by = "date")

# Scale for second y axis ------------------------
scaley <- 10

ggplot(data = ndata, aes(x = date)) + 
  geom_col(aes(y = n_chats, fill = n_chats)) + 
  geom_line(aes(y = n_member*scaley), size = 1, color = "coral") + 
  geom_point(aes(y = n_member*scaley), size = 2, color = "coral") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  scale_y_continuous(name = "No of Chat", 
                     breaks = seq(0, 4000, by = 500),
                     limits = c(0, 4000), 
                     sec.axis = sec_axis(~./scaley, name = "No of New member")) + 
  labs(title = "Monthly No of Chat and New Member",
       x = "Date") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(color = "darkblue"),
        axis.title.y = element_text(color = "darkblue"),
        axis.text.y.right = element_text(color = "coral"),
        axis.title.y.right = element_text(color = "coral", angle = 90))

########################################################################
#                                                                      #
#                   Daily no of chat and new member                    #
#                                                                      #
########################################################################

nchats_day <- tg_clean_df %>% 
  group_by(day) %>% 
  summarise(n_chats = n())

newmember_day <- tg_clean_df %>% 
  filter(type == "invite_members") %>% 
  group_by(day) %>% 
  summarise(n_member = n())

ndata_day <- left_join(nchats_day, newmember_day, by = "day")

ggplot(data = ndata_day, aes(x = day)) + 
  geom_col(aes(x = as.numeric(day), y = n_chats, fill = day)) + 
  geom_line(aes(x = as.numeric(day), y = n_member*scaley), size = 1, color = "coral") + 
  geom_point(aes(x = as.numeric(day), y = n_member*scaley), size = 2, color = "coral") + 
  xlim("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun") +
  scale_y_continuous(name = "No of Chat", 
                     breaks = seq(0, 6000, by = 500),
                     limits = c(0, 6000),
                     sec.axis = sec_axis(~./scaley, name = "No of New member")) + 
  labs(title = "No of Chat and New Member Join",
       x = "Day") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(color = "darkblue"),
        axis.title.y = element_text(color = "darkblue"),
        axis.text.y.right = element_text(color = "coral"),
        axis.title.y.right = element_text(color = "coral", angle = 90))


########################################################################
#                                                                      #
#                 No of chat and new member in a month                 #
#                                                                      #
########################################################################
year_period <- 2019
month_period <- "Jan"

nchat_jan <- tg_clean_df %>% 
  filter(year == year_period & month == month_period) %>% 
  group_by(date) %>% 
  summarise(n_chat = n())

nmember_jan <- tg_clean_df %>% 
  filter(type == "invite_members") %>% 
  filter(year == year_period & month == month_period) %>% 
  group_by(date) %>% 
  summarise(n_member = n())

ndata_jan <- left_join(nchat_jan, nmember_jan, by = "date") %>% 
  mutate(n_member = if_else(is.na(n_member), 0L, n_member))

ggplot(data = ndata_jan, aes(x = date)) + 
  geom_col(aes(y = n_chat, fill = n_chat)) + 
  geom_line(aes(y = n_member*scales), size = 1, color = "coral") + 
  geom_point(aes(y = n_member*scales), size = 2, color = "coral") + 
  scale_x_date(date_breaks = "2 days", 
               date_labels = "%d") + 
  scale_y_continuous(name = "No of Chat", 
                     breaks = seq(0, 400, by = 50),
                     limits = c(0, 400),
                     sec.axis = sec_axis(trans = ~./scales, 
                                         breaks = seq(0, 40, by = 5), 
                                         name = "No of New member")) + 
  labs(title = "No of Chat dan Member Join",
       subtitle = paste("on", month_period, year_period),
       x = "Date") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(color = "darkblue"),
        axis.title.y = element_text(color = "darkblue"),
        axis.text.y.right = element_text(color = "coral"),
        axis.title.y.right = element_text(color = "coral", angle = 90))


########################################################################
#                                                                      #
#                          Most Active Member                          #
#                                                                      #
########################################################################

year_period <- 2019
month_period <- "Apr"

tg_clean_df %>% 
  filter(year == year_period & month == month_period) %>% 
  group_by(month, from) %>%
  summarise(n = n()) %>% 
  drop_na() %>% 
  mutate(ranking = base::rank(desc(n), ties.method = "first"),
         from = paste0(ranking, ". ", from)) %>% 
  filter(ranking <= 20) %>% 
  mutate(hjust = if_else(n > 10, 1.05, -0.05)) %>% #view()
  ggplot(aes(x = reorder(from, -ranking), 
             y = n,
             fill = from, 
             color = from)) + 
  geom_col(alpha = 0.8, color = NA) +
  geom_text(aes(y = n, label = paste0(" ", n, " "), hjust = hjust), 
            vjust = 0.5, color = "black") + 
  labs(title = "Most Active Member",
       subtitle = paste("on", month_period, year_period),
       x = "Member",
       y = "No Chat") + 
  coord_flip(clip = "off", expand = FALSE) +
  theme_minimal() + 
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        axis.text.y = element_text(size = 12))


########################################################################
#                                                                      #
#                       Bar Chart Race Animation                       #
#                                                                      #
########################################################################

staticplot <- tg_clean_df %>% 
  mutate(date_frame = date %m+% months(1) %>% rollback()) %>%
  filter(date_frame >= as_date("2018-12-31")) %>%
  group_by(date_frame, from) %>%
  summarise(n = n()) %>% 
  drop_na() %>% 
  mutate(ranking = base::rank(desc(n), ties.method = "first"),
         Value_rel = n/n[ranking == 1],
         Value_lbl = paste0(" ", n)) %>% 
  group_by(from) %>% 
  filter(ranking <= 15) %>% 
  
# ggplot part --------------------------------------
  ggplot(aes(ranking, group = from,
             fill = from, 
             color = from)) +
  geom_tile(aes(y = n/2,
                height = n,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(from, " ")), vjust = 0.2, hjust = 1, size = 6) +
  geom_text(aes(y = n, label = Value_lbl), hjust = 0, size = 6) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line( size = .1, color = "grey" ),
        panel.grid.minor.x = element_line( size = .1, color = "grey" ),
        plot.title = element_text(size = 24, hjust = 0.5, face = "bold", colour = "grey", vjust = -1),
        plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic", color = "grey"),
        plot.caption = element_text(size = 12, hjust = 0.5, face = "italic", color = "grey"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.margin = margin(t = 2, r = 2, b = 2, l = 10, unit = "cm"))

anim <- staticplot + 
  transition_time(date_frame) +
  enter_fade() + 
  exit_fade() + 
  ease_aes("cubic-in-out") + 
  view_follow(fixed_x = TRUE)  +
  labs(title = "Most 15 Active Members on {format(frame_time, '%b %Y')}",
       subtitle = "Based on No of Chat")


##########################################################################
#                                                                        #
#       Do not run bellow code, take too long computation for demo       #
#                                                                        #
##########################################################################

# GIF format --------------------
animate(anim, nframes = 1000, fps = 50, end_pause = 100, width = 800, height = 600, 
        renderer = gifski_renderer("tg_member_aktif.gif"))

# MP4 format --------------------
animate(anim, nframes = 1000, fps = 50, end_pause = 100, width = 1000, height = 600, 
        renderer = av_renderer("tg_member_aktif.mp4"))

#######################################################################
#                                                                     #
#               Number and percentage of charts by day                #
#                                                                     #
#######################################################################

tg_clean_df %>% 
  filter(type == "message" & year == 2019) %>% 
  group_by(month, day) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = day, y = n)) + 
  geom_col(aes(fill = day)) + 
  geom_text(aes(label = paste0(round(pct*100, 1), "%")), vjust = -0.25) + 
  scale_y_continuous(limits = c(0, 800)) + 
  labs(y = "No of Chat",
       fill = "Days") + 
  facet_wrap(~ month, scales = "free_x") + 
  theme_light() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

#######################################################################
#                                                                     #
#               Number and percentage of charts by hour               #
#                                                                     #
#######################################################################

tg_clean_df %>% 
  filter(type == "message") %>% 
  ggplot(aes(x = hour)) + 
  geom_density(aes(x = hour, color = month), size = 0.8) + 
  scale_x_continuous(breaks = c(seq(0, 23, by = 3), 23)) + 
  labs(x = "Hour") + 
  facet_wrap(~ month) +
  theme_minimal() + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") + 
  gghighlight(unhighlighted_params = list(size = 0.5))

########################################################################
#                                                                      #
#                           Wordcloud Emoji                            #
#                                                                      #
########################################################################

tg_clean_df %>% 
  filter(any_emoji | is_emoji_only) %>% 
  transmute(emoji = case_when(is.na(sticker_emoji) & length(emoji) >= 1 ~ emoji,
                           TRUE ~ emo::ji_extract_all(sticker_emoji))
         ) %>% 
  unnest_longer(emoji) %>% 
  count(emoji, sort = TRUE) %>% 
  filter(!is.na(emoji)) %>% 
  wordcloud2(backgroundColor = "#0f2a42")

