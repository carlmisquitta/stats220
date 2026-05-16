library(tidyverse)
library(lubridate)

logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTPkMrSw1lZWYlizNVWxtyjwJTJq47H6w0EU5NlcyrUqySC4ZvtPZzUhVUZ-Gwkb6zNtE-bNvC9mfzK/pub?output=csv")

names(logged_data)

light_red <- "#fee0d2"
medium_red <- "#fc9272"
dark_red <- "#de2d26"

final_data <- logged_data %>%
  slice(32:61)

shorts_data <- final_data %>%
  mutate(
    category = `What category was the video?`,
    video_length = as.numeric(`What was the length of the video (seconds)`),
    watched_seconds = as.numeric(`How Much of the Short did u finish (in seconds)`),
    engagement = as.numeric(`Rate the video engagement out of 10`),
    likes = as.numeric(`How many likes does the short have?`),
    ai_use = `Does the Short use or discusses AI`,
    ai_group = case_when(
      str_detect(ai_use, "Yes") ~ "Uses or discusses AI",
      str_detect(ai_use, "Unsure") ~ "Unsure",
      TRUE ~ "Does not use or discuss AI"
    )
  )

# Plot 1: Categories of YouTube Shorts
category_data <- shorts_data %>%
  count(category) %>%
  arrange(desc(n))

plot1 <- category_data %>%
  ggplot() +
  geom_col(aes(y = reorder(category, n),
               x = n),
           fill = dark_red) +
  labs(
    title = "Most common categories in my YouTube Shorts observations",
    x = "Number of Shorts",
    y = "Category"
  ) +
  theme_minimal()

ggsave("plot1.png", plot1, width = 8, height = 5)

# Plot 2: AI use overall
ai_count_data <- shorts_data %>%
  group_by(ai_group) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ungroup()

plot2 <- ai_count_data %>%
  ggplot() +
  geom_col(aes(y = reorder(ai_group, n),
               x = n),
           fill = medium_red) +
  labs(
    title = "AI was not commonly used in my observed YouTube Shorts",
    x = "Number of Shorts",
    y = "AI use"
  ) +
  theme_minimal()

ggsave("plot2.png", plot2, width = 8, height = 5)

# Plot 3: Video length compared with likes after removing the highest-like outlier
length_likes_data <- shorts_data %>%
  arrange(desc(likes)) %>%
  slice(2:30)

plot3 <- length_likes_data %>%
  ggplot() +
  geom_point(aes(x = video_length,
                 y = likes),
             colour = dark_red,
             size = 3) +
  labs(
    title = "Video length compared with likes after removing one extreme value",
    x = "Video length in seconds",
    y = "Number of likes"
  ) +
  theme_minimal()

ggsave("plot3.png", plot3, width = 8, height = 5)

# Plot 4: Engagement ratings during logging session using lubridate
time_data <- shorts_data %>%
  mutate(
    timestamp = mdy_hms(Timestamp)
  ) %>%
  arrange(timestamp)

plot4 <- time_data %>%
  ggplot() +
  geom_point(aes(x = timestamp,
                 y = engagement),
             colour = dark_red,
             size = 3) +
  labs(
    title = "Engagement ratings during my logging session",
    x = "Time each observation was logged",
    y = "Engagement rating out of 10"
  ) +
  theme_minimal()

ggsave("plot4.png", plot4, width = 8, height = 5)