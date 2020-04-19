# Libraries
library(tidyverse)
library(lubridate)
library(ggdark)
library(countrycode) # List of countries
library(hrbrthemes)



# Loading the data
pred_df <- read_csv('predictions/ET_predictions.csv')   # predictions 
stat_df <- read_csv('data/JH_stats.csv')   # actual stats

# the week
all_dates <- lubridate::date(c("2020-04-04", "2020-04-11", "2020-04-18"))
latest_date <- all_dates[length(all_dates)] 

stat_df <- stat_df %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  filter(date <= latest_date)


# total number of cases = death + current_cases
res_df <- pred_df %>% 
  mutate(pred_date = lubridate::mdy(pred_date)) %>% 
  mutate(date = pred_date + 7  ) %>% 
  inner_join(stat_df, by = c('country', 'type', 'date'))


# scatter plot  -----------------------------------------------------------
p_scatter <- ggplot(res_df, aes(x = count, y = prediction, color = name)) +
  geom_jitter(aes(shape = type), alpha = .6, size = 2.3, width = .05) +
  # dark_theme_classic() + 
  scale_x_log10(labels = scales::comma) + 
  scale_y_log10(labels = scales::comma) + 
  geom_abline(aes(slope = 1, intercept = 0), lty = 2, color = 'grey60') + 
  xlab('Official stats') + ylab('El Tigre prediction') +
  scale_color_brewer(palette = 'Set1') + 
  theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.text.x = element_text(size = 14, hjust = 1, angle = 60), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        legend.background = element_rect(fill=NA,
                                         size=0.5, linetype="solid", 
                                         colour ="grey60"))
# p_scatter


# errors ------------------------------------------------------------------
df <- res_df %>% 
  mutate(error = count - prediction,
         normalized_error = (count - prediction) / count) 


names <- df %>%
  filter(date == !! latest_date) %>% 
  select(name) %>% 
  distinct()
names <- df$name
total_players_this_week <- length(names)

# this week's results -----------------------------------------------------

p_latest_error <- df %>%
  filter(date == !! latest_date) %>% 
  group_by(name) %>% 
  summarise(error = mean(abs(normalized_error * 100))) %>%
  ggplot(data = .) + 
  geom_bar(aes(x = reorder(name, error), y = error), stat = 'identity', position = 'dodge', fill = '#89aee1') + 
  # dark_theme_classic() + 
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) + 
  # ggtitle('Total cases error') +
  xlab('') + ylab('Average error') +
  theme(axis.text = element_text(size = 14), 
        axis.text.x = element_text(size = 18, hjust = 1, angle = 60), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.background = element_rect(fill=NA,
                                         size=0.5, linetype="solid", 
                                         colour ="grey60"))



p_latest_error_total <- df %>%
  filter(date == !! latest_date) %>% 
  filter(type == 'Total') %>% 
  group_by(name) %>% 
  summarise(error = mean(abs(normalized_error * 100))) %>%
  ggplot(data = .) + 
  geom_bar(aes(x = reorder(name, error), y = error), stat = 'identity', position = 'dodge', fill = '#89aee1') + 
  # dark_theme_classic() + 
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) + 
  # ggtitle('Total cases error') +
  xlab('') + ylab('Average error') +
  ggtitle('Total number of cases') +
  theme(axis.text = element_text(size = 14), 
        axis.text.x = element_text(size = 18, hjust = 1, angle = 60), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.background = element_rect(fill=NA,
                                         size=0.5, linetype="solid", 
                                         colour ="grey60"))


p_latest_error_death <- df %>%
  filter(date == !! latest_date) %>% 
  filter(type == 'Death') %>% 
  group_by(name) %>% 
  summarise(error = mean(abs(normalized_error * 100))) %>%
  ggplot(data = .) + 
  geom_bar(aes(x = reorder(name, error), y = error), stat = 'identity', position = 'dodge', fill = '#89aee1') + 
  # dark_theme_classic() + 
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) + 
  # ggtitle('Total cases error') +
  xlab('') + ylab('Average error') +
  ggtitle('Total number of death') +
  theme(axis.text = element_text(size = 14), 
        axis.text.x = element_text(size = 18, hjust = 1, angle = 60), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.background = element_rect(fill=NA,
                                         size=0.5, linetype="solid", 
                                         colour ="grey60"))



# timeline  ---------------------------------------------------------

plot_df <- df %>%
  filter(date <= !!latest_date) %>% 
  group_by(name, date) %>% 
  summarise(error = mean(abs(normalized_error * 100))) %>%
  ungroup() %>% 
  mutate(name = factor(name)) 

p_timeline <- ggplot(plot_df, aes(x = date, y = error, color = name)) + 
  geom_path(lwd = 2, alpha = .5) + 
  geom_point(size = 3) + 
  dark_theme_bw() + 
  scale_y_continuous(labels = scales::comma) + 
  # ggtitle('Total cases error') +
  xlab('') + ylab('Average error') +
  theme(axis.text = element_text(size = 14), 
        axis.text.x = element_text(size = 18, hjust = 1, angle = 60), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.background = element_rect(fill=NA,
                                         size=0.5, linetype="solid", 
                                         colour ="grey60"))



# player-country matrix  --------------------------------------------------

# x <- subset(df, country == 'Germany') %>% 
#   filter(date == !!latest_date) %>% 
#   group_by(name) %>% summarise(m = mean(abs(normalized_error)) * 100) %>% 
#   arrange(desc(m)) 

p_countries <- df %>% 
  filter(date == !!latest_date) %>% 
  group_by(country, name) %>% 
  summarise(total_error = mean(abs(normalized_error)) * 100) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(rank = as.character(rank(total_error))) %>% 
  ggplot(data = ., aes(x = name, y = country, fill = rank)) + 
  geom_tile(color = 'grey60',  width=0.7, height=0.7) + 
  scale_fill_brewer(palette = 'Spectral', direction = -1) + 
  theme_minimal() + 
  theme(axis.text = element_text(size = 14), 
        axis.text.x = element_text(size = 14, hjust = 1, angle = 60), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        legend.background = element_rect(fill=NA,
                                         size=0.5, linetype="solid", 
                                         colour ="grey60")) + 
  xlab('') + ylab('')


countries_tbl <- df %>% 
  filter(date == !!latest_date) %>% 
  group_by(country, name) %>% 
  summarise(total_error = round(mean(abs(normalized_error)) * 100, 3)) %>% 
  ungroup() %>% 
  spread(country, total_error) 

  # group_by(country) %>% 
  # mutate(rank = as.character(rank(total_error)))


p_country_error <- df %>% 
  filter(date <= !!latest_date) %>% 
  group_by(country, name) %>% 
  summarise(average_error = mean(normalized_error * 100)) %>% 
  ungroup() %>% 
  ggplot(data = ., aes(x = name, y = country, fill = average_error)) + 
  geom_tile(color = 'grey60',  width=0.7, height=0.7) + 
  ggplot2::scale_fill_gradient2(low = '#3492eb', mid = 'white', high = '#fa1129', breaks = seq(-150, 150, by = 50), 
                                name = 'Average error') + 
  theme_minimal() + 
  theme(axis.text = element_text(size = 14), 
        axis.text.x = element_text(size = 14, hjust = 1, angle = 60), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        legend.background = element_rect(fill=NA,
                                         size=0.5, linetype="solid", 
                                         colour ="grey60")) + 
  xlab('') + ylab('')


# Best/Worst predictions --------------------------------------------------
# df %>%
#   filter(date == !! latest_date) %>% 
#   arrange(abs(normalized_error)) %>% 
#   select(name, country, type, prediction, actual_count = count, error = normalized_error) %>% 
#   mutate(error = round(error*100, 2)) %>% 
#   head(10)




# badges definition -------------------------------------------------------

assign_badges <- function(error) {
  error <- abs(error)
  
  if (error == 0) {
    return("Oracle")
  }
  
  if (error <= 2) {
    return("Superhuman")
  }
  
  if (error <= 5) {
    return("Outstanding")
  }
  
  if (error <= 10) {
    return("Good")
  }
  
  if (error <= 30) {
    return("Average")
  }
  
  if (error <= 50) {
    return("Bad")
  }
  
  if (error <= 100) {
    return("Very bad")
  }
  
  return("Terrible")
}


df <- df %>% 
  rowwise() %>% 
  mutate(badge = assign_badges(normalized_error*100))


lst <- list("Average" = 0,   
            "Bad" = 0 ,
            "Good" = 0,
            "Oracle" = 0, 
            "Outstanding" = 0, 
            "Superhuman" = 0,
            "Terrible" = 0, 
            "Very bad" = 0)


score_lst <- list("Average" = 0,   
            "Bad" = -1,
            "Good" = 1,
            "Oracle" = 10, 
            "Outstanding" = 3, 
            "Superhuman" = 5,
            "Terrible" = -5, 
            "Very bad" = -3)


tbl <- df %>% 
  select(name, badge) %>% 
  ungroup() %>% 
  group_by(name, badge) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  spread(badge, n) %>% 
  tidyr::replace_na(lst) %>% 
  arrange(desc(Oracle), desc(Superhuman), desc(Outstanding), desc(Good), desc(Average)) %>% 
  select(Name = name, 
         Oracle, Superhuman, Outstanding, Good, Average, Bad, `Very bad`, Terrible)


M <- as.matrix(tbl[, 2:ncol(tbl)])
S <- matrix(unlist(score_lst[colnames(M)]))

scores <- M %*% S
tbl$Points <- scores[, 1]

fixture_tbl <- tbl %>% 
  arrange(desc(Points))


# Score evolution over time --------------------------------------------------------
assign_score <- function(badge, score_lst) {
  score_lst[[badge]]
}

df <- df %>% 
  rowwise() %>% 
  mutate(point = assign_score(badge, score_lst))

cumulative_points <- function(dfx, all_dates) { 
  all_points <- tibble(date = all_dates)
  all_points <- subset(all_points, date >= min(dfx$date))
  dfx <- dfx %>% 
    full_join(all_points, by = 'date') %>% 
    replace_na(list(total_point = 0, name = dfx$name[1])) %>% 
    arrange(date) 
  
  dfx$total_point <- cumsum(dfx$total_point)
  
  dfx
}

points_over_time <- df %>% 
  group_by(name, date) %>% 
  summarise(total_point = sum(point)) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  do(cumulative_points(., all_dates = all_dates))

points_evolution_over_time_plt <- points_over_time %>% 
  ggplot(aes(x = date, y = total_point, color = name)) +
  geom_path(lwd = 1.6) + 
  geom_point(size = 2.5) + 
  # geom_smooth(method = 'loess') + 
  theme_ipsum_rc(axis="xy") + 
  labs(x = 'Date', 
       y = 'Total points', 
       title = 'Evolution of total points for each player',
       caption = 'Players who did not participate in subsequent rounds (e.g., A-Aron), will maintain their latest score. ') + 
  scale_color_ipsum() # + 
  # ylim(-25, 20)

  

# Relationship of error to total cases ------------------------------------

# ggplot(df) + 
#   geom_jitter(aes(x = count, y = abs(normalized_error)), color = '#3d5675') + 
#   scale_x_log10() + 
#   theme_ipsum_rc() + 
#   geom_smooth(aes(x = count, y = abs(normalized_error)), method = 'lm')
    
