# Libraries
library(tidyverse)
library(lubridate)
library(ggdark)

# Loading the data
pred_df <- read_csv('predictions/ET_predictions.csv')   # predictions 
stat_df <- read_csv('data/JH_stats.csv')   # actual stats

# total number of cases = death + current_cases
res_df <- pred_df %>% 
  inner_join(stat_df, by = c('country', 'type'))

#
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

p_death_error <- df %>% 
  filter(type == 'Death') %>% 
  group_by(name) %>% 
  summarise(error = sum(abs(error))) %>% 
  ggplot(data = .) + 
  geom_bar(aes(x = reorder(name, error), y = error), stat = 'identity', position = 'dodge', fill = '#89aee1') + 
  # dark_theme_classic() + 
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) + 
  ggtitle('Total death error') +
  xlab('') + ylab('Total abosulte error') +
  theme(axis.text = element_text(size = 14), 
        axis.text.x = element_text(size = 18, hjust = 1, angle = 60), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.background = element_rect(fill=NA,
                                         size=0.5, linetype="solid", 
                                         colour ="grey60"))

p_total_error <- df %>% 
  filter(type == 'Total') %>% 
  group_by(name) %>% 
  summarise(error = sum(abs(error))) %>% 
  ggplot(data = .) + 
  geom_bar(aes(x = reorder(name, error), y = error), stat = 'identity', position = 'dodge', fill = '#89aee1') + 
  # dark_theme_classic() + 
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) + 
  ggtitle('Total cases error') +
  xlab('') + ylab('Total abosulte error') +
  theme(axis.text = element_text(size = 14), 
        axis.text.x = element_text(size = 18, hjust = 1, angle = 60), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.background = element_rect(fill=NA,
                                         size=0.5, linetype="solid", 
                                         colour ="grey60"))


p_error <- df %>% 
  # filter(type == 'Total') %>% 
  group_by(name) %>% 
  summarise(error = sum(abs(error))) %>% 
  ggplot(data = .) + 
  geom_bar(aes(x = reorder(name, error), y = error), stat = 'identity', position = 'dodge', fill = '#89aee1') + 
  # dark_theme_classic() + 
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) + 
  # ggtitle('Total cases error') +
  xlab('') + ylab('Total abosulte error') +
  theme(axis.text = element_text(size = 14), 
        axis.text.x = element_text(size = 18, hjust = 1, angle = 60), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.background = element_rect(fill=NA,
                                         size=0.5, linetype="solid", 
                                         colour ="grey60"))


# weighted errors ---------------------------------------------------------


p_death_error_weighted <- df %>% 
  filter(type == 'Death') %>% 
  group_by(name) %>% 
  summarise(error = mean(abs(normalized_error * 100))) %>% 
  ggplot(data = .) + 
  geom_bar(aes(x = reorder(name, error), y = error), 
           stat = 'identity', position = 'dodge', fill = '#89aee1') + 
  # dark_theme_classic() + 
  scale_y_continuous(labels = scales::comma) + 
  ggtitle('Total death error') +
  xlab('') + ylab('Average normalized error') +
  theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.text.x = element_text(size = 18, hjust = 1, angle = 60), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.background = element_rect(fill=NA,
                                         size=0.5, linetype="solid", 
                                         colour ="grey60"))

p_total_error_normalized <- df %>% 
  filter(type == 'Total') %>% 
  group_by(name) %>% 
  summarise(error = mean(abs(normalized_error * 100))) %>% 
  ggplot(data = .) + 
  geom_bar(aes(x = reorder(name, error), y = error), stat = 'identity', position = 'dodge', fill = '#89aee1') + 
  # dark_theme_classic() + 
  scale_y_continuous(labels = scales::comma) + 
  ggtitle('Total cases error') +
  xlab('') + ylab('Average error') +
  theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.text.x = element_text(size = 18, hjust = 1, angle = 60), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.background = element_rect(fill=NA,
                                         size=0.5, linetype="solid", 
                                         colour ="grey60"))


p_error_normalized <- df %>% 
  # filter(type == 'Total') %>% 
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


# player-country matrix  --------------------------------------------------

p_countries <- df %>% 
  group_by(country, name) %>% 
  summarise(total_error = sum(abs(normalized_error))) %>% 
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



p_country_error <- df %>% 
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


p_country_error

# # List of countries
# library(countrycode)
# 
# set.seed(666)
# 
# countries <- codelist$country.name.en
# selected_countries <- sample(countries, 10)
