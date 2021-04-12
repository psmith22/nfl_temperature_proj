#install nflfastR
install.packages("devtools")
devtools::install_github("mrcaseb/nflfastR")
install.packages("purrr")
install.packages("stargazer")
library(stargazer)
library(purrr)
library(broom)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggimage)
library(nflfastR)
install.packages("teamcolors")
library(teamcolors)
#avoid scientific notation
options(scipen = 9999)

data_2019 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
head(data_2019)

seasons <- 2010:2019
nfl_decade_temp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  ) %>%
    filter(rush == 1 | pass == 1, week <= 17, !is.na(epa), !is.na(posteam), posteam != "", roof != "dome", !is.na(temp)) %>%
    select(game_id, play_id, posteam, roof, temp, wind, epa)
})

head(nfl_decade_temp)

#threshold <- nfl_decade_temp%>%
 # mutate(temp2 = temp^2, temp3 = temp^3) 

#find inflection point
#threshold.lm <- lm(epa ~ temp + temp2 + temp3, data = threshold)
#summary(threshold.lm)


#game by game mean epa
nfl_decade_temp %>%
  group_by(game_id, posteam)%>%
  summarize(mean_epa = mean(epa), var_epa = var(epa), n()) %>%
  head()

#create a cold column
nfl_decade_temp %>%
  mutate(cold_game = if_else(temp <= 40, 1, 0)) %>%
  group_by(cold_game) %>%
  summarize(epa = mean(epa), n())

#store cold column
nfl_cold <- nfl_decade_temp %>%
  mutate(cold_game = if_else(temp <= 40, 1, 0)) 

head(nfl_cold)
View(nfl_cold)

nfl_cold <- nfl_cold %>%
  mutate(
    posteam = case_when(
      posteam == 'LV' ~ 'OAK',
      posteam == 'SD' ~ 'LAC',
      posteam == 'STL' ~ 'LA',
      TRUE ~ posteam
    )
  )

#NFL city data
library(readxl)
city_weather <- read_excel("Documents/Fall 2020/Sports Analytics/NFL City Weather.xlsx", sheet = "Sheet2")

monthly_temp <- city_weather %>% 
  filter(Month == 'Sep' | Month == 'Oct'| Month == 'Nov'| Month == 'Dec')%>%
  group_by(City) %>%
  summarize(avg_temp = mean(Temp), std = sqrt(var(Temp))) %>%
  arrange(avg_temp)
View(monthly_temp)


city_weather %>%
  filter(Month == 'Sep' | Month == 'Oct'| Month == 'Nov'| Month == 'Dec')%>%
  summarize(avg_temp = mean(Temp), std = sqrt(var(Temp)))


#add whether city is cold or warm
#nfl <- nfl_cold %>%
#  mutate(cityType = if_else(posteam == "GB" | posteam == "MIN" | posteam == "DEN" | posteam == "BUF" | 
 #                             posteam == "CHI" | posteam == "GB" |, 1, 0)) 


#NFL city data with correct abbreviations
library(readxl)
city_weather_abb <- read_excel("Documents/Fall 2020/Sports Analytics/NFL City Weather.xlsx", sheet = "Sheet3")
View(city_weather_abb)

monthly_temp_abb <- city_weather_abb %>% 
  filter(Month == 'Sep' | Month == 'Oct'| Month == 'Nov'| Month == 'Dec')%>%
  group_by(City) %>%
  summarize(city_avg_temp = mean(Temp)) %>%
  arrange(city_avg_temp)
View(monthly_temp_abb)

#join city temps with nflR data
nfl_cold %>%
  group_by(posteam) %>%
  summarize(n=n()) %>%
  arrange(n)

data <- nfl_cold %>%
  left_join(monthly_temp_abb, by = c('posteam' = 'City'))

data %>%
  filter(city_type == 0) %>%
  summarise(mean_epa = mean(epa), sd = sd(epa), min = min(epa), max = max(epa))

data <- data %>%
  mutate(city_type = if_else(city_avg_temp <= 50, 1, 0))
nrow(data)
data %>%
ggplot(data, aes(x = temp, y = mean_epa_play)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw(16) +
  ggtitle("EPA/Play vs. Temperature for NFL Games") +
  xlab("Temp (°F)")  +
  ylab("EPA/Play")+ 
  geom_smooth(method = "lm", level = 0.9)

#find each teams avg epa per game
head(data)
data <- data %>%
  group_by(game_id, posteam) %>%
  summarize(mean_epa_play = mean(epa), 
            var_epa = var(epa),
            temp = mean(temp), 
            wind = mean(wind), 
            cold_game = mean(cold_game), 
            city_avg_temp = mean(city_avg_temp), 
            cold_city = mean(city_type))
View(data)
head(data)

#z test diff for epa/play
data <- data %>%
  group_by(posteam, cold_game) %>%
  summarize(mean_epa_play = mean(epa), 
            var_epa = var(epa),
            n = n(),
            temp = mean(temp), 
            wind = mean(wind), 
            cold_game = mean(cold_game), 
            city_avg_temp = mean(city_avg_temp), 
            cold_city = mean(city_type))

data_cold <- data %>%
  pivot_wider(id_cols = posteam,
              names_from = cold_game, 
              names_glue = "{.value}_{cold_game}",
              values_from = c(mean_epa_play, var_epa, n))

data_cold <-  data_cold %>% left_join(data %>% ungroup() %>% 
                                        group_by(posteam) %>% 
                                        summarise(cold_city = max(cold_city)) %>% 
                                        select(posteam, cold_city), 
                                        by = 'posteam')

data_cold <- data_cold %>% 
  mutate(difference_cold_warm = mean_epa_play_1 - mean_epa_play_0, 
         var_denom = sqrt(var_epa_1/n_1 + var_epa_0/n_0), 
         z_test_stat = difference_cold_warm/var_denom)

ggplot(data_cold %>%
         group_by(posteam) %>% 
         summarise(diff =  difference_cold_warm, cold = max(cold_city)), 
       aes(x = reorder(posteam, diff), y = diff, fill = cold)) + 
  geom_bar(stat = 'identity')


data_cold$cold_city <- factor(data_cold$cold_city)
# View(data_cold)
# z stats plot

temp <- data_cold %>%
  group_by(posteam, cold_city) %>% 
  summarise(z_stat =  z_test_stat)

ggplot(temp, 
       aes(x = reorder(posteam, z_stat), y = z_stat, fill = cold_city)) + 
  geom_bar(stat = 'identity') + 
  geom_hline(yintercept = 0, col = 'black') + 
  scale_fill_manual(values = c('red', 'blue')) + 
  theme(legend.position = 'blank') + 
  theme_bw(16) + 
  xlab("Team") + ylab("Z Statistic") + 
  ggtitle("Figure 2: Test Statistics for Two Sample Z Test of EPA per Play (Cold Games – Warm Games) for NFL Teams" )+
  geom_hline(yintercept = qnorm(.975), col = 'black', linetype="dashed") + 
  geom_hline(yintercept = -qnorm(.975), col = 'black', linetype="dashed")


data %>%
  ungroup()%>%
  filter(cold_city == 0) %>%
  summarise(var_epa = var(mean_epa))
data %>% filter(epa < -10)
#run a regression - currently no intercept
temp.lm <- lm(epa ~ temp + wind + cold_game + city_type, data = data)
summary(temp.lm)
model.matrix(temp.lm)

ggplot(data, aes(x = jitter(temp,1), y = epa)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw(16) +
  ggtitle("Figure 1: EPA/Play vs. Temperature for NFL Games") +
  xlab("Jittered Temp (°F)")  +
  ylab("EPA/Play") + 
  geom_smooth(method = "lm", level = 0.9)
  

#should there be an intercept??????????? yes\
  

data %>% ggplot(aes(x = temp, y = mean_epa)) +
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  facet_grid(cold_game ~ cold_city, labeller = label_both) +
  theme_bw(16)
# View(data)
# data %>% filter(posteam == 'MIN') %>% View()
# DIFFERENCE IN GAMES PLAYED IS DUE TO DOME!!!!!
# 57 games
## new
monthly_temp_abb

str(data)

cold_teams_involved <- data %>% filter(cold_city == 1)


data %>% group_by(cold_city) %>% 
  summarise(mean_epa = mean(mean_epa), n())

posteam_cold_games_epa <- data %>% group_by(posteam, cold_game)%>% 
  summarise(mean_epa = mean(mean_epa), n(), var_epa = var(var_epa))
# filtering out ARI because they have zero cold games
# posteam_cold_games_epa <- posteam_cold_games_epa %>% filter(posteam != 'ARI')

posteam_cold_games_epa <- posteam_cold_games_epa %>%
  pivot_wider(id_cols = posteam,
              names_from = cold_game, 
              names_glue = "{.value}_{cold_game}",
              values_from = c(mean_epa, var_epa, `n()`))

posteam_cold_games_epa <-  posteam_cold_games_epa %>% left_join(data %>% 
                                                                  ungroup() %>% 
                                                                  group_by(posteam) %>% 
                                                                  summarise(cold_city = max(cold_city)) %>% 
                                                                  select(posteam, cold_city), 
                                                                by = 'posteam')

posteam_cold_games_epa <- posteam_cold_games_epa %>% 
  mutate(difference_cold_warm = mean_epa_1 - mean_epa_0, 
         var_denom = sqrt(var_epa_1/`n()_1` + var_epa_0/`n()_0`), 
         z_test_stat = difference_cold_warm/var_denom)
# View(posteam_cold_games_epa)
# difference plot
ggplot(posteam_cold_games_epa %>%
         group_by(posteam) %>% 
         summarise(diff =  difference_cold_warm, cold = max(cold_city)), 
       aes(x = reorder(posteam, diff), y = diff, fill = cold)) + 
  geom_bar(stat = 'identity')


posteam_cold_games_epa$cold_city <- factor(posteam_cold_games_epa$cold_city)
# View(posteam_cold_games_epa)
# z stats plot

temp <- posteam_cold_games_epa %>%
  group_by(posteam, cold_city) %>% 
  summarise(z_stat =  z_test_stat)

ggplot(temp %>% filter(posteam != 'TB'), 
       aes(x = reorder(posteam, z_stat), y = z_stat, fill = cold_city)) + 
  geom_bar(stat = 'identity') + 
  geom_hline(yintercept = 0, col = 'black') + 
  scale_fill_manual(values = c('red', 'blue')) + 
  theme(legend.position = 'blank') + 
  theme_bw() + 
  xlab("Team") + ylab("Z Statistic")

## cold vs warm in cold games
nfl_decade_temp
# data <- data %>%
#   group_by(game_id, posteam) %>%
#   summarize(mean_epa = mean(epa), 
#             var_epa = var(epa),
#             temp = mean(temp), 
#             wind = mean(wind), 
#             cold_game = mean(cold_game), 
#             city_avg_temp = mean(city_avg_temp), 
#             cold_city = mean(city_type))
# View(city_weather_abb)
# View(monthly_temp_abb)
nfl_decade_temp_n <- nfl_decade_temp %>%
  mutate(cold_game = if_else(temp <= 40, 1, 0)) 
nfl_decade_temp_n <- nfl_decade_temp_n %>%
  mutate(
    posteam = case_when(
      posteam == 'LV' ~ 'OAK',
      posteam == 'SD' ~ 'LAC',
      posteam == 'STL' ~ 'LA',
      TRUE ~ posteam
    )
  )
data_gen <- nfl_decade_temp_n %>%
  left_join(monthly_temp_abb, by = c('posteam' = 'City'))

data_gen <- data_gen %>%
  mutate(city_type = if_else(city_avg_temp <= 50, 1, 0))
nfl_decade_temp_n <- nfl_decade_temp_n %>%
  mutate(cold_game = if_else(temp <= 40, 1, 0)) 
data_gen_sum <- data_gen %>%
  group_by(city_type, cold_game) %>%
  summarize(mean_epa = mean(epa),
            var_epa = var(epa),
            n = n(),
            temp = mean(temp),
            wind = mean(wind),
            city_avg_temp = mean(city_avg_temp),
            cold_city = mean(city_type))
data_gen_sum

z_stat <- (data_gen_sum$mean_epa[4]-data_gen_sum$mean_epa[2]) / sqrt((data_gen_sum$var_epa[4]/data_gen_sum$n[4]) + (data_gen_sum$var_epa[2]/data_gen_sum$n[2]))
# qnorm(0.975)
#### 



#mean epa per play
nfl_decade_temp_play <- nfl_decade_temp %>%
  mutate(cold_game = if_else(temp <= 40, 1, 0)) 
data_play <- nfl_decade_temp_play %>%
  left_join(monthly_temp_abb, by = c('posteam' = 'City'))

data_play <- data_play %>%
  mutate(city_type = if_else(city_avg_temp <= 50, 1, 0))
nfl_decade_temp_n <- nfl_decade_temp_n %>%
  mutate(cold_game = if_else(temp <= 40, 1, 0)) 
data_play_sum <- data_play %>%
  group_by(posteam) %>%
  summarize(mean_epa = mean(epa),
            var_epa = var(epa),
            n = n(),
            temp = mean(temp),
            wind = mean(wind),
            city_avg_temp = mean(city_avg_temp),
            cold_city = mean(city_type))

data_play_sum %>%
  rename(mean_epa_play = "mean_epa")


colnames(posteam_cold_games_epa)
posteam_cold_games_epa_temp <- posteam_cold_games_epa %>% 
  pivot_wider(names_from = cold_game, values_from = mean_epa) 
colnames(posteam_cold_games_epa_temp)[4] = 'mean_epa_warm'
colnames(posteam_cold_games_epa_temp)[5] = 'mean_epa_cold'
# posteam_cold_games_epa %>% View()
# pivot longer to make cold game and mean epa their oown 
posteam_cold_games_epa %>% mutate()

#summary stats
sum_stats <- data%>%
  ungroup() %>%
  summarize(mean_temp = mean(temp), sd_temp = sd(temp), mean_wind = mean(wind),sd_wind = sd(wind), 
            mean_epa = mean(mean_epa), sd_epa = sd(mean_epa), mean_city_temp = mean(city_avg_temp), 
            sd_city_temp = sd(city_avg_temp))
sum_stats

sum_stats2 <- data%>%
  ungroup() %>% 
  summarize(min_temp = min(temp), max_temp = max(temp), min_wind = min(wind), max_wind = max(wind), 
            min_epa = min(mean_epa), max_epa = max(mean_epa), min_city_temp = min(city_avg_temp), 
            max_city_temp = max(city_avg_temp))
sum_stats2

#sum stats for cold games
sum_stats3 <- data%>%
  ungroup() %>%
  filter(cold_game == 1) %>%
  summarize(mean_temp = mean(temp), sd_temp = sd(temp), mean_wind = mean(wind),sd_wind = sd(wind), 
            mean_epa = mean(mean_epa), sd_epa = sd(mean_epa), mean_city_temp = mean(city_avg_temp), 
            sd_city_temp = sd(city_avg_temp))
sum_stats3

sum_stats4 <- data%>%
  ungroup() %>% 
  filter(cold_game == 1) %>%
  summarize(min_temp = min(temp), max_temp = max(temp), min_wind = min(wind), max_wind = max(wind), 
            min_epa = min(mean_epa), max_epa = max(mean_epa), min_city_temp = min(city_avg_temp), 
            max_city_temp = max(city_avg_temp))
sum_stats4

#sum stats for warm games
sum_stats5 <- data%>%
  ungroup() %>%
  filter(cold_game == 0) %>%
  summarize(mean_temp = mean(temp), sd_temp = sd(temp), mean_wind = mean(wind),sd_wind = sd(wind), 
            mean_epa = mean(mean_epa), sd_epa = sd(mean_epa), mean_city_temp = mean(city_avg_temp), 
            sd_city_temp = sd(city_avg_temp))
sum_stats5

sum_stats6 <- data%>%
  ungroup() %>% 
  filter(cold_game == 0) %>%
  summarize(min_temp = min(temp), max_temp = max(temp), min_wind = min(wind), max_wind = max(wind), 
            min_epa = min(mean_epa), max_epa = max(mean_epa), min_city_temp = min(city_avg_temp), 
            max_city_temp = max(city_avg_temp))
sum_stats6


#sum stats for cold city teams
sum_stats7 <- data%>%
  ungroup() %>%
  filter(cold_city == 1) %>%
  summarize(mean_temp = mean(temp), sd_temp = sd(temp), mean_wind = mean(wind),sd_wind = sd(wind), 
            mean_epa = mean(mean_epa), sd_epa = sd(mean_epa), mean_city_temp = mean(city_avg_temp), 
            sd_city_temp = sd(city_avg_temp))
sum_stats7

sum_stats8 <- data%>%
  ungroup() %>% 
  filter(cold_city == 1) %>%
  summarize(min_temp = min(temp), max_temp = max(temp), min_wind = min(wind), max_wind = max(wind), 
            min_epa = min(mean_epa), max_epa = max(mean_epa), min_city_temp = min(city_avg_temp), 
            max_city_temp = max(city_avg_temp))
sum_stats8

#sum stats for warm city teams
sum_stats9 <- data%>%
  ungroup() %>%
  filter(cold_city == 0) %>%
  summarize(mean_temp = mean(temp), sd_temp = sd(temp), mean_wind = mean(wind),sd_wind = sd(wind), 
            mean_epa = mean(mean_epa), sd_epa = sd(mean_epa), mean_city_temp = mean(city_avg_temp), 
            sd_city_temp = sd(city_avg_temp))
sum_stats9

sum_stats10 <- data%>%
  ungroup() %>% 
  filter(cold_city == 0) %>%
  summarize(min_temp = min(temp), max_temp = max(temp), min_wind = min(wind), max_wind = max(wind), 
            min_epa = min(mean_epa), max_epa = max(mean_epa), min_city_temp = min(city_avg_temp), 
            max_city_temp = max(city_avg_temp))
sum_stats10
