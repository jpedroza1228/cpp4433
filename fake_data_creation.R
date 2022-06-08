library(tidyverse)

set.seed(04112022)

between <- data.frame(group1 = rnorm(n = 40,
                                   mean = 20,
                                   sd = 5),
                      group2 = rnorm(n = 40,
                                        mean = 30,
                                        sd = 2.45)
                      )

between

between <- between %>% 
  pivot_longer(cols = 1:2,
               names_to = 'iv1',
               values_to = 'dv_values') %>% 
  mutate(iv2 = rep(c(1,2),
                   times = 40),
         iv2 = sample(iv2))

set.seed(04112022)

between <- between %>% 
  mutate(q1 = rpois(n = 80, lambda = 3.5),
         q2 = rpois(n = 80, lambda = 3),
         q3 = rpois(n = 80, lambda = 4),
         q4 = rpois(n = 80, lambda = 3))

map(
  between,
    ~ggplot(data = between,
            aes(.x)) + 
  geom_bar()
)

summary(aov(dv_values ~ iv1*iv2,
            data = between))

# write.csv(between, 'sample_between_subjects.csv')


set.seed(04112022)

mixed <- data.frame(
  time1 = rnorm(n = 40,
                mean = 50,
                sd = 7.3),
  time2 = rnorm(n = 40,
                mean = 40,
                sd = 3.21),
  time3 = rnorm(n = 40,
                mean = 80,
                sd = 6.31)
                    )

mixed <- mixed %>% 
  mutate(between_iv = rep(c(1, 2),
                          times = 20),
         between_iv = sample(between_iv))

mixed

set.seed(04112022)

mixed <- mixed %>% 
  mutate(q1_t1 = rpois(n = 40, lambda = 3.5),
         q2_t1 = rpois(n = 40, lambda = 3),
         q3_t1 = rpois(n = 40, lambda = 3),
         q4_t1 = rpois(n = 40, lambda = 3),
         q1_t2 = rpois(n = 40, lambda = 3.5),
         q2_t2 = rpois(n = 40, lambda = 3),
         q3_t2 = rpois(n = 40, lambda = 3),
         q4_t2 = rpois(n = 40, lambda = 3),
         q1_t3 = rpois(n = 40, lambda = 3.5),
         q2_t3 = rpois(n = 40, lambda = 3),
         q3_t3 = rpois(n = 40, lambda = 3),
         q4_t3 = rpois(n = 40, lambda = 3))

mixed

map(
  mixed,
  ~ggplot(data = mixed,
          aes(.x)) + 
    geom_bar()
)

psych::describe(mixed, na.rm = TRUE)

# write.csv(mixed, 'sample_mixed.csv')


# make some fake data for maya - depression and 
set.seed(0411202)

maya_data <- data.frame(group1 = rnorm(n = 40,
                                     mean = 9.83,
                                     sd = 3.96),
                      group2 = rnorm(n = 40,
                                     mean = 8.99,
                                     sd = 3.26)
)

maya_data

maya_data <- maya_data %>% 
  pivot_longer(cols = 1:2,
               names_to = 'iv1',
               values_to = 'dv_values') %>% 
  mutate(iv2 = rep(c(1,2, 3, 4, 5),
                   times = 16),
         iv2 = sample(iv2))

maya_data <- maya_data %>% 
  mutate(
    groups = recode(iv1,
                    'group1' = '1',
                    'group2' = '2'),
    groups = as.factor(groups)
         ) %>% 
  rename(time_spent_social_media = iv2) %>% 
  select(-iv1)

maya_data

# haven::write_sav(maya_data, 'C:/Users/cpppe/Desktop/github_projects/cpp4433/maya_data.sav')

maya <- rio::import(here::here('maya_data.sav'))

# View(maya)


# steven data

steven <- rio::import(here::here('cleaned_data.sav')) %>% 
  janitor::clean_names() %>% 
  arrange(start_date) %>% 
  drop_na(depression)

set.seed(4202022)

steven_new <- steven %>% 
  mutate(
    group = rep(x = c(1, 2),
                times = 15,
                length.out = 26),
    group_random = sample(group,
                          size = 26),
    video_pdf = recode(group_random,
                       '1' = 'pamphlet',
                       '2' = 'video'),
    video_pdf = as.factor(video_pdf)
  )
View(steven_new)

steven_new %>% 
  count(video_pdf)

# haven::write_sav(steven_new, 'C:/Users/cpppe/Desktop/github_projects/cpp4433/steven_new.sav')


# lex and samantha data

lex <- rio::import(here::here('stress_memory.sav')) %>% 
  janitor::clean_names() %>% 
  arrange(start_date)

set.seed(4202022)

lex_new <-
  lex %>% 
    mutate(
      group = rep(x = c(1, 2),
                  times = 30,
                  length.out = 54),
      group_random = sample(group,
                            size = 54),
      timer_group = recode(group_random,
                           '1' = 'visible_timer',
                           '2' = 'non_visible_timer'),
      timer_group = as.factor(timer_group)
    )

# haven::write_sav(lex_new, 'C:/Users/cpppe/Desktop/github_projects/cpp4433/lex_new.sav')


kayla <- 
  rio::import(here::here('kayla_data.sav')) %>% 
  janitor::clean_names() %>% 
  arrange(start_date)

kayla <- 
  kayla %>% 
  filter(distribution_channel != 'preview')

kayla <- 
  kayla %>% 
  unite(
    condition,
        c(
          condition_a_powerpoint, condition_b_video
          ),
    na.rm = TRUE
    ) %>% 
  mutate(
    condition = str_remove(
    condition,
    '_'
  ),
  condition = recode(
    condition,
    'A' = '1',
    'B' = '2'
  ),
  condition = as.factor(
    condition
    )
  )


# haven::write_sav(kayla, 'C:/Users/cpppe/Desktop/github_projects/cpp4433/kayla_new.sav')

music <- rio::import(here::here('music_study.sav')) %>% 
  janitor::clean_names() %>% 
  arrange(start_date)

# View(music)

music <- 
  music %>% 
  filter(distribution_channel != 'preview')

# music %>% 
#   View()

music <- 
music %>% 
  mutate(
    group = rep(x = c(1, 2),
                times = 40,
                length.out = 71),
    group_random = sample(group,
                          size = 71),
    music_type = recode(group_random,
                         '1' = 'lofi',
                         '2' = 'electronic'),
    music_type = as.factor(music_type)
  )

# View(music)

music <-
music %>% 
  mutate(
    age = q1 +17,
    q1_quiz = case_when(q1_0 == 3 ~ 1,
                        q1_0 != 3 ~ 0),
    q2_quiz = case_when(q2_1 == 1 ~ 1,
                        q2_1 != 1 ~ 0),
    q3_quiz = case_when(q3_0 == 3 ~ 1,
                        q3_0 != 3 ~ 0),
    q4_quiz = case_when(q4_0 == 4 ~ 1,
                        q4_0 != 4 ~ 0),
    q5_quiz = case_when(q5 == 4 ~ 1,
                        q5 != 4 ~ 0),
    q6_quiz = case_when(q6 == 1 ~ 1,
                        q6 != 1 ~ 0),
    quiz_score = q1_quiz + q2_quiz + q3_quiz + q4_quiz + q5_quiz + q6_quiz,
    q1_1 = case_when(q1_1 == 1 ~ 0,
                     q1_1 == 2 ~ 1,
                     q1_1 == 3 ~ 2,
                     q1_1 == 4 ~ 3,
                     q1_1 == 5 ~ 4),
    q2_2 = case_when(q2_2 == 1 ~ 0,
                     q2_2 == 2 ~ 1,
                     q2_2 == 3 ~ 2,
                     q2_2 == 4 ~ 3,
                     q2_2 == 5 ~ 4),
    q3_1 = case_when(q3_1 == 1 ~ 0,
                     q3_1 == 2 ~ 1,
                     q3_1 == 3 ~ 2,
                     q3_1 == 4 ~ 3,
                     q3_1 == 5 ~ 4),
    q4_1 = case_when(q4_1 == 1 ~ 0,
                     q4_1 == 2 ~ 1,
                     q4_1 == 3 ~ 2,
                     q4_1 == 4 ~ 3,
                     q4_1 == 5 ~ 4),
    q5_0 = case_when(q5_0 == 1 ~ 0,
                     q5_0 == 2 ~ 1,
                     q5_0 == 3 ~ 2,
                     q5_0 == 4 ~ 3,
                     q5_0 == 5 ~ 4),
    q6_0 = case_when(q6_0 == 1 ~ 0,
                     q6_0 == 2 ~ 1,
                     q6_0 == 3 ~ 2,
                     q6_0 == 4 ~ 3,
                     q6_0 == 5 ~ 4),
    q7 = case_when(q7 == 1 ~ 0,
                     q7 == 2 ~ 1,
                     q7 == 3 ~ 2,
                     q7 == 4 ~ 3,
                     q7 == 5 ~ 4),
    q8 = case_when(q8 == 1 ~ 0,
                     q8 == 2 ~ 1,
                     q8 == 3 ~ 2,
                     q8 == 4 ~ 3,
                     q8 == 5 ~ 4),
    q9 = case_when(q9 == 1 ~ 0,
                     q9 == 2 ~ 1,
                     q9 == 3 ~ 2,
                     q9 == 4 ~ 3,
                     q9 == 5 ~ 4),
    q10 = case_when(q10 == 1 ~ 0,
                     q10 == 2 ~ 1,
                     q10 == 3 ~ 2,
                     q10 == 4 ~ 3,
                    q10 == 5 ~ 4),
    anxiety = (q1_1 + q2_2 + q3_1 + q4_1 + q5_0 + q6_0 + q7 + q8 + q9 + q10)/10,
    quiz_groups = case_when(quiz_score < 3 ~ 'low_score',
                            quiz_score >= 3 ~ 'high_score'),
    quiz_groups = as.factor(quiz_groups)
  )

ggplot(data = music,
       aes(quiz_score)) + 
  geom_histogram(bins = 10, color = 'white')

music %>% 
  count(quiz_groups)

psych::describe(music$quiz_score)

summary(lm(anxiety ~ quiz_groups*music_type, data = music))

# haven::write_sav(music, 'C:/Users/cpppe/Desktop/github_projects/cpp4433/music.sav')
