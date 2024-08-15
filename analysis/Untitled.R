# for analysis
pacman::p_load(tidyverse, readr, dplyr, magrittr, here, readxl, ggplot2, broom, gridExtra)
str(master_data_test)
str(df_master)

# master_data_testとdf_master

# Descriptive statistics
# count NA
na_counts <- colSums(is.na(master_data_test))
na_counts

# summary statistics
summary(master_data_test)
# what's the "semester calender?"
masterdata_summary <- master_data_test %>% 
  mutate(swich_group = ifelse(is.na(transition_year), "Never switcher", "Switcher"))

calculate_summary <- function(data){
  df_summary <- data %>% 
    summarise(
      Mean_instatetuition = mean(instatetuition, na.rm = TRUE),
      Cohort_size = mean(totcohortsize, na.rm = TRUE),
      Women_cohort = mean(w_cohortsize, na.rm = TRUE),
      Men_cohort = mean(m_cohortsize, na.rm = TRUE),
      Fouryear_graduation_rate = mean(gradrate4yr, na.rm = TRUE),
      Fouryear_women_graduation_rate = mean(womengradrate4yr, na.rm = TRUE),
      Fouryear_men_graduation_rate = mean(mengradrate4yr, na.rm = TRUE)
    )
}

summary_never_switcher <- masterdata_summary %>% 
  filter(swich_group == "Never switcher") %>% 
  calculate_summary()
summary_all <- masterdata_summary %>% 
  calculate_summary()
summary_switcher <- masterdata_summary %>% 
  filter(swich_group == "Switcher") %>% 
  calculate_summary()

combined_summary <- bind_rows(summary_all, summary_never_switcher, summary_switcher)


# 3. 4年卒業率の平均推移four year graduation rates(gradrate4yr)

gf_gradrate <- master_data_test %>% 
  group_by(year) %>% 
  summarise(average_gradrate4yr = sum(gradrate4yr)/n()) %>% 
  ggplot(aes(x = year, y = average_gradrate4yr))+
  geom_line()
gf_gradrate

# 4. semester導入率

gf_shareofsemester <- master_data_test %>% 
  group_by(year) %>% 
  summarise(share_of_semester = sum(semester) / n()) %>% 
  ggplot(aes(x = year, y = share_of_semester))+
  geom_line(linetype = "dashed")
gf_shareofsemester

# 5. scatter figure----
scatter_function <- function(data, x_var, y_var){
  x_var = sym(x_var)
  
  ggplot(data, aes(x = !!x_var, y = !!y_var)+
           geom_point(size = .5, alpha = .5, col = "darkseagreen4")+
           labs(x = quo_name(x_var), y = quo_name(y_var))
         
           )
}
x_vars <- c("per_women_cohort", "per_white_cohort", "instatetuition")
plots <- map(x_vars,  ~ scatter_function(master_data_test, .x, "gradrate4yr"))

## 女子学生比率
gf_shareofwomen <- master_data_test %>% 
  ggplot(aes(x = per_women_cohort, y = gradrate4yr))+
  geom_point(size = .5, alpha = .5, col = "darkseagreen4")

## 白人学生割合
gf_shareofwhite <- master_data_test %>% 
  ggplot(aes(x = per_white_cohort, y = gradrate4yr))+
  geom_point(size = .5, alpha = .5, col = "coral")

## 学費(instatetuition)
gf_instatetuition <- master_data_test %>% 
  ggplot(aes(x = instatetuition, y = gradrate4yr))+
  geom_point(size = .5, alpha = .5, col = "blue3")

grid.arrange(gf_shareofwomen, gf_shareofwhite, gf_instatetuition)

# regression analysis----
library(broom)
rg <- lm(data = master_data_test, gradrate4yr ~ year_dummy) %>% 
  tidy()
rg
stargazer::stargazer(rg)

