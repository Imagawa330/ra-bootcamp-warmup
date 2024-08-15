# data cleaning
# (a) Semester Data----
pacman::p_load(tidyverse, readr, dplyr, magrittr, here, readxl)

# read data
semester_dummy_1 <- read_csv("~/Desktop/RAcamp/warmup training package/01_data/raw/semester_dummy/semester_data_1.csv")
semester_dummy_2 <- read_csv("~/Desktop/RAcamp/warmup training package/01_data/raw/semester_dummy/semester_data_2.csv")

# rename column
colnames(semester_dummy_1) <- semester_dummy_1[1, ] 
semester_dummy_1 <- semester_dummy_1[-1, ]

names(semester_dummy_1)
names(semester_dummy_2)
semester_dummy_2 %<>% rename("unitid" = "x1", "instnm" = "x2", "semester" = "x3", "quarter" = "x4", 
                             "year" = "x5", "Y" = "x6")

# delete Y
data_semester <- rbind(semester_dummy_1, semester_dummy_2) %>% select(-Y)


transition_years <- data_semester %>% 
  group_by(instnm) %>% 
  mutate(year_adopt = if_else(quarter == 0 & lag(quarter) == 1 & semester == 1 & lag(semester) == 0, year, NA)) %>% 
  summarize(transition_year = na.omit(year_adopt))

data_semester %<>% left_join(transition_years, by = "instnm")

# add transition_dummy row
data_semester %<>% mutate(year_dummy = if_else(year >= transition_year, 1, 0))

str(data_semester)
data_semester$unitid <- as.numeric(data_semester$unitid)
data_semester$year <- as.numeric(data_semester$year)
data_semester$semester <- as.numeric(data_semester$semester)
data_semester$quarter <- as.numeric(data_semester$quarter)
data_semester$transition_year <- as.numeric(data_semester$transition_year)


# (b) Gradrate data----
# 1994 was excluded
# data combining
file_path <- "~/Desktop/RAcamp/warmup training package/01_data/raw/outcome/"
all_data <- list()
for (year in 1991:2016) {
  file_name <- paste0(file_path, year, ".xlsx")
  tryCatch({
    data <- read_excel(file_name)
    all_data[[as.character(year)]] <- data  
  }, error = function(e){
    message(paste("File for year", year, "not found. Skipping."))
  })
}
gradrate_data <- bind_rows(all_data)

str(gradrate_data)

gradrate_data %<>% mutate_if(is.character, as.numeric)

gradrate_data %<>% 
  mutate(womengradrate4yr = women_gradrate_4yr * 0.01,
         mengradrate4yr = m_4yrgrads / m_cohortsize,
         gradrate4yr = tot4yrgrads / totcohortsize) %>% 
  mutate(mengradrate4yr = formatC(mengradrate4yr, digits = 3, flag = "#"),
         gradrate4yr = formatC(gradrate4yr, digits = 3, flag = "#")) %>% # 3 significant figures 
  filter(!year %in% 2011:2016) %>% 
  mutate(mengradrate4yr = as.numeric(mengradrate4yr),
         gradrate4yr = as.numeric(gradrate4yr))
  
str(gradrate_data)



df_master <- read_csv("~/Desktop/RAcamp/warmup training package/01_data/intermediate/master.csv")

# (c) Covariates data----
data_cov <- read_excel("~/Desktop/RAcamp/warmup training package/01_data/raw/covariates/covariates.xlsx")
names(data_cov)
data_cov <- data_cov %>% 
  rename("unitid" = "university_id") %>% 
  mutate(unitid = stringr::str_remove(unitid, "aaaa$"))

# change to wider
data_cov_wide <- data_cov %>% 
  pivot_wider(names_from = "category", 
              values_from = "value")

# download outcome data
data_outcome <- read_csv("~/Desktop/RAcamp/warmup training package/01_data/intermediate/clean_outcome.csv")

# prepare year adjustment
unique(data_outcome$year)# 1991-2010, except for 1994
unique(data_cov_wide$year)# delete 1987-1990, 1994, 2011-2016
delete_year <- c(1987:1990, 1994, 2011:2016)

# prepare unitid adjustment
outcome_unitid <- data_outcome$unitid


data_cov_final <-  data_cov_wide %>% 
  filter(!year %in% delete_year) %>% # adjust year
  filter(unitid %in% outcome_unitid) # adjust unitid
  
# just confirm  
unique(data_cov_final$year)

data_cov_final %<>% mutate_if(is.character, as.numeric)
str(data_cov_final)

# (4) Master data----

names(data_semester)
names(gradrate_data)
names(data_cov_final)

master_data_test <- data_semester %>% 
  left_join(data_cov_final, by = c("unitid", "year")) %>% 
  left_join(gradrate_data, by = c("unitid", "year"))
str(master_data_test)


# it's for analysis...----
## 女性学生比率per_women_cohortと白人学生割合per_white_cohortの追加
master_data_test %<>% mutate(per_women_cohort = w_cohortsize / totcohortsize,
                             per_white_cohort = white_cohortsize / totcohortsize)
str(master_data_test)
