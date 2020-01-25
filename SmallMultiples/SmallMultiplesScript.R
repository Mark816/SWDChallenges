#####SWD Challenge - Small Multiples#####
library(tidyverse)
library(readr)
library(dplyr)
library(purrr)

#initial data load of employee size and union
files <- dir(pattern = "*Z2_with_ann.csv")

bp_data <- files %>% 
  map(function(file_name){
    assign(x = str_remove(file_name,"_00CZ2_with_ann.csv"),
         value = read_csv(paste0(file_name),skip =1),
         envir = .GlobalEnv)}) %>% 
  reduce(rbind) %>%
  select(-Id2,
         -`Employment size of establishment`, 
         -`Geographic identifier code`,
         -`2012 NAICS code`) %>%
  rename(area_name = `Geographic area name`,
         naics_code = `Meaning of 2012 NAICS code`,
         size = `Meaning of Employment size of establishment`,
         establishments = `Number of establishments`)

bp_data2 <- bp_data %>% 
  separate(area_name, into = c("zip", "zip_code", "city"),sep = '\\s')
  
bp_data2$city <- str_remove_all(bp_data2$city,"[(,]")
bp_data2$zip_code <- as.integer(bp_data2$zip_code)

bp_data3 <- bp_data2 %>% 
  select(-zip) %>%
  filter(naics_code == "Total for all sectors")

bp_data3$size <- str_replace_all(bp_data3$size,
                             c("All establishments" = "total",
                               "Establishments with 1 to 4 employees" = "< 4",
                               "Establishments with 5 to 9 employees" ="5-9",
                               "Establishments with 10 to 19 employees" = "10-19",
                               "Establishments with 20 to 49 employees" = "20-49",
                               "Establishments with 50 to 99 employees"= "50-99",
                               "Establishments with 100 to 249 employees" = "100-249",
                               "Establishments with 250 to 499 employees" = "250-499",
                               "Establishments with 500 to 999 employees" = "500-999",
                               "Establishments with 1,000 employees or more" = "1,000+"))
                          
#Explore total establishments for STL by employment size
stl_total <- bp_data3 %>% 
  group_by(zip_code,Year) %>%
  summarize(total_est = sum(establishments))

#lattice scatterplots for size vs. establishments by zip by year
stl_chart <- stl_total %>%
  ggplot(aes(size,total_est,colour = zip_code)) + 
  geom_point() + 
  facet_grid(~Year) + 
  theme_minimal() +
  labs(x= "Number of Employees", y = "Number of Companies",
       title = "St. Louis, MO Industry Growth")
stl_chart

#zip code list to combine with payroll data
zip_code <- bp_data2 %>%
  select(zip_code) %>%
  unique() %>%
  mutate(stl = "stl")

#load payroll data
payfiles <- dir(pattern = "*Z1_with_ann.csv")

col_names <- names(read_csv("BP_2016_00CZ1_with_ann.csv",skip = 1, n_max = 0))

bp_paydata <- payfiles %>% 
  map(function(file_name2){
    assign(x = str_c("PR","_",str_remove(file_name2,"_00CZ1_with_ann.csv")),
           value = read_csv(paste0(file_name2),col_names = col_names, skip =2 ),
           envir = .GlobalEnv)}) %>% 
  reduce(rbind) %>%
  select(-Id2,
         -`Geographic identifier code`,
         -`2012 NAICS code`) %>%
  rename(area_name = `Geographic area name`,
         naics_code = `Meaning of 2012 NAICS code`,
         employees = `Paid employees for pay period including March 12 (number)`,
         establishments = `Number of establishments`)

bp_paydata2 <- bp_paydata %>% 
  separate(area_name, into = c("zip", "zip_code","city"),sep = '\\s', extra = "merge")

bp_paydata2$city <- str_remove_all(bp_paydata2$city, "[(,)]")

bp_paydata3 <- bp_paydata2 %>%
  separate(city, into = c("city","state"), sep = '\\s', extra = "drop")

bp_paydata3$employees <- as.integer(bp_paydata3$employees)
bp_paydata3$zip_code <- as.integer(bp_paydata3$zip_code)
bp_paydata4 <- bp_paydata3 %>% 
  select(-zip,
         -state) %>%
  filter(naics_code == "Total for all sectors")

#join to St. Louis city area codes for subsetting
stl_data <- left_join(bp_paydata4,zip_code) %>%
  filter(stl == "stl",
         employees > 0)

bp_paydata5 <- stl_data %>%
  group_by(Year,city) %>%
  summarize(employees = sum(as.integer(employees)), 
            annual_payroll = sum(as.integer(`Annual payroll ($1,000)`)), 
            establishments = sum(as.integer(establishments)))

#chart for new data 
stl_chart2 <- bp_paydata5 %>%
  ggplot(aes(employees,annual_payroll,colour = city)) + 
  geom_point() + 
  facet_grid(~Year) + 
  theme_minimal() +
  labs(x= "Number of Employees", y = "Annual Payroll",
       title = "St. Louis, MO Industry Growth") +
  theme(legend.position = "none")
stl_chart2