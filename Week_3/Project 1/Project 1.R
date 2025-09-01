# library(dplyr)
# library(readr)
# library(magrittr)
# library(ggplot2)
# library(tidyr)
# library(officer)

data_scientists <- read.csv("C:Week_5/Data/r project data.csv")

str(data_scientists)
View(data_scientists)
#My thoughts on what to look at:
#global average salary - a good starting point
##distribution of salary by location
##salary by employment type
##remote salary vs non-remote + location

#I want to do a step to remove any missing or NA salaries first:

data_scientists <- data_scientists %>%
  filter(!is.na(salary_in_usd) & salary_in_usd > 0)

#I want to change a few columns to factor so it can be categorical
data_scientists <- data_scientists %>%
  mutate(across(c(work_year,experience_level
                  ,employment_type,job_title
                  , company_size,remote_ratio
                  , employee_residence, job_title)
                ,as.factor)
  )

str(data_scientists)

#I also want to know the unique roles/job titles:

unique_jobs <- unique(data_scientists$job_title)

unique_remote <- unique(data_scientists$remote_ratio)

unique_remote

print(unique_jobs)

data_scientist_role_global <- data_scientists %>%
  filter(grepl("Data Scientist", job_title, ignore.case = TRUE)) %>%
  filter(employee_residence != "US") %>%
  filter(company_size == "M")

data_scientist_role_US <- data_scientists %>%
  filter(grepl("Data Scientist", job_title, ignore.case = TRUE)) %>%
  filter(employee_residence == "US") %>%
  filter(company_size == "M")

View(data_scientist_role_US)

global_avg_salary <- mean(data_scientist_role_global$salary_in_usd, na.rm = TRUE)
global_min_salary <- min(data_scientist_role_global$salary_in_usd, na.rm = TRUE)
global_max_salary <- max(data_scientist_role_global$salary_in_usd, na.rm = TRUE)
global_median_salary <- median(data_scientist_role_global$salary_in_usd, na.rm = TRUE)
global_IQR_salary <- IQR(data_scientist_role_global$salary_in_usd, na.rm = TRUE)
#IQR shows the spread of salary is centered around $87274. IQR shows 
#the spread of the middle 50% of the data

#make into a presentable dataframe for presentation:

global_summary_stats <- data.frame(
  Statistic = c("Global Average salary"
            , "Global Minimum Salary"
            , "Global Max Salary"
            , "Global Median Salary"
            , "Global IQR Salary"),
  Value = c(global_avg_salary
            , global_min_salary
            , global_max_salary
            , global_median_salary
            , global_IQR_salary)
)

global_summary_stats$Value <- scales::dollar(global_summary_stats$Value)


View(global_summary_stats)

#creating a graph to quickly show the global Salary Distribution stats 
#alongside a table:
global_salary_distribution_plot <-data_scientist_role_global %>%
  ggplot(aes(y = salary_in_usd)) +
  geom_boxplot(fill = "lavender", outlier.colour = "orange") +
  geom_hline(aes(yintercept =  global_avg_salary),
             linetype = "dashed",
             color = "blue") +
  geom_hline(aes(yintercept =  global_min_salary),
             linetype = "dashed",
             color = "red") +
  geom_hline(aes(yintercept =  global_max_salary),
             linetype = "dashed",
             color = "purple") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  ylab("Salary in USD") +
  ggtitle("Global Salary Distribution") +
  theme_classic()

global_salary_distribution_plot

##Now since we are in the US, its a good idea to subset to focus on the US

average_us_salary <- aggregate(salary_in_usd ~ 1, data_scientist_role_US, mean, na.rm = TRUE)$salary_in_usd
median_us_salary <- aggregate(salary_in_usd ~ 1, data_scientist_role_US, median, na.rm = TRUE)$salary_in_usd
min_us_salary <- aggregate(salary_in_usd ~ 1, data_scientist_role_US, min, na.rm = TRUE)$salary_in_usd
max_us_salary <- aggregate(salary_in_usd ~ 1, data_scientist_role_US, max, na.rm = TRUE)$salary_in_usd
IQR_us_salary <- aggregate(salary_in_usd ~ 1, data_scientist_role_US, IQR, na.rm = TRUE)$salary_in_usd


US_salaries <- data.frame(
  Statsistic = c("Average US Salary",
            "Median US Salary",
            "Minimum US Salary",
            "Max US Salary",
            "IQR US Salary"),
  Value = c(average_us_salary
            ,median_us_salary
            ,min_us_salary
            ,max_us_salary
            ,IQR_us_salary)
)

View(US_salaries)


US_salaries$Value <- scales::dollar(US_salaries$Value)
View(US_salaries)

#Distribution of US salaries

us_salary_distribution_plot <- data_scientist_role_US %>%
  ggplot(aes(y = salary_in_usd)) +
  geom_boxplot(fill = "pink", outlier.colour = "violet") +
  geom_hline(yintercept = average_us_salary
             , linetype = "dashed"
             , color = "blue") +
  geom_hline(yintercept = min_us_salary
             , linetype = "dashed"
             , color = "salmon4") +
  geom_hline(yintercept = max_us_salary
             , linetype = "dashed"
             , color = "darkseagreen")+
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  ylab("Salary in USD") +
  ggtitle("US Salary Distribution") +
  theme_classic()

us_salary_distribution_plot




#salary by Role and experience level in the US: 

  salary_by_Role_US <- data_scientist_role_US %>%
  group_by(job_title, experience_level) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE)
            ,min_salary = min(salary_in_usd, na.rm = TRUE)
            ,median_salary = median(salary_in_usd, na.rm = TRUE)
            ,max_salary = max(salary_in_usd, na.rm = TRUE)
            ,.groups = "drop")


avg_by_job_and_experience_plot <- salary_by_Role_US %>%
  ggplot(aes(x = job_title
             ,y = avg_salary
             , fill = experience_level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::dollar_format()(avg_salary)),
            position = position_stack(vjust = 0.5),
            size = 2.4, color = "black") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  ggtitle("Average salary of different Data scientists in the US") +
  xlab("Job Title") +
  ylab("Average Salary (USD)") +
  scale_fill_discrete(labels = c("Entry-level"
                                 ,"Mid-level"
                                 ,"Senior-level"
                                 ,"Executive-level/Director")) +
  theme_classic() +
  theme(legend.key.size = unit(0.3, "cm")
        , legend.position = "bottom"
        , legend.text = element_text(size = 10)
        , legend.title = element_blank()) +
  coord_flip()

avg_by_job_and_experience_plot

#Salary by employment type for the US:

salary_by_employment_type_us <- data_scientist_role_US %>%
  group_by(job_title, employment_type) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE),
            min_salary = min(salary_in_usd, na.rm = TRUE),
            median_salary = median(salary_in_usd, na.rm = TRUE),
            max_salary = max(salary_in_usd, na.rm = TRUE))

View(salary_by_employment_type_US)


salary_by_employment_type_us_plot <- salary_by_employment_type_us %>%
  ggplot(aes(x = job_title
             ,y = avg_salary
             , fill = employment_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::dollar_format()(avg_salary)),
            position = position_stack(vjust = 0.5),
            size = 2.4, color = "black") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  ggtitle("Average salary of different Data scientists in the US") +
  xlab("Job Title") +
  ylab("Average Salary (USD)") +
  scale_fill_discrete(labels = c("Contract"
                                 ,"Full-Time")) +
  theme_classic() +
  theme(legend.key.size = unit(0.3, "cm")
        , legend.position = "bottom"
        , legend.text = element_text(size = 10)
        , legend.title = element_blank()) +
  coord_flip()

salary_by_employment_type_us_plot


##remote salary vs non-remote for US and Global:


data_scientist_role_global$Region <- "Global"

data_scientist_role_US$Region <- "US"


us_global_combined <- rbind(data_scientist_role_global
                            , data_scientist_role_US)

aggregated_us_and_global <- us_global_combined %>%
  group_by(remote_ratio, Region) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE),
            min_salary = min(salary_in_usd, na.rm = TRUE),
            max_salary = max(salary_in_usd, na.rm = TRUE),
            median_salary = median(salary_in_usd, na.rm = TRUE),
            .groups = "drop"
            )
    
View(aggregated_us_and_global)


aggregated_us_and_global_plot <- aggregated_us_and_global %>%
  ggplot(aes(x =remote_ratio
             , fill = Region)
         ) +
  geom_bar(aes(y = avg_salary), stat = "identity"
           , position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = min_salary, ymax = max_salary)
                , stat = "identity"
                , position = position_dodge(width = 0.7)
                , width = 0.2) +
  geom_point(aes(y = median_salary), size = 3
             , shape = 17
             , color = "purple"
             , stat = "identity"
             , position = position_dodge(width = 0.7)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  ggtitle("Comparing Global vs US Salaries by Remote work") +
  xlab("Remote Ratio") +
  ylab("Average Salary (USD)")


aggregated_us_and_global_plot




project1 <- read_pptx()

#Title slide

project1 <- project1 %>%
  add_slide(layout = "Title Slide"
            , master = "Office Theme") %>%
  ph_with(type = "title"
          , location = ph_location_type(type = "ctrTitle")
          , value = "How to be competitive when looking for Data Scientists") %>%
  ph_with(type = "body"
          , type = "body", location = ph_location_type(type = "subTitle")
          , value = "Auriana Anderson")


#slide 1: Global vs US salary

#Global vs US image saves
ggsave("Global Salary Distribution.png"
       , plot = global_salary_distribution_plot
       , width = 10
       , height = 7
       , dpi = 300)

ggsave("US Salary Distribution.png"
       , plot = us_salary_distribution_plot
       , width = 10
       , height = 7
       , dpi = 300)

project1 <- project1 %>%
  add_slide(layout = "Title and Content"
            , master = "Office Theme") %>%
  ph_with(type = "body"
          , location = ph_location_left()
          , value = "") %>%
  ph_with(type = "body"
          , location = ph_location_right()
          , value = "") %>%
  
  ph_with(external_img("Global Salary Distribution.png"
                       , width = 4
                       , height = 3.5)
          ,location = ph_location_left()) %>%

  ph_with(external_img("US Salary Distribution.png"
                       , width = 4
                       , height = 3.5),
          location = ph_location_right()) %>%
  
  ph_with(value = global_summary_stats
          , ph_location_type(type = "body")) %>%
  
  ph_with(value = US_salaries
          , ph_location_type(type = "body"))

# slide 2: Job title, experience level, employment type

ggsave("Salary by job title and Experience.png"
       , plot = avg_by_job_and_experience_plot
       , width = 10
       , height = 7
       , dpi = 300)


project1 <- project1 %>%
  
  add_slide(layout = "Title and Content"
            , master = "Office Theme") %>%
  
  ph_with(type = "title", location = ph_location_type(type = "title")
          , value = "Salary by job Title and Experience") %>%
  
  ph_with(external_img("Salary by job title and Experience.png"
                       , width = 6, height = 5.5)
          , location = ph_location(left = 1
                                   , top = 1))


#slide 3: Salary by employment type:

ggsave("Salary by employment Type.png"
       , plot = salary_by_employment_type_us_plot
       , width = 10
       , height = 7
       , dpi = 300)


project1 <- project1 %>%
  
  add_slide(layout = "Title and Content"
            , master = "Office Theme") %>%
  
  ph_with(type = "title", location = ph_location_type(type = "title")
          , value = "Salary by Employment Type") %>%
  
  ph_with(external_img("Salary by employment Type.png"
                       , width = 6, height = 5.5),
          location = ph_location(left = 1
                                 , top = 1))

# Slide 4:

aggregated_us_and_global_plot
ggsave("Comparison of Global vs US salaries based on Remote work.png"
       , plot = aggregated_us_and_global_plot
       , width = 10
       , height = 7
       , dpi = 300)

project1 <- project1 %>%
  
  add_slide(layout = "Title and Content"
            , master = "Office Theme") %>%
  
  ph_with(type = "title", location = ph_location_type(type = "title")
          , value = "Salary by Remote work") %>%
  
  ph_with(external_img("Comparison of Global vs US salaries based on Remote work.png"
                       , width = 6, height = 4)
          ,location = ph_location(left = 1
                                  , top = 1))

project1 <- project1 %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  
  ph_with(type = "title"
          , location = ph_location_type(type = "title")
          , value = "Final Recommendations") %>%
  
  ph_with(type = "body"
          , location = ph_location_type(type = "body")
          , value = "US_salaries")


print(project1, target = "Data_scientis_salary_analysis.pptx")
