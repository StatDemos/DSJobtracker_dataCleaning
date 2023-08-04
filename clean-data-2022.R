library(tidyverse)
library(readxl)

data <- read_xlsx("DSjobtracker_2022.xlsx")

# Filter the columns

column_names <- c("Job_Level", "Industry", "Duplicate/Unique", "Experience UL (in years)", 
                  "Employment type",  
                  "Work Setting", "Salary UL", "Modelling", "VBA macro", "EDA",                                                 
                  "Hypothesis Testing", "Data Analytics", "Statistics",                                           
                  "Data Cleaning", "Survey Design",  "Dashboard Creation",                                   
                  "Data Governance", "Data Stratergy", "Data Policy",                                          
                  "Data Engineering", "Software_development_process", "Data Structuring",
                  "Vertica", "SOQL", "SOSL", "Quickbooks", "Outlook", "Data Studio",
                  "PostgreSQL", "Shopify", "Looker", "Snowflake", "MixPanel",
                  "EC2", "EMR", "Athena", "DynamoDB", "Safe Habour", "Glue",                                                 
                  "Lambda", "RDS", "Kinesis", "ECS", "Fargate", "Analytical Skills",                                    
                  "Intepretation", "Project Management", "Entrepreneurial Skills",                               
                  "Motivation", "Time Management", "Report Writting",  "Multi-tasking",
                  "Flash_Actionscript", "Wordpress", "Recruitments", "Payrolls",                                             
                  "Branding", "ETL/ELT", "VMware", "Azure", "Saleforce Data Loader",                                
                  "Copado Data Deployer", "Talend open studio",                                  
                  "Experience with remote sensing", "Shell", "Go", "PyTorch",                                             
                  "ArcGIS", "PostGIS", "STATA", "Leadership", "Collaboration Skills",                                 
                  "Research Skills", "Crystal Reports", "CIMA", "ACCA", "CIM",                                                   
                  "CA", "CFA", "Chartered Engineer", "Currency", "Additional_languages",
                  "Educational qualifications (if they need more than 1)", "Diploma",
                  "Mphil", "Data_translation")

data1 <- data %>% select(-column_names)

# Renaming column names

data1 <- data1 %>% rename("Location"="Country", "Salary"="Salary LL", 
                         "English Needed"="English proficiency", "BSc_needed"="BSc",
                         "MSc_needed"="MSc", "PhD_needed"="Dphil/PhD",
                         "Job_Category"="Job_Field")

data1 <- data1 %>% mutate(Knowledge_in = rep("NA", 360), year = rep(2022,360),
                          Educational_qualifications = rep("NA", 360))


dim(data1)


# Creating new variables

# Categorizing experience variable

data1 <- data1 %>% mutate(Experience_Category = case_when(
  `Experience LL (in years)` %in% c(0,0.5,1,2) ~ "Two or less years",
  `Experience LL (in years)` %in% c(3,4,5) ~ "More than 2 and less than 5 years",
  `Experience LL (in years)` %in% c(6:10) ~ "More than 5 and less than 10 years",
  `Experience LL (in years)` %in% c(11,12) ~ "More than 10 years",
  `Experience LL (in years)` %in% c("NA", NA) ~ "Unknown or Not needed"
  
))

# Removing experience column
data1 <- data1 %>% select(-`Experience LL (in years)`)


# Categorizing payment frequency

data1 <- data1 %>% mutate(`Payment Frequency` = case_when(
  `Payment Frequency` == "NA" ~ NA,
  `Payment Frequency` == "Monthly" ~ "monthly",
  `Payment Frequency` == "Hourly" ~ "hourly",
  `Payment Frequency` %in% c("Annually", "Annual") ~ "annual",
  `Payment Frequency` == "Daily" ~ "daily",
  `Payment Frequency` == "Yearly" ~ "yearly"
  
))


write_csv(data1, "DStidy_2022.csv")






























