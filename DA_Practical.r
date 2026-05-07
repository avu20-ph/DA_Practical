#Task List
#Your written report should include written text summaries and graphics of the following:
#Data validation:
#Describe validation and cleaning steps for every column in the data
#Exploratory Analysis:
#Include two different graphics showing single variables only to demonstrate the characteristics of data
#Include at least one graphic showing two or more variables to represent the relationship between features
#Describe your findings
#Definition of a metric for the business to monitor
#How should the business use the metric to monitor the business problem
#Can you estimate initial value(s) for the metric based on the current data
#Final summary including recommendations that the business should undertake

#Loading packages and data 
library(tidyverse)
library(scales)
account_info <- read.csv("da_fitly_account_info.csv")
customer_support <- read.csv("da_fitly_customer_support.csv")
user_activity <- read.csv("da_fitly_user_activity.csv")

##Begin Data Cleaning

#Checking first several rows for each df
head(account_info)
head(customer_support)
head(user_activity)

#Ensure there are no duplicates or missing values; no duplicates or missing values
sum(duplicated(account_info))
sum(duplicated(customer_support))
sum(duplicated(user_activity))
colSums(is.na(account_info))
colSums(is.na(customer_support))
colSums(is.na(user_activity))

#Checking structure and categories for each df
glimpse(account_info)
summary(account_info)
table(account_info$state)
table(account_info$plan)
table(account_info$plan_list_price)
table(account_info$churn_status)

glimpse(customer_support)
summary(customer_support)
table(customer_support$channel)
table(customer_support$topic)
table(customer_support$state)
table(customer_support$comments)

glimpse(user_activity)
summary(user_activity)
table(user_activity$event_type)

#Converting customer_id to integer and removing "C" suffix so it can be joined with the other dfs
account_info <- account_info %>%
mutate(user_id = as.integer(gsub("C", "", customer_id)))
#Converting timestamps to datetime format
customer_support <- customer_support %>%
mutate(ticket_time = as.POSIXct(ticket_time))
user_activity <- user_activity %>%
mutate(event_time = as.POSIXct(event_time))
#Renaming missing channel values to "other"
customer_support <- customer_support %>% mutate(channel = if_else(channel == "-", "other", channel))
#Dummy coding churn_status
account_info <- account_info %>%
mutate(churn_status = ifelse(churn_status == "Y", "Yes", "No"))
#Double checking changes    
str(account_info)
str(customer_support)
str(user_activity)
#Aggregating data before joining datasets
#Support summary
support_summary <- customer_support %>% group_by(user_id) %>% 
summarize(tickets = n(), avg_resolution_time = mean(resolution_time_hours))
#Activity summary
activity_summary <- user_activity %>% group_by(user_id) %>% summarize(total_events = n(), 
track_workouts = sum(event_type == "track_workout"), watch_videos = sum(event_type == "watch_video"), read_articles = sum(event_type == "read_article"), share_workout = sum(event_type == "share_workout"))
#Joining dfs and cleaning new dataset
churn_df <- account_info %>% left_join(support_summary, by = "user_id") %>%
left_join(activity_summary, by = "user_id") %>%
mutate(tickets = replace_na(tickets, 0), avg_resolution_time = replace_na(avg_resolution_time, 0), total_events = replace_na(total_events, 0), track_workouts = replace_na(track_workouts, 0), watch_videos = replace_na(watch_videos, 0), read_articles = replace_na(read_articles, 0), share_workout = replace_na(share_workout, 0))
#Ordering categories for plans
churn_df <- churn_df %>% mutate(plan = factor(plan, levels = c("Free", "Basic", "Pro", "Enterprise")))

#Begin Exploratory Data Analysis
#KEY: Discuss engagement, support activity, and plan type; identify patterns and potential drivers of churn and important KPIs

#Distribution of users by plan; basic plan is most popular, followed by free, enterprise, and pro
churn_df %>% count(plan) %>% mutate(percent = n / sum(n)*100)
churn_df %>% ggplot(aes(plan)) + geom_bar(aes(y = after_stat(count / sum(count)))) + scale_y_continuous(labels = scales::percent_format()) + 
labs(title = "Distribution of Users by Plan", x = "Plan Type", y = "Percentage of Users")
#Churn rates by plan; Highest churn in users with free plan (40% have churned); similar rates across other plans (20-25%)
churn_df %>% count(plan, churn_status) %>% group_by(plan) %>% mutate(rate = n/sum(n)) %>%
ggplot(aes(plan, rate, fill = as.factor(churn_status))) + geom_col(position="dodge") + 
scale_y_continuous(labels = scales::percent_format()) +
labs(title = "Churn by Plan", x = NULL, y = "Percentage", fill = "Churn Status")
#Events per user; only about 10% of users log more than 2 events total; the majority have very low engagement; # of events may be a predictor of churn
churn_df %>% count(total_events) %>% mutate(percent = n / sum(n)*100) 
ggplot(churn_df, aes(total_events)) + geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 1) + scale_y_continuous(labels = scales::percent_format()) + labs(title = "Distribution of Total Events per User", x = "Total Events", y = "Number of Users")
#Workout tracking frequency; most users do not track workouts; less than 1/4 users track at least 1 and less than 0.5% track 2 or more; seems users are not forming habits using the app
churn_df %>% count(track_workouts) %>% mutate(percent = n / sum(n)*100)
ggplot(churn_df, aes(track_workouts)) + geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 1) + scale_y_continuous(labels = scales::percent_format()) + labs(title = "Workout Tracking Frequency", x = "Number of Workouts Logged", y = "Percentage of Users")
#Tickets per user; those that churn have a higher ticket rate where 96% have had at least 1 ticket while those that do not have 90%; both are high but those that churned are higher, suggesting problems with the interface, onboarding, or how functional the app is
churn_df %>% summarize(avg_tickets = mean(tickets), median_tickets = median(tickets), max_tickets = max(tickets), pct_with_ticket = mean(tickets > 0)*100)
churn_df %>% group_by(churn_status) %>% summarize(avg_tickets = mean(tickets), median_tickets = median(tickets), pct_with_ticket = mean(tickets > 0)*100)
#Support topic volume by support topic; topics are equally distributed so churn is not driven by a specific type of problem 
customer_support %>% count(topic, sort = TRUE) %>% mutate(precent = n / sum(n)*100)
customer_support %>% count(topic, sort = TRUE) %>% mutate(proportion = n / sum(n), label = percent(proportion)) %>% ggplot(aes(x = "", y = proportion, fill = topic)) + geom_bar(width = 1, stat = "identity") + coord_polar("y") + geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 4) + labs(title = "Customer Support Topics", x = NULL, y = NULL) 
#Average Resolution time; average resolution time is about 10.3 hours
customer_support %>% summarize(avg_resolution_time = mean(resolution_time_hours))
#Plot resolution time by channel; customers receive supports equally across different channels; 
customer_support %>% group_by(channel) %>% summarize(avg_resolution = mean(resolution_time_hours), ticket_count = n()) %>% arrange(avg_resolution)
customer_support %>% group_by(channel) %>% summarize(avg_resolution = mean(resolution_time_hours)) %>% 
ggplot(aes(reorder(channel, avg_resolution), avg_resolution, fill = channel)) + geom_col(show.legend = FALSE) + labs(title = "Average Resolution Time by Support Channel", x = NULL, y = "Avg Resolution Time (hours)")
#Plot user activity frequency; read article is most common activity, followed by watch video, track workout, and share workout; users consume more content than using the app to track workouts; the tracking and sharing workout functions are underutilized and needs to be improved
user_activity %>% count(event_type, sort=TRUE) %>% mutate(proportion=n/sum(n)) %>% 
ggplot(aes(reorder(event_type, proportion), proportion, fill=event_type)) + geom_col(show.legend = FALSE) + scale_y_continuous(labels = scales::percent_format()) + labs(title = "User Activity by Event Type", x = NULL, y = "Percentage of Users")

