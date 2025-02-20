# Import required Libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(chron)
library(ggplot2)
library(psych)
library(EnvStats)
library(ggpubr)
library(GGally)
library(ggstatsplot)
library(multcomp)
library(Hmisc)
library(nortest)
library(DescTools)
library(ggcorrplot)
getwd()
df <- read.csv("jnj-data.csv") #Import dataset

# Operationalising variables of interest

# Engagement
eng_share <- as.numeric(df$Shares)
eng_like <- as.numeric(df$Likes)
eng_comment <- as.numeric(df$Comments)

## Can use the summary() function see a brief of descriptive statistics
summary(eng_share)

# Reactions (Love, Wow, Haha, Sad, Angry, Care) 
reaction_love <- as.numeric(df$Love)
reaction_wow <- as.numeric(df$Wow)
reaction_haha <- as.numeric(df$Haha)
reaction_sad <- as.numeric(df$Sad)
reaction_angry <- as.numeric(df$Angry)
reaction_care <- as.numeric(df$Care)

#Video views
views <- as.numeric(df$Total.Views)
## Compute the percentage of each type of reactions out of the total reactions.
total_reactions <- reaction_love + reaction_wow + reaction_haha + reaction_sad + reaction_angry + reaction_care
perc_reaction_love <- reaction_love / total_reactions
perc_reaction_wow <- reaction_wow / total_reactions
perc_reaction_haha <- reaction_haha / total_reactions
perc_reaction_sad <- reaction_sad / total_reactions
perc_reaction_angry <- reaction_angry / total_reactions
perc_reaction_care <- reaction_care / total_reactions

# Brand popularity
page_likes <- as.numeric(df$Likes.at.Posting)
page_followers <- as.numeric(df$Followers.at.Posting) 

# Date and time when a post was created
## Extract year, month, day and treat them as categorical variables
date_post <- as.Date(df$Post.Created.Date)
year_post <- as.factor(year(df$Post.Created.Date))
summary(year_post)
month_post <- as.factor(month(df$Post.Created.Date))
summary(month_post)
day_post <- as.factor(day(df$Post.Created.Date))
summary(day_post)

# Extract time of day in seconds (a day has 24*60*60=86400 seconds). A larger number means a later time in the day.
time_post <- as.numeric(hms(df$Post.Created.Time))
head(time_post, 3)
summary(time_post)

# Convert Date to Weekdays 
weekday <- weekdays(as.Date(df$Post.Created.Date))
head(weekday, 10)

weekday_post <- ifelse(weekday == "Sunday",1,
                       ifelse(weekday=="Monday",2,
                              ifelse(weekday=="Tuesday",3,
                                     ifelse(weekday=="Wednesday",4,
                                            ifelse(weekday=="Thursday",5,
                                                   ifelse(weekday=="Friday",6,
                                                          ifelse(weekday=="Saturday",7,99)))))))

head(weekday_post,10)
weekday_post <- as.factor(weekday_post)
str(weekday_post)

# Convert Date to Weekend (1 = weekend, 0 = weekday)
weekend_post <- ifelse(weekday == "Sunday",1,
                       ifelse(weekday=="Saturday",1,0))
head(weekend_post,10)
weekend_post <- as.factor(weekend_post)
str(weekend_post)

#Convert Time-Post to factors in day
time_in_day <- cut(
  time_post,
  breaks = c(0, 5*3600, 12*3600, 17*3600, 21*3600, 24*3600),
  labels = c("Night", "Morning", "Afternoon", "Evening", "Night"),
  include.lowest = TRUE,
  right = FALSE
)
## Adjust for the night time window crossing midnight
time_in_day[time_post >= 21*3600 | time_post < 5*3600] <- "Night"
head(time_in_day)
str(time_in_day)

# Type of post
## Count how many posts for each type
type_summary <- group_by(df, Type) %>%
  summarise(
    count = n(),
  )
type_summary
## Code photo_post as a binary variable: 1 for post includes photos, 0 otherwise
photo_post <- as.factor(ifelse(df$Type == "Photo", 1, 0))
head(photo_post, 10)

## Code video_post as a binary variable: 1 for post includes videos, 0 otherwise 
### (see the output of type_post above to identify different types of video)
video_type = c("Live Video Complete", "Native Video")
video_post <- as.factor(ifelse(df$Type %in% video_type, 1, 0))
head(video_post, 10) 

## Code link_post as a binary variable: 1 for post includes link, 0 otherwise 
link_post <- as.factor(ifelse(df$Type == "Link",1, 0))
head(link_post, 10)
## Simplified Post type
type_post <- as.factor(ifelse(df$Type == "Photo","Photo",
                    ifelse(df$Type %in% video_type,"Video",
                           ifelse(df$Type == "Link","Link","Other"
                           ))))

# Create a dataframe df1 that includes the new variables for data analysis
df1 <- data.frame(eng_share, eng_like, eng_comment, total_reactions,
                  reaction_angry, reaction_care, reaction_haha, reaction_love, reaction_sad, reaction_wow,
                  perc_reaction_wow, perc_reaction_haha, perc_reaction_sad, perc_reaction_angry, perc_reaction_care, perc_reaction_love,
                  page_likes, page_followers,
                  date_post, year_post, month_post, day_post, time_post, weekday_post, weekend_post, time_in_day,
                  photo_post, video_post, link_post,type_post, views)
head(df1, 10)

# Check missing values for each variable
colSums(is.na(df1))

# Filter the dataset to only include complete cases
df2 <- df1[complete.cases(df1), ]

# Remove rows with any missing values
df2 <- na.omit(df1)

# Descriptive statistics
describe(df2) # variables with * (e.g., year_post*) are categorical variables 

## Histograms of numeric variables
for (col_name in names(df1)) {
  # Check if the column is numeric
  if (is.numeric(df1[[col_name]])) {
    # Create a histogram for the numeric column
    hist(df1[[col_name]], 
          breaks = 10,
          main = paste("Histogram of", col_name),
          xlab = col_name, 
          col = "skyblue", 
          border = "black")
  } else {
    message(paste(col_name, "is not numeric, skipping..."))
  }
}
## Count plot of factor variables
for (col_name in names(df1)) {
  # Check if the column is factor
  if (is.factor(df1[[col_name]])) {
    # Create a count plot of factor column
    p <- ggplot(data = df1, aes(.data[[col_name]])) +
      geom_bar(stat = "count", fill = "skyblue", colour = "black")
    print(p)
  } else {
    message(paste(col_name, "is not factor, skipping..."))
  }
}

#Log transformation engagement
df1$log_eng_like <- log(eng_like + 1)
df1$log_eng_share <- log(eng_share + 1)
df1$log_eng_comment <- log(eng_comment + 1)
df1$log_total_reactions <- log(total_reactions + 1)
df1$log_page_likes <- log(page_likes + 1)
df1$log_page_followers <- log(page_followers + 1)

# Apply Logit transformation for reactions
# The logit transformation is used to transform proportions or probabilities that are bounded between 0 and 1 into values that range from negative infinity to positive infinity. 
# This transformation is particularly useful when working with data that represents probabilities, proportions, or percentages.

epsilon <- 0.001  # Small value to replace 0 and 1
# Adjust 0s and 1s
df1$perc_reaction_angry_adj <- ifelse(df1$perc_reaction_angry == 0, epsilon,
                                    ifelse(df1$perc_reaction_angry == 1, 1 - epsilon, df1$perc_reaction_angry))
df1$perc_reaction_care_adj <- ifelse(df1$perc_reaction_care == 0, epsilon,
                                    ifelse(df1$perc_reaction_care == 1, 1 - epsilon, df1$perc_reaction_care))
df1$perc_reaction_haha_adj <- ifelse(df1$perc_reaction_haha == 0, epsilon,
                                     ifelse(df1$perc_reaction_haha == 1, 1 - epsilon, df1$perc_reaction_haha))
df1$perc_reaction_love_adj <- ifelse(df1$perc_reaction_love == 0, epsilon,
                                     ifelse(df1$perc_reaction_love == 1, 1 - epsilon, df1$perc_reaction_love))
df1$perc_reaction_sad_adj <- ifelse(df1$perc_reaction_sad == 0, epsilon,
                                     ifelse(df1$perc_reaction_sad == 1, 1 - epsilon, df1$perc_reaction_sad))
df1$perc_reaction_wow_adj <- ifelse(df1$perc_reaction_wow == 0, epsilon,
                                     ifelse(df1$perc_reaction_wow == 1, 1 - epsilon, df1$perc_reaction_wow))

# Apply logit transformation
df1$logit_perc_reaction_wow <- log(df1$perc_reaction_wow_adj / (1 - df1$perc_reaction_wow_adj))
df1$logit_perc_reaction_angry <- log(df1$perc_reaction_angry_adj / (1 - df1$perc_reaction_angry_adj))
df1$logit_perc_reaction_care <- log(df1$perc_reaction_care_adj / (1 - df1$perc_reaction_care_adj))
df1$logit_perc_reaction_haha <- log(df1$perc_reaction_haha_adj / (1 - df1$perc_reaction_haha_adj))
df1$logit_perc_reaction_love <- log(df1$perc_reaction_love_adj / (1 - df1$perc_reaction_love_adj))
df1$logit_perc_reaction_sad <- log(df1$perc_reaction_sad_adj / (1 - df1$perc_reaction_sad_adj))

# Create list of each type columns name
#Engagement
eng_cols <- c("eng_like","eng_share","eng_comment") #Engagement set
log_eng_cols <- c("log_eng_like","log_eng_share","log_eng_comment") #Log Engagement set
#Brand Popularity
page_cols <- c("page_likes","page_followers") #Brand Popularity set
log_page_cols <- c("log_page_likes", "log_page_followers") #Log Brand Popularity set
#Time
time_cols <- c("year_post","month_post","weekday_post","weekend_post","time_in_day")
#Reaction
reaction_cols <- c("reaction_angry","reaction_care","reaction_haha","reaction_love","reaction_sad","reaction_wow")
perc_reaction_cols <- c("perc_reaction_wow", "perc_reaction_haha", "perc_reaction_sad", "perc_reaction_angry", "perc_reaction_care", "perc_reaction_love")
log_reaction_cols <- c("logit_perc_reaction_wow", "logit_perc_reaction_angry","logit_perc_reaction_care","logit_perc_reaction_haha","logit_perc_reaction_love", "logit_perc_reaction_sad")

# Use the sapply() function to apply the Anderson-Darling test to each numeric column in the dataset df1.
# Apply Anderson-Darling test to each numeric column in df1
ad_test_results <- sapply(df1, function(x) {
  if (is.numeric(x)) {
    ad_test <- ad.test(x)
    return(ad_test$p.value)
  } else {
    return(NA) # For non-numeric columns, return NA
  }
})

# Print the p-values for each numeric column
print(ad_test_results) 

#Engagement Analysis
## Engagement Correlation
ggcorrmat(
  data = df1[,c(eng_cols) ],
  type = "nonparametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)
## Histogram of engagement after log transformation its Anderson-Darling Test result
for (i in log_eng_cols) {
  p <- ggplot(data = df1, aes(.data[[i]])) +
    geom_histogram(bins = 10, fill = "skyblue", colour = "black")
  print(p)
  print(ad.test(df1[[i]]))
}

# Brand Popularity Ananlysis
## Brand popularity over Time
plot(date_post, page_likes, type = "l", col = "red", lty = 2,
     main = "Page likes over time",
     xlab = "Date",
     ylab = "Engagement",
     axes = FALSE
     )
axis(1, at =pretty(date_post), 
     labels = format(pretty(date_post), "%Y"))
axis(2, at = pretty(page_likes), 
     labels = paste0(formatC(pretty(page_likes)/1000, format = "f", digits =0), "k"))
lines(date_post, page_followers, type = "l", col ="blue", lty = 3)
legend("topleft", legend = c("Like","Follower"),
       col = c("red","blue"), lty = c(2,3),
       title = "Legend")

#Correlation chart Page Popularity ~ Engagement
ggcorrmat(
  data = df1[,c(page_cols, eng_cols) ],
  type = "nonparametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)


# Time Effect Analysis
## Covid-19 Analysis
#Classified Covid-19 period from 11-03-2020 to 05-05-2023
df1$is_covid <- as.factor(ifelse((df1$date_post <= as.Date("2023-05-05")) &
                                   (df1$date_post >= as.Date("2020-03-11")),
                                 "Covid-19","Normal"))
#Summarise average engagement during and outside Covid-19 period
mu <- df1 %>% group_by(is_covid) %>%
  dplyr::summarise(mean_log_eng_like = mean(log_eng_like)) %>%
  ungroup()
print(mu)
#Histogram Like distribution during Covid and out-of Covid period
p <- ggplot(data = df1,  aes(x = log_eng_like, color = is_covid)) + 
  geom_histogram(fill = "white", position = "dodge") +
  geom_vline(data = mu, aes(xintercept = mean_log_eng_like, color = is_covid),
             linetype = "dashed") +
  theme(legend.position = "top")
print(p)
t.test(df1$log_eng_like ~ df1$is_covid, mu = 0,
       alternative = "greater", conf.level = 0.95, var.equal = TRUE)
## We try to distinguish the effect of Covid-19 to the post in day to suggest a more approriate strategy after the Covid-19
#Subset the Non-covid dataset
df_noncovid <- subset(df1, is_covid == "Normal")
str(df_noncovid)
### Time of day and Weekend effect
for (time_group in time_cols) {
  #Create dataframe for predicted value
  df1_pred <- df_noncovid %>%
    group_by(.data[[time_group]]) %>%
    dplyr::summarise(mean_log_eng_share = mean(log_eng_share, na.rm = TRUE),
                     se_log_share = sd(log_eng_share, na.rm = TRUE)/sqrt(n()),
                     mean_log_eng_like = mean(log_eng_like, na.rm = TRUE),
                     se_log_like = sd(log_eng_like, na.rm = TRUE)/sqrt(n()),
                     mean_log_eng_comment = mean(log_eng_comment, na.rm = TRUE),
                     se_log_comment = sd(log_eng_comment, na.rm = TRUE)/sqrt(n()),
                     mean_log_reactions = mean(log_total_reactions, na.rm = TRUE),
                     se_log_reactions = sd(log_total_reactions, na.rm = TRUE)/sqrt(n())
    ) %>%
    ungroup()
  ### Visualise the Mean and the Range within 1 SE
  #Share engagment
  p1 <- ggplot(df1_pred, aes(x = .data[[time_group]], y = mean_log_eng_share)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean_log_eng_share - se_log_share, ymax = mean_log_eng_share + se_log_share), width = 0.2) +
    geom_line(group = 1) +
    labs(x = NULL, y = NULL) +
    theme_classic()
  #Like engagment
  p2 <- ggplot(df1_pred, aes(x = .data[[time_group]], y = mean_log_eng_like)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean_log_eng_like - se_log_like, ymax = mean_log_eng_like + se_log_like), width = 0.2) +
    geom_line(group = 1) +
    labs(x = NULL, y = NULL) +
    theme_classic()
  #Comment engagment
  p3 <- ggplot(df1_pred, aes(x = .data[[time_group]], y = mean_log_eng_comment)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean_log_eng_comment - se_log_comment, ymax = mean_log_eng_comment + se_log_comment), width = 0.2) +
    geom_line(group = 1) +
    labs(x = NULL, y = NULL) +
    theme_classic()
  #Combine 3 graphs into 1
  p <- ggarrange(p1,p2,p3, widths = c(2,2,2), heights = c(2,2,2), nrow = 3, ncol = 1,
            labels = c("Share","Like","Comment"), label.x = 0.1, label.y = 1)
  print(p)
}
## ANOVA test difference within the time group
for (time_group in time_cols) {
  # Dynamically create the formula for the ANOVA
  formula_like <- as.formula(paste("log_eng_like ~", time_group))
  anova_like <- aov(formula_like, data = df_noncovid)
  print(summary(anova_like))
  cat("\n") #Print blank line to seperate results
  # Perform Tukey's HSD test
  turkey_like <- TukeyHSD(anova_like)
  print(turkey_like)
  # Plot the results of Tukey's test
  p <- plot(turkey_like, las = 1)
  print(p)
}
#Bar chart to investigate the see the distribution of post in day
ggplot(data=df1, aes(x = is_covid, fill = time_in_day)) + 
  geom_bar(stat = "count", colour = "black" ) +
  theme_minimal()

# Reactions Analysis
## Scatter Plot Interaction with Engagement
### Engagement ~ Reactions
for (i in c("log_eng_like", "log_eng_share", "log_eng_comment")) {
  angry <- ggplot(data = df1, aes(x = .data[[i]], y = logit_perc_reaction_angry)) + 
    geom_point() +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_classic()
  
  wow <- ggplot(data = df1, aes(x = .data[[i]], y = logit_perc_reaction_wow)) + 
    geom_point() +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_classic()
  
  sad <- ggplot(data = df1, aes(x = .data[[i]], y = logit_perc_reaction_sad)) + 
    geom_point() +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_classic()
  
  love <- ggplot(data = df1, aes(x = .data[[i]], y = logit_perc_reaction_love)) + 
    geom_point() +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_classic()
  
  haha <- ggplot(data = df1, aes(x = .data[[i]], y = logit_perc_reaction_haha)) + 
    geom_point() +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_classic()
  
  care <- ggplot(data = df1, aes(x = .data[[i]], y = logit_perc_reaction_care)) + 
    geom_point() +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_classic()
  
  combined_plot <- ggarrange(angry, wow, sad, love, haha, care,
                             ncol = 2, nrow = 3,
                             labels = c("Angry", "Wow", "Sad", "Love", "Haha", "Care"),
                             label.x = 1-0.4, label.y = 1) #Adjust the position
  print(combined_plot)
}
#Correlation chart Log_Engagement ~ Log_Reaction
ggcorrmat(
  data = df1[,c(log_eng_cols, log_reaction_cols) ],
  type = "nonparametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

#Correlation chart Log_Engagement ~ Log_Reaction outside Covid-19
ggcorrmat(
  data = df_noncovid[,c(log_eng_cols, log_reaction_cols) ],
  type = "nonparametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

# Type of Post analysis
## Post type effect on Engagement
#Create dataframe for predicted value
df1_pred <- df1 %>%
  filter(type_post != "Other") %>%
  group_by(type_post) %>%
  dplyr::summarise(mean_log_eng_share = mean(log_eng_share, na.rm = TRUE),
                   se_log_share = sd(log_eng_share, na.rm = TRUE)/sqrt(n()),
                   mean_log_eng_like = mean(log_eng_like, na.rm = TRUE),
                   se_log_like = sd(log_eng_like, na.rm = TRUE)/sqrt(n()),
                   mean_log_eng_comment = mean(log_eng_comment, na.rm = TRUE),
                   se_log_comment = sd(log_eng_comment, na.rm = TRUE)/sqrt(n()),
                   mean_log_reactions = mean(log_total_reactions, na.rm = TRUE),
                   se_log_reactions = sd(log_total_reactions, na.rm = TRUE)/sqrt(n())
  ) %>%
  ungroup()
### Visualise the Mean and the Range within 1 SE
#Share engagment
p1 <- ggplot(df1_pred, aes(x = type_post, y = mean_log_eng_share)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_log_eng_share - se_log_share, ymax = mean_log_eng_share + se_log_share), width = 0.2) +
  geom_line(group = 1) +
  labs(x = NULL, y = NULL) +
  theme_classic()
#Like engagment
p2 <- ggplot(df1_pred, aes(x = type_post, y = mean_log_eng_like)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_log_eng_like - se_log_like, ymax = mean_log_eng_like + se_log_like), width = 0.2) +
  geom_line(group = 1) +
  labs(x = NULL, y = NULL) +
  theme_classic()
#Comment engagment
p3 <- ggplot(df1_pred, aes(x = type_post, y = mean_log_eng_comment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_log_eng_comment - se_log_comment, ymax = mean_log_eng_comment + se_log_comment), width = 0.2) +
  geom_line(group = 1) +
  labs(x = NULL, y = NULL) +
  theme_classic()
#Combine 3 graphs into 1
p <- ggarrange(p1,p2,p3, widths = c(2,2,2), heights = c(2,2,2), nrow = 3, ncol = 1,
               labels = c("Share","Like","Comment"), label.x = 0.1, label.y = 1)
print(p)
#Anova test the difference between post type
formula_like <- as.formula(log_eng_like ~ type_post)
anova_like <- aov(formula_like, data = df1)
print(summary(anova_like))
cat("\n") #Print blank line to seperate results
# Perform Tukey's HSD test
turkey_like <- TukeyHSD(anova_like)
print(turkey_like)
#Anova test the difference between post type of non-Covid period
formula_like <- as.formula(log_eng_like ~ type_post)
anova_like <- aov(formula_like, data = df_noncovid)
print(summary(anova_like))
cat("\n") #Print blank line to seperate results
# Perform Tukey's HSD test
turkey_like <- TukeyHSD(anova_like)
print(turkey_like)

#Barplot showing distribution of postype during Covid and outside it
ggplot(data = df1, aes(x = is_covid, fill = type_post )) + 
  geom_bar(stat = "count")

#Analysing video length effect on engagement and view
##Subset a dataset of only video
df_video <- df[which(df$Type %in% video_type),]
df_video$video_length <- as.numeric(hms(df_video$Video.Length))
df_video$views <- as.numeric(df_video$Total.Views)

##Histogram distribution of video length and the views
hist(log(df_video$video_length))
hist(log(df_video$views))

##Scatter plot to see the linear-relationship of video_length with the views
ggplot(df_video,aes(log(video_length),log(views))) + 
  geom_point() +
  geom_smooth(method = "lm")
## Testing the correlation of video length and views
### Checking if they follow normal distribution
ad.test(df_video$video_length)
ad.test(df_video$views)
### Test result indicates that they're not following normal distribution so spearman method was selected
corr.test(df_video$views, df_video$video_length, method = "spearman")
##Scatter plot to see the linear-relationship of the views and Like
ggplot(df_video,aes(log(views),log(Likes))) +
  geom_point() +
  geom_smooth(method = "lm")
## Testing the correlation of likes and views
### Checking if they follow normal distribution
ad.test(df_video$views)
ad.test(df_video$Likes)
### Test result indicates that they're not following normal distribution so spearman method was selected
corr.test(df_video$views, df_video$Likes, method = "spearman")


#Adjust the page likes and follower for analysis
df1 <- df1 %>%
  arrange(date_post) %>%
  mutate(like_diff = page_likes - lag(page_likes),
         follower_diff = page_followers - lag(page_followers))
max_like = max(df1$like_diff, na.rm = TRUE)
max_follower = max(df1$follower_diff, na.rm = TRUE)
print(df1[(df1$like_diff== max_like) & (df1$follower_diff==max_follower),"date_post"])

##Because the page like and follower drastically rocket from 24-08-2021 but plateau since that we will try to elimante this rocket rise to see if the organic rise of like and follower have positive effect on the engagement
df1 <- df1 %>%
  mutate(adjusted_page_likes = ifelse(date_post >= as.Date("2021-08-24"), page_likes - max_like, page_likes),
         adjusted_page_followers = ifelse(date_post >= as.Date("2021-08-24"), page_followers - max_follower, page_followers)
         )

###Line chart to see the trend over time
ggplot(df1, aes(x = date_post)) +
  geom_line(aes(y = adjusted_page_likes), color = "blue") +  # First line (Likes)
  geom_line(aes(y = adjusted_page_followers), color = "red") +  # Second line (Followers)
  labs(title = "Adjusted Page Likes and Followers Over Time",
       x = "Date",
       y = "Adjusted Values") +
  theme_minimal()

###Log transformation the adjusted like and follower to reduce variation
df1$log_adjusted_page_likes = log(df1$adjusted_page_likes)
df1$log_adjusted_page_followers = log(df1$adjusted_page_followers)
###
ad.test(df1$log_adjusted_page_likes)
ad.test(df1$log_adjusted_page_followers)

for (i in c("log_adjusted_page_likes","log_adjusted_page_followers")) {
  #Share engagment
  p1 <- ggplot(df1, aes(x = .data[[i]], y = log_eng_share)) +
    geom_point(size = 3) +
    labs(x = NULL, y = NULL) +
    theme_classic()
  #Like engagment
  p2 <- ggplot(df1, aes(x = .data[[i]], y = log_eng_like)) +
    geom_point(size = 3) +
    labs(x = NULL, y = NULL) +
    theme_classic()
  #Comment engagment
  p3 <- ggplot(df1, aes(x = .data[[i]], y = log_eng_comment)) +
    geom_point(size = 3) +
    labs(x = NULL, y = NULL) +
    theme_classic()
  #Combine 3 graphs into 1
  p <- ggarrange(p1,p2,p3, widths = c(2,2,2), heights = c(2,2,2), nrow = 3, ncol = 1,
                 labels = c("Share","Like","Comment"), label.x = 0.1, label.y = 1)
  print(p)
}
# Run Linear Regression on Transformed model

## Colinearity review
numeric_df1 <- df1[sapply(df1, is.numeric)]
cor_matrix <- cor(numeric_df1, method = "spearman", use = "complete.obs")

#Filter out correlations below |0.8|
filtered_cor_matrix <- ifelse(abs(cor_matrix) >= 0.8, cor_matrix, NA)

#Plot the filtered correlation matrix using ggcorrplot
ggcorrplot(
  filtered_cor_matrix, 
  method = "square", 
  type = "lower",          # Only show lower triangle
  outline.col = "white",   # Cell border color
  lab = TRUE,              # Add correlation coefficient labels
  lab_size = 3,            # Size of the labels
  colors = c("darkred", "white", "steelblue"),  # Change default colors
  ggtheme = theme_bw()
) + 
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.spacing = unit(4, "lines")  # Adjust text size and spacing
  )


model1 <- lm(log_eng_like ~ log_page_likes + weekend_post + is_covid +
               logit_perc_reaction_sad + logit_perc_reaction_love + logit_perc_reaction_haha + logit_perc_reaction_care + logit_perc_reaction_angry + logit_perc_reaction_wow +
               video_post + link_post + photo_post + log(views+1),
             data = df1)
summary(model1)

model2 <- lm(log_eng_like ~ log_page_followers + weekend_post + is_covid +
               logit_perc_reaction_sad + logit_perc_reaction_love + logit_perc_reaction_haha + logit_perc_reaction_care + logit_perc_reaction_angry + logit_perc_reaction_wow +
               video_post + link_post + photo_post + log(views+1),
             data = df1)       
summary(model2)
