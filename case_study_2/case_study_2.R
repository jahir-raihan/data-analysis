daily_activity = read_csv('Desktop/data_analysis/case_study_2_data/dailyActivity_merged.csv')

#ignore data set 
daily_calories = read_csv('Desktop/data_analysis/case_study_2_data/dailyCalories_merged.csv')
daily_intensites = read_csv('Desktop/data_analysis/case_study_2_data/dailyIntensities_merged.csv')
daily_steps = read_csv('Desktop/data_analysis/case_study_2_data/dailySteps_merged.csv')
# end ignore data set

hourly_calories = read_csv('Desktop/data_analysis/case_study_2_data/hourlyCalories_merged.csv')
hourly_intensities = read_csv('Desktop/data_analysis/case_study_2_data/hourlyIntensities_merged.csv')
hourly_steps = read_csv('Desktop/data_analysis/case_study_2_data/hourlySteps_merged.csv')
sleep_day = read_csv('Desktop/data_analysis/case_study_2_data/sleepDay_merged.csv')
weight_log = read_csv('Desktop/data_analysis/case_study_2_data/weightLoginfo_merged.csv')


# Checking data sets if they contain the same information or not.

#  daily steps

daily_steps = rename(daily_steps, ActivityDate = ActivityDay, TotalSteps = StepTotal)
check_data = data.frame(select(daily_steps,Id, ActivityDate, TotalSteps) == select(daily_activity, Id, ActivityDate, TotalSteps))
# now we need to confirm that their is no un matched value
sum(check_data == FALSE) 

# Wolla there is no unmatched values , we can ignore daily steps dataset.

#  daily calories

daily_calories = rename(daily_calories, ActivityDate = ActivityDay)
check_data2 = data.frame(select(daily_calories,Id, ActivityDate, Calories) == select(daily_activity, Id, ActivityDate, Calories))

sum(check_data2 == FALSE) 

# There is no unmatched values , we can ignore daily calories dataset.

#  daily intensitey

daily_intensites = rename(daily_intensites, ActivityDate = ActivityDay)
check_data3 = data.frame(select(daily_intensites,Id, ActivityDate,
                                SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes,
                                SedentaryActiveDistance, LightActiveDistance, ModeratelyActiveDistance, 
                                VeryActiveDistance) == select(daily_activity, Id, ActivityDate, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes,
                                                              SedentaryActiveDistance, LightActiveDistance, ModeratelyActiveDistance, 
                                                              VeryActiveDistance))

sum(check_data3 == FALSE) 

# Now we can  conclude that daily intensity data is also in daily activity and they are same. 
# So we will ignore these three data sets.

# Check for uncleaned or null data

# Daily activity contains wrong type data in ActivityDate 

daily_activity$ActivityDate = as.Date(daily_activity$ActivityDate, format='%m/%d/%Y')

sum(is.na(daily_activity)) # no null values in daily activity
sum(is.na(hourly_calories)) # no null values in hourly calories
sum(is.na(hourly_intensities)) # no null values in hourly intensities
sum(is.na(hourly_steps)) # no null values in hourly steps
sum(is.na(sleep_day)) # no null values in sleep day
sum(is.na(weight_log)) # 65 null values in wight log , we will figure out what data is missing



## Adding weekday to daily_activity data set according Activity date

daily_activity$day_of_week <- weekdays(as.Date(daily_activity$ActivityDate, format='%m/%d/%Y'))

#daily_activity$day_of_week <- factor(daily_activity$day_of_week, levels=c('Saturday','Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))


## Grouping and adding new columns in hourly intensity data for analysis

hourly_intensities =  hourly_intensities %>%
  mutate(ActivityDate = as.Date(ActivityHour, format='%m/%d/%Y'))

hourly_intensities$WeekDay = weekdays(hourly_intensities$ActivityDate)

hourly_intensities_summary_data = hourly_intensities %>% 
  group_by(Id, ActivityDate, WeekDay) %>%
  summarise(Total_Intensity = sum(TotalIntensity)) %>%
  arrange(desc(Total_Intensity))


## Grouping and adding new columns in hourly steps for further analysis

hourly_steps =  hourly_steps %>%
  mutate(ActivityDate = as.Date(ActivityHour, format='%m/%d/%Y'))

hourly_steps$WeekDay = weekdays(hourly_steps$ActivityDate)

hourly_steps_summary_data = hourly_steps %>% 
  group_by(Id, ActivityDate, WeekDay) %>%
  summarise(Total_Steps = sum(StepTotal)) %>%
  arrange(desc(Total_Steps))


# Find why weight log data set have 65 null value
summary(weight_log$Fat) 

# As we can see there is 65 null values in Fat column 
# We would've filled this missing values if we were provided age data . Hence we will mark them as missing values.
# formula to calculate fat of a person (1.20 x BMI) + (0.23 x Age) - 5.4

  

weight_log = weight_log %>% # Replaced NA values with 0 for column Fat
  replace(is.na(.), 0)

# Now our data is ready for analysis


## Analyze

# first we will view the summary of all data sets

## Daily Activity

summary(daily_activity)

## Daily activity summary takeaways

# This summary shows the average user is taking 7638 steps a day, missing the recommended 10,000 steps
# for health by the CDC. On average, users are getting 21.16 minutes of very active or vigorous
# activity a day, this equates to 148.12 minutes a week. The CDC recommends 75 minutes of vigorous
# activity a week, so the typical Fitbit user is doing well in this area and achieving additional
# health benefits. In contrast, participants are averaging 991.2 minutes, or 16.52 hours of sedentary
# time a day! This is a significant amount of time and can lead to other health issues because the
# body functions best upright. Scientists have determined that 40 minutes of moderate to vigorous
# activity a day will balance out the effects of sitting up to 10 hours a day. Furthermore, this
# summary shows the average user is burning 2304 calories a day. Studies show the average person in
# the population burns 1800 calories a day, but burning 3500 is needed to lose a pound of weight. The
# Fitbit users in this case are burning more than the norm, and are on track to lose a few pounds a
# week if they so choose to.

## Hourly Calories

summary(hourly_calories)

## Hourly calories summary takeaways

# From the summary we can see that on a average scale a person is burning up to 97.39 calories per hour 
# Which is 97.39*24 = 2337.36 if we take 24 hours on scale. And the maximum value is 948 , we know that 
# burning 3500 calories is needed to lose a pound , i think those who are burning at this rate are in
# hurry to lose all their extra weights :)

## Hourly intensity

summary(hourly_intensities_summary_data)


## Hourly intensity summary takeaways

# From our summary we can see that a person on average scale is in intense activity of 12.04 minutes per hour,
# which is 12.04*24 = 288.96 minutes equivalent to  288.96/60 = 4.816 or around 5 hours of intense activity  each day
# And the highest value intensity of a single is 904.0 which is almost 904/60 = 15.06 hour on Sunday. 


## Sleep Day

summary(sleep_day)

## Sleep Day summary takeaways

# The summary of the sleep data frame displays the average user sleeps once per day for 419.5 minutes, or roughly 7 hours.
# This falls within the CDC’s recommendations for adults in order to get the proper amount of rest. The average
# participant is spending 458.6 minutes in bed, or 7.64 hours. This means the typical user is spending 38.6 minutes awake
# in bed. According to Health Central, people should not spend more than 1 hour in bed awake. This is to prevent a mental
# link being formed between being awake and being in bed, which can lead to insomnia.

## Weight Log

summary(weight_log)

## Weight log summary takeaways

# Data frame has a low number of participants, and the average BMI is 25.19. This is considered as overweight BMI.
# However, BMI can be a screening tool and does not diagnose the body fatness or health of an person.


## Share

# Activity overview

min_in_day = 60*24

Sedentary = (mean(daily_activity$SedentaryMinutes)/ min_in_day ) * 100
VeryActive = (mean(daily_activity$VeryActiveMinutes)/ min_in_day) * 100
LightActive = (mean(daily_activity$LightlyActiveMinutes)/ min_in_day) * 100
FairlyActive = (mean(daily_activity$FairlyActiveMinutes)/ min_in_day) * 100

activity_df = data.frame(Status = c('Sedentary', 'VeryActive', 'LightActive', 'FairlyActive'),
                         Percentage = c(Sedentary, VeryActive, LightActive, FairlyActive))

  
ggplot(activity_df) + geom_col(mapping=aes(x=Status, y=Percentage, fill=Status))

# From the visualization we can see that majority of users almost ( 69% ) percentage are spending
# time in sedentary state . And only 13% of them are Lightly Active.

## Active weekday frequency

daily_activity$day_of_week <- factor(daily_activity$day_of_week, levels=c('Saturday','Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))


ggplot(daily_activity) + geom_bar(mapping=aes(x=day_of_week, fill=day_of_week))

# From the above visualization we can see that users are most active on "Tuesday" , "Wednesday" 
# "Thursday" and least active on "Sunday", "Monday".


## Steps weekday frequency

step_freq = daily_activity %>%
  group_by(day_of_week) %>%
  summarise(avg_steps = mean(TotalSteps))

ggplot(step_freq) + geom_col(mapping=aes(x=day_of_week, y=avg_steps, fill=day_of_week))

# From the above summary we can see that on average scale users didn't achieved recommended 10000
# steps in any day of the week. But the highest average steps are from Saturday and Tuesday. And the 
# Lowest is from Sunday , we can conclude that users are being lazy at weekends. 


## Steps vs Calories

ggplot(daily_activity, aes(x=TotalSteps, y=Calories)) + geom_point() +
  geom_smooth(col='blue', method=lm, se=FALSE) + 
  facet_wrap(~day_of_week)

# This visualization is too much faded , lets try to separate these visualizations by week day.
 
# Saturday

ggplot(filter(daily_activity, day_of_week=='Saturday'), aes(TotalSteps, Calories)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE)

# Sunday
ggplot(filter(daily_activity, day_of_week=='Sunday'), aes(TotalSteps, Calories)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE)

# Monday
ggplot(filter(daily_activity, day_of_week=='Monday'), aes(TotalSteps, Calories)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE)

# Tuesday
ggplot(filter(daily_activity, day_of_week=='Tuesday'), aes(TotalSteps, Calories)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE)

# Wednesday
ggplot(filter(daily_activity, day_of_week=='Wednesday'), aes(TotalSteps, Calories)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE)

# Thursday
ggplot(filter(daily_activity, day_of_week=='Thursday'), aes(TotalSteps, Calories)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE)

# Friday
ggplot(filter(daily_activity, day_of_week=='Friday'), aes(TotalSteps, Calories)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE)


## Steps vs Calories Visualization takeaways

# From above all scatter plots , we can see that their is a positive relationship between
# Steps and Calories burned. The more you walk , more you will burn Calories.


## Sleep vs Calories 

# First we need to merge daily_activity with sleep data in order to get Calories data.
cal_daily = select(daily_activity, Id, ActivityDate, Calories, day_of_week)
sleep_day =  sleep_day %>%
  mutate(ActivityDate = as.Date(SleepDay, format='%m/%d/%Y'))

sleep_day$day_of_week = weekdays(sleep_day$ActivityDate)


sleep_data = merge(sleep_day, cal_daily, by=c('Id', 'ActivityDate', 'day_of_week'))

ggplot(sleep_data, aes(TotalMinutesAsleep, Calories)) + geom_point() +
  geom_smooth(method=lm)

# It seems that their is no significant relation between sleep time and calories burned.

# Average Sleep time in week
avg_sleep = sleep_data %>%
  group_by(day_of_week) %>%
  summarise(avg_sleep_time = mean(TotalMinutesAsleep))

ggplot(avg_sleep) + geom_col(mapping=aes(x=day_of_week, y=avg_sleep_time, fill=day_of_week))

# From the column chart we can see that Average sleep throughout the week is almost same,
# except Sunday , that's probably because Sunday is weekend.

# Average time wasted in bed after sleep
avg_wasted = sleep_data %>%
  mutate(time_wasted = TotalTimeInBed - TotalMinutesAsleep) %>%
  group_by(day_of_week) %>%
  summarise(avg_wasted = mean(time_wasted))

ggplot(avg_wasted) + geom_col(mapping=aes(x=day_of_week, y=avg_wasted, fill=day_of_week))

# From above visualization we can see that most of the users waste more then 50 minutes in bed 
# when it is Sunday. During other weekdays wasted times are between 30-40 minutes


## Weight Log 

ggplot(weight_log) + geom_bar(mapping=aes(x=IsManualReport, fill=IsManualReport))

# We don't have enough data to analyse weight log data deeply, and fat column also contains many 
# NA values . But from our above bar chart we can see that maximum weight log inputs were done 
# manually , which is not a good thing for a business and customer satisfaction.


## Notable  Points

#1 Based on analyzing how Fitbit consumers use and respond to features, recommendations can be
#  made to promote further growth for Bellabeat.

#2 Rather than simply providing data on user’s health, the app should further encourage users
#  to meet fitness goals and become a social media platform

#3 Center of Disease Control recommends working out with a friend in order to feel motivated
#  and be more adventurous in trying new workouts.


## Analysis Takeaways & Recommendation

#1 Enable social networking system an application,  so users can post their favorite workouts,
#  wellness tips, healthy meals, and workout tips in the app.

#2 Enable users to add friends and view each other’s activity to create a competitive environment.

#3 Create weekly fitness and wellness challenges to encourage users , so that they workout
#  a decent time in a week.

#4 Have health and fitness companies pay for advertising in the application.

#5 Recommend users to get 10,000 steps a day and enable alert notifications to encourage users to
#  meet goal.

#6 Recommend users get at least 75 minutes of vigorous activity in a week and enable alert
# notifications to encourage users to meet recommended vigorous time .

#7 Encourage users to enter Age, height and Weight to track BMI .

#8 Alert users if their rest heart rate varies from  normal.

#9 Set notification if a users spends more then an hour awake in bed . 

#10 Alert users if they are spending more time then normal in sedentary state. Encourage them
#   to do some activity rather then getting onto sedentary state.


#11 For  weight log , the input is system is manual . It will be great if weight data can be
#   Tracked and update automatically under Wifi connection. 


## Recommendation for Bellabeat Membership

#1 Offer 30-day free trial subscription.
#2 Offer reduced subscription fee when a member refers a friend.
#3 Offer discounts for Bellabeat smart device products with membership.
#4 Partner with health & fitness companies and offer discounts for members.
#  Recommendations for Bellabeat products:
#5 Offer a bundle deal for the Spring and Leaf together.


