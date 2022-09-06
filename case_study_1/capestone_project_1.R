

datas = list(read_csv("/Users/joy/Desktop/data_analysis/case_study_1_data/202207-divvy-tripdata.csv"),
         read_csv("/Users/joy/Desktop/data_analysis/case_study_1_data/202206-divvy-tripdata.csv"),
         read_csv("/Users/joy/Desktop/data_analysis/case_study_1_data/202205-divvy-tripdata.csv"),
         read_csv("/Users/joy/Desktop/data_analysis/case_study_1_data/202204-divvy-tripdata.csv"),
         read_csv("/Users/joy/Desktop/data_analysis/case_study_1_data/202203-divvy-tripdata.csv"),
         read_csv("/Users/joy/Desktop/data_analysis/case_study_1_data/202202-divvy-tripdata.csv"),
         read_csv("/Users/joy/Desktop/data_analysis/case_study_1_data/202201-divvy-tripdata.csv"),
         read_csv("/Users/joy/Desktop/data_analysis/case_study_1_data/202112-divvy-tripdata.csv"),
         read_csv("/Users/joy/Desktop/data_analysis/case_study_1_data/202111-divvy-tripdata.csv"),
         read_csv("/Users/joy/Desktop/data_analysis/case_study_1_data/202110-divvy-tripdata.csv"),
         read_csv("/Users/joy/Desktop/data_analysis/case_study_1_data/202109-divvy-tripdata.csv"),
         read_csv("/Users/joy/Desktop/data_analysis/case_study_1_data/202108-divvy-tripdata.csv")
      )


combined_df = bind_rows(datas)

cleaned_data = combined_df %>%  
  mutate(distance_traveled = distHaversine(cbind(start_lng, start_lat), cbind(end_lng, end_lat))) %>%
  filter(distance_traveled > 0) %>%
  mutate(duration_in_min = as.double(difftime(ended_at, started_at, units = 'mins'))) %>%
  filter(duration_in_min>0) %>% 
  replace(is.na(.), 'missing_value') %>%
  arrange(started_at)

cleaned_data$day_of_week <- weekdays(as.Date(cleaned_data$started_at))
cleaned_data$day_of_week <- factor(cleaned_data$day_of_week, levels=c('Saturday','Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))

# visualization section

## weekly frequency distribution 
ggplot(data = cleaned_data) +
  geom_bar(mapping = aes(x = day_of_week, fill = rideable_type)) +
  facet_wrap(~member_casual) + 
  theme(axis.text.x=element_text(angle=45, hjust=1))



## duration histogram

ggplot(filter(cleaned_data, cleaned_data$duration_in_min < 100)) +
  geom_histogram(mapping=aes(x=duration_in_min)) + 
  facet_wrap(~member_casual)

#Only observation here is that members tend to take short trips than casuals.
#Or casuals take longer trips than members. We will talk about the mean trip
#duration later using summary function.


## distance traveled in meters density graph

ggplot(filter(cleaned_data, cleaned_data$distance_traveled<10000))+
  geom_density(mapping=aes(x=distance_traveled)) + 
  facet_wrap(~member_casual)
# its hard to make any observations from this histogram . Instead we will summarize the data

## Summary of data casual and member separated.

members = filter(cleaned_data, cleaned_data$member_casual == 'member')
casual = filter(cleaned_data, cleaned_data$member_casual == 'casual')

# member summary 
member_summary = summary(drop_na(select(members, c('day_of_week', 'distance_traveled', 'duration_in_min'))))
# casual summary
casual_summary = summary(drop_na(select(casal, c('day_of_week', 'distance_traveled', 'duration_in_min'))))

##summary takeaways

#From above summary, we can observe that mean distance traveled by members and
#casuals are almost same, however, members mean trip duration ~12 min. is almost
#1 times less than casual mean trip duration ~23 min.
# And maximum distance traveled by members 42319m is 100x time less then casual 
# which is 1190854m 


## Most popular stations
#member 
head(count(members, start_station_name, sort=T), n=10)
head(count(members, end_station_name, sort=T), n=10)

#casual 
head(count(casual, start_station_name, sort=T), n=10)
head(count(casual, end_station_name, sort=T), n=10)

#From above summary we can see that members  most used start and 
# are same . And casual most used start and end stations are also same

### Concatenate start station with end station to observe most popular station
# member 
members$route = paste(members$start_station_name, ' ------ ', members$end_station_name)
head(count(members, route, sort=T), n=10)

# casual
casual$route = paste(casual$start_station_name, ' ------ ', casual$end_station_name)
head(count(casual, route, sort=T), n=10)

## Now we will summarize the data 

df = data.frame(
  'User Type'=c('Member', 'Casual'),
  'Amount'=c('3220072 (58.14%)', '2317603 (41.86%)'),
  'Avg_and_median_trip_dur'=c('12.70 min - 9.15 min', '23.62 min - 14.23 min'),
  'Avg_and_median_trip_distance'= c('2195.85 meters - 1587.75 meters', '2455 meters - 1891 meters'),
  'Busiest Day' = c('Tuesday', 'Saturday'),
  'Preferred bike type' = c('classic & electric ', 'electric'),
  'Most occaurd route' = c('Ellis Ave & 60th St -Ellis Ave & 55th St (5667)', 'DuSable Lake Shore Dr & Monroe St - Streeter Dr & Grand Ave (5463)')
  
)
formattable(df,
            align=c('l', 'c', 'c', 'c', 'c', 'c', 'r'),
            list('User Type' = formatter('span', style = ~style(color='gray', font.weight='bold'))))


# Now we will try to figure out why there is so many missing values
missing_data = drop_na(filter(cleaned_data, start_station_name=='missing_value'))
summary(select(missing_data, c('day_of_week', 'distance_traveled', 'duration_in_min')))

head(count(missing_data , member_casual, rideable_type, sor=T), n=10)
count(filter(cleaned_data, rideable_type=='electric_bike'))

# Among the missing values all of their bike type is electric . 
# There is total 2484562 electric bikes in use , and among them missing  count 
# is 737,501 which is 29.68% of total.


#### Total key observations 

# All though we can't answer the business question clearly because of data , but
# we will point out some analysis 

#1 Members bike usage is quite similar throughout the week except sunday which indicates that 
# most of them are employees or workers . And the busiest days are Tuesday, Wednesday and Thursday.

#2 Casual riders bike usage is pretty slow except Saturday and Sunday which indicates casual riders usage increases 
# during weekends.

#3 Among members most preferred bike is classic and electric bikes . And among casual riders most 
# preferred bikes are same as members . This derives  to a conclusion that both casual and members 
# likes using electric and classic bikes most.

#4 Average distance traveled by members and causal are almost same . However members avg trip duration
# is 12 minutes, which is 2 time less then casual riders. 

#5 In most cases both members and casual riders start and end stations are same .

#6 Most lengthy trips were taken by causal riders and they are abnormally long . For instance 
# top five of them are respectively 28, 23, 22, 20 and 19 days long.

# 7 All occurred  missing data around (750k) of start and end station names occurred only for electric
# bikes . There are around (2.5 million) electric bikes and among them 29.68% are missing . which is almost 750k.


## Business Answer and recommendations 

#Considering the above observations and insights we can suggest the following:
#  We see that members take shorter trips to work with bikes during Monday to Saturday, since it is financially viable and fast transportation. However, casuals prefer longer trips especially Saturday and Sunday. Thus:
#  We could increase the renting price of the bikes for the weekend to target casual users into having a membership especially for docked bikes, since they are preferred more by causal users.
#Providing a special service or perks for only members might motivate casual users to have a membership. Services might include free ice cream or lemonade, free tour guide, or fast line for renting without any line etc.
#Also, since we know the most popular start station names and routes for casual users, we can put banners or special discount advertisements in those areas or routes that would target casual users.

#Furthermore, all missing start and end station names occurred with electric bikes. We have to learn why that is the case and fix the infrastructure if necessary.

# writing csv file with fwrite from data.table
fwrite(cleaned_data, '/Users/joy/Desktop/data_analysis/case_study_1_c_data/cleaned_data.csv')


