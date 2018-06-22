library(tidyverse)
secdata <- read_csv("Consolidated-Secondary-Data-with-grade.csv")

#For this analysis, we downloaded data from opendata.go.tz related to secondary education. I wanted to
#employ some concepts from R for Data Science book's first section. Mainly piping and visualizations!

#So let us begin. The number one thing that drew me to learn R again is its powerful ggplot. I outright
#avoided visualizations in Python because it just wasn't clicking for me even though I felt good
#about wrangling data in Python. Here's a quick bar chart showing number of schools that had
#less than 40 or more than 40 candidates at the school.

secdata %>%
  group_by(Candidates) %>%
  summarise(`Number of Schools` = n()) %>%
  ggplot(mapping = aes(x=Candidates, y=`Number of Schools`, fill=Candidates)) +
  geom_col()

#Nothing fancy, but effective visual aid. I owe it to Python that coding in R isn't as daunting anymore.
#It took me months to work out anything in Python. I understood this syntax in a matter of one read-through.

#Let's get to the meat of this post. This data claims to have a metric for "qualified teachers". Spoke with
#@her and we both agreed that we don't really know what it means to be a qualified teacher in Tanzania.
#More research will need to be done to get a sense for what this could possibly mean - some initial thoughts
#include: diploma, etc.

#Let's add some more complexity to our visualization. Tanzanian schools are either run by the national
#government or privately. With just a few lines of R code, we can create a powerful visualization that
#showcases the effect of increasing the proportion of unqualified teachers in a school with the passing
#rate in 2016.

secdata %>%
  mutate(teach_diff_female = (`Total Teachers-Female` - `Qualified Teachers-Female`)/`Total Teachers-Female`,
         teach_diff_male = (`Total Teachers-Male` - `Qualified Teachers-Male`)/`Total Teachers-Male`) %>%
  dplyr::filter(teach_diff_female < 1, teach_diff_female > 0.05, teach_diff_male < 1, teach_diff_male > 0.05) %>%
  ggplot(mapping = aes(x=teach_diff_male, y=`Pass Rate (2016)`))+
  geom_point() + 
  geom_point(mapping = aes(x=teach_diff_female, y=`Pass Rate (2016)`, color='Female Teachers')) +
  geom_smooth(se=FALSE, mapping=aes(color="Male Difference")) +
  geom_smooth(se=FALSE, mapping=aes(x=teach_diff_female, color="Female Difference")) +
  facet_wrap(~ `SCHOOL OWNERSHIP`) +
  labs(x = "Proportion of Unqualified Teachers",
       title = "Effects of Unqualified Teachers on Pass Rates",
       colour = "Legend")

#A lot of the data was concentrated on the extreme - that is schools reported 0% unqualified teachers. We'll
#return to that momentarily. This plot gets us close to answering whether having more unqualified teachers
#affects student pass rates. It doesn't seem like there
#is any clear trend between these variables. The average pass rate for private schools was much higher than
#public schools, but the proportion of unqualified male or female teachers had no real effect on how well 
#the kids did.

#Similar inconclusive trends when taking the proportion of all teachers. For private schools, the trend
#appears to be negative, but overall no crazy drop offs in performance despite increasingly "unqualified"
#teachers.

secdata %>%
  mutate(teach_diff = ((`Total Teachers-Female` + `Total Teachers-Male`) - (`Qualified Teachers-Female` + `Qualified Teachers-Male`))/(`Total Teachers-Female` + `Total Teachers-Male`)) %>%
  dplyr::filter(teach_diff < 1, teach_diff > 0) %>%
  ggplot(mapping = aes(x=teach_diff, y=`Pass Rate (2016)`))+
  geom_point() + 
  geom_point(mapping = aes(x=teach_diff, y=`Pass Rate (2016)`), colour = "#B4CDCD") +
  geom_smooth(se=FALSE) +
  facet_wrap(~ `SCHOOL OWNERSHIP`) +
  labs(x = "Proportion of Unqualified Teachers",
       title = "Effects of Unqualified Teachers on Pass Rates",
       colour = "Legend")

#Now I previously mentioned the reported 0% unqualified teachers - let's return to this phenomenon. 
#What if we wanted to know, on average, how is the proportion of unqualified teachers different 
#for government vs. non-gov schools? 

secdata %>%
  group_by(`SCHOOL OWNERSHIP`) %>%
  summarise(schools = n(),
            mean_pass_rate = mean(`Pass Rate (2016)`, na.rm=TRUE),
            prop_unqualified_female = mean((`Total Teachers-Female` - `Qualified Teachers-Female`)/`Total Teachers-Female`, na.rm=TRUE),
            prop_unqualified_male = mean((`Total Teachers-Male` - `Qualified Teachers-Male`)/`Total Teachers-Male`, na.rm = TRUE)) %>%
  gather(key = 'teacher_proportion', value= 'mean_proportion' , prop_unqualified_male, prop_unqualified_female, -`SCHOOL OWNERSHIP`) %>%
  ggplot(mapping = aes(x=`SCHOOL OWNERSHIP`, y = mean_proportion, fill = teacher_proportion)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(y = "Mean Proportion of Teachers",
       title = "Proportion of Unqualified Teachers",
       subtitle = "by Gender and School Type") +
  scale_fill_discrete(name = "Legend",
                      breaks = c("prop_unqualified_female", "prop_unqualified_male"),
                      labels = c("Unqualified Female Teachers", "Unqualified Male Teachers"))

#On average, private schools have almost 4 times more unqualified female teachers, and almost
#5 times unqualified male teachers.

#When using medians, a statistically more robust measure, things get kinda strange.

secdata %>%
  group_by(`SCHOOL OWNERSHIP`) %>%
  summarise(schools = n(),
            median_pass_rate = median(`Pass Rate (2016)`, na.rm=TRUE),
            prop_unqualified_female = median((`Total Teachers-Female` - `Qualified Teachers-Female`)/`Total Teachers-Female`, na.rm=TRUE),
            prop_unqualified_male = median((`Total Teachers-Male` - `Qualified Teachers-Male`)/`Total Teachers-Male`, na.rm = TRUE)) %>%
  gather(key = 'teacher_proportion', value= 'mean_proportion' , prop_unqualified_male, prop_unqualified_female, -`SCHOOL OWNERSHIP`) %>%
  ggplot(mapping = aes(x=`SCHOOL OWNERSHIP`, y = mean_proportion, fill = teacher_proportion)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(y = "Median Proportion of Teachers",
       title = "Proportion of Unqualified Teachers",
       subtitle = "by Gender and School Type") +
  scale_fill_discrete(name = "Legend",
                      breaks = c("prop_unqualified_female", "prop_unqualified_male"),
                      labels = c("Unqualified Female Teachers", "Unqualified Male Teachers"))

#There are far more 0s in the vector than one would expect! But we can make sense of the strangeness. 
#It's all self-reported and when counting the distribution of answers
#you can begin to suspect a human's hand in the data (note the 0, 1, .25, .33, .5, etc.). 

secdata %>%
  mutate(prop = (`Total Teachers-Female` - `Qualified Teachers-Female`)/`Total Teachers-Female`,
         schools = ifelse(`SCHOOL OWNERSHIP` == 'GOVERNMENT', sum(`SCHOOL OWNERSHIP` == 'GOVERNMENT'),
                          sum(`SCHOOL OWNERSHIP` != 'GOVERNMENT'))) %>%
  group_by(prop, `SCHOOL OWNERSHIP`) %>% 
  summarise(number = n()) %>% 
  mutate(`% of Schools` = ifelse(`SCHOOL OWNERSHIP` == 'GOVERNMENT', 
                                 number/(sum(secdata$`SCHOOL OWNERSHIP` == 'GOVERNMENT')),
                                 number/(sum(secdata$`SCHOOL OWNERSHIP` == 'NON-GOVERNMENT')))) %>%
  arrange(desc(number))

secdata %>%
  mutate(prop = (`Total Teachers-Male` - `Qualified Teachers-Male`)/`Total Teachers-Male`,
         schools = ifelse(`SCHOOL OWNERSHIP` == 'GOVERNMENT', sum(`SCHOOL OWNERSHIP` == 'GOVERNMENT'),
                          sum(`SCHOOL OWNERSHIP` != 'GOVERNMENT'))) %>%
  group_by(prop, `SCHOOL OWNERSHIP`) %>% 
  summarise(number = n()) %>% 
  mutate(`% of Schools` = ifelse(`SCHOOL OWNERSHIP` == 'GOVERNMENT', 
                                 number/(sum(secdata$`SCHOOL OWNERSHIP` == 'GOVERNMENT')),
                                 number/(sum(secdata$`SCHOOL OWNERSHIP` == 'NON-GOVERNMENT')))) %>%
  arrange(desc(number))

#Many schools reported that all their teachers are
#"qualified". With no clear sense of what it means to be qualified and likely no validation/confirmation
#process by the government, there is an incentive to claim all of the school's teachers are qualified
#even if this may not be entirely true.
#But also note how high the number of 
#NaNs are for female teachers. Several explanations could hold water for this value and none are particularly
#encouraging. No female teachers, female teachers unaccounted for (invisble women?), or poor data collection.



#Conclusion: Could be encouraging to think that in the classroom, teachers are more often than not qualified.
#Alternatively, if we assume these numbers are accurate and that teachers are qualified as reported, then
#the current means of qualifying teachers is not an effective barometer for the quality of the teacher.
#I find that conclusion to be unnecessarily cynical. A more optimistic, though still challenging reason,
#is that placing good teachers in the classroom is not enough. There are still uncaptured challenges
#for schools to overcome in order to close achievement gaps. We don't have enough data to describe how
#students in private vs. public schools differ, but one can imagine that these differences may account
#for a good portion of the notable difference in average passing rate in 2016 between the 
#two types of schools.
  