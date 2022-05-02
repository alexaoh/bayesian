library(dplyr)
library(Hmisc)
library(ggplot2)
library(forcats)


##################################################
#### Data exploration and feature engineering ####
##################################################

df <- read.csv2("./2021.05.11 KA1 mobilities eligible finalised started in 2019.csv")
dim(df)
head(df)
names(df)

# Remove non-necessary columns
df <- df %>% select(-c("Project.Reference", "Mobility.Start.Month",
                       "Mobility.End.Month", "Field.of.Education",
                       "Education.Level", "Participant.Profile", "Special.Needs",
                       "Fewer.Opportunities", "GroupLeader", "Sending.City",
                       "Sending.Organization", "Sending.Organisation.Erasmus.Code",
                       "Receiving.City", "Receiving.Organization",
                       "Receiving.Organisation.Erasmus.Code", "Academic.Year"))

# Convert to proper data types
df <- df %>% transmute(duration = as.numeric(Mobility.Duration),
                    age = as.numeric(Participant.Age),
                    gender = as.factor(Participant.Gender),
                    nationality = as.factor(Participant.Nationality),
                    sending.country = as.factor(Sending.Country.Code),
                    receiving.country = as.factor(Receiving.Country.Code),
                    activity = as.factor(Activity..mob.),
                    participants = Participants)
summary(df)

# Observation: we should only include standard erasmus exchange students
df <- df %>% filter(activity == "Student mobility for studies between Programme Countries")
summary(df)

# Observations:
# - some weird age values
# - not many NA's, think we can safely remove those rows
# - more participants than one is an error I believe, we can probably remove those

df <- df %>% na.omit %>% filter(age > 15, age < 80, participants == 1)
summary(df)

# Observations:
# - Is 600 days duration too much? Should we treat is as an outlier?
# - Is one month too short of a stay? Should we not include those? 
length(df[df$duration >= 365,])
# Eight observations which has an Erasmus period over a year
df <- df %>% filter(duration < 365, duration > 20)
summary(df)
describe(df)



###############
#### Plots ####
###############

### Age
ggplot(data = df) +
  geom_bar(aes(age)) +
  xlim(17, 60) +
  labs(title = "Age of erasmus students",
       x = "Age",
       y = "Number of students")
ggsave("./plots/age_bar.png")

ggplot(data = df) + 
  geom_boxplot(aes(gender, age)) + 
  labs(title = "Age distribution for genders",
       x = "Gender",
       y = "Age")
ggsave("./plots/age_gender_box.png")

# Observations:
# - Higher age for males than females
# - Some old erasmus students!

### Countries

ggplot(data = df) + 
  geom_bar(aes(x = fct_infreq(sending.country), fill = sending.country)) + 
  labs(x = "Sending country",
       y = "Number of students",
       fill = "Sending country")
ggsave("./plots/sending_countries.png")

ggplot(data = df) + 
  geom_bar(aes(x = fct_infreq(receiving.country), fill = receiving.country)) +
  labs(x = "Receiving country",
       y = "Number of students",
       fill = "Receiving country")
ggsave("./plots/receiving_countries.png")

# Observations:
# - Spain has most incoming students, could plot more detailed about which 
#   institutions. Nor relevant for the analysis, but they might like it for 
#   the presentation.


### Duration
ggplot(data = df, aes(duration, color = gender, fill = gender)) + 
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") + 
  labs(x = "Duration [days]",
       y = "Number of students",
       color = "Gender",
       fill = "Gender")
ggsave("./plots/duration_hist.png")

ggplot(data = df, aes(duration, color = gender)) + 
  geom_density(position = "identity") +
  labs(x = "Duration [days]",
       y = "Density",
       color = "Gender",
       fill = "Gender")
ggsave("./plots/duration_densities.png", width = 10, height = 6)
