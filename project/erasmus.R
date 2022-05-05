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
any(is.na(df %>% select(age))) # There are no age NA values after filtering on the activity above (only standard Erasmus exchange).
colSums(is.na(df)) # There are only 4 missing nationalities, that's it.

# - more participants than one is an error I believe, we can probably remove those
dim(df %>% filter(participants > 1)) #There are approx 9000 of these. 


df <- df %>% na.omit %>% filter(age > 15, age < 80, participants == 1)
summary(df)

# Observations:
# - Is 600 days duration too much? Should we treat it as an outlier?
# - Is one month too short of a stay? Should we not include those? 
dim(df %>% filter(duration > 365)) 
# 88 observations which has an Erasmus period over a year.
dim(df %>% filter(duration < 20)) 
# 32 observations have an Erasmus period of less than 20 days. 
df <- df %>% filter(duration <= 365, duration >= 20)
summary(df)
describe(df)

# I write this datafram to a csv file in order to easily read in other files (already cleaned)
write.csv(df,"cleaned.csv", row.names = FALSE)

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
ggsave("./626fca86090ba51a6aff419a/plots/age_bar.pdf")

ggplot(data = df) + 
  geom_boxplot(aes(gender, age)) + 
  labs(title = "Age distribution for genders",
       x = "Gender",
       y = "Age")
ggsave("./626fca86090ba51a6aff419a/plots/age_gender_box.pdf")

# Observations:
# - Higher age for males than females
# - Some old erasmus students!

### Countries

ggplot(data = df) + 
  geom_bar(aes(x = fct_infreq(sending.country), fill = sending.country)) + 
  labs(x = "Sending country",
       y = "Number of students",
       fill = "Sending country")
ggsave("./626fca86090ba51a6aff419a/plots/sending_countries.pdf")

ggplot(data = df) + 
  geom_bar(aes(x = fct_infreq(receiving.country), fill = receiving.country)) +
  labs(x = "Receiving country",
       y = "Number of students",
       fill = "Receiving country")
ggsave("./626fca86090ba51a6aff419a/plots/receiving_countries.pdf")

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
ggsave("./626fca86090ba51a6aff419a/plots/duration_hist.pdf")

ggplot(data = df, aes(duration, color = gender)) + 
  geom_density(position = "identity") +
  labs(x = "Duration [days]",
       y = "Density",
       color = "Gender",
       fill = "Gender") 
ggsave("./626fca86090ba51a6aff419a/plots/duration_densities.pdf", width = 10, height = 6)

# Observation / theories: 
# - To me it looks like the duration can be reasonably well described by a mix of two Gaussians: 
# 1) One Gaussian with mean around 130 ish, with a smaller variance.
# 2) Another Gaussian with mean around 280, with a larger variance. 
# Se eksempel nedenfor!
N <- 100000
components <- sample(1:2,prob=c(0.7,0.3),size=N,replace=TRUE)
mus <- c(120,280)
sds <- c(10,18) 

samples <- rnorm(N)*sds[components]+mus[components]

tibble(samples) %>% 
  ggplot(aes(samples)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  ggtitle("Mix of Gaussian")
