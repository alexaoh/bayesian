library(dplyr)
library(Hmisc)


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

describe(df)
summary(df)

# Observation: we should only include standard erasmus exchange students
df <- df %>% filter(activity == "Student mobility for studies between Programme Countries")
summary(df)

# Observations:
# - some weird age values
# - not many NA's, think we can safely remove those rows
# - more participants than one is an error I believe, we can probably remove those

