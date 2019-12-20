library(dplyr)
library(ggplot2)

getwd()

#Load data
voted <- read.delim("data/history_stats_20161108.txt", header = TRUE, sep = "\t")
registered <- read.delim("data/voter_stats_20161108.txt", header = TRUE, sep = " ", quote = "\"")

#Check consistency of data in both the data frames for 8 common fields:
all(levels(voted$county_desc) == levels(registered$county_desc)) #ok
all(levels(voted$precinct_abbrv) == levels(registered$precinct_abbrv)) #bad

#check levels
levels(voted$precinct_abbrv)
levels(registered$precinct_abbrv)

#Registered has blank values 
nrow(registered[registered$precinct_abbrv == " ", ])

registered_cleaned <- registered[registered$precinct_abbrv != " ", ]

# Drop unused levels from cleanup
registered_cleaned$precinct_abbrv <- droplevels(registered_cleaned$precinct_abbrv)

all(levels(voted$precinct_abbrv) == levels(registered_cleaned$precinct_abbrv)) #ok now

all(levels(voted$vtd_abbrv) == levels(registered_cleaned$vtd_abbrv)) #ok
all(levels(voted$age) == levels(registered_cleaned$age)) #bad, 
levels(registered_cleaned$age)
levels(voted$age) #has extra age group in voted df

nrow(voted[voted$age == "Age < 18 Or Invalid Birth Dates", ]) # 1 row
voted_cleaned <- voted[(voted$age != "Age < 18 Or Invalid Birth Dates"), ]
#drop the deleted level
voted_cleaned$age <- droplevels(voted_cleaned$age)
all(levels(voted_cleaned$age) == levels(registered_cleaned$age)) #ok

all(levels(voted_cleaned$voted_party_cd) == levels(registered_cleaned$party_cd)) #bad
levels(registered_cleaned$party_cd)
levels(voted_cleaned$voted_party_cd) #has blanks
nrow(voted_cleaned[voted_cleaned$voted_party_cd == "", ]) # 47 rows
voted_cleaned <-  voted_cleaned[voted_cleaned$voted_party_cd != "", ]
#drop the deleted level
voted_cleaned$voted_party_cd <- droplevels(voted_cleaned$voted_party_cd)


all(levels(voted_cleaned$race_code) == levels(registered_cleaned$race_code)) #bad
levels(voted_cleaned$race_code)
levels(registered_cleaned$race_code) #has empty value in registered

#remove the empty records - 1136 rows
nrow(registered_cleaned[registered_cleaned$race_code == "", ]) # 3 rows

registered_cleaned <- registered_cleaned[registered_cleaned$race_code != "", ]

#drop the deleted level
registered_cleaned$race_code <- droplevels(registered_cleaned$race_code)
all(levels(voted_cleaned$race_code) == levels(registered_cleaned$race_code)) #OK now

all(levels(voted_cleaned$ethnic_code) == levels(registered_cleaned$ethnic_code)) #ok
all(levels(voted_cleaned$sex_code) == levels(registered_cleaned$sex_code)) #bad
levels(registered_cleaned$sex_code) 
levels(voted_cleaned$sex_code) #has empty gender and N for voted

nrow(voted_cleaned[voted_cleaned$sex_code == "N", ]) #  1 row
nrow(voted_cleaned[voted_cleaned$sex_code == " ", ]) # 7 rows

# drop these
voted_cleaned <- voted_cleaned[!(voted_cleaned$sex_code == " "), ]
voted_cleaned <- voted_cleaned[!(voted_cleaned$sex_code == "N"), ]

#drop the deleted level
voted_cleaned$sex_code <- droplevels(voted_cleaned$sex_code)

all(levels(voted_cleaned$sex_code) == levels(registered_cleaned$sex_code)) #ok


#Now, all the fields are in sync with same number of levels and empty/blanks fields removed
# drop party_cd column from voted df (duplicate with voted_party_cd)
voted_cleaned <- subset(voted_cleaned, select = -c(party_cd))
# rename voted voted_party_cd to party_cd to align naming with registered
voted_cleaned$party_cd <- voted_cleaned$voted_party_cd
# drop voted voted_party_cd column
voted_cleaned <- subset(voted_cleaned, select = -c(voted_party_cd))

# drop voted election_date, update_date, stats_type, voting_method, voting_method_desc column
voted_cleaned <- subset(voted_cleaned, select = -c(election_date, update_date, stats_type, voting_method, voting_method_desc))
# drop registered election_date, stats_type column
registered_cleaned <- subset(registered_cleaned, select = -c(election_date, stats_type))

# rename voted total_voters to actual_voters to avoid same name with registered
voted_cleaned$actual_voters <- voted_cleaned$total_voters
# drop voted total_voters column
voted_cleaned <- subset(voted_cleaned, select = -c(total_voters))


# no duplicate rows in registered
any(duplicated(registered_cleaned))
# duplicate rows in voted due to removal of voting_method
any(duplicated(voted_cleaned))
# aggregate actual_voters based on 8 demo variables in voted, resave into new df --> 403343 rows
voted_cleaned_agg <- aggregate(voted_cleaned$actual_voters, by=list(voted_cleaned$county_desc, 
                                                                    voted_cleaned$precinct_abbrv, 
                                                                    voted_cleaned$vtd_abbrv, 
                                                                    voted_cleaned$party_cd,
                                                                    voted_cleaned$race_code,
                                                                    voted_cleaned$ethnic_code,
                                                                    voted_cleaned$sex_code,
                                                                    voted_cleaned$age), FUN=sum)

colnames(voted_cleaned_agg) <- c("county_desc", "precinct_abbrv", "vtd_abbrv", 
                                 "party_cd", "race_code", "ethnic_code", 
                                 "sex_code", "age", "actual_voters")

any(duplicated(voted_cleaned_agg))

# merge voted_cleaned_agg with registered_cleaned --> 397687 rows with 10 variables
voting2016 <- inner_join(voted_cleaned_agg, registered_cleaned)
summary(voting2016)
str(voting2016)

# select 20 random counties --> 48720 rows
set.seed(1000)
sample_county <- sample(unique(voting2016$county_desc), 20, replace=F)
sample_voting2016 <- voting2016[is.element(voting2016$county_desc, sample_county),]

### data cleaning after sampling df

# remove unused levels
sample_voting2016 <- droplevels(sample_voting2016)
# data summary
summary(sample_voting2016)
str(sample_voting2016)

# new column for number of non voters
sample_voting2016$non_voters <- sample_voting2016$total_voters - sample_voting2016$actual_voters
# 363 rows with -ve non voter counts: actual voters more than total 
nrow(sample_voting2016[sample_voting2016$non_voters<0,])
# remove and save --> 48357 rows
sample_voting2016 <- sample_voting2016[!(sample_voting2016$non_voters<0), ]

#Final Output file
write.csv(sample_voting2016, file = "sample_voting2016.csv")