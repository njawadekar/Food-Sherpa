## Cross tab of NY, SF, and BO; given sherpa table
# 
# We need to match on violations given the lookup table provided
# by the sherpa team. Once matched, we can run a cross tab. 

setwd("//home/tbonza//projects//Food-Sherpa")
library(sqldf)

# load data for Boston & NYC
ny <- read.csv("data//NY//Food_Service_Establishment__Last_Inspection.csv")
bo <- read.csv("data//BO//Food_Establishment_Inspections.csv")

# load data from food sherpa team (thanks Neal)
sherpa <- read.csv("data//SHERPA//MATCHED_violations.csv")

# SF provides 3 files but we really only need 'vioplus'
bizplus <- read.csv("data//SF//businesses_plus.csv")
insplus <- read.csv("data//SF//inspections_plus.csv")
vioplus <- read.csv("data//SF//violations_plus.csv")

# create a subset of NY, BO, and SF data for only 2014

# SF, vioplus needs a year column so we can filter on 2014
summary(as.numeric(substr(vioplus$date, 1,4)))
year <- as.numeric(substr(vioplus$date, 1,4))
vioplus <- cbind(vioplus, year)
vioplus2014 <- vioplus[vioplus$year == 2014,]
summary(vioplus2014$year)

# BO, also needs a year column
head(as.numeric(substr(as.character(bo$VIOLDTTM),7,10)))
summary(as.numeric(substr(as.character(bo$VIOLDTTM),7,10)))
year <- as.numeric(substr(as.character(bo$VIOLDTTM),7,10))
bo <- cbind(bo,year)
bo2014 <- bo[bo$year == 2014,]
summary(bo2014$year)


# NY needs a year column too
summary(as.numeric(substr(ny$LAST.INSPECTED, 7, 10)))
year <- as.numeric(substr(ny$LAST.INSPECTED, 7, 10))
ny <- cbind(ny, year)
ny2014 <- ny[ny$year == 2014,]
summary(ny2014$year)

# We need to check our lookup table from sherpa team to see if it 
# actually matches descriptions in the provided data.

descMatchPercent <- function(sherpa.desc, city.desc) {
  count = 0
  for (value in city.desc) {
    for (item in sherpa.desc) {
      if (is.na(value) != TRUE && value == item) {
        count = count + 1
      }
    }
  }
  return(count/length(city.desc))
}

# checking to see if descriptions match

# SF

# looks like our lookup table has duplicate values, that's ok
summary(duplicated(sherpa$SF_ViolDesc))
summary(sherpa$SF_ViolDesc[duplicated(sherpa$SF_ViolDesc)])

# we just need to note that we found a one-to-many relationship between
# BO and SF, then check for matches with duplicates removed.

# check the match percent: about 60%
descMatchPercent(unique(sherpa$SF_ViolDesc), vioplus2014$description) 


# BO

# checking for duplicates, there's none
summary(duplicated(sherpa$BO_ViolDesc))

# check the match percent: about 70%
descMatchPercent(sherpa$BO_ViolDesc, bo2014$ViolDesc)

# NY 

# checking for duplicates
summary(duplicated(ny$VIOLATIONS))

# check the match percent: 
descMatchPercent(sherpa$NY_ViolDesc, ny$VIOLATIONS)



# Given that we have a reasonable number (~60/70%), let's put together
# a dataset where BO & SF descriptions match, given our sherpa lookup table.
# This will allow us to have a ballpark idea about how certain violations are
# enforced in the two cities. 
comparison.bo <- sqldf("SELECT Violation, ViolLevel, ViolDesc, ViolStatus, Property_ID, Zip, Location 
                     FROM bo2014
                     INNER JOIN sherpa ON sherpa.BO_ViolDesc = bo2014.ViolDesc;")

# sanity check: BO should be ~70%
nrow(comparison.bo)/length(bo2014$ViolDesc)

sherpaSF <- data.frame(unique(sherpa$SF_ViolDesc))
colnames(sherpaSF) <- "SF_ViolDesc"

comparison.sf <- sqldf("SELECT business_id, date, ViolationTypeID, risk_category, description, year
                       FROM vioplus2014
                       INNER JOIN sherpaSF ON sherpaSF.SF_ViolDesc = vioplus2014.description;")

# sanity check: SF should be ~60%
nrow(comparison.sf)/length(vioplus2014$description)

# presenting our findings. We want to show how many times a specific violation was enforced. 
# We're also fighting a one-to-many relationship between SF and BO violations. I think the 
# best way to deal with it is to include the number of duplicates for SF in the frequency output.



# generate frequency table for BO
freq_bo <- data.frame(with(comparison.bo, table(ViolDesc)))
freq_sf <- data.frame(with(comparison.sf, table(description)))

# generate number of duplicates in sherpa table for SF
dupe_sf <- data.frame(aggregate(numdup ~., data=transform(sherpa$SF_ViolDesc, numdup=1), length))
colnames(dupe_sf) <- c("description","numdup")


# add number of duplicates to the original sherpa table along with frequencies from BO & SF

sf_final <- sqldf("SELECT freq_sf.description, freq_sf.Freq, dupe_sf.numdup
                  FROM freq_sf
                  INNER JOIN dupe_sf ON dupe_sf.description = freq_sf.description
                  ORDER BY freq_sf.description ASC;")

bo_final <- sqldf("SELECT 'sherpa.Index', sherpa.BO_ViolLevel, sherpa.BO_Violation,
                  sherpa.SF_ViolDesc, sherpa.SF_ViolLevel, sherpa.SF_Violation,
                  sherpa.NY_ViolDesc, sherpa.NY_ViolLevel, sherpa.NY_Violation,
                  sherpa.BO_Comments, sherpa.BO, sherpa.NY, sherpa.SF, freq_bo.Freq AS BO_freq
                  FROM sherpa
                  INNER JOIN freq_bo ON freq_bo.ViolDesc = sherpa.BO_ViolDesc;")

sherpa_final <- sqldf("SELECT bo_final.*, sf_final.Freq AS SF_freq, sf_final.numdup AS SF_dupes
                      FROM bo_final 
                      INNER JOIN sf_final ON sf_final.description = bo_final.SF_ViolDesc;")


# cool, this is the first draft. You'll have to include NY in there when more time becomes available.
write.csv(sherpa_final, "sherpa_final.csv")


