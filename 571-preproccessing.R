#crime dataset
crimeData = read.csv("Major_Crime_Indicators_Open_Data.csv")

#set to 2023
crimeIndex = which(crimeData$REPORT_YEAR == "2023")
crimeData = crimeData[crimeIndex,]

#eda
summary(crimeData)
str(crimeData)

pie(table(crimeData$MCI_CATEGORY))

pie(table(crimeData$OFFENCE))

#twitter dataset
tweetData = read.csv("tweets.csv")

#fidning instances including key words/terms
tweetIndex = c(
  which(grepl("theft", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("break and enter", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("robbery", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("assault", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("B&E", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("weapon", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("battery", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("steal", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("burglary", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("firearm", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("attack", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("force", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("harm", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("Burglary", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("gun", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("knife", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("pistol", tweetData$Text, ignore.case = TRUE) == T),
  which(grepl("kill", tweetData$Text, ignore.case = TRUE) == T)
)

tweetFil = tweetData[tweetIndex,]
tweetFil = na.omit(tweetFil)

write.csv(tweetFil, "tweetsFiltered.csv")

