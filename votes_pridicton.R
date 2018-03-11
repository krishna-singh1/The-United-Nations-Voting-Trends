library(countrycode)
library(plyr)
library(ggplot2)
votes1 <- read.table("VotingData.csv",sep = ",",header = TRUE)
View(votes1)
summary(votes1)
nrow(votes1)

#filter data
votes1 <- subset(votes1,vote<=3)
summary(votes1)
nrow(votes1)

#create column "year"
votes1$year <- votes1$session + 1945
head(votes1)
#total Votes
totalVotes<-nrow(votes1)
totalVotes

#total percent of positive votes
yesVotes<-subset(votes1,vote==1)
nrow(yesVotes)

yesPercent<-(nrow(yesVotes)/totalVotes)
yesPercent

#Percent of positive votes across all years and countries.

#adding country column with special library with country code
votes1$country <- countrycode(votes1$ccode, "cown", "country.name")
head(votes1)

#group by year
#total votes by year
total_votes_by_yearly<-ddply(votes1,.(year),summarize,total_votes=length(vote))
#positive votes by year
positive_votes<-subset(votes1,vote==1)
positive_votes_by_yearly<-ddply(positive_votes,.(year),summarize,total_votes=length(vote))

#abstain votes by year 
abstain_votes<- subset(votes1,vote==2) 
abstain_votes_by_yearly <-ddply(abstain_votes,.(year),summarize,total_votes=length(vote))

#negative votes by year
negative_votes<-subset(votes1,vote==3)
negative_votes_by_yearly<-ddply(negative_votes,.(year),summarize,total_votes=length(vote))

#merging data frames with Reduce method
votes_by_year<- Reduce(function(x, y) merge(x, y, all=TRUE,by= "year"), list(total_votes_by_yearly, positive_votes_by_yearly, abstain_votes_by_yearly, negative_votes_by_yearly))
votes_by_year<-setNames(votes_by_year,c("year","Total_Votes", "Positive_Votes", "Abstain_Votes", "Negative_Votes"))
head(votes_by_year)

#Percent of positive votes across all years

#percentage of Positive
votes_by_year$Percent_Positive <- round((votes_by_year$Positive_Votes / votes_by_year$Total_Votes), 2)

#percentage of Abstain 
votes_by_year$Percent_Abstain <- round((votes_by_year$Abstain_Votes / votes_by_year$Total_Votes), 2)

#percentage of Negative
votes_by_year$Percent_Negative <- round((votes_by_year$Negative_Votes / votes_by_year$Total_Votes), 2)
head(votes_by_year)


#visualiziong persent changes over years. 

ggplot(votes_by_year, aes(x = year, y = Percent_Positive)) + geom_line() + geom_smooth() + ggtitle("Trends in Positive Percent and Year") 
ggplot(votes_by_year, aes(x = year, y = Percent_Negative)) + geom_line() + geom_smooth() + ggtitle("Trends in Negative Percent and Year") 
ggplot(votes_by_year, aes(x = year, y = Percent_Abstain)) + geom_line() + geom_smooth() + ggtitle("Trends in Abstain Percent and Year") 


#Percent of positive votes across all years countries
#total votes by Country
total_votes_by_country<-ddply(votes1,.(country),summarize,total_votes=length(vote))

#positive votes by Country
positive_votes_by_country<-ddply(positive_votes,.(country),summarize,total_votes=length(vote))

#abstain votes by year 
abstain_votes_by_country <-ddply(abstain_votes,.(country),summarize,total_votes=length(vote))

#negative votes by year
negative_votes_by_country<-ddply(negative_votes,.(country),summarize,total_votes=length(vote))


#merging data frames with Reduce method
votes_by_country <-  Reduce(function(x, y) merge(x, y, all=TRUE,by="country"), list(total_votes_by_country, positive_votes_by_country, abstain_votes_by_country,negative_votes_by_country))
votes_by_country<-setNames(votes_by_country,c("Country","Total_Votes","Positive_Votes","Abstain_Votes","Negative_Votes"))
head(votes_by_country)

# Percentage Positive by country
votes_by_country$Percent_Positive <- round((votes_by_country$Positive_Votes / votes_by_country$Total_Votes), 3)
# Percent Negative by country 
votes_by_country$Percent_Negative <- round((votes_by_country$Negative_Votes / votes_by_country$Total_Votes), 3)
# Percent Abstain by country 
votes_by_country$Percent_Abstain <- round((votes_by_country$Abstain_Votes / votes_by_country$Total_Votes), 3)

head(votes_by_country)

#Find top 5 countries which vote mostly positive.
SortedPositive<-arrange(votes_by_country,Positive_Votes,decreasing=TRUE)
top_Positive<-SortedPositive[1:5, c(1,2,3, 6)]
top_Positive

#Find top 5 countries which vote mostly negative.
SortedNegative<-arrange(votes_by_country,Negative_Votes,decreasing=TRUE)
top_Negative<-SortedNegative[1:5, c(1,2,5,7)]
top_Negative



#some visualization tricks. Things, which ggplot can do for you. 
votes_factor<-votes1
votes_factor$vote <- factor(votes1$vote)
str(votes_factor)

#renamig factors
votes_factor$vote<-revalue(votes_factor$vote,c("1"="Positive","2"="Abstain","3"="Negative"))
head(votes_factor)


#stacked bar 
ggplot(votes_factor, aes(year, fill=vote)) + geom_bar() + ggtitle("Stacked Bar Showing votes distribution yearly")


#grouped bar
ggplot(votes_factor[votes_factor$year > 1985,], aes(year, fill=vote)) + geom_bar(position = "dodge") +ggtitle("Grouped Bar Showing votes distribution yearly")

