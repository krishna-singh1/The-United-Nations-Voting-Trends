---
title: "The United Nations Voting Trends"
author: "Krrish"
date: "March 10, 2018"
output:
  html_document:
    keep_md: true
---

##importing library

```r
library(countrycode)
library(plyr)
library(ggplot2)
```
##reading CSV Files

```r
votes1 <- read.table("VotingData.csv",sep = ",",header = TRUE)
summary(votes1)
```

```
##      ccode          session           rcid           vote      
##  Min.   :  2.0   Min.   : 1.00   Min.   :   3   Min.   :1.000  
##  1st Qu.:290.0   1st Qu.:26.00   1st Qu.:1333   1st Qu.:1.000  
##  Median :452.0   Median :38.00   Median :2664   Median :1.000  
##  Mean   :469.8   Mean   :37.65   Mean   :2699   Mean   :3.629  
##  3rd Qu.:680.0   3rd Qu.:51.00   3rd Qu.:4009   3rd Qu.:8.000  
##  Max.   :990.0   Max.   :69.00   Max.   :9036   Max.   :9.000  
##  NA's   :81                                     NA's   :15714
```

```r
nrow(votes1)
```

```
## [1] 1048575
```
##filter data

```r
votes1 <- subset(votes1,vote<=3)
summary(votes1)
```

```
##      ccode          session           rcid           vote      
##  Min.   :  2.0   Min.   : 1.00   Min.   :   3   Min.   :1.000  
##  1st Qu.:220.0   1st Qu.:32.00   1st Qu.:1818   1st Qu.:1.000  
##  Median :438.0   Median :41.00   Median :3059   Median :1.000  
##  Mean   :447.2   Mean   :41.48   Mean   :2995   Mean   :1.276  
##  3rd Qu.:660.0   3rd Qu.:54.00   3rd Qu.:4225   3rd Qu.:1.000  
##  Max.   :990.0   Max.   :68.00   Max.   :9036   Max.   :3.000
```

```r
nrow(votes1)
```

```
## [1] 709095
```
##create column "year"

```r
votes1$year <- votes1$session + 1945
head(votes1)
```

```
##    ccode session rcid vote year
## 4    651       1    3    2 1946
## 21    91       1    3    1 1946
## 27    94       1    3    1 1946
## 28    92       1    3    1 1946
## 35   220       1    3    3 1946
## 36   165       1    3    1 1946
```
##total Votes

```r
totalVotes<-nrow(votes1)
totalVotes
```

```
## [1] 709095
```

##total percent of positive votes

```r
yesVotes<-subset(votes1,vote==1)
nrow(yesVotes)
```

```
## [1] 565342
```

```r
yesPercent<-(nrow(yesVotes)/totalVotes)
yesPercent
```

```
## [1] 0.7972726
```
#Percent of positive votes across all years and countries.

##adding country column with special library with country code

```r
votes1$country <- countrycode(votes1$ccode, "cown", "country.name")
```

```
## Warning in countrycode(votes1$ccode, "cown", "country.name"): Some values were not matched unambiguously: 260, 816
```

```r
head(votes1)
```

```
##    ccode session rcid vote year     country
## 4    651       1    3    2 1946       Egypt
## 21    91       1    3    1 1946    Honduras
## 27    94       1    3    1 1946  Costa Rica
## 28    92       1    3    1 1946 El Salvador
## 35   220       1    3    3 1946      France
## 36   165       1    3    1 1946     Uruguay
```
#group by year
##total votes by year

```r
total_votes_by_yearly<-ddply(votes1,.(year),summarize,total_votes=length(vote))
##positive votes by year
positive_votes<-subset(votes1,vote==1)
positive_votes_by_yearly<-ddply(positive_votes,.(year),summarize,total_votes=length(vote))

##abstain votes by year 
abstain_votes<- subset(votes1,vote==2) 
abstain_votes_by_yearly <-ddply(abstain_votes,.(year),summarize,total_votes=length(vote))

##negative votes by year
negative_votes<-subset(votes1,vote==3)
negative_votes_by_yearly<-ddply(negative_votes,.(year),summarize,total_votes=length(vote))
```
##merging data frames with Reduce method

```r
votes_by_year<- Reduce(function(x, y) merge(x, y, all=TRUE,by= "year"), list(total_votes_by_yearly, positive_votes_by_yearly, abstain_votes_by_yearly, negative_votes_by_yearly))
```

```
## Warning in merge.data.frame(x, y, all = TRUE, by = "year"): column names
## 'total_votes.x', 'total_votes.y' are duplicated in the result
```

```r
votes_by_year<-setNames(votes_by_year,c("year","Total_Votes", "Positive_Votes", "Abstain_Votes", "Negative_Votes"))
head(votes_by_year)
```

```
##   year Total_Votes Positive_Votes Abstain_Votes Negative_Votes
## 1 1946        2143           1229           270            644
## 2 1947        2039           1161           279            599
## 3 1948        5685           2288          1148           2249
## 4 1949        3469           1518           821           1130
## 5 1950        3079           1562           542            975
## 6 1951        1434            839           242            353
```
#Percent of positive votes across all years

##percentage of Positive

```r
votes_by_year$Percent_Positive <- round((votes_by_year$Positive_Votes / votes_by_year$Total_Votes), 2)

##percentage of Abstain 
votes_by_year$Percent_Abstain <- round((votes_by_year$Abstain_Votes / votes_by_year$Total_Votes), 2)

##percentage of Negative
votes_by_year$Percent_Negative <- round((votes_by_year$Negative_Votes / votes_by_year$Total_Votes), 2)
head(votes_by_year)
```

```
##   year Total_Votes Positive_Votes Abstain_Votes Negative_Votes
## 1 1946        2143           1229           270            644
## 2 1947        2039           1161           279            599
## 3 1948        5685           2288          1148           2249
## 4 1949        3469           1518           821           1130
## 5 1950        3079           1562           542            975
## 6 1951        1434            839           242            353
##   Percent_Positive Percent_Abstain Percent_Negative
## 1             0.57            0.13             0.30
## 2             0.57            0.14             0.29
## 3             0.40            0.20             0.40
## 4             0.44            0.24             0.33
## 5             0.51            0.18             0.32
## 6             0.59            0.17             0.25
```

#visualiziong persent changes over years. 

```r
ggplot(votes_by_year, aes(x = year, y = Percent_Positive)) + geom_line() + geom_smooth() + ggtitle("Trends in Positive Percent and Year") 
```

```
## `geom_smooth()` using method = 'loess'
```

![](United_Nations_Voting_Trends_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
ggplot(votes_by_year, aes(x = year, y = Percent_Negative)) + geom_line() + geom_smooth() + ggtitle("Trends in Negative Percent and Year") 
```

```
## `geom_smooth()` using method = 'loess'
```

![](United_Nations_Voting_Trends_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
ggplot(votes_by_year, aes(x = year, y = Percent_Abstain)) + geom_line() + geom_smooth() + ggtitle("Trends in Abstain Percent and Year") 
```

```
## `geom_smooth()` using method = 'loess'
```

![](United_Nations_Voting_Trends_files/figure-html/unnamed-chunk-11-3.png)<!-- -->
#Percent of positive votes across all years countries
##total votes by Country

```r
total_votes_by_country<-ddply(votes1,.(country),summarize,total_votes=length(vote))

##positive votes by Country
positive_votes_by_country<-ddply(positive_votes,.(country),summarize,total_votes=length(vote))

##abstain votes by year 
abstain_votes_by_country <-ddply(abstain_votes,.(country),summarize,total_votes=length(vote))

##negative votes by year
negative_votes_by_country<-ddply(negative_votes,.(country),summarize,total_votes=length(vote))
```
##merging data frames with Reduce method

```r
votes_by_country <-  Reduce(function(x, y) merge(x, y, all=TRUE,by="country"), list(total_votes_by_country, positive_votes_by_country, abstain_votes_by_country,negative_votes_by_country))
```

```
## Warning in merge.data.frame(x, y, all = TRUE, by = "country"): column names
## 'total_votes.x', 'total_votes.y' are duplicated in the result
```

```r
votes_by_country<-setNames(votes_by_country,c("Country","Total_Votes","Positive_Votes","Abstain_Votes","Negative_Votes"))
head(votes_by_country)
```

```
##             Country Total_Votes Positive_Votes Abstain_Votes
## 1       Afghanistan        4804           4029           496
## 2           Albania        3349           2417           403
## 3           Algeria        4359           3915           306
## 4           Andorra        1410            918           301
## 5            Angola        2950           2721           185
## 6 Antigua & Barbuda        2521           2312           196
##   Negative_Votes
## 1            279
## 2            529
## 3            138
## 4            191
## 5             44
## 6             13
```


```r
# Percentage Positive by country
votes_by_country$Percent_Positive <- round((votes_by_country$Positive_Votes / votes_by_country$Total_Votes), 3)
# Percent Negative by country 
votes_by_country$Percent_Negative <- round((votes_by_country$Negative_Votes / votes_by_country$Total_Votes), 3)
# Percent Abstain by country 
votes_by_country$Percent_Abstain <- round((votes_by_country$Abstain_Votes / votes_by_country$Total_Votes), 3)

head(votes_by_country)
```

```
##             Country Total_Votes Positive_Votes Abstain_Votes
## 1       Afghanistan        4804           4029           496
## 2           Albania        3349           2417           403
## 3           Algeria        4359           3915           306
## 4           Andorra        1410            918           301
## 5            Angola        2950           2721           185
## 6 Antigua & Barbuda        2521           2312           196
##   Negative_Votes Percent_Positive Percent_Negative Percent_Abstain
## 1            279            0.839            0.058           0.103
## 2            529            0.722            0.158           0.120
## 3            138            0.898            0.032           0.070
## 4            191            0.651            0.135           0.213
## 5             44            0.922            0.015           0.063
## 6             13            0.917            0.005           0.078
```
#Find top 5 countries which vote mostly positive.

```r
SortedPositive<-arrange(votes_by_country,Positive_Votes,decreasing=TRUE)
top_Positive<-SortedPositive[1:5, c(1,2,3, 6)]
top_Positive
```

```
##       Country Total_Votes Positive_Votes Percent_Positive
## 1      Mexico        5208           4509            0.866
## 2 Philippines        5166           4457            0.863
## 3       Egypt        5178           4413            0.852
## 4   Venezuela        5201           4390            0.844
## 5    Pakistan        5113           4367            0.854
```
#Find top 5 countries which vote mostly negative.

```r
SortedNegative<-arrange(votes_by_country,Negative_Votes,decreasing=TRUE)
top_Negative<-SortedNegative[1:5, c(1,2,5,7)]
top_Negative
```

```
##          Country Total_Votes Negative_Votes Percent_Negative
## 1  United States        5217           2596            0.498
## 2         Israel        4772           1761            0.369
## 3 United Kingdom        5197           1466            0.282
## 4         France        5151           1194            0.232
## 5        Belgium        5217           1110            0.213
```


#Createing stacked and grouped bar charts showing votes distribution from 1985 in three categories, Positive, Negative, Abstain.

```r
votes_factor<-votes1
votes_factor$vote <- factor(votes1$vote)
str(votes_factor)
```

```
## 'data.frame':	709095 obs. of  6 variables:
##  $ ccode  : int  651 91 94 92 220 165 155 130 160 41 ...
##  $ session: int  1 1 1 1 1 1 1 1 1 1 ...
##  $ rcid   : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ vote   : Factor w/ 3 levels "1","2","3": 2 1 1 1 3 1 1 1 1 1 ...
##  $ year   : num  1946 1946 1946 1946 1946 ...
##  $ country: chr  "Egypt" "Honduras" "Costa Rica" "El Salvador" ...
```

```r
#renamig factors
votes_factor$vote<-revalue(votes_factor$vote,c("1"="Positive","2"="Abstain","3"="Negative"))
head(votes_factor)
```

```
##    ccode session rcid     vote year     country
## 4    651       1    3  Abstain 1946       Egypt
## 21    91       1    3 Positive 1946    Honduras
## 27    94       1    3 Positive 1946  Costa Rica
## 28    92       1    3 Positive 1946 El Salvador
## 35   220       1    3 Negative 1946      France
## 36   165       1    3 Positive 1946     Uruguay
```
#stacked bar 

```r
ggplot(votes_factor, aes(year, fill=vote)) + geom_bar() + ggtitle("Stacked Bar Showing votes distribution yearly")
```

![](United_Nations_Voting_Trends_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

#grouped bar

```r
ggplot(votes_factor[votes_factor$year > 1985,], aes(year, fill=vote)) + geom_bar(position = "dodge") +ggtitle("Grouped Bar Showing votes distribution yearly")
```

![](United_Nations_Voting_Trends_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

