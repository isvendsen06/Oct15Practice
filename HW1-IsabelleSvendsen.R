Izzy Svendsen
#First we need to import the data into Rstudio 
election <- read.csv('Homework1_Bonds.csv')
head(election)

#1
#Since the data set already has them labeled of "Carried" or "Defeated" all we need to do is use the table function.
table(election$Result)
#This will give us a table of the result column

prop.table(table(election$Result,election$Type),2) #column percentages this makes it so it adds up to 1 for each column.
#This allows us to see the rates of approved bonds across the 4 government types in percentages.


#2

#We want to calculate a new variable for the sum of the votes "for" and "against" the bond measure.
Votes_Total <- election$VotesFor+election$VotesAgainst
head(Votes_Total)#double checking it worked
election$Votes_Total <- Votes_Total #this adds the total votes into the data set.

#we need to find the max 
max(Votes_Total, na.rm = TRUE)

maxvote <- election[Votes_Total==1030414,] #creates subset to isolate the max voted bond. 
head(maxvote)#gives us the information to figure out when and where as well as other details. 


#3

#create a subjet that is made up of the bonds carried that has 100 or more total votes to it. 
highcarriedbonds <- election[election$Result=='Carried'&Votes_Total>=100,]
head(highcarriedbonds)#checking my work

forbond <- highcarriedbonds$VotesFor/highcarriedbonds$Votes_Total #creates a variable that finds the proportion of votes for the bond measure.

#now we have to make a graph 
hist(forbond,main='Total Votes in Favor of the Bond Measure',xlab='For the Bond',col='dark green',xlim=c(0.5,1),ylim=c(0,700))

fivenum(forbond)#five number summary to get the stats needed like Q1, Median, and Q3
IQR(forbond)# IQR


#4
#since we have two numerical values we want to use a scatterplot to compare our data.
plot(highcarriedbonds$Amount,forbond,main='Cost vs For Bond Percent',xlab='Cost ($)',ylab='Percentage',pch=20)

cor(highcarriedbonds$Amount, forbond)#this is to find the correlation to see if cost and the margin the bond was approved is related. 
