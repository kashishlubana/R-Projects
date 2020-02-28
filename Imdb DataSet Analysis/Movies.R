
  
movies<-Dataset_4_Movies
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)
library(dplyr)
library(psych)
library(Hmisc)
library(GGally)

movies$movie_title <-  (sapply(movies$movie_title,gsub,pattern="\\¬???",replacement=""))

moviessubset <- subset(movies, country == "USA")

moviesnew<- moviessubset[order(as.integer(moviessubset$gross), decreasing = TRUE), ]

grossrev$Gross <- paste(round(grossrev$gross / 1e6, 1), "M")





#Top 10 Highest grossing movies plot
grossrev<- head(moviessubset, 10)


ggplot(grossrev, aes(y = Gross, x=movie_title)) + 
     theme_bw()+
      geom_col()+
      coord_flip()+
     labs (y="Revenue" , x = "Movie name" , title="Top 10 Highest Grossing")


#Top 10 Highest grossing movies table
Numberofmovies<- moviessubset[!duplicated(moviessubset[c("movie_title", "director_name")]),]



top10grosss <- Numberofmovies[,c("movie_title", "gross")]
top10<- head(arrange(top10grosss, desc(gross)), n=10)  

top10$gross<- paste(round(top10$gross/ 1e6, 1), "Million", "USD")
View(top10)


moviessubset<-Numberofmovies

#Top 10 Highest Budget movies table

moviessubset <- subset(movies, country == "USA")

moviessubset$budget<-as.numeric(moviessubset$budget)

budgetnew<- moviessubset[order(as.integer(movies$budget), decreasing = TRUE), ]

top10bud<- (arrange(moviessubset, desc(budget)))

top10bud$budget <- paste(round(top10bud$budget / 1e6, 1), "Million", "USD")
top10budget <- top10bud[,c("movie_title", "budget")]
top10budgets <- head(top10budget,10)








#Top 10 Highest Budget movies plot
ggplot(highbudget, aes(y = budget, x=movie_title)) + 
  theme_bw()+
  geom_col()+
  coord_flip()+
  labs (y="budget" , x = "Movie name" , title="Top 10 Highest bugdet")






#Top 10 Lowest Budget movies table

moviessubset$budget<-as.numeric(moviessubset$budget)


lowbudget<- moviessubset[order(as.numeric(moviessubset$budget), decreasing = FALSE), ]
#lowestbudget$budget<- as.numeric(lowbudget$budget)
New<- head(lowbudget, 10)
New$budget<- paste(round(New$budget / 1e3, 1), "Thousand", "USD")


Top_10_Lowest_Bugdet<- New[,c("movie_title", "budget")]




#Top 10 Lowest Budget movies plot


budgetnew2<- movies[order(as.integer(movies$budget), decreasing = FALSE), ]

lowestbudget<- head(budgetnew2, 10)

ggplot(lowestbudget, aes(y = budget, x=movie_title)) + 
  theme_bw()+
  geom_col()+
  coord_flip()+
  labs (y="budget" , x = "Movie name" , title="Top 10 Lowest bugdet")






#Top 10 Directors on the basis of number of  movies plot




ggplot(movies, aes(x = director_name)) + 
  theme_bw()+
  geom_bar()+
  coord_flip()+
  labs (x = "directors" , title="Top 10 Directors")


#Top 10 Directors on the basis of number of  movies table

Numberofmovies<- movies[!duplicated(movies[c("movie_title", "director_name")]),]

Count_movie<- Numberofmovies %>%
  group_by(director_name) %>%
  summarise(count=n()) %>%
  na.omit(Count_movie)




#Top 10 Directors on the basis of highest revenue

Numberofmovies<- moviessubset[!duplicated(moviessubset[c("movie_title", "director_name")]),]


Numberofmovies$gross[is.na(Numberofmovies$gross)]<-0

top10Dir<- Numberofmovies %>%
  group_by(director_name) %>%
  summarise(Total_revenue =sum(gross)) %>%
  na.omit(top10Dir)

abcd <- top10Dir[ order(-top10Dir$Total_revenue), ]

abcd$Total_revenue <- paste(round(abcd$Total_revenue / 1e9, 1),"Billion", "USD")

View(abcd)


#Relationship between budget, gross revenue, IMDb rating and other variables


imdb_new <- as.numeric(as.character(movies$imdb_score))

X <- subset(movies, select = c(budget,gross,imdb_new))
pairs(X[,1:3])

p1 <- ggplot(data=movies,aes(x=gross , y=budget)) + geom_point()
p2 <- ggplot(data=movies,aes(x=gross, y=imdb_new)) + facet_wrap(content_rating~) + geom_point()
p3 <- ggplot(data=movies,aes(x=budget, y=imdb_new)) + geom_point()
p4 <- ggplot(data=movies,aes(x=language, y=imdb_new)) + geom_point()


grid.arrange(p1, p2, p3, ncol=length(names(movies)))

moviessubset$imdb_score<- as.numeric(moviessubset$imdb_score)
moviessubset$budget<- as.numeric(moviessubset$budget)
moviessubset$gross<- as.numeric(moviessubset$gross)
moviessubset$Title_year <- as.numeric(as.character(moviessubset$title_year))

parameters_1 = c("gross","budget","imdb_score","Title_year")

ggpairs(moviessubset, columns=parameters_1)




#Relationship between gross revenue, facebook likes, number of reviews and other variables


moviessubset$gross<- as.numeric(moviessubset$gross)
moviessubset$movie_facebook_likes<- as.numeric(moviessubset$movie_facebook_likes)
moviessubset$cast_total_facebook_likes<- as.numeric(moviessubset$cast_total_facebook_likes)
moviessubset$num_user_for_reviews<- as.numeric(moviessubset$num_user_for_reviews)
moviessubset$num_critic_for_reviews<- as.numeric(moviessubset$num_critic_for_reviews)
moviessubset$num_voted_users<- as.numeric(moviessubset$num_voted_users)


parameters_2 = c("gross","movie_facebook_likes","num_user_for_reviews","num_voted_users")

ggpairs(moviessubset, columns=parameters_2)


