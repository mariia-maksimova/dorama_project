library(recommenderlab)
library(dplyr)

#matrix_rates <- sapply( matrix_rates, as.numeric )

#matrix_rates<- as(  as(matrix_rates, "matrix")   , "realRatingMatrix")

save(matrix_rates, file='matrix_rates.RData')

load('matrix_rates.RData')

#recc_model <- Recommender(data = matrix_rates, method = "POPULAR", param=NULL)
#recc_model

#save(recc_model, file='recc_model.RData')

load('recc_model.RData')

user_data<-matrix(data=NA, ncol = ncol(matrix_rates))
colnames(user_data)<-colnames(matrix_rates)

#вставляем оценки пользователя!!Стыковка!!
user_data[43]=2
user_data[443]=5
user_data[775]=3
user_data[413]=4
user_data[13]=1
user_data[32]=5
user_data[100]=5
user_data[323]=2
user_data[578]=5
user_data[143]=5
user_data[493]=5
user_data[432]=5
user_data[1]=5
###Стыковка


user_data<-as(user_data,"realRatingMatrix")

#рекомендация для нашего - newdata - его строка

recc_predicted <- predict(object = recc_model, newdata = user_data, n = 50)
recc_user_1 <- recc_predicted@items[[1]]
recc_user_ratings<-recc_predicted@ratings[[1]]

dorama_id<- sapply( recc_predicted@itemLabels[recc_user_1], as.integer)
dorama_ids<-cbind(dorama_id, data.frame(rating=recc_user_ratings))


KR_rating_full$id=as.numeric(rownames(KR_rating_full))
dramas=left_join(dorama_ids, KR_rating_full, by='dorama_id')


###Стыковка
user_wants=as.data.frame(matrix(nrow=1, ncol=10))
colnames(user_wants)<-c('School',	'History',	'Gender',	'Super',	'Pro',	'Investigation',	'Cinderella',	'Revenge','Friendship','Secrets')

user_wants$School=7
user_wants$History=3
user_wants$Gender=5
user_wants$Super=2
user_wants$Pro=8
user_wants$Investigation=4
user_wants$Cinderella=6
user_wants$Revenge=9
user_wants$Friendship=10
user_wants$Secrets=9

###


###Любимые жанры пользователя!!! Стыковка##

user_genres=as.data.frame(matrix(nrow=1, ncol=5))
user_genres[1]=6
user_genres[2]=1
user_genres[3]=7
user_genres[4]=35
user_genres[5]=36

###



