library(recommenderlab)
library(dplyr)

#matrix_rates <- sapply( matrix_rates, as.numeric )

#matrix_rates<- as(  as(matrix_rates, "matrix")   , "realRatingMatrix")

#save(matrix_rates, file='matrix_rates.RData')

load('matrix_rates.RData')

#recc_model <- Recommender(data = matrix_rates, method = "POPULAR", param=NULL)
#recc_model

#save(recc_model, file='recc_model.RData')

load('recc_model.RData')

user_data<-matrix(data=NA, ncol = ncol(matrix_rates))
colnames(user_data)<-colnames(matrix_rates)
user_data<-t(user_data)
###Любимые жанры пользователя!!! Стыковка##

user_genres=as.data.frame(matrix(nrow=1, ncol=5))
user_genres[1]=6
user_genres[2]=1
user_genres[3]=7
user_genres[4]=35
user_genres[5]=36

###

dramas_matrix_work<-filter(dramas_matrix, is.element(genre_id,user_genres)==TRUE)
first_page_dramas<-data.frame(dorama_id=stack(dramas_matrix_work[,2:6])[,1])
first_page_dramas2<-filter(KR_shows, is.element(dorama_id,first_page_dramas$dorama_id)==TRUE )

#вставляем оценки пользователя!!Стыковка!!

for (i in first_page_dramas2$dorama_id){
  user_data[rownames(user_data)==i]=5
}
user_data<-t(user_data)

#user_data[43]=2
#user_data[443]=5
#user_data[775]=3
#user_data[413]=4
#user_data[13]=1
#user_data[32]=5
#user_data[100]=5
#user_data[323]=2
#user_data[578]=5
#user_data[143]=5
#user_data[493]=5
#user_data[432]=5
#user_data[1]=5
###Стыковка


user_data<-as(user_data,"realRatingMatrix")

#рекомендация для нашего - newdata - его строка

recc_predicted <- predict(object = recc_model, newdata = user_data, n = 300)
recc_user_1 <- recc_predicted@items[[1]]
recc_user_ratings<-recc_predicted@ratings[[1]]

dorama_id<- sapply( recc_predicted@itemLabels[recc_user_1], as.integer)
dorama_ids<-cbind(dorama_id, data.frame(rating=recc_user_ratings))

library(dplyr)

KR_rating_full$id=as.numeric(rownames(KR_rating_full))
KR_rating_full2<-KR_rating_full

m <- gregexpr('[[:digit:]]+', KR_rating_full$genres[1:1248], perl=TRUE)
genres=regmatches(KR_rating_full$genres[1:1248], m)
genres<-sapply( genres, as.numeric )

for (each in KR_rating_full2$id){
  if (all(is.element(genres[[each]], user_genres))==FALSE){
    KR_rating_full2[each,]=NA
  }
}
KR_rating_full2<-na.omit(KR_rating_full2)

dramas=inner_join(dorama_ids, KR_rating_full2, by='dorama_id')


#Вывод дорам отфильтрованных по жанру (та же самая матрица и смотрится, чтобы хотя б один жанры каждой дорамы был в user_genres)

###Стыковка
user_wants=as.data.frame(matrix(nrow=1, ncol=10))
colnames(user_wants)<-c('School',	'History',	'Gender',	'Super',	'Pro',	'Investigation',	'Cinderella',	'Revenge','Friendship','Secrets')

user_wants$School=4
user_wants$History=3
user_wants$Gender=5
user_wants$Super=2
user_wants$Pro=5
user_wants$Investigation=4
user_wants$Cinderella=4
user_wants$Revenge=6
user_wants$Friendship=5
user_wants$Secrets=4

###

#После каждого ОК Строится новая таблица drama_filter, в которой есть только дорамы +-1 отличающиеся по оценке и они выводятся

dramas_wants<-filter(dramas, School>=user_wants$School-3, School<=user_wants$School+3)
dramas_wants<-filter(dramas_wants, School>=user_wants$History-3, School<=user_wants$History+3)
dramas_wants<-filter(dramas_wants, School>=user_wants$Gender-3, School<=user_wants$Gender+3)
dramas_wants<-filter(dramas_wants, School>=user_wants$Super-3, School<=user_wants$Super+3)
dramas_wants<-filter(dramas_wants, School>=user_wants$Pro-3, School<=user_wants$Pro+3)
dramas_wants<-filter(dramas_wants, School>=user_wants$Investigation-3, School<=user_wants$Investigation+3)
dramas_wants<-filter(dramas_wants, School>=user_wants$Cinderella-3, School<=user_wants$Cinderella+3)
dramas_wants<-filter(dramas_wants, School>=user_wants$Revenge-3, School<=user_wants$Revenge+3)
dramas_wants<-filter(dramas_wants, School>=user_wants$Friendship-3, School<=user_wants$Friendship+3)
dramas_wants<-filter(dramas_wants, School>=user_wants$Secrets-3, School<=user_wants$Secrets+3)





