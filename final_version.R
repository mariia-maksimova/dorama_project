#libraries
library(recommenderlab)
library(dplyr)
library(readr)

#data loads
load('matrix_rates.RData')
load('recc_model.RData')
load('dramas_matrix.RData')
load('KR_genres.RData')
load('KR_rates.RData')
load('KR_rating_full.RData')
load('KR_shows.RData')

#genres of the user

user_genres=as.data.frame(matrix(nrow=1, ncol=5))
user_genres[1]=6
user_genres[2]=1
user_genres[3]=8
user_genres[4]=35
user_genres[5]=36

dramas_matrix_work<-filter(dramas_matrix, is.element(genre_id,user_genres)==TRUE)
first_page_dramas<-data.frame(dorama_id=stack(dramas_matrix_work[,2:6])[,1])
first_page_dramas2<-filter(KR_shows, is.element(dorama_id,first_page_dramas$dorama_id)==TRUE )

#user vector (doramas)

user_data<-matrix(data=NA, ncol = ncol(matrix_rates))
colnames(user_data)<-colnames(matrix_rates)
user_data<-t(user_data)

for (i in first_page_dramas2$dorama_id){
  user_data[rownames(user_data)==i]=5
}
user_data<-t(user_data)

user_data<-as(user_data,"realRatingMatrix")

#recommend

recc_predicted <- predict(object = recc_model, newdata = user_data, n = 300)
recc_user_1 <- recc_predicted@items[[1]]
recc_user_ratings<-recc_predicted@ratings[[1]]

dorama_id<- sapply( recc_predicted@itemLabels[recc_user_1], as.integer)
dorama_ids<-cbind(dorama_id, data.frame(rating=recc_user_ratings))

KR_rating_full$id=as.numeric(rownames(KR_rating_full))

dramas=inner_join(dorama_ids, KR_rating_full, by='dorama_id')




#user wants (interactive part)

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

#filter

dramas_wants<-filter(dramas, School>=user_wants$School-2, School<=user_wants$School+2)
dramas_wants<-filter(dramas_wants, School>=user_wants$History-2, School<=user_wants$History+2)
dramas_wants<-filter(dramas_wants, School>=user_wants$Gender-2, School<=user_wants$Gender+2)
dramas_wants<-filter(dramas_wants, School>=user_wants$Super-2, School<=user_wants$Super+2)
dramas_wants<-filter(dramas_wants, School>=user_wants$Pro-2, School<=user_wants$Pro+2)
dramas_wants<-filter(dramas_wants, School>=user_wants$Investigation-2, School<=user_wants$Investigation+2)
dramas_wants<-filter(dramas_wants, School>=user_wants$Cinderella-2, School<=user_wants$Cinderella+2)
dramas_wants<-filter(dramas_wants, School>=user_wants$Revenge-2, School<=user_wants$Revenge+2)
dramas_wants<-filter(dramas_wants, School>=user_wants$Friendship-2, School<=user_wants$Friendship+2)
dramas_wants<-filter(dramas_wants, School>=user_wants$Secrets-2, School<=user_wants$Secrets+2)
