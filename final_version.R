#libraries

library(recommenderlab)
library(dplyr)

#data loads
load('matrix_rates.RData')
load('recc_model.RData')

#genres of the user

user_genres=as.data.frame(matrix(nrow=1, ncol=5))
user_genres[1]=6
user_genres[2]=1
user_genres[3]=7
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

