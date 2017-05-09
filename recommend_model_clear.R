library(recommenderlab)
library(dplyr)

matrix_rates_crop_1 <- sapply( matrix_rates_crop_1, as.numeric )

matrix_rates<- as(  as(matrix_rates_crop_1, "matrix")   , "realRatingMatrix")

recc_model <- Recommender(data = matrix_rates, method = "POPULAR", param=NULL)
recc_model

user_data<-matrix(data=NA, ncol = ncol(matrix_rates))
colnames(user_data)<-colnames(matrix_rates)

#вставляем оценки пользователя
user_data[1]=2

user_data<-as(user_data,"realRatingMatrix")

rownames(KR_shows)<-KR_shows$dorama_id

#рекомендация для нашего - newdata - его строка

recc_predicted <- predict(object = recc_model, newdata = user_data, n = 20)
str(recc_predicted)
recc_user_1 <- recc_predicted@items[[1]]
dorama_ids<- recc_predicted@itemLabels[recc_user_1]
dorama_ids

dramas=array(NA)
for (i in 1:20){
  dramas[i]=KR_shows$dorama_title[KR_shows$dorama_id==dorama_ids[i]]
}

dramas
