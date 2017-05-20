names<-data.frame(dorama_id=as.integer(colnames(matrix_rates_crop_1)))

library(dplyr)

KR_shows2<-right_join(KR_shows, names, by="dorama_id")

