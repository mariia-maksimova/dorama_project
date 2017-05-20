library(readr)
KR_genres <- read_delim("KR_genres.csv", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
colnames(KR_genres)<-c('genre_id', 'genre_title')

KR_rates <- read_delim("KR_rates.csv", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

colnames(KR_rates)<-c('user_id', 'dorama_id', 'status', 'rating')

KR_shows <- read_delim("KR_shows.csv", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

colnames(KR_shows)<-c('dorama_id', 'dorama_title', 'overall_rating', 'watching', 'seasons', 'year', 'genres')

KR_rating_full <- read_csv("KR_shows_csv.csv")

colnames(KR_rating_full)<-c('dorama_id', 'dorama_title', 'overall_rating', 'watching', 'seasons', 'year', 'genres', 'School',	'History',	'Gender',	'Super',	'Pro',	'Investigation',	'Cinderella',	'Revenge','Friendship','Secrets')


dramas_matrix <- read_delim("5_dramas_matrix.csv", ";", escape_double = FALSE, trim_ws = TRUE)

colnames(dramas_matrix)<-c('genre_id', 'drama1', 'drama2', 'drama3', 'drama4', 'drama5')

