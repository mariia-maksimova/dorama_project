library(readr)
KR_genres <- read_delim("~/dorama_project/KR_genres.csv", 
                        +     "\t", escape_double = FALSE, col_names = FALSE, 
                        +     trim_ws = TRUE)
colnames(KR_genres)<-c('genre_id', 'genre_title')

KR_rates <- read_delim("~/dorama_project/KR_rates.csv", 
                       +     "\t", escape_double = FALSE, col_names = FALSE, 
                       +     trim_ws = TRUE)

colnames(KR_rates)<-c('user_id', 'dorama_id', 'status', 'rating')

KR_shows <- read_delim("~/dorama_project/KR_shows.csv", 
                       +     "\t", escape_double = FALSE, col_names = FALSE, 
                       +     trim_ws = TRUE)

colnames(KR_shows)<-c('dorama_id', 'dorama_title', 'overall_rating', 'watching', 'seasons', 'year', 'genres')
