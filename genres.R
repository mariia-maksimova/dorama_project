
m <- gregexpr('[[:digit:]]+', KR_shows$genres[1:1248], perl=TRUE)
genres=regmatches(KR_shows$genres[1:1248], m)

