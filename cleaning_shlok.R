### CLEANING OF GENRES ###

genres <- dat$Genres
split_genres <- strsplit(genres, split = " ")
head(split_genres,2)
genre_number = numeric(length = 5000)
for(i in 1:5000){
  genre_number[i] <- length(split_genres[[i]])
}


unlisted_genres <- unlist(split_genres)
single_unlisted_genres <- substring(unlisted_genres, 1, nchar(unlisted_genres)/2)
unique(split_genres)
single_unlisted_genres
##single_genres <- substring(split_genres, 1, nchar(split_genres)/2)
##head(single_genres,5)

genre_number_sum= numeric(length = 5000)
genre_number_sum[1] = 4
for(i in 2:5000){
  genre_number_sum[i] = genre_number_sum[i-1] + genre_number[i]
}
final_genres = list(length = 5000)

final_genres[[1]] = single_unlisted_genres[1:(genre_number_sum[1])]
single_unlisted_genres[5:7]
single_unlisted_genres[1:4]
genre_number_sum[1]
genre_number_sum[4]
for(i in 2:5000){
  final_genres[[i]] = single_unlisted_genres[(genre_number_sum[i-1]+1):(genre_number_sum[i])]
}
head(final_genres)
final_genres[[4999]]
final_genres[[5000]]
dat$Final_Genres <- final_genres
head(dat,1)

### CLEANING OF SOURCE ###
