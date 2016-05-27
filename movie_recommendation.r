movies.unwatched = sort(unique(u.data_items$"item id"))
movies.watched = sort(unique(i_am$`item id`))
movie_matrix = data.frame(matrix(0,nrow=length(movies.watched),ncol=length(movies.unwatched)))
colnames(movie_matrix) = movies.unwatched
row.names(movie_matrix) = movies.watched

st = proc.time()
for(i in 1:nrow(movie_matrix)){
  for(j in 1:i){
    movie_matrix[i,j] = movie_similarity(as.numeric(row.names(movie_matrix[i,])),as.numeric(colnames(movie_matrix)[j]),u.data)
  }
}
proc.time() - st
