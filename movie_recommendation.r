movies.unwatched = sort(unique(u.data_items$"item id"))
movies.watched = sort(unique(i_am$`item id`))
movie_matrix = data.frame(matrix(0,nrow=length(movies.watched),ncol=length(movies.unwatched)))
colnames(movie_matrix) = movies.unwatched
row.names(movie_matrix) = movies.watched

###create the item similarity matrix
###note that columns must be identical to the rows (ie, they must refer to the same item)
st = proc.time()
for(i in 1:nrow(movie_matrix)){
  for(j in 1:i){
    movie_matrix[i,j] = movie_similarity(as.numeric(row.names(movie_matrix[i,])),as.numeric(colnames(movie_matrix)[j]),u.data)
  }
}
proc.time() - st
###compute similarity score between two item
###could be other similarity scores like cosine similarity
###
movie_similarity = function(x.movie,y.movie,movie.data){
  movie_x = movie.data[movie.data$"item id"==x.movie,]
  movie_y = movie.data[movie.data$"item id"==y.movie,]
  common.users = intersect(movie_x$"user id",movie_y$"user id")
  movie.x.y = data.frame(common.users,movie_x[movie_x$"user id" %in% common.users,3],movie_y[movie_y$"user id" %in% common.users,3])
  return(cor(movie.x.y[,2],movie.x.y[,3]))
}

###generation of suggestions
###requires an input of items (the more the better) and their corresponding scores
###this returns 10 recommendations, but could be more or fewer
movie_suggestion = function(movies.ids,movie.scores,movie.correlationmatrix){
  user.matrix = data.frame(matrix(NA,nrow=length(movies.ids),ncol=length(movies.943) - length(movies.ids)))
  row.names(user.matrix) = movies.ids
  names(user.matrix) = setdiff(movies.idents,movies.ids)
  for(i in 1:nrow(user.matrix)){
    for(j in 1:ncol(user.matrix)){
      native.row = row.names(user.matrix[i,])
      native.col = colnames(user.matrix[j])
      if(as.numeric(native.row) > as.numeric(native.col)){
        user.matrix[i,j] = movies.943[native.row,native.col]
      }
      else{
        user.matrix[i,j] = movies.943[native.col,native.row]
      }
    }
  }
  nonempty.user.matrix = user.matrix[,sapply(user.matrix, function(x) sum(is.na(x)))==0]
  ne.ma.weights = sapply(nonempty.user.matrix,function(x) x*movie.scores)
  score.sum = sum(movie.scores)
  ne.na.scores = sapply(as.data.frame(ne.ma.weights), function(x) sum(x)/score.sum)
  ne.na.sort = sort(ne.na.scores,decreasing = T)
  return(ne.na.sort[1:10])
}
