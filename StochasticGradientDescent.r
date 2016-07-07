###make way to ask for the rank of factor matrices be an input
 
#10x10 matrix
values = sample(seq(1,5,by=0.25),size=100,replace=T)
matrix = matrix(values,nrow=10,ncol=10)
blanks = sample(1:100,size=20,replace=F)
View(matrix)
matrix[blanks] = NA
#initialize 2 low-rank matrices, rank = 4 just as an example
#values of these matrices will be setup like its mother matrix
#not even sure if that's 'random' enough to be a good starting point
user.matrix = matrix(sample(seq(0,1,by=0.01),size=40,replace=T),nrow=10,ncol=4)
item.matrix = matrix(sample(seq(0,1,by=0.01),size=40,replace=T),nrow=4,ncol=10)
 
entry_square_error = function(nonblank.entry, user.matrix,item.matrix,user.row,item.col){
  return(nonblank.entry - (user.matrix[user.row,] %*% t(t(item.matrix[,item.col]))))
}
 
matrix_square_error = function(matrix,user.matrix,item.matrix){
  return(sum(matrix - (user.matrix %*% item.matrix))^2)
}
###set the tolerance, learning rate, and regularisation parameter
 
 
 
sgd = function(matrix,user.matrix,item.matrix,tol,learning.rate,reg.param){
  approx.matrix = matrix(,nrow=nrow(user.matrix),ncol=ncol(item.matrix))
  rms.error = 9999 #hard-coded
  while(rms.error>tol){
    for(i in 1:nrow(matrix)){
      for(j in 1:ncol(matrix)){
        if(is.na(matrix[i,j])==F){
          ij.error = entry_square_error(matrix[i,j],user.matrix,item.matrix,i,j)
          user.matrix[i,] = user.matrix[i,] + learning.rate*(ij.error*item.matrix[,j] - reg.param*user.matrix[i,])
          item.matrix[,j] = item.matrix[,j] + learning.rate*(ij.error*user.matrix[i,] - reg.param*item.matrix[,j])
          approx.matrix = user.matrix %*% item.matrix
          #print(approx.matrix)
        }
      }
    }
  rms.error = (sum((matrix - approx.matrix)^2,na.rm=T)/(length(matrix)-sum(is.na(matrix))))^0.5
  ###the more correct error function should include the regularisation term, but that slows things
  }
  print(user.matrix)
  print(item.matrix)
  print(rms.error)
  return(approx.matrix)
}
 
