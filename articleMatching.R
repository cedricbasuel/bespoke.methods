setwd("C:/Users/Guest/Desktop/cedric/jan08")
#docs1read = readLines("1 article_07jan2016.txt")


txt_files = list.files(pattern="article_08jan2016.txt")
article_list = lapply(txt_files,readLines)

article_list = lapply(article_list,function (x) paste(x,sep=" ",collapse=""))

#=paste(docs1read,sep=" ",collapse="")
#docs1read=gsub("\t","",docs1read)

article_list = lapply(article_list,function(x) gsub("\t","",x))

#article_list0
#article_list0unlist = unlist(article_list0)

#include all words in the list
keywords = c("acquir","allot","boost","continually","creat","develop","envision","eventually","expectation","increas","propose","plan","new","invest","bought","collaborat","control","determine","emerg","invest","building","open","bigger","residential","accomodate","beyond","branch","contractor","launch","lead","offer","target","widen","introduce","coming","confirm","convinc","craft","expan","inten","pair","talk","underserved","construct","bid","commence","convert","debut","emerge","hectare","sq")
#scoreKey = c()
#for(i in 1:length(keywords)){
#matchthis = grepl(keywords[i],article_list[10])
     #print(paste(keywords[i]," ",matchthis))
#     scoreKey[i] = matchthis
#}
#sum(scoreKey)

#######

article_scores = data.frame(matrix(ncol=6,nrow=length(article_list)))
names(article_scores) = c("08 jan Article","Number of matches","Words matched","Date/Money details","Location/s","Organization/s")
articleString = list()
article_annotations = list()
article_docs = list()
for(j in 1:length(article_list)){
  scoreKey = c()
  truematch = c()
  for(i in 1:length(keywords)){
    matchthis = grepl(keywords[i],article_list[j])
    #print(paste(keywords[i]," ",matchthis))
    if(matchthis){
      truematch = append(truematch,keywords[i])
    }
    scoreKey[i] = matchthis
  }
  article_scores[j,1] = paste("article",j,sep="_")
  article_scores[j,2] = sum(scoreKey)
  article_scores[j,3] = as.String(truematch)
  articleString[[j]] = as.String(article_list[[j]])
  article_annotations[[j]] = annotate(articleString[[j]],pipeline)
  article_docs[[j]] = AnnotatedPlainTextDocument(articleString[j],article_annotations[[j]])
  article_scores[j,4] = try(as.String(entities(article_docs[[j]],kind = "date")))
  article_scores[j,5] = try(as.String(entities(article_docs[[j]],kind = "location")))
  article_scores[j,6] = try(as.String(entities(article_docs[[j]],kind = "organization")))
}
write.csv(article_scores,"article_scores.csv")
