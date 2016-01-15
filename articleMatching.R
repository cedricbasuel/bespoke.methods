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

word_an = Maxent_Word_Token_Annotator()
sent_an = Maxent_Sent_Token_Annotator()
money_an = Maxent_Entity_Annotator(kind = "money")
date_an = Maxent_Entity_Annotator(kind = "date")
location_an = Maxent_Entity_Annotator(kind = "location")
#misc_an = Maxent_Entity_Annotator(kind = "misc")
org_an = Maxent_Entity_Annotator(kind = "org")
pipeline = list(sent_an,word_an,money_an,date_an,location_an,org_an)

entities = function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}


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
txt_files_index = substr(txt_files,1,2)
txt_files_index = gsub(" ","",txt_files_index)
txt_files_index = as.numeric(txt_files_index)
newstitles = read.csv("newslinkstoscrape_Jan13.csv")
article_scores$"Headline" = NA
for(i in 1:nrow(article_scores)){
  row = txt_files_index[i]
  article_scores$Headline[i] = as.String(newstitles[row,2])
}

write.csv(article_scores,"article_scores.csv")
