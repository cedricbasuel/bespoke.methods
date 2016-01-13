#library(rvest)
#library(XML)
#library(doParallel)

#set working directory
setwd("C:/Users/Guest/Desktop/cedric/jan08")
#load html
bworldjan08 = html("http://www.bworldonline.com/landing.php?page=corporate_news")
#get raw text (in this case, news headlines)
#html node chosen with the help of bookmarklet 'selectorgadget'
newstitle08 = bworldjan08 %>% html_node("#landing_page td:nth-child(2)") %>% html_text()
#cleaning the raw text obtained
newstitle08 = gsub("\t"," ",newstitle08)
newstitle08
newstitle08 = strsplit(newstitle08," \r\n")
newstitle08
newstitle08 = unlist(newstitle08)
newstitle08
newstitle08 = newstitle08[-c(21)]
newstitle08
#newstitle08 = gsub(" ","",newstitle08)
#newstitle08
#getting the links of the headlines scraped
#we are only getting links found in the same html node the headlines were taken from
newslinks08 = bworldjan08 %>% html_node("#landing_page td:nth-child(2)")
newslinks08_list = as.vector(xpathSApply(htmlParse(newslinks08),"//a/@href"))
newslinks08_list = as.vector(xpathSApply(htmlParse(newslinks08),"//a/@href"))
newslinks08_list
newslinks08_list = paste("http://www.bworldonline.com/",newslinks08_list,sep="")

#putting the headlines together with their links
newslinkstoscrape08 = data.frame(newstitle08,newslinks08_list)
View(newslinkstoscrape08)
write.csv(newslinkstoscrape08,"newslinkstoscrape08.csv")

#scraping each link that's on our list
#i'll make a code that employs parallel computation to save computing time
#this will only work if all the pages we're going to scrape follow exactly the same format/layout
#again use 'selectorgadget' to get the desired html node
loadhtmlperarticle08 = list()
loadnodesperarticle08 = array()
for(i in 1:20){
loadhtmlperarticle08[[i]] = html(newslinks08_list[i])
loadnodesperarticle08[i] = loadhtmlperarticle08[[i]] %>% html_node("#story_bottom") %>% html_text()
filename = paste(i,"article_08jan2016.txt",sep="_")
write(loadnodesperarticle08[i],file=filename)
}

