newslinkstoscrape
loadhtmlperarticle = list()
loadnodesperarticle = array()

for(i in 1:length(newslinkstoscrape){
  loadhtmlperarticle[[i]] = html(newslinkstoscrape[i])
  loadnodesperarticle[i] = loadhtmlperarticle[[i]] %>% html_node("#story_bottom") %>% html_text()
  filename = paste(i,"article_07jan2016.txt")
  write(loadnodesperarticle[i],file=filename)
  
}
