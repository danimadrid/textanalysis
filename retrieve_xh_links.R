##Generating a list of links to extract for Xinhua
data = read.xlsx("categories.xlsx",1, header=TRUE)
x=as.numeric(data$numid)
y=as.numeric(data$pags)
z=as.character(data$geo)
k=as.character(data$topic)
s1=rep("http://search.news.cn/mb/language/english/search/search.jspa?pno=",sum(y))
s3=rep("&namespace=%2Fmb%2Flanguage%2Fenglish&siteid=2&search.jspa=&styleurl=http%3A%2F%2Fwww.xinhuanet.com%2Fenglish2010%2Fstatic%2Fcss.css&nodetype=3&hl=no&nodeid=",sum(y))
s2=c()
s4=c()
s5=c()
s6=c()
for(i in 1:length(x)){
  temp2 = seq(1,y[i]);
  s2=c(s2,temp2);
  temp4=rep(x[i],y[i]);
  s4=c(s4,temp4);
  temp5=rep(z[i],y[i]);
  s5=c(s5,temp5)
  temp6=rep(k[i],y[i]);
  s6=c(s6,temp6)
}
final_output=paste(s1,s2,s3,s4,sep="")
df=data.frame(final_output,s5,s6)
colnames(df) = c("url","geo","topic")
head(final_output)
write.csv(xinhua.raw, "xinhua/raw_list_search_links.csv")

##Retrieve Xinhua URLS
library(XML)
install.packages("mail")
library(mail)
install.packages("notifyR")
library(notifyR)
xinhua = as.character(df$url)
#x = 1
#y = 5
#xinhua.urls = list()

for(i in 1:200){
  retrieved = lapply(xinhua[x:y], getHTMLLinks)
  xinhua.urls = append(xinhua.urls,retrieved)
  x = x + 5
  y = y + 5
}
send_push(userkey,"Desktop Code Finished")

xinhua.urls.bup = xinhua.urls
xup = x
yup = y

xinhua.urls[10862:11270] = NULL

# Alternative way to rerieving links
#xinhua.urls
#write.csv(xinhua, "testxi.csv")
#write.csv(as.data.frame(xinhua.urls), "testxu.csv")
#head(xinhua)
#links2 = lapply(xinhua[1001:2000], getHTMLLinks)
#links3 = lapply(xinhua[2001:3000], getHTMLLinks)
#links4 = lapply(xinhua[3001:4000], getHTMLLinks)
#links5 = lapply(xinhua[4001:5000], getHTMLLinks)
#links6 = lapply(xinhua[5001:6000], getHTMLLinks)
#links7 = lapply(xinhua[6001:7000], getHTMLLinks)
#links8 = lapply(xinhua[7001:8000], getHTMLLinks)
#links9 = lapply(xinhua[8001:9000], getHTMLLinks)
#links10 = lapply(xinhua[9001:10861], getHTMLLinks)

##Turn list into Dataframe
install.packages("plyr")
library(plyr)
xh = plyr::ldply(xinhua.urls, rbind)
head(xh)
xh[]=lapply(xh, as.character)
#Change colnames in dataframe
n=length(xh)
prefix = "link"
suffix = seq(1:n)
my.names = paste(prefix, suffix, sep="")
names(xh) = my.names
#Add metainformation
xh$geo=df$geo
xh$topic=df$topic
xh$case=c(1:10861)
#Reshape the data
install.packages("reshape2")
library(reshape2)
colnames(xh)
molten_xh = melt(xh,id=c("case","geo","topic"))
molten_xh$variable=NULL
molten_xh$case=NULL
head(molten_xh)
#Delete repeated
install.packages("data.table")
library(data.table)
all_links_xinhua = unique(molten_xh, by="value")
all_links_xinhua$count = str_count(all_links_xinhua$value, "search")
all_links_xinhua=subset(all_links_xinhua, count==0)
all_links_xinhua$count=NULL
all_links_xinhua=subset(all_links_xinhua, link!="http://")
colnames(all_links_xinhua)[3] = "link"
final_xinhua=all_links_xinhua
write.csv(final_xinhua, "final_list_links_xinhua.csv")
#Remove 2016 data
install.packages("stringr")
library(stringr)
xinhua.raw = as.data.frame(xinhua)
final_xinhua_processed = final_xinhua
final_xinhua_processed$link = str_replace(final_xinhua_processed$link, "http://news.xinhuanet.com/english/2016[:punct:][:digit:][:digit:][:punct:][:digit:][:digit:][:punct:][:alnum:]+", "")
final_xinhua_processed$count = str_count(final_xinhua_processed$link, "http")
final_xinhua_processed=subset(final_xinhua_processed, count!=0)
#Remove non-relevant links
final_xinhua_processed$count = str_count(final_xinhua_processed$link, "search.jspa")
final_xinhua_processed=subset(final_xinhua_processed, count==0)
final_xinhua_processed$count=NULL
write.csv(xinhua.raw, "xinhua_raw.csv")

#Some statistics
#Total number of links extracted = 1,401,069
#After removing duplicates = 583,227
#After removing 2016 data = 544,559
#After removing links to pages other than articles = 533,273

#Writing CSV file with all preliminary link extraction from Xinhua
#write.csv(final_xinhua_processed, "xinhua/final_list_links_xinhua.csv")
final_xinhua_processed = read.csv("final_list_links_xinhua.csv", header = TRUE)
final_xinhua_processed$X=NULL

#Parsing using XML
library(XML)
library(rvest)
library(selectr)
library(xml2)

#Setting up all the empty df to store data
xh.news = data.frame(title = "", date = "", text = "", pubdate = "", section = "", source = "", keywords = "", lead = "", url = "", geo = "", topic = "")
xh.df = data.frame()
xinhua.news = c()
xinhua.list = c()
test = data.frame()
xh.links = data.frame()
links=""

#For loop to retrieve the data
ptm <- proc.time()
for(j in 0:1000){
for(i in 1:500){
  try({xinhua_url = read_html(as.character(final_xinhua_processed$link[[(j*500)+i]]))}, silent = TRUE)
  try({
    title = xinhua_url %>% 
      html_node("title") %>%
      html_text()
    date = xinhua_url %>% 
      html_node(".sj") %>%
      html_text()
    text = xinhua_url %>% 
      html_node("#Content") %>%
      html_text()}, silent = TRUE)
  #Capture text in articles that have a different Xpath name
  if(length(text)==0){
    text = xinhua_url %>% 
      html_node(".td") %>%
      html_text()
  }
  if(length(text)==0){
    text = xinhua_url %>% 
      html_node("#content") %>%
      html_text()
  }
  try({
  pubdate = as.character(html_nodes(xinhua_url, "meta[name=pubdate]") %>% 
                           html_attr("content"))}, silent = TRUE)
  #Avoid error when metadata is absent
  if(length(pubdate)==0){
    pubdate=NA
  }
  try({
  section = as.character(html_nodes(xinhua_url, "meta[name=section]") %>% 
                           html_attr("content"))}, silent = TRUE)
  #Avoid error when metadata is absent
  if(length(section)==0){
    section=NA
  }
  try({
  source = as.character(html_nodes(xinhua_url, "meta[name=source]") %>% 
                          html_attr("content"))}, silent = TRUE)
  #Avoid error when metadata is absent  
  if(length(source)==0){
    source=NA
  }
  try({
  keywords = as.character(html_nodes(xinhua_url, "meta[name=keywords]") %>% 
                            html_attr("content"))}, silent = TRUE)
  if(length(keywords)==0){
    keywords=NA
  }
  try({
  lead = as.character(html_nodes(xinhua_url, "meta[name=description]") %>% 
                        html_attr("content"))}, silent = TRUE)
  if(length(lead)==0){
    lead=NA
  }
  #Adding meta information from original list of links
  url = final_xinhua_processed$link[[i]]
  geo = final_xinhua_processed$geo[[i]]
  topic = final_xinhua_processed$topic[[i]]
  #Saves output into a list
  xinhua.news = c(title, date, text, pubdate, section, source, keywords, lead, url)
  xinhua.list = append(xinhua.list, list(xinhua.news))
  #Saves output into a df
  xh.news = data.frame(title, date, text, pubdate, section, source, keywords, lead, url, geo, topic)
  xh.df = rbind(xh.df, xh.news)
  #Empties all the fields
  xinhua.news = ""
  xh.news = data.frame(title = "", date = "", text = "", pubdate = "", section = "", source = "", keywords = "", lead = "", url = "", geo = "", topic = "")
  title = ""
  date = ""
  text = ""
  pubdate = ""
  section = ""
  source = ""
  keywords = ""
  url = ""
  lead = ""
  geo= ""
  topic= ""
  xinhua_url= ""
  #Extract all the other links in each page
  try({
    links = getHTMLLinks(as.character(final_xinhua_processed$link[[i]])) 
  }, silent = TRUE)
  xh.links = rbind(xh.links, as.data.frame(links))
  xh.links=unique(xh.links)
  links = ""
}
}
send_push(userkey,"Desktop finished processing")


#Create a function to scrape
xh.scraper = function(x){
  try({xinhua_url = read_html(as.character(final_xinhua_processed$link[[x]]))}, silent = TRUE)
  try({
    title = xinhua_url %>% 
      html_node("title") %>%
      html_text()
    date = xinhua_url %>% 
      html_node(".sj") %>%
      html_text()
    text = xinhua_url %>% 
      html_node("#Content") %>%
      html_text()}, silent = TRUE)
  #Capture text in articles that have a different Xpath name
  if(length(text)==0){
    text = xinhua_url %>% 
      html_node(".td") %>%
      html_text()
  }
  if(length(text)==0){
    text = xinhua_url %>% 
      html_node("#content") %>%
      html_text()
  }
  pubdate = as.character(html_nodes(xinhua_url, "meta[name=pubdate]") %>% 
                           html_attr("content"))
  #Avoid error when metadata is absent
  if(length(pubdate)==0){
    pubdate=NA
  }
  section = as.character(html_nodes(xinhua_url, "meta[name=section]") %>% 
                           html_attr("content"))
  #Avoid error when metadata is absent
  if(length(section)==0){
    section=NA
  }
  source = as.character(html_nodes(xinhua_url, "meta[name=source]") %>% 
                          html_attr("content"))
  #Avoid error when metadata is absent  
  if(length(source)==0){
    source=NA
  }
  keywords = as.character(html_nodes(xinhua_url, "meta[name=keywords]") %>% 
                            html_attr("content"))
  if(length(keywords)==0){
    keywords=NA
  }
  lead = as.character(html_nodes(xinhua_url, "meta[name=description]") %>% 
                        html_attr("content"))
  if(length(lead)==0){
    lead=NA
  }
  #Adding meta information from original list of links
  url = final_xinhua_processed$link[[x]]
  geo = final_xinhua_processed$geo[[x]]
  topic = final_xinhua_processed$topic[[x]]
  #Saves output into a list
  xinhua.news = c(title, date, text, pubdate, section, source, keywords, lead, url)
  xinhua.list = append(xinhua.list, list(xinhua.news))
  #Saves output into a df
  xh.news = data.frame(title, date, text, pubdate, section, source, keywords, lead, url, geo, topic)
  xh.df = rbind(xh.df, xh.news)
  #Empties all the fields
  xinhua.news = ""
  xh.news = data.frame(title = "", date = "", text = "", pubdate = "", section = "", source = "", keywords = "", lead = "", url = "", geo = "", topic = "")
  title = ""
  date = ""
  text = ""
  pubdate = ""
  section = ""
  source = ""
  keywords = ""
  url = ""
  lead = ""
  geo= ""
  topic= ""
  xinhua_url= ""
  #Extract all the other links in each page
  try({
    links = getHTMLLinks(as.character(final_xinhua_processed$link[[x]])) 
  }, silent = TRUE)
  xh.links = rbind(xh.links, as.data.frame(links))
  xh.links=unique(xh.links)
  links = ""
}

ptm <- proc.time()
x=1
y=2000
sapply(1,xh.scraper)
x=x+2000
y=y+2000
proc.time() - ptm
send_push(userkey,"Desktop finished processing")


