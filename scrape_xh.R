##Scraping content from Xinhua website
links = c("http://news.xinhuanet.com/english/2015-12/21/c_134935806.htm","http://news.xinhuanet.com/english/2015-12/05/c_134886563.htm","http://news.xinhuanet.com/english/2015-09/13/c_134618330.htm")
for(i in 1:3){
  xinhua_url = read_html(links[[i]])
  title = xinhua_url %>% 
    html_node("title") %>%
    html_text()
  date = xinhua_url %>% 
    html_node(".sj") %>%
    html_text()
  text = xinhua_url %>% 
    html_node("#Content") %>%
    html_text()
  pubdate = as.character(html_nodes(xinhua_url, "meta[name=pubdate]") %>% 
                           html_attr("content"))
  section = as.character(html_nodes(xinhua_url, "meta[name=section]") %>% 
                           html_attr("content"))
  source = as.character(html_nodes(xinhua_url, "meta[name=source]") %>% 
                          html_attr("content"))
  keywords = as.character(html_nodes(xinhua_url, "meta[name=keywords]") %>% 
                            html_attr("content"))
  lead = as.character(html_nodes(xinhua_url, "meta[name=description]") %>% 
                        html_attr("content"))
  xinhua_news = c(title, date, text, pubdate, section, source, keywords, lead)
  xinhua = append(xinhua, list(xinhua_news))}