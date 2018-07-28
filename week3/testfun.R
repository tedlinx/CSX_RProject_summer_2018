url <- "https://topic.udn.com/event/20180611"
article <- read_html(url)
article

content <- article %>%
  html_nodes(xpath = "/html/body[@id='topic-1']/div[@class='wrapper']/section[@class='article-holder']")%>%
  html_text()

content
