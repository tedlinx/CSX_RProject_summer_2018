library(rvest)
library(xml2)
id = c(4018:4020)
url <- paste0("https://www.ptt.cc/bbs/HatePolitics/index", id,".html")
filename <- paste0(id, ".txt")

myptttm <- function(url ,filename){
 
# 定義爬一篇內容的函數article_detail
article_detail<- function(url){
 url <- "https://www.ptt.cc/bbs/HatePolitics/index4020.html"
  raw_html <- read_html(url)
  main_content_xpath <- "/html/body/div[@id='main-container']/div[@id='main-content']"
  article_content <- raw_html %>%
    html_nodes(xpath = main_content_xpath) %>%
    html_text()
}

# 將所有文章連結抓出來
article_link_xpath <- "//div[@class='title']/a"
article_links <- raw_html %>%
  html_nodes(xpath = article_link_xpath) %>%
  html_attr("href")
article_links <- paste("https://www.ptt.cc", article_links, sep = "")
article_links
# 爬下來的內文存成清單
alltext <- sapply(article_links, article_detail)
alltext

#存成txt.
write.table(alltext, file="0.txt")
}
myptttm(url, filename)
