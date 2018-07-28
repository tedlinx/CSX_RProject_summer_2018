docs <- readLines("speech.txt") # 讀檔
head(docs)

# 清洗
deleter <- function(x, pattern) {
  gsub(pattern, x, replacement = "") %>%
    return()
}
docs <- deleter(docs, "[0-9]+")
head(docs)
docs <- deleter(docs,"[[:punct:]]")
head(docs)

# 斷詞+把斷詞結果轉換成 term-document matrix
docs.corpus <- Corpus(VectorSource(docs))
docs.seg <- tm_map(docs.corpus, segment("tag"))
docs.tdm <- TermDocumentMatrix(docs.seg, control = list())
inspect(docs.tdm)