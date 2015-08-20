library("RTextTools")
library("tm")

get_tfidf_matrix <- function(file) {
    d <- readLines(file)
    df <- as.data.frame(d)
    dt <- create_matrix(df[1], removePunctuation=TRUE, removeStopwords=TRUE, weighting=weightTf)
    return(dt)
}

get_top_n_terms <- function(dt, top_n=4) {
    l = 0
    while(l == 0) {
      n <- findFreqTerms(dt, top_n)
      if(length(n) > 4) {
        l = 1
        break
      } else {
        top_n = top_n-1
      }
    }
  print(n)  
  return(n)
}

get_assoc_terms <- function(dt, l) {
  num_words <- length(l)
  thresh = 0.9
  done = 0
  op = vector()
  for(i in 1:num_words) {
    while(done == 0) {
      n <- lapply(l[i], function(x) findAssocs(dt, l[i], thresh))
      nd <- as.data.frame(n)
      assoc_names <- rownames(nd)
      if(length(assoc_names) < 1) {
        thresh= thresh - 0.1
      } else {
        print(assoc_names)
        op <- union(op, assoc_names)        
        break
      }
    }
  }  
  op <- union(op, l)
  return(op)
}

main_get_words <- function(f) {
  library(RTextTools)
  library(tm)
  dt <- get_tfidf_matrix(f)
  print("got tfidf matrix")
  rel_words <- get_top_n_terms(dt, 5)
  print("top n termss")
  #rel_words <- get_assoc_terms(dt, rel_words)
  #print("got assoc terms")
  outfile <- sprintf("%s.terms", f)
  write(rel_words, outfile)
  gc()
  return(rel_words)
}
