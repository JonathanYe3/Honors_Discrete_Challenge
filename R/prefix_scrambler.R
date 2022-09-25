string <- "+++111"

full_scramble <- function(s_word) {
      s_word <- as.character(s_word)
      i <- sample(1:nchar(s_word))
      sep_word <- unlist(strsplit(s_word, ""))
      paste(sep_word[i], collapse = "")
}

my_list <- list()
for(i in 1:1000){
      s <- full_scramble(string)
      my_list[[i]] <- s
}

permutations <- unique(my_list) %>% unlist()

x <- c("+++111","1+1+1+", "++11+1", "1++11+", "+1++11",
       "1++1+1", "+1+1+1", "1+++11", "++111+", "+1+11+",
       "+11++1", "+11+1+", "++1+11", "1+1++1")
