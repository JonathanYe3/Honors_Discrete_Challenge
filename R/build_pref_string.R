# Ditching the binary idea, too complicated and not optimal
build_pref_string <- function(int){
      if (int == 0)return("+-11")
      if (int == 1)return("1")
      
      # Prepare negatives
      neg <- int < 0
      int <- ifelse(neg, -1*int, int)
      
      # Prepare odds
      isodd <- mod(int,2)==1
      int <- ifelse(isodd, int-1, int)
      
      # Build even prefix string
      pref_string <- even_pref_string(int)
      
      # Handle odds
      pref_string <- ifelse(isodd, paste("+", pref_string, "1", sep = ""), pref_string)
      
      # Handle negatives
      pref_string <- ifelse(neg, paste("*", pref_string, "-1", sep = ""), pref_string)
      
      return(pref_string)
}

# Prefix string builder for even numbers
# Only takes even, nonnegative integers

even_pref_string <- function(int){
      if(int == 2)return("+11")
      two_exp <- "^+11"
      num_exp_2 <- trunc(log2(int)) 
      remainder <- int - 2^num_exp_2
      
      term_1 <- paste("(", two_exp, stri_dup("+", num_exp_2-1), stri_dup("1", num_exp_2), ")", sep = "")
      term_2 <- paste(stri_dup("+", remainder-1), stri_dup("1", remainder), sep = "")
      
      pref_string <- ifelse(term_2 != "NA", paste("+", term_1, term_2, sep = ""), paste(term_1, sep = "")) %>% 
            unlist()
      pref_string <- gsub('[A-Z]', '', pref_string)
      
      return(pref_string)
}

