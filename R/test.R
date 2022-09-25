pos_pref_string <- function(int, base){
      stri_base <- paste("^", stri_dup("+", base-1), stri_dup("1", base), sep="")
      num_exp <- trunc(log(x=int, base=base)) 
      remainder <- int - base^num_exp
      
      if(num_exp < 8){
            # term 1 is the exponent, term 2 is the remainder
            term_1 <- paste(stri_base, stri_dup("+", num_exp-1), stri_dup("1", num_exp), sep = "")
            term_2 <- paste(stri_dup("+", remainder-1), stri_dup("1", remainder), sep = "")
      }
      else{
            temp <- pos_pref_string(num_exp, 2)
            temp <- ifelse(nchar(temp)>nchar(pos_pref_string(num_exp, 3)), pos_pref_string(num_exp, 3), temp)
      }
      pref_string <- ifelse(term_2 != "NA", paste("+", term_2, term_1, sep = ""), paste(term_1, sep = "")) %>% 
            unlist()
      pref_string <- gsub('[A-Z]', '', pref_string)
      
      return(pref_string)
}