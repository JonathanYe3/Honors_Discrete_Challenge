#' Used in build_skeleton

remainder <- function(int, base, pos){
      if(pos){
            num_exp <- trunc(log(x=int, base=base)) 
            remainder <- int - base^num_exp
      }
      else{
            num_exp <- trunc(log(x=int, base=base)) + 1
            remainder <- base^num_exp - int
      }
      
      return(remainder)
}


#' @param pos positive remainder
#' @param neg negative remainder

build_skeleton <- function(int){
      # setup which combo - remainder type and base
      base <- c(2,3,2,3)
      pos <- c(T,F,F,T)
      remainder <- mapply(remainder, int, base, pos)
      opt_index <- which.min(remainder)
      
      base <- base[opt_index]
      pos <- pos[opt_index]
      remainder <- remainder[opt_index]
      num_exp <- ifelse(pos, trunc(log(x=int, base=base)), trunc(log(x=int, base=base)) + 1)
      
      # Build prefix strings, regular range
      stri_base <- paste("^", stri_dup("+", base-1), stri_dup("1", base), sep="")
      # Term 1
      if(num_exp<8){
            term_1 <- paste(stri_base, stri_dup("+", num_exp-1), stri_dup("1", num_exp), sep = "")
      }
      else if(num_exp>=8){
            num_exp_string <- build_skeleton(num_exp)
            term_1 <- paste(stri_base, num_exp_string, sep = "")
      }
      
      # Term 2
      if(pos & (remainder < 8)){
            term_2 <- paste(stri_dup("+", remainder-1), stri_dup("1", remainder), sep = "")
      }
      else if(!pos & (remainder < 8)){
            term_2 <- paste(stri_dup("+", remainder-1), stri_dup("-1", remainder), sep = "")
      }
      else if(pos & (remainder >= 8)){
            remainder_string <- build_skeleton(remainder)
            term_2 <- paste(stri_dup("+", remainder-1), stri_dup("1", remainder), remainder_string, sep = "")
      }
      else if(!pos & (remainder >= 8)){
            remainder_string <- build_skeleton(remainder)
            term_2 <- paste(stri_dup("+", remainder-1), stri_dup("-1", remainder), remainder_string, sep = "")
      }
      
      # Assemble full string
      pref_string <- ifelse(term_2 != "NA", paste("+", term_2, term_1, sep = ""), paste(term_1, sep = "")) %>% 
            unlist()
      pref_string <- gsub('[A-Z]', '', pref_string)
      
      return(pref_string)
}

build_pref_string <- function(int){
      # Catch
      if (int == 0)return("+-11")
      if (int == 1)return("1")
      
      # Prepare negatives
      neg <- int < 0
      int <- ifelse(neg, -1*int, int)
      pref_string <- ifelse(neg, "*-1", "")
      
      pref_string <- paste(pref_string, build_skeleton(int), sep = "")
      pref_string <- gsub(" ", "", pref_string)
      
      return(pref_string)
}
