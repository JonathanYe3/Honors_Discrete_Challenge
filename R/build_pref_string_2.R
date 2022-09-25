#' Goal: improve previous script by adding generalizable base
#' Should include tests to see if the int is closer to a power of 2 or 3

build_pref_string <- function(int){
      if (int == 0)return("+-11")
      if (int == 1)return("1")
      
      # Prepare negatives
      neg <- int < 0
      int <- ifelse(neg, -1*int, int)
      
      # Handle 3^n
      if (remainder(int, 3)<remainder(int,2)){
            isodd <- F
            pref_string <- gen_pref_string(int = int, base = 3)
      }
      else{ # Int must be even or a non 3^n odd
            # Prepare odds
            isodd <- mod(int,2)==1
            int <- ifelse(isodd, int-1, int)
            
            pref_string <- gen_pref_string(int = int, base = 2)
      }
      
      # Handle odds
      pref_string <- ifelse(isodd, paste("+1", pref_string, sep = ""), pref_string)
      
      # Handle negatives
      pref_string <- ifelse(neg, paste("*-1", pref_string, sep = ""), pref_string)
      
      return(pref_string)
}

gen_pref_string <- function(int, base){
      stri_base <- paste("^", stri_dup("+", base-1), stri_dup("1", base), sep="")
      num_exp <- trunc(log(x=int, base=base)) 
      remainder <- int - base^num_exp
      
      #term_1 <- paste("(", stri_base, stri_dup("+", num_exp-1), stri_dup("1", num_exp), ")", sep = "")
      term_1 <- paste(stri_base, stri_dup("+", num_exp-1), stri_dup("1", num_exp), sep = "")
      term_2 <- paste(stri_dup("+", remainder-1), stri_dup("1", remainder), sep = "")
      
      pref_string <- ifelse(term_2 != "NA", paste("+", term_1, term_2, sep = ""), paste(term_1, sep = "")) %>% 
            unlist()
      pref_string <- gsub('[A-Z]', '', pref_string)
      
      return(pref_string)
}

# Remainder function, prints int - value of base exponent closest to the integer
remainder <- function(int, base){
      num_exp <- trunc(log(x=int, base=base)) 
      remainder <- int - base^num_exp
      return(remainder)
}
