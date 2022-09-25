library(dplyr)
library(stringi)
library(numbers)

int_to_string <- function(number){
      bits <- as.numeric(intToBits(number))
      
      # number of 1's correspond to number of leading pluses
      ones <- sum(bits)-1

      # ideces of 1's
      ones_i <- ones_indeces(bits)
      
      # build pref strings for indeces
      ones_i <- lapply(ones_i, build_pref_string) %>% unlist()
      
      pref_string <- paste(ones_i, collapse = '^+11')
      pref_string <- paste(stri_dup("+", ones),'^+11', pref_string, sep = "")
      
      return(ones_i)

}

# Get vector - fill with indexes of all 1's
# Some sort of vapply with function that takes the index of one and spits corresponding
# prefix string encoding. Split the indexes into prime factorization
# Do some shit with remainders, mod, iNthroot

# The following function breaks the index of the binary string into powers of 2 plus any accessories
# This only has to manage up to 32, since intToBits only returns up to 32 bits
# Ex: If original int is 4, we have 0010... reading from the opposite direction

# Returns a vector of the indeces that have a 1 for the binary string
ones_indeces <- function(bits){
      indeces <- which(bits == 1)
      indeces <- indeces - 1
      return(indeces)
}

# Input is the subsetted output from ones_indeces, a number 
# Outputs a prefix string encoding
# Idea is to vapply(ones_i, build_pref_string) and then concatenate
# Later version - test if subtracting would be faster

build_pref_string <- function(index){
      if (index == 0){
            return(NULL)
      }
      
      if (index == 1){
            return(1)
      }

      two_exp <- "^+11"
      num_exp_2 <- trunc(log2(index)) 
      remainder <- index - 2^num_exp_2

      term_1 <- paste("(", two_exp, stri_dup("+", num_exp_2), stri_dup("1", num_exp_2), ")", sep = "")
      term_2 <- paste(stri_dup("+", remainder-1), stri_dup("1", remainder), sep = "")
            
      pref_string <- ifelse(term_2 != "NA", paste("+", term_1, term_2, sep = ""), paste(term_1, sep = "")) %>% 
            unlist()
      pref_string <- gsub('[A-Z]', '', pref_string)
      
      return(pref_string)
}
