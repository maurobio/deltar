#==============================================================================
#
# Project : FREE DELTA - Software system for processing taxonomic description
#           coded in DELTA (DEscription Language for TAxonomy) format
#
# Module  : Identification - Function for testing item identification
#           with DeltaR package.
#
# Release : 0.10 alpha - January 2001
# Author  : Denis ZIEGLER  denis.ziegler@free.fr (original C++)
# Port    : R version using DeltaR package - August 2026
# Author  : Mauro CAVALCANTI  maurobio@gmail.com (R port)
#
# File    : identification.R
#
# Portability : R (cross-platform)
#
# (C) Copyright 2001 - Denis ZIEGLER (original C++)
# (C) Copyright 2026 - Mauro CAVALCANTI (R port)
#==============================================================================

message_identification <- function() {
  cat("\nIDENTIFICATION\n")
  cat("This function demonstrates the possibility using DeltaR package for taxa\n")
  cat("identification.\n")
  cat("In this example, the identification process has been simplified a lot.\n")
  cat("It is based on successive comparisons between observed characters values,\n")
  cat("and the items descriptions stored in the Delta files.\n")
  cat("Advanced features, such as values combinations, implicit values, dependant\n")
  cat("characters are not implemented. There is no characters selection during\n")
  cat("identification (like the 'best' command in Intkey).\n")
  cat("Of course, the user interface is very simple, and should be improved in\n")
  cat("a public release.\n\n")
  cat("Usage :\n")
  cat("To make an identification, you should have a printed version of characters\n")
  cat("description (Delta chars file).\n")
  cat("Select a character and a value according to the taxon you want to identify.\n")
  cat("After each selection, the program displays all characters values already\n")
  cat("selected, and the list of remaining taxa.\n")
  cat("To cancel a previous selected character, select it again, and input the\n")
  cat("UNKNOWN value (-1). You can also modify a previously selected value.\n\n")
}

identification <- function(delta) {
  #----- Initialisation -----
  nbval <- DeltaR::get_chars_nb(delta)
  nbitm <- DeltaR::get_items_nb(delta)
  
  tabval <- rep(NA, nbval)
  tabitm <- rep(1, nbitm)
  
  message_identification()
  
  #----- User input and search matching items -----
  while (TRUE) {
    #--- Input character number and value
    cat("Select a character describing the item to identify :\n")
    cat("  Enter character number (0=exit) : ")
    charnum <- as.integer(readline())
    
    if (is.na(charnum) || charnum == 0) break
    
    if (charnum < 1 || charnum > nbval) {
      cat("  Invalid character number\n")
      next
    }
    
    cat("  Enter character state/value (-1=unknown) : ")
    charval <- as.numeric(readline())
    
    if (is.na(charval)) {
      cat("  Invalid value, please enter a number\n")
      next
    }
    
    #--- Stores value
    tabval[charnum] <- if (charval == -1) NA else charval
    
    #--- Update items table considering selected values
    for (i in 1:nbitm) {
      for (j in 1:nbval) {
        if (is.na(tabval[j])) {
          tabitm[i] <- 1
        } else {
          tabitm[i] <- as.integer(DeltaR::matches(delta, i, j, tabval[j]))
        }
        if (tabitm[i] == 0) break
      }
    }
    
    #--- Displays selected characters and values, and remaining items list
    cat("\nCHARACTERS SELECTION\n")
    for (j in 1:nbval) {
      if (!is.na(tabval[j])) {
        cat(j, ". ", DeltaR::get_char_feature(delta, j), "\n", sep = "")
        cat("    state/value : ", tabval[j], sep = "")
        
        if (DeltaR::get_char_type(delta, j) == 2) {
          cat(" - ", DeltaR::get_state(delta, j, as.integer(tabval[j])), "\n", sep = "")
        } else if (DeltaR::get_char_type(delta, j) == 4) {
          cat(" ", DeltaR::get_char_unit(delta, j), "\n", sep = "")
        } else {
          cat("\n")
        }
      }
    }
    
    cat("ITEMS REMAINING\n")
    for (i in 1:nbitm) {
      if (tabitm[i] == 1) {
        cat(delta$get_item_name(i), "\n")
      }
    }
    cat("\n")
  }
}

#==============================================================================
# Example usage (commented out):
#
# library(DeltaR)
# delta <- load_delta("chars_viola", "items_viola")  # read DELTA files
# identification(delta)
#==============================================================================