
normalize.feature <- function( feature ) {
  
  ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) - min(feature, na.rm = T)))
  
  
}

