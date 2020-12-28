# This file contains all the self-written functions used for importing and transmuting the given data

## @knitr genre_functions

anycharacter <- "([a-zA-Z]|-+)"

genre_to_uppercase_unique <- function(genre) {
  if (length(genre) == 0 | genre == "") {
    return(NA)
  }
  
  curr <- ""
  genre_vector <- c()
  c <- strsplit(genre, "")[[1]]
  
  for (i in 1:length(c)) {
    if (grepl(anycharacter, c[i])) {
      curr <- gsub(" ", "", str_squish(paste(curr, c[i])))
    }
    if (!grepl(anycharacter, c[i]) | i == length(c)) {
      if (i > 1 & grepl(anycharacter, c[i-1])) {
        whole_word = strsplit(curr, "")[[1]]
        single_genre <- ""
        
        for (j in 1:length(whole_word)) {
          if (j == 1) {
            single_genre <- gsub(" ", "", str_squish(toupper(whole_word[j])))
          } else {
            single_genre <- gsub(" ", "", str_squish(paste(single_genre, whole_word[j])))
          }
        }
        
        if (is.na(match(single_genre, genre_vector))) {
          genre_vector <- c(genre_vector, single_genre)
        }
      }
      curr <- ""
    }
  }
  
  all_genres <- ""
  for (i in 1:length(genre_vector)) {
    if (i != length(genre_vector)) {
      all_genres <- str_squish(paste(all_genres, genre_vector[i], "/"))
    } else {
      all_genres <- paste(all_genres, genre_vector[i])
    }
  }
  return(all_genres)
}

genres_to_uppercase_unique <- function(genres) {
  genres_vector <- c()
  for (genre in genres) {
    genres_vector <- c(genres_vector, c(genre_to_uppercase_unique(genre)))
  }
  return(genres_vector)
}