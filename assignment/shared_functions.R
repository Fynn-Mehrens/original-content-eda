# This file contains all the self-written function used for importing and transmuting the given data

anycharacter <- "([a-zA-Z]|-+)"

genre_to_uppercase <- function(genre) {
  if (length(genre) == 0 | genre == "") {
    return(NA)
  }
  curr <- ""
  all_genres <- ""
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
        if (i != length(c)) {
          all_genres <- str_squish(paste(all_genres, single_genre, "/"))
        } else {
          all_genres <- paste(all_genres, single_genre)
        }
      }
      curr <- ""
    }
  }
  return(all_genres)
}

genres_to_uppercase <- function(genres) {
  genres_vector <- c()
  for (genre in genres) {
    genres_vector <- c(genres_vector, c(genre_to_uppercase(genre)))
  }
  return(genres_vector)
}