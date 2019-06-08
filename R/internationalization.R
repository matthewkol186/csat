translation_map <- function(trans_file_url="./translation.csv") {
  translate_matrix <- read.csv(trans_file_url, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE)
  langs <- colnames(translate_matrix)
  if(!('en' %in% langs)){
    stop("'en' must be a column in the translation file.")
  }
  langs <- langs[langs != 'en'] # remove english
  lang_to_vocab <- new.env() # hack for hashmap
  for(l in langs){
    lang_to_vocab[[l]] <- new.env()
  }
  for(row in 1:nrow(translate_matrix)){
    eng_msg = translate_matrix[row, 'en']
    for(l in langs){
      lang_to_vocab[[l]][[eng_msg]] <-  translate_matrix[row, l]
    }
  }
  return(lang_to_vocab)
}

# Translate a string if necessary
# Args: string of interest in English, language
# Returns: (potentially) translated string
i18n <- function(s, lang, vocab){
  if(lang == 'en'){
    return(s)
  }
  # check language is supported
  if(!exists(lang, envir=vocab, inherits=FALSE)){
    stop("Language is not supported in translation")
  }
  # check text is found
  if(!exists(s, envir=vocab[[lang]], inherits=FALSE)){
    stop("Requested translation was not found")
  }
  return(vocab[[lang]][[s]])
}
