# Author: Sam Turner
# Source file for functions that utilize google translate to translate text.
# Note that this does not use the standard google translate API which is a paid
# service.  It instead utilizes the API used by the google translate chrome
# extention.  It's unknown whether there are limits to the use of this service.
# It seems to be able to handle a maximum of around 1400 characters per request 
# which omes out to about 311 English words.  This is likely due to URL request
# restrictions.  As a workaround, strings with a length greater than 1500 by
# default are split into smaller sets before passing to API.

# LIBRARIES
library(RJSONIO)  #jsonlite will not process query correctly
library(dplyr)

# URL
# Chrome translate extention base URL
api.key <- "PLACE KEY HERE"
url <- "https://www.googleapis.com/language/translate/v2?format=text&key=" %>%
  paste0(api.key)
url <- "https://www.googleapis.com/language/translate/v2?q=Test&format=text&target=de&key={YOUR_API_KEY}" %>%
  

#FUNCTIONS
gTranslate <- function(line, lang.in = "", lang.out, max.length = 1500) {
  # Translates a line of text via api call.
  #
  # Args:
  #   line: Line of text to be translated.
  #   lang.in: Input language of text.  Default setting of "" will automatically
  #            detect language.
  #   lang.out: Output langage of text.  See below URL for a list of lang codes
  #             https://cloud.google.com/translate/docs/languages
  #   max.length: Maximum length of string to send to API to be translated.
  #               Strings greater than this length will be split before passing
  #               to API.  Absolute maximum is approximately 1400.
  # Returns:
  #   A vector containing the translated text and the input language code
  
  # Checks to see if line is greater than max allowed by API
  if(nchar(line) > max.length) {
    line <- gStringSplit(line, max.length)  # Split to smaller groups
  }
  
  if(lang.in != "")
    lang.in = paste0("&source=", lang.in)
  
  # Builds a list of API calls and gets JSON for each translation
  g.url <- lapply(line, function(x)
    paste0(url, lang.in, "&target=", lang.out, "&q=", x))
  g.JSON <- lapply(g.url, function(x) fromJSON(x, encoding = "UTF-8"))
  
  # Reads through JSON, extracting the translated text and puts into a string
  g.trans <- lapply(g.JSON, function(j)
    lapply(j[[1]], function(x) return(x[1]))) %>%
    unlist %>%
    paste(collapse = "")
  
  # Attach langauge code
  g.lang <- ifelse(lang.in == ""
                   , g.JSON[[1]][[9]][[4]][1]
                   , lang.in)
  
  return(c(g.trans, g.lang))
}

gTranslate.df <- function(v, lang.in = "", lang.out, max.length = 1500) {
  # Translates a vector or list of text via api call.
  #
  # Args:
  #   v: Vector or list containing a text to translate in each row.
  #   lang.in: Input language of text.  Default setting "" will automatically
  #            detect language.
  #   lang.out: Output langage of text.  See below URL for a list of lang codes
  #             https://cloud.google.com/translate/docs/languages
  #   max.length: Maximum length of string to send to API to be translated.
  #               Strings greater than this length will be split before passing
  #               to API.  Absolute maximum is approximately 1400.
  # Returns:
  #   A dataframe of the form original text, input language, translated text, 
  #   and output language.
  
  # Applies gTranslate function to each row in vector and puts results into
  # a dataframe
  g.df <- lapply(v, function(r) gTranslate(r, lang.in, lang.out)) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    t %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  # Builds final dataframe, converting all cols to chars
  g.df <- cbind(v, select(g.df, 2, 1), c(lang.out)) %>%
    setNames(c("original", "lang.in", "translation", "lang.out")) %>%
    transform(original = as.character(original)
              , lang.out = as.character(lang.out))
  
  rownames(g.df) <- NULL
  
  return(g.df)
}

gStringSplit <- function(line, max.length = 1500) {
  # Function to split strings longer than max.length before passing to API
  # NOTE: Split may be larger or smaller than max.lengh.  For this reason,
  # values no larger than 1500 are recommended.
  #
  # Args:
  #   line: line to be split into groups.
  #   max.length: Maximum length of string to send to API to be translated.
  #               Strings greater than this length will be split before passing
  #               to API.  Absolute maximum is approximately 1400.
  # Returns:
  #   A list containing line split into groups of size about max.length
  
  line.groups <- line %>%
    strsplit("[.]") %>%  # Split by periods
    unlist %>%
    lapply(function(x) paste0(x, ".")) %>%
    lapply(nchar) %>%  # List of counts of number of chars in each sentence
    as.numeric %>%
    cumsum %>%
    `/`(max.length) %>%
    floor %>%  # Above three lines split counts of chars into groups
    cbind(paste0(unlist(strsplit(line, "[.]")), ".")) %>%  # Attach group numbers
    as.data.frame %>%
    setNames(c("grp", "text")) %>%
    transform(text = as.character(text)
              , grp = as.factor(as.character(grp)))
  line.groups <- aggregate(line.groups, by = list(line.groups$grp)
                           , function(x) paste(x, collapse = ""))  # paste grps
  
  return(line.groups$text)
}