rm(list=ls())

#Load Packages
library(dplyr)
library(tidytext)
library(rjson)


#Select data. Doc_id is the response ID or unique identifier and text is the column with the open-ended responses.
df <- data.frame(doc_id = c("doc_1", "doc_2", "doc_3", "doc_4", "doc_5", "doc_6", "doc_7"),
                 text = c("All cat are dog.", "This text asdfsdf", "qwerqwxcasd", "hopefully this works very well", "Teruhashi", "this sentence says fuck", ""),
                 stringsAsFactors = FALSE)

#Loading dictionary
dict_dir <- tempdir()
dict_url <- 'http://downloads.sourceforge.net/wordlist/scowl-2016.01.19.zip'
dict_local_zip <- file.path(dict_dir, basename(dict_url))
if (! file.exists(dict_local_zip)) {
  download.file(dict_url, dict_local_zip)
  unzip(dict_local_zip, exdir=dict_dir)
}

dict_files <- list.files(file.path(dict_dir, 'final'), full.names=TRUE)
dict_files_match <- as.numeric(tools::file_ext(dict_files)) <= 60 & grepl("english-", dict_files, fixed = TRUE)
dict_files <- dict_files[ dict_files_match ]

words <- unlist(sapply(dict_files, readLines, USE.NAMES=FALSE))
length(words)

#Acceptable Words
words2 <- c(words, "teruhashi") #Enter any proper nouns that you do not want the script to detect as gibberish (DO LOWER CASE)
length(words2)

#Choose bad words
bad_words <- fromJSON(file= "https://raw.githubusercontent.com/web-mech/badwords/master/lib/lang.json" )
bad_words <- unlist(bad_words)
#bad_words <- c("asshole", "bitch", "fuck", "motherfucker","shit") #If you prefer to make a customized list, build it here.

# Make dataset long with one word per line
df_test <- df %>%
  unnest_tokens(word, text)

output <- df_test %>% 
  mutate(real_word = ifelse(word %in% words2, 1, 0)) %>% 
  group_by(doc_id) %>% 
  summarize(pct_real_words = mean(real_word))

bad_words_output <- df_test %>% 
  mutate(real_word = ifelse(word %in% bad_words, 1, 0)) %>% 
  group_by(doc_id) %>% 
  summarize(pct_bad_words = mean(real_word))

output2 <- data.frame(output)
output3 <- data.frame(bad_words_output)
df3 <- merge(df, output2)
df4 <- merge(df3, output3)

#Test for Gibberish and Swear words
df4$outcome <- "Gibberish"
df4$outcome[df4$pct_real_words>.75] <- "Not Gibberish" #Can adjust threshold (.75 seemed reasonable)
df4$swears <- "Safe"
df4$swears[df4$pct_bad_words>0] <- "Contains Swear"
df4
getwd()

#Export the output
write.csv(df4, "df4.csv")
