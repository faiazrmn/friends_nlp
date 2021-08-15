require(tidyverse)
require(wordcloud)

txt_files <- fs::dir_ls("C:/Users/User/Desktop/Friends NLP/", regexp = "\\.txt$")
txt_files # List of all txt files in the folder

dat <- txt_files %>% # map_dfr reads files and bind rows, tsv read tab separated values file
  map_dfr(~read_tsv(. ,  col_names = FALSE, skip_empty_rows = T), .id = "source")

# Cleaning the Episode Names & Season number
dat_clean <- dat %>% mutate( episode =  str_split_fixed(source, " ", n = 3), # Separate Folder name, season, episode name by space
                             Season = str_remove(episode[, 2], "^NLP/"), # Delete Folder Name
                             Season = str_remove(Season, "^\\d-S\\d\\d"), # \\d detects a number, so this step removes 
                             Season = str_remove(Season, "^\\d-s\\d\\d"), # For a special case
                             Season = str_remove(Season, "E\\d\\d*"), # remove episode part
                             Season = str_remove(Season, "-S\\d\\dE\\d\\d*"), # For two episodes combined s1e1-s1e2, remove the last part
                             Season = str_remove(Season, "^S|^s"), # remove S or s to keep the season number only
                             Episode = str_remove(episode[ , 2], "^NLP/S\\d\\dE|^NLP/s\\d\\dE"), # Remove all parts except the episode number
                             Episode = str_remove(Episode, "-S\\d\\dE\\d\\d*"), # for two episodes combined s1e1-s1e2, remove the last part
                             Episode_Name = episode[, 3], # Episode name from file name
                             Episode_Name = str_remove(Episode_Name, ".txt*"), # Remove .txt part from Episode names
                             Episode_Name = str_trim(Episode_Name) # Remove white space if any, from end and start 
                             ) %>%   
  
  # Cleaning the Dialouges
  # Remove Numbered Names in Episodes in First Line
  mutate(X1 = str_remove(X1, "^[0-9]+ - [0-9]+ - |^[0-9]+ - |^[0-9]+-[0-9]+ - |^[0-9]+- |^[0-9]+|^[0-9]+ - ")) %>% 
  # 915 - 916 - The one in Barbados, 123 - , 1024-1023 - , 291-
  
  # Special case of TOW phoebes rat
  mutate(X1 = str_replace(X1, "^TOW ", "The One With ")) %>%
  
  # Remove lines Containing other than dialogues
  filter(!str_detect(X1, "closing credit|Closing Credit|^commercial break|^Commercial Break|^Part|^Dedicated|^NOTE|^Written|^Directed|^Hosted|^Produced|^Teleplay|^Friends|^FRIENDS|^opening|^Opening|^The One|^The one|^The Last|^ The One|THE ONE|^End|^The|^Originally|^Final|^Opening|^Story|^part|^Part|^Transcribed|^Lisa|^Matt|^Jennifer|OPENING")) %>% 
  
  # Remove lines starting with ( or [ or { or <
  filter(!str_detect(X1, "^\\(|^\\{+|^\\[+|\\<+")) %>% 
  
  # Get everything to lower cases
  mutate(X1 = str_to_lower(X1)) %>% 
  
  # Get Character & dialogue by splitting in two parts with ":"
  mutate(`Character&Dialogue` = str_split_fixed(X1, ":", n = 2)) %>% 
  mutate(Character = `Character&Dialogue`[, 1],
         Dialogue = `Character&Dialogue`[ , 2],
         Dialogue = str_trim(Dialogue, side = "both")) %>%  # Remove White Spaces in each line
  
  # Filter out special episode
  filter(Season != 07 & Episode != 24) %>% 
  mutate(Character = if_else(Character == "rach", "rachel", Character)) %>% 
  
  # Remove Unnecessary Columns
  select(-source, -X1, -episode, -`Character&Dialogue`) 

dat_clean %>% head()

# Final Check of the clean data
# Filter First row of each episode
dat_clean %>%
  group_by(Season, Episode) %>%
  filter(row_number() == 1 | row_number() == n()) %>% 
  select(Dialogue) %>% 
  View()

# Only The main 6 character dialogues are important
dat_clean %>% 
  count(Character, sort = T) %>% 
  head(n = 25) %>%
  ggplot(aes(n, reorder(Character, n))) +
  geom_col() +
  theme_minimal()
  
dat_clean %>% 
  filter(Character %in% c("rachel", "joey", "phoebe", "ross", "monica", "chandler")) %>% 
  group_by(Character, Season) %>% 
  summarise(n()) %>%
  left_join(dat_clean %>% 
              filter(Character %in% c("rachel", "joey", "phoebe", "ross", "monica", "chandler")) %>% 
              group_by(Character) %>% 
              summarise(sn = n()), by = "Character") %>% 
  ggplot(aes(x = reorder(Character,`n()`), y = `n()`, fill = Season)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label = `n()`, group = Season), position = position_stack(vjust = .5), size = 3.5) +
  geom_text(aes(Character, sn, label = sn), nudge_y = 500) +
  labs(x = "Character", y = "Dialogues") +
  theme_minimal()


plotly::ggplotly( 
  dat_clean %>% 
    filter(Character %in% c("rachel", "joey", "phoebe", "ross", "monica", "chandler")) %>% 
    group_by(Character, Season) %>% 
    summarise(n()) %>%
    left_join(dat_clean %>% 
                filter(Character %in% c("rachel", "joey", "phoebe", "ross", "monica", "chandler")) %>% 
                group_by(Character) %>% 
                summarise(sn = n()), by = "Character") %>% 
    ggplot(aes(x = reorder(Character,`n()`), y = `n()`, fill = Season)) + 
    geom_bar(stat="identity") +
    geom_text(aes(label = `n()`, group = Season), position = position_stack(vjust = .5), size = 3.5) +
    geom_text(aes(Character, sn, label = sn), nudge_y = 500) +
    labs(x = "Character", y = "Dialogues") +
    theme_minimal()
)

dat_clean %>% 
  filter(Character %in% c("rachel", "joey", "phoebe", "ross", "monica", "chandler")) %>% 
  group_by(Season) %>% 
  summarise(n()) %>% 
  ggplot(aes(x = reorder(Season,`n()`), y = `n()`, fill = Season)) + geom_bar(stat="identity")


### unnest Tokens
require(tidytext)

### COnvert Dialogues into words
dat_word <- dat_clean %>%
  unnest_tokens(Word, Dialogue, drop = T) 

# Most common words by character
dat_word %>% 
  filter(Character %in% c("rachel", "joey", "phoebe", "ross", "monica", "chandler")) %>% 
  count(Character, Word, sort = T) %>% 
  group_by(Character) %>% 
  top_n(30) %>% 
  ggplot(aes(n, reorder_within(Word, n, Character), fill = Character)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~Character, scales = "free") +
  theme_minimal()

# Most common words by character Without Stopwords
dat_word %>% 
  anti_join(stop_words, by = c("Word" = "word")) %>% 
  filter(Character %in% c("rachel", "joey", "phoebe", "ross", "monica", "chandler")) %>% 
  count(Character, Word, sort = T) %>% 
  group_by(Character) %>% 
  top_n(30) %>% 
  ggplot(aes(n, reorder_within(Word, n, Character), fill = Character)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~Character, scales = "free") +
  theme_minimal()


# Word Frequency and Histogram
dat_word %>% 
  anti_join(stop_words, by = c("Word" = "word")) %>% 
  filter(Character %in% c("rachel", "joey", "phoebe", "ross", "monica", "chandler")) %>% 
  count(Character, Word, sort = T) %>% 
  group_by(Character) %>% 
  add_count(Total = sum(n)) %>% 
  
  ggplot(aes(n/Total, fill = Character)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0005) +
  scale_y_log10() + ### Took log
  facet_wrap(~Character, ncol = 3, scales = "free_y")


# Wordcloud for Monica Without Stopwords
dat_word %>% 
  anti_join(stop_words, by = c("Word" = "word")) %>% 
  filter(Character == "monica") %>% 
  count(Word, sort = T) %>%
  with(wordcloud(words = Word, freq = n, random.order = T, max.words = 100 ))


# Ross and Rachel Words
library(scales)

dat_word %>% 
  anti_join(stop_words, by = c("Word" = "word")) %>% 
  filter(Character %in% c("rachel", "ross")) %>% 
  mutate(Word = str_extract(Word, "[a-z']+")) %>% 
  count(Character, Word) %>%
  group_by(Character) %>% 
  mutate(proportion = round(n / sum(n), 100)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Character, values_from = proportion) %>% 
  
  ggplot(aes(x = rachel, y = ross, color = abs(rachel - ross) )) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.05, size = 1, width = 0.1, height = 0.1) +
  geom_text(aes(x = rachel, y = ross, label = Word), check_overlap = T, vjust = 0.5, size = 3) + 
  scale_x_log10() +
  scale_y_log10() +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme_minimal() +
  theme(legend.position="none") 
  
# Chandler and monica Words
library(scales)

dat_word %>% 
  anti_join(stop_words, by = c("Word" = "word")) %>% 
  filter(Character %in% c("chandler", "monica")) %>% 
  mutate(Word = str_extract(Word, "[a-z']+")) %>% 
  count(Character, Word) %>%
  group_by(Character) %>% 
  mutate(proportion = round(n / sum(n), 100)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Character, values_from = proportion) %>% 
  
  ggplot(aes(x = chandler, y = monica, color = abs(chandler - monica) )) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.05, size = 1, width = 0.1, height = 0.1) +
  geom_text(aes(x = chandler, y = monica, label = Word), check_overlap = T, vjust = 0.5, size = 3) + 
  scale_x_log10() +
  scale_y_log10() +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme_minimal() +
  theme(legend.position="none") 

# Joey and phoebe Words
library(scales)

dat_word %>% 
  anti_join(stop_words, by = c("Word" = "word")) %>% 
  filter(Character %in% c("joey", "phoebe")) %>% 
  mutate(Word = str_extract(Word, "[a-z']+")) %>% 
  count(Character, Word) %>%
  group_by(Character) %>% 
  mutate(proportion = round(n / sum(n), 100)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Character, values_from = proportion) %>% 
  
  ggplot(aes(x = joey, y = phoebe, color = abs(joey - phoebe) )) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.05, size = 1, width = 0.1, height = 0.1) +
  geom_text(aes(x = joey, y = phoebe, label = Word), check_overlap = T, vjust = 0.5, size = 3) + 
  scale_x_log10() +
  scale_y_log10() +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme_minimal() +
  theme(legend.position="none")


# tf-tdf

dat_tf <- dat_clean %>% 
  filter(Character %in% c("rachel", "joey", "phoebe", "ross", "monica", "chandler")) %>% 
  unnest_tokens(Word, Dialogue, drop = F) %>%
  count(Character, Word, sort = TRUE)

dat_tfidf <- dat_tf %>% 
  bind_tf_idf( term =  Word, document = Character, n = n) %>% 
  arrange(desc(tf_idf))

dat_tfidf %>% 
  group_by(Character) %>% 
  slice_max(tf_idf, n = 50) %>% 
  ggplot(aes(tf_idf, reorder_within(Word, tf_idf, Character), fill = Character)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~Character, scales = "free") +
  theme_minimal()


# tf-tdf WIthout Stopwords

dat_tf <- dat_clean %>% 
  filter(Character %in% c("rachel", "joey", "phoebe", "ross", "monica", "chandler")) %>% 
  unnest_tokens(Word, Dialogue, drop = F) %>%
  anti_join(stop_words, by = c("Word" = "word")) %>% 
  count(Character, Word, sort = TRUE)

dat_tfidf <- dat_tf %>% 
  bind_tf_idf( term =  Word, document = Character, n = n) %>% 
  arrange(desc(tf_idf))

dat_tfidf %>% 
  group_by(Character) %>% 
  slice_max(tf_idf, n = 20) %>% 
  ggplot(aes(tf_idf, reorder_within(Word, tf_idf, Character), fill = Character)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~Character, scales = "free") +
  theme_minimal()



