# Leticia Han ------
# Math 533
# Fall 2020

# Library ------
library(schrute)
library(tidyverse)
library(tidytext)
library(textrecipes)
library(textmineR)
library(textdata) # for data dictionary for sentiment analysis
library(textfeatures)
library(tidylo)
library(broom)
library(tidymodels)
library(gridExtra)
library(ggalt)
library(tm)
library(quanteda) # for nlp
library(tokenizers)
library(SnowballC)
library(spacyr)
library(stopwords)
library(wordcloud)
library(ggwordcloud)
library(quanteda.textmodels)
library(caret)
library(glmnet)
library(gt)
library(hardhat)
library(vip)
library(sentimentr)
library(ggraph)
library(igraph)
library(widyr)
library(topicmodels)
library(tictoc)
library(future)
library(keras)
# conflicted::conflict_prefer("filter", "dplyr")
# conflicted::conflict_prefer("vi", "vip")
# conflicted::conflict_prefer("neighbors", "dials")
# conflicted::conflict_prefer("annotate", "ggplot2")
# source('theme.R')
source('LeticiaHan_Math533_FinalPaper_officetheme.R')

# EDA --------------
data <- theoffice
glimpse(data) # 55,130 rows 12 col
# remove air_date, index, and text with direction
data <- data %>% select(-air_date, -index, -text_w_direction)
my_stopwords <- c("yeah", "hey", "uh", "um", "guy", "time", "go", "michael")
my_stopwords <- c(my_stopwords, "ah", "hmm", "huh", "ya", "'s", "ay", "ha", "je", "mo", "yy", letters)
my_stopwords <- c(my_stopwords, "ok", "well", "oh", "okay", "become", "every")

# * Data cleaning--------
# remove the parts 1&2 in episode_name
data <- data %>% mutate(episode_name = str_squish(str_replace_all(episode_name, "\\(Parts 1&2\\)", "")))
# fix spelling mistakes
correct_spelling <- function(data, column, from, to) {
  ind <- which(data[,column] == from)
  data[ind, column] <- to
  return(data)
}

fr <- c("Charles McDougal", "Claire Scanlon", "Greg Daneils", "Ken Wittingham", "Paul Lieerstein")
to <- c("Charles McDougall", "Claire Scanlong", "Greg Daniels", "Ken Whittingham", "Paul Lieberstein")
for(i in seq_along(fr)) {
  data <- correct_spelling(data, "director", fr[i], to[i])
}
fr <- c("Michae", "MIchael", "Michal", "Micheal", "Michel", "Video Michael", "Warehouse Michael", "Young Michael", "M ichael", "Micael", "Mihael", "Miichael")
for(i in seq_along(fr)) {
  data <- correct_spelling(data, "character", fr[i], "Michael")
}
# remove double quotations
data$character <- str_replace_all(data$character, '"', "")
data$character <- str_replace_all(data$character, " \\[on phone\\]", "")
# fix characters names
fr <- c("Deangelo", "David", "Packer", "Todd", "Daryl", "Ryan Howard", "Robert California", "Carroll", "Micahel", "DwightKSchrute", "Darrly", "Meridith", "Phylis", "Stanely", "Angel", "Angels", "Anglea", "Carrol", "Chares", "Dacvid Walalce", "Dacvid Wallace", "Darry", "David Wallcve", "DeAgnelo", "Denagelo", "Dight", "Dwight.", "JIm", "Jo Bennett",  "Phyliss", "sAndy", "Bob", "Bob Vance, Vance Refrigeration")
to <- c("DeAngelo", "David Wallace", "Todd Packer", "Todd Packer", "Darryl", "Ryan", "Robert", "Carol", "Michael", "Dwight", "Darryl", "Meredith", "Phyllis", "Stanley", "Angela", "Angela", "Angela", "Carol", "Charles", "David Wallace", "David Wallace", "Darryl", "David Wallace", "DeAngelo", "DeAngelo", "Dwight", "Dwight", "Jim", "Jo", "Phyllis", "Andy", "Bob Vance", "Bob Vance")
for(i in seq_along(fr)) {
  data <- correct_spelling(data, "character", fr[i], to[i])
}
data <- data %>% mutate(director = str_squish(director), character = str_squish(character))


# * Ratings ------------

ratings <- data %>% select(season, episode, episode_name, director, writer, imdb_rating, total_votes) %>% distinct()
# hist(ratings$imdb_rating)
ratings %>% ggplot(aes(imdb_rating)) + geom_histogram(breaks = seq(6.5, 10, .5), fill = '#64A6B0', color = 'black', alpha = 0.5) +
  labs(title = 'Histogram of IMDB Ratings')

# Rating by Season
ratings %>% group_by(season) %>%
  summarise(no_of_ep = n(), avg_rating = mean(imdb_rating))
ratings %>%
  ggplot(aes(season, imdb_rating, fill = as.factor(season))) +
  geom_boxplot(show.legend = FALSE) +
  annotate("text", x = 7.9, y = 9, label = "Michael leaves") +
  annotate(
    geom = "curve", x = 8, y = 8.9, xend = 7.5, yend = 8.5,
    curvature = -.3, arrow = arrow(length = unit(2, "mm"))
  ) +
  scale_fill_manual(values = office_pal(length(unique(ratings$season)))) +
  scale_x_continuous(breaks = 1:9) +
  labs(x = 'Season', y = 'Rating', title = 'IMDB Ratings by Season')

# Rating by Episode
ratings %>% ggplot(aes(episode, imdb_rating, fill = as.factor(episode))) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = office_pal(length(unique(ratings$episode)))) +
  labs(x = "Episode Number", y = "Rating", title = "IMDB Ratings by Episode Number")

# Total Votes by Season
data %>% distinct(season, episode_name, total_votes, imdb_rating) %>%
  group_by(season) %>% summarise(`avg_votes` = round(mean(total_votes)))

# * Directors-----
ratings <- ratings %>% mutate(episode_number = row_number())
director <- ratings %>%
  select(season, episode, episode_name, director, imdb_rating, total_votes,
         episode_number) %>%
  distinct() %>%
  separate_rows(director, sep = ';') %>%
  add_count(director) %>%
  arrange(desc(n))

director %>% group_by(director, n) %>% summarise(`Average Rating` = round(mean(imdb_rating), 2)) %>%
  ungroup() %>% arrange(desc(n)) %>% slice(1:10)

director %>% group_by(director, n) %>% summarise(`Average Rating` = round(mean(imdb_rating), 2)) %>%
  ungroup() %>% arrange(desc(`Average Rating`)) %>% slice(1:10)

# TODO: FIX DIRECTOR_RATING WITH THE REST
director_rating <- director %>% group_by(director, n) %>% summarise(avg_rating = mean(imdb_rating))

director_rating %>% arrange(desc(avg_rating)) %>%
  mutate(director = fct_inorder(director), n2 = ifelse(n>5, n, NA)) %>%
  ggplot(aes(reorder(director, avg_rating), avg_rating)) +
  geom_segment(aes(yend = 7.0, xend = director), size = 0.2) +
  geom_point(aes(size = n, color = n > 5), show.legend = FALSE) +
  geom_text(aes(label = n2), size = 3, color = 'white') +
  scale_color_manual(values = c("gray40", beet)) +
  coord_flip() +
  labs(x = NULL, y = 'Average Rating', title = "Average IMDB Rating by Directors",
       subtitle = 'size represents number of episodes directed, \n color represents > 5 episodes directed') +
  theme(panel.grid.major.y = element_blank())



# * Writers------
writers <- ratings %>% select(season, episode, episode_name, writer, imdb_rating, total_votes, episode_number) %>%
  distinct() %>%
  separate_rows(writer, sep = ';') %>% add_count(writer) %>% arrange(desc(n))

# TODO: FIX WRITER RATINGS WITH THE REST
writer_rating <- writers %>% group_by(writer, n) %>% summarise(avg_rating = mean(imdb_rating))

writers %>% group_by(writer, n) %>%
  summarise(`Average Rating` = round(mean(imdb_rating),2)) %>%
  ungroup() %>% arrange(desc(n)) %>% slice(1:10) %>% rename('No. Episodes' = n)

writers %>% group_by(writer, n) %>%
  summarise(`Average Rating` = round(mean(imdb_rating), 2)) %>%
  ungroup() %>% arrange(desc(`Average Rating`)) %>% slice(1:10) %>% rename('No. Episodes' = n)


writer_rating %>% arrange(desc(avg_rating)) %>%
  mutate(writer = fct_inorder(writer), n2 = ifelse(n>9, n, NA)) %>%
  ggplot(aes(reorder(writer, avg_rating), avg_rating)) +
  geom_segment(aes(yend = 6.5, xend = writer), size = 0.2) +
  geom_point(aes(size = n, color = n > 9), show.legend = FALSE) +
  geom_text(aes(label = n2), size = 3, color = 'white') +
  scale_color_manual(values = c("gray40", ofc)) +
  coord_flip() +
  labs(x = NULL, y = 'Average Rating', title = "Average IMDB Rating by Writers", subtitle = 'size represents number of episodes written, \n color represents > 9 episodes written') +
  theme(panel.grid.major.y = element_blank())


# * Characters------
length(unique(data$character))

characters <- data %>% count(character, sort = TRUE) # reduced to 715
# characters with more than 100 lines
main_char <- characters %>% filter(n > 100) %>%
  filter(!character %in% c("Pete", "Clark")) %>%
  pull("character")
main_char <- data %>% filter(character %in% main_char)
# choose characters that came out in more than 2 season
morethan2season <- main_char %>% select(season, character) %>% distinct() %>%
  count(character) %>% filter(n > 2) %>% pull(character)
main_char <- main_char %>% filter(character %in% morethan2season)
# total lines per episode per season
total_lines <- data %>% count(season, episode)
# number of lines per season
char_lines <- main_char %>% count(season, episode, character, name = "char_lines")
char_lines %>% left_join(total_lines) %>% mutate(prop = char_lines/n) %>% group_by(season, character) %>%
  summarise(avg_prop = mean(prop)) %>%
  ggplot(aes(season, avg_prop)) + geom_area(na.rm = TRUE, fill = ofc, alpha = 0.8, color = darkofc)  +
  geom_point(size = 0.5) +
  facet_rep_wrap(~reorder(character, -avg_prop), repeat.tick.labels = TRUE, ncol = 7) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = '', x = 'Season',
       title = 'Average Percentage of Lines per Season for each Main Character',
       subtitle = 'Characters appearing in more than two seasons and with more than 100 lines in total') +
  theme(panel.grid.minor.x = element_blank())

# percentage of episodes each character showed up in
total_ep <- length(unique(data$episode_name))
main_char %>% distinct(episode_name, character) %>% count(character) %>% mutate(prop_appear = n/total_ep) %>%
  mutate(character = fct_reorder(character, prop_appear)) %>%
  ggplot(aes(prop_appear, character)) + geom_segment(aes(xend = 0, yend = character), size = 0.2) + geom_point(size = 7, color = '#6275A5') + geom_text(aes(label = scales::percent(prop_appear, suffix = "", accuracy = 1)), size = 2.5, color = 'white') + scale_x_continuous(labels = scales::percent) +
  labs(x = 'Percentage of Appearance', y = NULL, title = 'Character Appearances in The Office') +
  theme(panel.grid.major.y = element_blank())


# * Text EDA------
# (did not use these two plots in the paper)
data %>% group_by(episode_name) %>% summarise(text = paste0(text, collapse = " ")) %>%
  mutate(char_count = nchar(text)) %>%
  ggplot(aes(char_count)) + geom_histogram(bins = 20, alpha = 0.8) +
  scale_x_log10() +
  labs(x = 'Number of characters', title = "Distribution of Character Counts")
data %>% group_by(episode_name) %>% summarise(text = paste0(text, collapse = " ")) %>%
  unnest_tokens(word, text) %>%
  count(episode_name) %>%
  ggplot(aes(n)) + geom_histogram(bins = 25) +
  scale_x_log10() +
  labs(x = "Number of Words per Episode", y = "Number of Episodes", title = "Distribution of Word Count throught The Office")
#-------


# Character TF-IDF: which words are specific to characters------
# lemmatization
spacy_initialize(entity = FALSE)
char_parse <- main_char %>% mutate(doc_id = paste0("doc", row_number()),
                                   text = str_replace_all(text, "-", " "),
                                   text = str_replace_all(text, '"', " ")) %>%
  select(doc_id, text) %>%
  spacy_parse() %>%
  filter(!pos %in% c("SCONJ", "PART", "PRON", "DET", "NUM", "SPACE","PUNCT")) %>%
  filter(str_detect(lemma, "'d", negate = TRUE)) %>%
  mutate(lemma = str_to_lower(lemma)) %>%
  anti_join(get_stopwords(), by = c("lemma" = "word"))

char_lemma <- char_parse %>% select(doc_id, lemma) %>% rename(word = lemma)
char_text_tfidf <- main_char %>% mutate(doc_id = paste0("doc", row_number())) %>% left_join(char_lemma)
# remove periods and empty strings
char_text_tfidf <- char_text_tfidf %>% mutate(word = str_replace_all(word, "\\.", "")) %>% filter(word != "") %>% filter(!word %in% letters)
tokeep <- main_char %>% count(character) %>% mutate(perc_line = n/nrow(main_char)) %>%
  filter(n > 600) %>% pull(character)
char_text_tfidf %>%
  filter(character %in% unique(tokeep)) %>%
  mutate(character = as.factor(character)) %>%
  count(character, word, sort = TRUE) %>%
  bind_tf_idf(word, character, n) %>%
  group_by(character) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(character = as.factor(character),
         word = reorder_within(word, tf_idf, character)) %>%
  ggplot(aes(word, tf_idf, fill = character)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~character, scales = 'free', ncol = 4) +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(values = office_pal(length(unique(tokeep)))) +
  theme(strip.text = element_text(size = 9), axis.text.x = element_text(size = rel(0.6), face = 'bold'),
        axis.text.y = element_text(size = 8)) +
  labs(x = '', y = 'tf-idf', title = "Words specific to characters based on tf-idf")


# correlation between number of lines of main characters and imdbrating
# calculate percent of word use across all novels
office_pct <- data %>%
  unnest_tokens(word, text, strip_punct = TRUE, strip_numeric = TRUE) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))
# calculate percent of word use within each season
frequency <- data %>%
  unnest_tokens(word, text, strip_punct = TRUE, strip_numeric = TRUE) %>%
  mutate(word = textstem::lemmatize_words(word)) %>%
  anti_join(stop_words) %>%
  filter(!word %in% my_stopwords) %>%
  filter(str_detect(word, "[:digit:]", negate = TRUE)) %>%
  count(season, word)
frequency <- frequency %>%
  group_by(season) %>%
  summarise(total_season = sum(n)) %>%
  ungroup() %>%
  left_join(frequency) %>%
  mutate(season_words = n/total_season) %>%
  left_join(office_pct) %>%
  arrange(desc(season_words))

ggplot(frequency, aes(x = season_words, y = all_words, color = abs(all_words - season_words))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.5, height = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.01), low = "darkslategray4", high = "gray75") +
  facet_wrap(~ season, ncol = 3) +
  theme(legend.position="none") +
  labs(x = "Word frequency in season", y = "Word frequency in all season",
       title = "Word frequency across entire show vs within each season")



# Sentiment Analysis --------
core_char <- main_char %>% filter(character %in% tokeep, text != "") %>% select(season, episode, episode_name, character, text)

season_char_text_df <- core_char %>% group_by(character, season) %>%
  summarise(text = paste0(text, collapse = " ")) %>% ungroup() %>%
  mutate(element_id = row_number())
season_char_sentence <- season_char_text_df %>% get_sentences(text)
season_char_text <- season_char_sentence %>%
  sentiment(polarity_dt = lexicon::hash_sentiment_sentiword) %>%
  sentiment_by(averaging.function = average_weighted_mixed_sentiment)

char_text <- sentiment_by(season_char_text, by = season_char_text_df$character,
                          averaging.function = average_weighted_mixed_sentiment)

# plot(season_char_text)
season_char_text_df %>% select(character, season, element_id) %>%
  left_join(season_char_text %>% select(element_id, ave_sentiment)) %>%
  left_join(char_text, by = "character") %>%
  ggplot(aes(ave_sentiment.x, reorder(character, -ave_sentiment.y))) +
  geom_path(color = "#6376A8", size = 0.4) +
  geom_point(size = 2, shape = 21, fill = "#6376A8", alpha = 0.5) +
  geom_text(aes(label = ifelse(ave_sentiment.x < -0.25 | ave_sentiment.x > 0, season, "")), vjust = -1, size = 3) +
  geom_point(aes(ave_sentiment, character), data = char_text, size = 3, shape = 21, fill = "#F2AE54") +
  labs(x = "Average Sentiment Score", y = NULL, title = "Sentiment Polarity of each Character throughout The Office")

emotions <- core_char %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!grepl('[0-9]', word)) %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive")) %>%
  group_by(character, sentiment) %>%
  summarize( freq = n()) %>%
  mutate(percent=round(freq/sum(freq)*100)) %>%
  select(-freq) %>%
  spread(sentiment, percent, fill=0) %>%
  ungroup()
## Normalize data
sd_scale <- function(x) {
  (x - mean(x))/sd(x)
}
emotions[,c(2:9)] <- scale(emotions[,c(2:9)])
emotions <- as.data.frame(emotions)
rownames(emotions) <- emotions[,1]
emotions3 <- emotions[,-1]
emotions3 <- as.matrix(emotions3)
## Using a heatmap and clustering to visualize and profile emotion terms expression data

gplots::heatmap.2(
  emotions3,
  dendrogram = "both",
  scale      = "none",
  trace      = "none",
  key        = TRUE,
  col    = colorRampPalette(c("#74CFEB", "#EFD922", "#AE484B")),
  cexCol = 0.85,
  cexRow = 0.8
)

core_char %>% unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  cast_dfm(sentiment, word, n) %>%
  textplot_wordcloud(comparison = TRUE, max_words = 100, color = c("#E8696C", "#30613B"),
                     random_order = FALSE, font = "StaffMeetingPlain",
                     labelcolor = darkofc)

# Clustering -------
text_df <- main_char %>% group_by(character) %>% summarise(text = paste0(text, collapse = " ")) %>% ungroup() %>% mutate(doc_id = character) %>% select(doc_id, text)

dtm <- text_df %>%
  unnest_tokens(word, text, strip_numeric = TRUE) %>%
  mutate(word = SnowballC::wordStem(word)) %>%
  anti_join(stop_words) %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n, weighting = tm::weightTfIdf)

# Remove sparse terms
dtm <- removeSparseTerms(dtm,sparse=0.90)
dtm_tx <- weightTfIdf(dtm)
mat_dtm <- proxy::as.matrix(dtm_tx)

# Term Normalization
normalise_dtm <- function(y) {y/apply(y, MARGIN=1,
                                     FUN = function(k) {sum(k^2)^.5})}
dtm_norm <- normalise_dtm(mat_dtm)
distance <- proxy::dist(dtm_norm, method="cosine")
hc <- hclust(distance, method="ward.D2")

library(factoextra)
fviz_dend(hc, cex = 1,
          k = 10,
          k_colors = office_pal(10),
          color_labels_by_k = TRUE,
          main = "Dendrogram of Main Characters",
          xlab = "Main Characters",  ylab = "Cosine similarity",
          sub = "",
          horiz = FALSE,
          rect = TRUE,
          labels_track_height = 0.1,
          ggtheme = theme_classic(base_family = "StaffMeetingPlain"))

# Regression------
# * Data preparation-------

# Feature engineering:
# season:
season_features <- data %>% distinct(episode_name, season) %>% transmute(episode_name, feature = paste("season:", season), value = 1)
# episode:
episode_features <- data %>% distinct(episode_name, episode) %>% transmute(episode_name, feature = paste("episode:", episode), value = 1)
# director & writer:
director_writer_features <- data %>% distinct(episode_name, director, writer) %>%
  gather(type, value, director, writer) %>%
  separate_rows(value, sep = ";") %>%
  unite(feature, type, value, sep = ": ") %>%
  group_by(feature) %>%
  filter(n() > 1) %>%
  mutate(value = 1) %>%
  ungroup()

# main characters:
character_features <- main_char %>% distinct(episode_name, character, text) %>%
  count(episode_name, character) %>%
  transmute(episode_name, feature = character, value = n)

# each episode is considered as one document
# Pre-processing of Text Data before any feature engineering and modeling:
text_docs <- data %>% group_by(episode_name) %>% summarise(text = paste0(text, collapse = " ")) %>% ungroup()

# tokenize using spacy to lemmatize
spacy_initialize(entity = FALSE)
text_tokens <- text_docs %>% mutate(doc_id = paste0("doc", row_number()),
                                    text = str_replace_all(text, "-", " "),
                                    text = str_replace_all(text, '"', " "),
                                    text = str_replace_all(text, "/", " "),
                                    text = str_replace_all(text, "[^[:graph:]]", " ")) %>%
  select(doc_id, text) %>%
  spacy_parse()

pos_id <- text_tokens %>% distinct(pos)
text_tokens$`pos`[which(text_tokens$`pos` == "PART" & text_tokens$`lemma` == "s*x")] <- "VERB"
text_tokens <- text_tokens %>%
  mutate(lemma = str_to_lower(lemma),
         lemma = str_replace_all(lemma, "[\\.']", "")) %>%
  filter(!pos %in% c("PUNCT", "SPACE", "AUX", "CCONJ", "NUM", "SYM", "PART")) %>%
  filter(!lemma %in% c("-pron-")) %>%
  select(doc_id, lemma) %>%
  group_by(doc_id) %>%
  summarise(text = paste0(lemma, collapse = " ")) %>% ungroup()

text_docs <- text_docs %>% mutate(doc_id = paste0("doc", row_number())) %>% select(-text) %>%
  left_join(text_tokens) %>% select(-doc_id)

# add original text for count features:
original_text <- data %>% group_by(episode_name) %>% summarise(text = paste0(text, collapse = " ")) %>% ungroup() %>% rename(original_text = text) %>% select(episode_name, original_text)

features <- bind_rows(director_writer_features, season_features, episode_features, character_features) %>%
  pivot_wider(names_from = feature, values_from = value, values_fill = list(value = 0)) %>%
  left_join(text_docs) %>%
  left_join(original_text) %>%
  left_join(data %>% distinct(season, episode_name, imdb_rating, total_votes))

# Data splitting 80/20
set.seed(1234)
office_split <- initial_split(features, prop = 4/5, strata = season)
office_train <- training(office_split)
office_test <- testing(office_split)

# cross validation:
set.seed(123)
office_folds <- vfold_cv(office_train, strata = season)

# * Recipe-------
# Pre-processing specification:
office_recipe <- recipe(imdb_rating ~ ., data = office_train) %>%
  step_rm(season) %>%
  update_role(episode_name, new_role = "ID") %>%
  step_textfeature(original_text) %>%
  step_tokenize(text, token = "ngrams", options = list( #unigram + bigram
    n = 2, n_min = 1,
    stopwords = c(stopwords(source = "snowball"), my_stopwords)
  )) %>%
  step_tokenfilter(text, max_tokens = 1000, min_times = 5) %>%
  step_tfidf(text) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_predictors()) %>%
  step_normalize(all_predictors())

# * LASSO ------
# very sparse data
library(hardhat)
sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

# Model specification:
tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

# Combine preprocessing and model spec into tunable workflow:
tune_wf <- workflow() %>%
  add_recipe(office_recipe, blueprint = sparse_bp) %>%
  add_model(tune_spec)
tune_wf

doParallel::registerDoParallel()
set.seed(2020)
# tune hyperparameter using cross val
office_rs <- tune_grid(tune_wf,
                       office_folds,
                       grid = 20,
                       metrics = metric_set(rmse, mae, mape)
)

office_rs %>% collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err, ymax = mean + std_err), alpha = 0.5, width = 0.1) +
  geom_line(size = 1.5) + facet_wrap(~.metric, scales = "free", nrow = 3) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = "Lasso model performance across regularization penalties",
       x = "penalty (log)") +
  scale_color_manual(values = office_pal(3))

# best model:
lowest_rmse <- office_rs %>% select_best("rmse")
office_rs %>% show_best("rmse")
train_valid <- office_rs %>% show_best("rmse", n = 1) %>%
  select(mean, std_err) %>% mutate(model = "LASSO")

# Finalize workflow:
final_wf <- finalize_workflow(
  tune_wf, lowest_rmse
)
final_wf

# coefficients
lasso_coef <- final_wf %>% fit(office_train) %>% pull_workflow_fit() %>% tidy()
lasso_coef %>% filter(estimate != 0) # went from 1,121 variables to 68

# Fit our full training data and evaluate on test data
final_lasso <- last_fit(final_wf, office_split)
collect_metrics(final_lasso)

# Important variables:
office_imp <- pull_workflow_fit(final_lasso$.workflow[[1]]) %>%
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Variable = str_remove_all(Variable, "tfidf_text_"),
    Variable = str_remove_all(Variable, "textfeature_original_text_"),
    Variable = fct_reorder(Variable, Importance)
  )

office_imp %>%
  group_by(Sign) %>%
  top_n(10, abs(Importance)) %>%
  ungroup() %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c(beet, ofc)) +
  labs(y = NULL, title = "Variable Importance by LASSO")


office_imp %>%
  filter(str_detect(Variable, "episode|writer|director|season|^[A-Z]|total_votes")) %>%
  group_by(Sign) %>%
  top_n(7, abs(Importance)) %>%
  ungroup() %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c(beet, ofc)) +
  labs(y = NULL, title = "Variable Importance by LASSO", subtitle = "non-text features")


final_lasso %>%
  collect_predictions() %>%
  ggplot(aes(imdb_rating, .pred)) +
  geom_point(alpha = 0.8) +
  labs(x = 'Observed IMDB Rating', y = 'Predicted IMDB Rating',
       title = "Predicted vs. observed IMDB ratings for the test data set of \n The Office transcripts using Lasso Regression") +
  theme(plot.title = element_text(size = 12)) +
  coord_obs_pred()

# * SVR-----

# Model specification:
tune_spec_svm <- svm_poly(cost = tune(), degree = tune(), scale = tune(),
                          margin = tune()) %>%
  set_mode("regression") %>%
  set_engine("kernlab")
tune_spec_svm

# Combine preprocessing and model spec into tunable workflow:
tune_svm_wf <- workflow() %>%
  add_recipe(office_recipe) %>%
  add_model(tune_spec_svm)
tune_svm_wf

doParallel::registerDoParallel() # 12:54-1:09
set.seed(2020)
office_rs_svm <- tune_grid(tune_svm_wf,
                           office_folds,
                           grid = 10,
                           metrics = metric_set(rmse, mae, mape)
)

office_rs_svm %>% collect_metrics() %>%
  mutate(cost = log2(cost), scale_factor = log10(scale_factor)) %>%
  rename(`Cost (log-2)` = cost, `Polynomial Degree` = degree,
         `Scale Factor (log-10)` = scale_factor, `Insensitivity Margin` = margin) %>%
  pivot_longer(cols = 1:4) %>%
  ggplot(aes(value, mean, color = name)) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err),
                alpha = 0.5, width = 0, show.legend = FALSE) +
  facet_grid(.metric ~ name, scales = "free") +
  theme(panel.border = element_rect(fill = NA)) +
  labs(
    title = "Support vector regression performance across different hyperparameters",
    x = NULL
  ) +
  scale_color_manual(values = office_pal(4))


# best model:
lowest_rmse_svm <- office_rs_svm %>% select_best("rmse")
office_rs_svm %>% show_best("rmse", n = 1)
train_valid <- office_rs_svm %>% show_best("rmse", n = 1) %>% select(mean, std_err) %>%
  mutate(model = "SVR") %>% bind_rows(train_valid)

# Finalize workflow:
final_svm_wf <- finalize_workflow(
  tune_svm_wf, lowest_rmse_svm
)
final_svm_wf

# Fit our full training data and evaluate on test data
final_svm <- last_fit(final_svm_wf, office_split)
collect_metrics(final_svm)

# * XBGoost------
# Model specification:
office_recipe_xgb <- recipe(imdb_rating ~ ., data = office_train) %>%
  step_rm(season) %>%
  update_role(episode_name, new_role = "ID") %>%
  step_textfeature(original_text) %>%
  step_tokenize(text, token = "ngrams", options = list( #unigram + bigram
    n = 2, n_min = 1,
    stopwords = c(stopwords(source = "snowball"), my_stopwords)
  )) %>%
  step_tokenfilter(text, max_tokens = 1000, min_times = 5) %>%
  step_tfidf(text) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_predictors())


tune_spec_xgb <- boost_tree(trees = tune(), mtry = tune(), tree_depth = tune(),
                            learn_rate = tune()) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

# Combine preprocessing and model spec into tunable workflow:
tune_xgb_wf <- workflow() %>%
  add_recipe(office_recipe_xgb) %>%
  add_model(tune_spec_xgb)
tune_xgb_wf

# cross validation:
doParallel::registerDoParallel() # 3:55
set.seed(2020)
office_rs_xgb <- tune_grid(tune_xgb_wf,
                           office_folds,
                           grid = 20,
                           metrics = metric_set(rmse, mae, mape)
)


office_rs_xgb %>% collect_metrics() %>%
  mutate(learn_rate = log10(learn_rate)) %>%
  rename(`# Randomly Selected Predictors` = mtry,
         `# Trees` = trees,
         `Learning Rate (log-10)` = learn_rate,
         `Tree Depth` = tree_depth) %>%
  pivot_longer(cols = 1:4) %>%
  ggplot(aes(value, mean, color = .metric)) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err),
                alpha = 0.5, show.legend = FALSE) +
  facet_grid(.metric ~ name, scales = "free") +
  theme(panel.border = element_rect(fill = NA)) +
  labs(title = "XGBoost Performance across Different Hyperparameters",
       x = NULL, y = NULL) +
  scale_color_manual(values = office_pal(3))
# errorbars are very small

# best model:
lowest_rmse_xgb <- office_rs_xgb %>% select_best("rmse")
office_rs_xgb %>% show_best("rmse")
train_valid <- office_rs_xgb %>% show_best("rmse", n = 1) %>% select(mean, std_err) %>%
  mutate(model = "XGBoost") %>% bind_rows(train_valid)

# Finalize workflow:
final_xgb_wf <- finalize_workflow(
  tune_xgb_wf, lowest_rmse_xgb
)
final_xgb_wf

xgb_vip <- final_xgb_wf %>%
  fit(data = office_train) %>%
  pull_workflow_fit() %>%
  vi()
xgb_vip %>%
  mutate(Variable = str_remove_all(Variable, "tfidf_text_"),
         Variable = str_remove_all(Variable, "textfeature_original_text_")) %>%
  vip(aesthetics = list(fill = ofc, alpha = 0.8, color = ofc)) +
  labs(title = "Top 10 Important Variables by XGBoost",
       subtitle = "Lowercase words are spoken text")

xgb_vip %>%
  filter(str_detect(Variable, "tfidf_text_", negate = TRUE),
         str_detect(Variable, "textfeature_original_text_", negate = TRUE)) %>%
  vip(aesthetics = list(fill = ofc, alpha = 0.8, color = ofc)) +
  labs(title = "Top 10 Important Variables by XGBoost",
       subtitle = "Excluding text variables")

# Fit our full training data and evaluate on test data
final_xgb <- last_fit(final_xgb_wf, office_split)
collect_metrics(final_xgb)



final_xgb %>%
  collect_predictions() %>%
  ggplot(aes(imdb_rating, .pred)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.8) +
  labs(x = 'Observed IMDB Rating', y = 'Predicted IMDB Rating', title = "Predicted vs. observed IMDB ratings for the test data set of \n The Office transcripts using xgb Regression") +
  theme(plot.title = element_text(size = 12))

# * KNN------
# Model specification:
tune_spec_knn <- nearest_neighbor(neighbors = tune(), dist_power = tune()) %>%
  set_mode("regression") %>%
  set_engine("kknn")

# Combine preprocessing and model spec into tunable workflow:
tune_knn_wf <- workflow() %>%
  add_recipe(office_recipe) %>%
  add_model(tune_spec_knn)
tune_knn_wf

knn_param <- tune_knn_wf %>%
  parameters() %>%
  Matrix::update(
    neighbors = neighbors(c(3, 129))
  )
ctrl <- control_bayes(verbose = TRUE)

# cross validation:
doParallel::registerDoParallel()
set.seed(2020)
# bayesian optimization
office_rs_knn <- tune_bayes(tune_knn_wf,
                            office_folds,
                            initial = 5,
                            iter = 30,
                            param_info = knn_param,
                            control = ctrl,
                            metrics = metric_set(rmse, mae, mape)
)

office_rs_knn %>% collect_metrics() %>%
  rename(`# Nearest Neighbors` = neighbors, `Minkowski Distance Order` = dist_power) %>%
  pivot_longer(cols = c(1, 2)) %>%
  ggplot(aes(value, mean, color = .metric)) + geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), width = 0, show.legend = FALSE) +
  facet_grid(.metric~name, scales = "free") +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = NULL, y = NULL, title = "kNN Performance across Different Hyperparameters") +
  scale_color_manual(values = office_pal(3))

office_rs_knn %>% show_best("rmse")


# best model:
lowest_rmse_knn <- office_rs_knn %>% select_best("rmse")
train_valid <- office_rs_knn %>% show_best("rmse", n = 1) %>% select(mean, std_err) %>%
  mutate(model = "kNN") %>% bind_rows(train_valid)

# Finalize workflow:
final_knn_wf <- finalize_workflow(
  tune_knn_wf, lowest_rmse_knn
)

# Fit our full training data and evaluate on test data
final_knn <- last_fit(final_knn_wf, office_split)
collect_metrics(final_knn)


final_results <- list(final_lasso, final_svm, final_xgb, final_knn) %>%
  map(collect_metrics) %>% bind_rows() %>%
  mutate(model = rep(c("LASSO", "SVR", "XGBoost", "kNN"), each = 2)) %>%
  select(model, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

# * Neural net---------
# Recipe for NN
simple_recipe <- function(dataset){
  recipe(imdb_rating ~ ., data = dataset) %>%
    step_rm(season) %>%
    step_rm(episode_name) %>%
    step_textfeature(original_text) %>%
    step_tokenize(text, token = "ngrams", options = list( #unigram + bigram
      n = 2, n_min = 1,
      stopwords = c(stopwords(source = "snowball"), my_stopwords)
    )) %>%
    step_tokenfilter(text, max_tokens = 1000, min_times = 5) %>%
    step_tfidf(text) %>%
    step_zv(all_predictors()) %>%
    step_corr(all_predictors()) %>%
    step_range(all_predictors())
}

library(neuralnet)
# prep and bake training data and testing data
office_nn_train <- prep(simple_recipe(office_train), training = office_train)
office_nn_juice <- juice(office_nn_train) %>% janitor::clean_names()
feat = names(office_nn_juice)
form = as.formula(paste("imdb_rating~", paste(feat[!feat %in% "imdb_rating"], collapse = "+"), collapse = "+"))
office_nn_test <- bake(office_nn_train, new_data = office_test) %>%
  janitor::clean_names()

# cross validation
nn_cv <- function(hidden_units, split, id){
  analysis_set <- analysis(split)
  analysis_prep <- prep(simple_recipe(analysis_set), training = analysis_set)
  analysis_processed <- juice(analysis_prep) %>%
    janitor::clean_names()
  feat = names(analysis_processed)
  form = as.formula(paste("imdb_rating~", paste(feat[!feat %in% "imdb_rating"],
                                                collapse = "+"), collapse = "+"))
  model <- neuralnet(form, analysis_processed, hidden = hidden_units,
                     linear.output = TRUE,
                     err.fct = "sse")
  assessment_set <- assessment(split)
  assessment_processed <- bake(analysis_prep, new_data = assessment_set) %>%
    janitor::clean_names()
  tibble::tibble("id" = id,
                 "truth" = assessment_processed$imdb_rating,
                 "prediction" = unlist(predict(model, newdata = assessment_processed)))
}

# cross validation given hidden units
results_example <- map2_df(.x = office_folds$splits,
                           .y = office_folds$id,
                           ~nn_cv(c(100, 50, 50, 25), split = .x, id = .y))

train_valid <- results_example %>%
  group_by(id) %>%
  summarise(rmse = sqrt(mean((truth-prediction)^2))) %>% ungroup() %>%
  summarise(mean = mean(rmse), std_err = sd(rmse)) %>%
  mutate(model = "Neural Net") %>%
  bind_rows(train_valid)

# evaluate on test data
office_nn_mod <- neuralnet(form, office_nn_juice, c(100,50,50,25),
                           linear.output = TRUE,
                           err.fct = "sse")
final_nn_pred <- tibble::tibble("truth" = office_nn_test$imdb_rating,
                                "prediction" = unlist(predict(office_nn_mod,
                                                              newdata = office_nn_test)))

final_nn_pred %>%
  mutate(errors = (truth - prediction)^2) %>%
  ggplot(aes(truth, prediction)) + geom_point() +
  geom_abline(lty = 2) +
  labs(title = "Predicted vs. Actual")

final_nn_pred %>%
  mutate(errors = (truth - prediction)) %>%
  ggplot(aes(prediction, errors)) + geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Fitted vs. Residuals")


final_nn_pred %>%
  summarise(rmse = sqrt(mean((truth - prediction)^2))) %>%
  mutate(model = "Neural Net") %>%
  select(model, everything()) %>%
  bind_rows(final_results)

# * Training/Validation--------
train_valid
train_valid %>%
  arrange(mean) %>%
  mutate(model = as_factor(model)) %>%
  ggplot(aes(model, mean, color = model)) +
  geom_point(show.legend = FALSE, size = 3) +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), size = 1,
                width = 0.5,
                show.legend = FALSE) +
  labs(x = NULL, y = "rmse", title = "Model performance on training data") +
  scale_color_manual(values = office_pal(5))


final_results

