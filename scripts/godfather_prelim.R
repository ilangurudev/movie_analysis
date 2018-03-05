pacman::p_load(tidyverse, tidytext, pdftools, feather, rebus, wordcloud, ggrepel, viridisLite)
devtools::install_github("r-lib/svglite")

book <- pdf_text("data/godafather_book.pdf")
movie <- pdf_text("data/godfather_screenplay.pdf")

# pdf_attachments("data/godafather_book.pdf")

book[2] <- book[2] %>% str_replace("Behind every great fortune there is a crime – Balzac", "")
  
book <- tibble(text = book[2:length(book)])

book_sentences <-
  book %>% 
  mutate(chapter = text %>% str_detect("Chapter" %R% SPACE %R% one_or_more(DIGIT)),
         chapter = cumsum(chapter),
         page = row_number(),
         text = text %>% 
                str_replace(literal('“The Godfather” By Mario Puzo') %R%
                            zero_or_more(SPACE) %R%
                            one_or_more(DIGIT) %R%
                            NEWLINE , "") %>% 
                str_trim(),
         text = text %>%
                 str_replace("Book" %R%
                             SPACE %R%
                             one_or_more(ALPHA) %R%
                             NEWLINE %R%
                             one_or_more(SPACE) %R%
                             "Chapter " %R% one_or_more(DIGIT), "")) %>% 
  split(.$page) %>% 
  map_df(function(df_page){
    sentence <- df_page$text %>% 
                str_split(or(DOT, QUESTION, "!"), simplify = T) %>% 
                as.vector() %>% 
                str_trim() %>% 
                str_replace_all(NEWLINE, " ")
    tibble(text = sentence, chapter = df_page$chapter, page = df_page$page)
  }) %>% 
  filter(text != "") %>% 
  mutate(sentence = row_number())

book_sentences %>% 
  write_feather("data/godfather_book_sentences.feather")

useful_words <- 
  book_sentences %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word")

useful_words %>% write_feather("data/godfather_book_words.feather")

# sentiments <- 
  useful_words %>%
  count(word, sort = T) %>%
  inner_join(get_sentiments("bing")) %>% 
  group_by(sentiment) %>%
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  mutate(n = ifelse(sentiment == "negative", n*-1, n),
         word = word %>% fct_reorder(n) ) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal()

x <- 
  useful_words %>%
  count(word, sentiment, sort = 100) 


set.seed(1)
# cairo_pdf("plots/wordcloud.pdf")
svglite("plots/wordcloud.svg")
wordcloud(x$word, x$n, max.words = 200, colors = rev(viridis(50)))
dev.off()


x %>% 
  top_n(50) %>% 
  ggplot(aes(x = 1, y = 1, size = n, label = word, col = -n))   +
  geom_text_repel(segment.size = 0, force = 10) +
  scale_size(range = c(5, 10), guide = FALSE) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  scale_color_gradient(low = "darkgreen", high = "darkred") +
  labs(x = '', y = '') +
  theme_classic()


# 
# %R%
#   zero_or_more(SPACE) %R%
#   literal("Book") %R% 
#   SPACE %R% 
#   one_or_more(ALPHA) %R%
#   NEWLINE %R%
#   zero_or_more(SPACE) %R%
#   literal("Chapter") %R% 
#   one_or_more(DIGIT) %R%
#   one_or_more(DIGIT)