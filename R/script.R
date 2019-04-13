library(pdftools)
library(tidyverse)
library(tidytext)

bjpRaw <- pdf_text("data/bjp.pdf")
congressRaw <- pdf_text("data/congress.pdf")

bjp <- bjpRaw %>% 
  tbl_df() %>% 
  unnest_tokens(word, value) %>% 
  mutate(party = "bjp")

congress <- congressRaw %>% 
  tbl_df() %>% 
  unnest_tokens(word, value) %>% 
  mutate(party = "congress")


df <- rbind(bjp, congress)

dfProcessed <- df %>% 
  filter(is.na(as.numeric(word)), !str_detect(word,"ÿ")) %>% 
  anti_join(stop_words)

theme_set(theme_light())

dfProcessed %>% 
  count(word, party, sort = TRUE) %>% 
  group_by(party) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col(aes(fill = party)) +
  geom_label(aes(label = n), alpha = 0.2, size = 3) +
  coord_flip() +
  facet_wrap(~party, scales = "free_y") +
  labs(title = "Most common words per party",
       y = "Count",
       x = "Word",
       caption = "Sources: https://www.bjp.org/en/manifesto2019\nhttps://manifesto.inc.in/pdf/english.pdf")

library(scales)
dfProcessed %>% 
  count(word, party) %>% 
  bind_tf_idf(word, party, n) %>% 
  split(.$party) %>%
  map(~tbl_df(data = .)) %>% 
  map(~mutate(.data = ., word = fct_reorder(word, tf_idf))) %>% 
  map(~arrange(.data = ., desc(tf_idf))) %>% 
  map(~head(20, x = .)) %>% 
  map(~ggplot(data = ., aes(word, tf_idf)) +
        geom_col(aes(fill = tf_idf), color = "black") +
        coord_flip() +
        scale_fill_distiller(direction = 1, palette = "RdYlBu") +
        labs(title = paste("Words more specific in",.$party, "'s manifesto"),
             subtitle = "Words weighted by tf-idf",
             caption = "Sources: https://www.bjp.org/en/manifesto2019\nhttps://manifesto.inc.in/pdf/english.pdf"))
