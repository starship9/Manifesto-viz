library(pdftools)
library(tidyverse)
library(tidytext)

bjpRaw <- pdf_text("../data/bjp.pdf")
congressRaw <- pdf_text("../data/congress.pdf")

filterWords <-
  c(
    "ÿ",
    "impo",
    "ance",
    "oppo",
    "unities",
    "di",
    "erent",
    "unde",
    "ake",
    "ts",
    "bene",
    "se",
    "ing",
    "electri",
    "cation",
    "commi",
    "ed",
    "diately",
    "ve",
    "pa",
    "er",
    "icipation"
  )
bjp <- bjpRaw %>%
  tbl_df() %>%
  unnest_tokens(word, value) %>%
  mutate(party = "bjp",
         section = row_number() %/% 10) %>%
  filter(!word %in% filterWords,
         str_detect(word, "[a-zA-Z]"))

congress <- congressRaw %>%
  tbl_df() %>%
  unnest_tokens(word, value) %>%
  mutate(party = "congress",
         section = row_number() %/% 10) %>% 
  filter(!word %in% filterWords)


df <- rbind(bjp, congress)

dfProcessed <- df %>%
  filter(is.na(as.numeric(word)),!str_detect(word, "ÿ")) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "[0-9]"),
         str_detect(word,"[a-zA-Z]")) %>%
  filter(!word %in% filterWords)

theme_set(theme_light())


wordCount <- function(num) {
  dfProcessed %>%
    count(word, party, sort = TRUE) %>%
    filter(!word %in% filterWords) %>% 
    group_by(party) %>%
    arrange(desc(n)) %>% 
    top_n(num) %>%
    ungroup() %>%
    mutate(word = fct_reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(aes(fill = party)) +
    geom_label(aes(label = n), alpha = 0.2, size = 3) +
    coord_flip() +
    facet_wrap( ~ party, scales = "free_y") +
    labs(
      title = paste(num,"most common words per party"),
      y = "Count",
      x = "Word",
      caption = "Sources: https://www.bjp.org/en/manifesto2019\nhttps://manifesto.inc.in/pdf/english.pdf"
    )
}

library(scales)

tfidfWords <- function(partyInput) {
  dfProcessed %>%
    filter(party == partyInput) %>%
    filter(!word %in% filterWords) %>% 
    count(word, section) %>%
    bind_tf_idf(word, section, n) %>%
    arrange(desc(tf_idf)) %>%
    head(30) %>%
    mutate(word = fct_reorder(word, tf_idf)) %>%
    ggplot(aes(word, tf_idf)) +
    geom_col(aes(fill = tf_idf), color = "black") +
    coord_flip() +
    scale_fill_distiller(direction = 1, palette = "RdYlBu") +
    labs(
      title = paste("Words more specific in", partyInput, "'s manifesto"),
      subtitle = "Words weighted by tf-idf",
      caption = "Sources: https://www.bjp.org/en/manifesto2019\nhttps://manifesto.inc.in/pdf/english.pdf"
    )
}

library(igraph)
library(ggraph)
library(widyr)

bjpBG <- bjpRaw %>%
  tbl_df() %>%
  unnest_tokens(word, value, token = "ngrams", n = 2) %>%
  mutate(party = "bjp") %>% 
  filter(str_detect(word,"[a-zA-Z]"))

congressBG <- congressRaw %>%
  tbl_df() %>%
  unnest_tokens(word, value, token = "ngrams", n = 2) %>%
  mutate(party = "congress")

dfBG <- rbind(bjpBG, congressBG)

dfBGProcessed <- dfBG %>%
  filter(!str_detect(word, "ÿ")) %>%
  separate(word, into = c("word1", "word2"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word &
      !word2 %in% stop_words$word &
      is.na(as.numeric(word1)) & is.na(as.numeric(word2))
  ) %>%
  filter(!word1 %in% filterWords &
         !word2 %in% filterWords) %>%
  filter(str_detect(word1,"[a-zA-Z]") &
         str_detect(word2,"[a-zA-Z]")) %>% 
  count(word1, word2, party, sort = TRUE)


bgPlotFunc <- function(partyInput) {
  set.seed(100)
  
  dfBGProcessed %>%
    filter(party == partyInput) %>%
    filter(n > 5) %>%
    graph_from_data_frame() %>%
    ggraph() +
    geom_edge_link(aes(edge_colour = n, edge_width = n)) +
    geom_node_point(alpha = 0.8, size = 3) +
    geom_node_text(aes(label = name), repel = TRUE) +
    scale_edge_color_distiller(palette = "RdPu", direction = 1) +
    guides(alpha = FALSE, edge_alpha = FALSE) +
    labs(title = paste("Frequent word pairings in", partyInput, "'s manifesto"),
         subtitle = "Darker, wider colors are more frequent",
         caption = "Sources: https://www.bjp.org/en/manifesto2019\nhttps://manifesto.inc.in/pdf/english.pdf") +
    theme_void()
}

corWords <- function(partyInput) {
  set.seed(100)
  dfProcessed %>%
    filter(party == partyInput) %>%
    filter(!word %in% filterWords) %>%
    group_by(word) %>%
    filter(n() > 20) %>%
    pairwise_cor(word, section, sort = TRUE, upper = FALSE) %>%
    filter(!is.na(correlation),
           correlation > 0.04,
           !item1 %in% filterWords &
           !item2 %in% filterWords) %>%
    graph_from_data_frame() %>%
    ggraph() +
    geom_edge_link(aes(edge_colour = correlation, edge_width = correlation)) +
    geom_node_point(size = 3, alpha = 0.8) +
    geom_node_text(aes(label = name), repel = TRUE) +
    scale_edge_color_distiller(palette = "RdPu", direction = 1) +
    labs(title = paste("Correlated words in",partyInput,"'s manifesto"),
         subtitle = "Darker, wider colors are more correlated",
         caption = "Sources: https://www.bjp.org/en/manifesto2019\nhttps://manifesto.inc.in/pdf/english.pdf") +
    theme_void()
}
library(stm)

topicWords <- function(partyInput) {
  dfSparse <- dfProcessed %>%
    filter(party == partyInput) %>%
    count(section, word, sort = TRUE) %>%
    cast_sparse(section, word, n)
  
  topicModel <-
    stm(dfSparse,
        K = 3,
        verbose = FALSE,
        init.type = "LDA")
  
  
  tdBeta <- tidy(topicModel)
  
  tdBeta %>%
    group_by(topic) %>%
    arrange(desc(beta)) %>%
    top_n(20, beta) %>%
    ungroup() %>%
    mutate(topic = paste("Topic", topic),
           term = fct_reorder(term, beta)) %>%
    ggplot(aes(term, beta)) +
    geom_col(alpha = 0.8, aes(fill = topic)) +
    facet_wrap( ~ topic, scales = "free") +
    coord_flip() +
    labs(title = paste("Possible topics for ", partyInput),
         subtitle = "Topic modelling via LDA used",
         caption = "Sources: https://www.bjp.org/en/manifesto2019\nhttps://manifesto.inc.in/pdf/english.pdf")
  
  
  
}

freqPercent <- dfProcessed %>%
  group_by(party) %>%
  count(word, sort = TRUE) %>%
  left_join(dfProcessed %>%
              group_by(party) %>%
              summarise(total = n())) %>%
  mutate(freq = n / total)


freqPlot <- function() {
  
  freqPercent %>%
    select(party, word, freq) %>%
    spread(party, freq) %>%
    arrange(bjp, congress) %>%
    ggplot(aes(bjp, congress)) +
    geom_jitter(
      alpha = 0.2,
      size = 4,
      width = 0.25,
      height = 0.25
    ) +
    geom_text(aes(label = word),
              size = 4,
              check_overlap = TRUE,
              vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    geom_abline(color = "red",
                alpha = 0.8,
                linetype = "dashed") +
    labs(title = "Word frequences in the manifestos by BJP and Congress",
         subtitle = "Words above and below the red line are more frequently\npresent in Congress and BJP respectively",
         caption = "Sources: https://www.bjp.org/en/manifesto2019\nhttps://manifesto.inc.in/pdf/english.pdf")
  
  
}
