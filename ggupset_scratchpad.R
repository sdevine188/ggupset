library(tidyverse)
library(ggupset)

# from package tutorial
# https://github.com/const-ae/ggupset

# note tidy_movies has 4977 titles, but 5000 title/year combos (some duplicate or re-released titles)  
# note each title/year has 10 reviews each, with a duplicate row for each unique review
tidy_movies
tidy_movies %>% distinct(title) %>% nrow() # 4977
tidy_movies %>% distinct(title, year) %>% nrow() # 5000
tidy_movies %>% nrow() # 50000
tidy_movies %>% add_count(title) %>% filter(n == 10) %>% nrow() # 49540
tidy_movies %>% add_count(title) %>% filter(n > 10) %>% arrange(title, year) %>% print(n = 20)
tidy_movies %>% add_count(title, year) %>% filter(n == 10) %>% nrow() # 50000
tidy_movies %>% filter(title == "Not as a Stranger")
tidy_movies %>% distinct(title, year, length, .keep_all = TRUE) %>% nrow() # 5000

# note that the variable used by ggupset to describe set membership (Genres) is a nested list column
# note that list columns with zero items are removed when unnested unless keep_empty arg is set to TRUE
tidy_movies %>% distinct(title, year, length, .keep_all = TRUE) %>% 
        filter(title %in% c("Not as a Stranger", "Pang shen feng", "Mind Benders, The"))
tidy_movies %>% distinct(title, year, length, .keep_all = TRUE) %>% 
        filter(title %in% c("Not as a Stranger", "Pang shen feng", "Mind Benders, The")) %>% 
        unnest(Genres, keep_empty = TRUE) %>% print(n = nrow(.))

# note that when data is fed into ggupset, it must be at one-row-per-record
tidy_movies %>%
        distinct(title, year, length, .keep_all = TRUE) %>%
        filter(title == "Pantry Panic")

# create upset plot
tidy_movies %>%
        distinct(title, year, length, .keep_all = TRUE) %>%
        ggplot(data = ., aes(x = Genres)) +
        geom_bar() +
        scale_x_upset(n_intersections = 20)


#//////////////////////


# the list column can be unnnested to get a longer dataset
tidy_movies %>%
        distinct(title, year, length, .keep_all = TRUE) %>% unnest(Genres, keep_empty = TRUE)

# and longer data can be nested into a list column        
tidy_movies %>%
        distinct(title, year, length, .keep_all = TRUE) %>% 
        unnest(Genres, keep_empty = TRUE) %>%
        group_by(title, year, length) %>% mutate(distinct_genre = unique(Genres), distinct_genre_list = list(unique(Genres)))
# note that this mutate/list/unique method of creating list columns results in  
# title/year combos with NA genres will show up as having a list column of length 1, even though it's NA value
# this is different than the original dataset which had list columns of zero length when containing only NA value
tidy_movies %>%
        distinct(title, year, length, .keep_all = TRUE) %>% 
        unnest(Genres, keep_empty = TRUE) %>%
        group_by(title, year, length) %>% mutate(distinct_genre = unique(Genres), distinct_genre_list = list(unique(Genres))) %>%
        filter(title == "Mind Benders, The") %>% unnest(distinct_genre_list)

# you can also collapse the Genre values into a single string, then call distinct to get one-row-per-record view
tidy_movies %>%
        distinct(title, year, length, .keep_all = TRUE) %>% unnest(Genres, keep_empty = TRUE) %>%
        group_by(title, year, length) %>% mutate(distinct_genre_list = list(unique(Genres))) %>%
        ungroup() %>% 
        select(title, year, length, distinct_genre_list) %>%
        distinct(title, year, length, .keep_all = TRUE) %>% nrow() # 5000
tidy_movies %>%
        distinct(title, year, length, .keep_all = TRUE) %>% unnest(Genres, keep_empty = TRUE) %>%
        group_by(title, year, length) %>% mutate(distinct_genre_list = list(unique(Genres))) %>%
        ungroup() %>% 
        select(title, year, length, distinct_genre_list) %>%
        distinct(title, year, length, .keep_all = TRUE) %>%
        filter(title == "Pantry Panic")

# plot
tidy_movies %>%
        distinct(title, year, length, .keep_all = TRUE) %>% unnest(Genres, keep_empty = TRUE) %>%
        group_by(title, year, length) %>% mutate(distinct_genre_list = list(unique(Genres))) %>%
        ungroup() %>% 
        select(title, year, length, distinct_genre_list) %>%
        distinct(title, year, length, .keep_all = TRUE) %>%
        ggplot(data = ., aes(x = distinct_genre_list)) +
                geom_bar() +
                scale_x_upset(n_intersections = 20)








