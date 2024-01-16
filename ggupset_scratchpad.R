library(tidyverse)
library(ggupset)

# https://cran.r-project.org/web/packages/ggupset/readme/README.html


# setwd()
setwd("C:/Users/sjdevine/OneDrive - USCIS/R/ggupset")


#//////////////////////////////////////////////////////////////////////////////////////////////


# example list column using starwars
starwars
starwars_names <- starwars %>% select(gender, name) %>% 
        group_by(gender) %>%
        summarize(name = list(name))
starwars_names

# nest tibbles
df <- tibble(x = c(1, 1, 1, 2, 2, 3), y = 1:6, z = 6:1)
df
df %>% nest(data = c(y, z))

# unnest tibbles
nested_tbl <- tibble(x = 1:3,
                     y = list(NULL,
                              tibble(a = 1, b = 2),
                              tibble(a = 1:3, b = 3:1, c = 4)))
nested_tbl
nested_tbl %>% unnest(y)
nested_tbl %>% unnest(y, keep_empty = TRUE)


#//////////////////////////////////////////////////////////////////////////////////////////////


tidy_movies

# note it uses a list-column 
tidy_movies %>%
        distinct(title, year, length, .keep_all = TRUE) %>%
        ggplot(aes(x = Genres)) +
        geom_bar() +
        scale_x_upset(n_intersections = 20)

# create the same upset plot without using list-columns, which enables easier wrangling, checking, and visualization
chart_data <- tidy_movies %>% 
        distinct(title, year, Genres) %>% 
        unnest(Genres) %>% 
        add_count(title, year, name = "genre_count_for_movie") %>%
        arrange(desc(genre_count_for_movie)) %>%
        group_by(title, year, genre_count_for_movie) %>%
        summarize(genre_collapsed = str_c(Genres, collapse = "_x_")) %>%
        ungroup() %>%
        arrange(desc(genre_count_for_movie)) %>%
        add_count(genre_collapsed, name = "genre_collapsed_count") %>%
        arrange(desc(genre_collapsed_count)) %>%
        distinct(genre_collapsed, genre_collapsed_count) %>%
        arrange(desc(genre_collapsed_count)) %>%
        slice(1:10) %>%
        mutate(genre_collapsed = fct_reorder(.f = genre_collapsed, .x = genre_collapsed_count, .desc = TRUE),
               color = "#2474B6",
               color_bin = genre_collapsed)

chart_data

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# plot
plot <- chart_data %>%
        ggplot(data = ., mapping = aes(x = genre_collapsed, y = genre_collapsed_count, fill = color_bin)) +
        geom_col() +
        geom_text(mapping = aes(label = genre_collapsed_count), nudge_y = 100) +
        scale_fill_manual(values = chart_data_color_list, guide = "none") +
        axis_combmatrix(sep = "_x_") +
        theme_combmatrix(
                combmatrix.label.make_space = TRUE,
                combmatrix.label.width = NULL,
                combmatrix.label.height = NULL,
                combmatrix.label.extra_spacing = 3,
                combmatrix.label.total_extra_spacing = unit(10, "pt"),
                combmatrix.label.text = NULL,
                combmatrix.panel.margin = unit(c(1.5, 1.5), "pt"),
                combmatrix.panel.striped_background = TRUE,
                combmatrix.panel.striped_background.color.one = "white",
                combmatrix.panel.striped_background.color.two = "#F7F7F7",
                combmatrix.panel.point.size = 3,
                combmatrix.panel.line.size = 1.2,
                combmatrix.panel.point.color.fill = "#000000",
                combmatrix.panel.point.color.empty = "#E0E0E0"
        ) +
        theme(axis.text.x = element_text(family = "Calibri", face = "plain", size = 11, color = "#333333",
                                         margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 45, hjust = 1),
              axis.text.y = element_text(family = "Calibri", face = "plain", size = 11, color = "#333333",
                                         margin = margin(t = 0, r = 0, b = 0, l = 0)))

plot <- plot + plot_layout(widths = c(1), heights = unit(c(3), c('cm')))

# inspect
plot


#//////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(plot)
dev.off()

# add emf to word doc
read_docx() %>%
        body_add_img(src = filename, width = 6, height = 6) %>%
        print(target = "plot.docx")


#//////////////////////////////////////////////////////////////////////////////////////////////


# but can equivalently also use collapsed strings instead of list columns
# note that scale_x_upset is a wrapper for scale_x_mergelist() and axis_combmatrix()
# but scale_x_upset also orders the categories by freq or degree
tidy_movies %>%
        distinct(title, year, length, .keep_all=TRUE) %>%
        ggplot(aes(x = Genres)) +
        geom_bar() +
        scale_x_mergelist(sep = "-") +
        theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))

tidy_movies %>%
        distinct(title, year, length, .keep_all=TRUE) %>%
        ggplot(aes(x=Genres)) +
        geom_bar() +
        scale_x_mergelist(sep = "-") +
        axis_combmatrix(sep = "-")

tidy_movies %>%
        distinct(title, year, length, .keep_all=TRUE) %>%
        mutate(Genres_collapsed = sapply(Genres, function(x) paste0(sort(x), collapse = "-"))) %>%
        ggplot(aes(x=Genres_collapsed)) +
        geom_bar() +
        theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))

# order by freq is descending obs count per category grouping
# order by degree is how many objects are combining in category grouping
tidy_movies %>%
        distinct(title, year, length, .keep_all=TRUE) %>%
        ggplot(aes(x=Genres)) +
        geom_bar() +
        # scale_x_upset(order_by = "degree")
        scale_x_upset(order_by = "freq")


#//////////////////////////////////////////////////////////////////////////////////////////////


# formatting
tidy_movies %>%
        distinct(title, year, length, .keep_all=TRUE) %>%
        ggplot(aes(x=Genres)) +
        geom_bar() +
        scale_x_upset(order_by = "degree") +
        theme_combmatrix(combmatrix.panel.point.color.fill = "green",
                         combmatrix.panel.line.size = 0,
                         combmatrix.label.make_space = FALSE)


#//////////////////////////////////////////////////////////////////////////////////////////////


# scale_x_upset is also generic for any categorical x_axis, so can be added to geom_violin, geom_boxplot, or geom_tile()

# geom_violin
tidy_movies %>%
        distinct(title, year, length, .keep_all=TRUE) %>%
        ggplot(aes(x=Genres, y=year)) +
        geom_violin() +
        scale_x_upset(order_by = "freq", n_intersections = 12)

# geom_boxplot
df_complex_conditions %>%
        mutate(Label = pmap(list(KO, DrugA, Timepoint), function(KO, DrugA, Timepoint){
                c(if(KO) "KO" else "WT", if(DrugA == "Yes") "Drug", paste0(Timepoint, "h"))
        })) %>%
        ggplot(aes(x=Label, y=response)) +
        geom_boxplot() +
        geom_jitter(aes(color=KO), width=0.1) +
        geom_smooth(method = "lm", aes(group = paste0(KO, "-", DrugA))) +
        scale_x_upset(order_by = "degree",
                      sets = c("KO", "WT", "Drug", "8h", "24h", "48h"),
                      position="top", name = "") +
        theme_combmatrix(combmatrix.label.text = element_text(size=12),
                         combmatrix.label.extra_spacing = 5)

# geom_tile
# note that this example uses concatenated strings separated by "-" instead of list column, 
# so scale_x_upset (specifically scale_x_merge_list that it wraps) is not needed, only axis_combmatrix is used (with sep = "-" argument)
avg_rating <- tidy_movies %>%
        mutate(Genres_collapsed = sapply(Genres, function(x) paste0(sort(x), collapse="-"))) %>%
        mutate(Genres_collapsed = fct_lump(fct_infreq(as.factor(Genres_collapsed)), n=12)) %>%
        group_by(stars, Genres_collapsed) %>%
        summarize(percent_rating = sum(votes * percent_rating)) %>%
        group_by(Genres_collapsed) %>%
        mutate(percent_rating = percent_rating / sum(percent_rating)) %>%
        arrange(Genres_collapsed)
avg_rating
avg_rating %>% count(Genres_collapsed)

ggplot(avg_rating, aes(x=Genres_collapsed, y=stars, fill=percent_rating)) +
        geom_tile() +
        # stat_summary_bin(aes(y=percent_rating * stars), fun = sum,  geom="point", 
        #                  shape="—", color="red", size=6) +
        axis_combmatrix(sep = "-", levels = c("Drama", "Comedy", "Short", 
                                              "Documentary", "Action", "Romance", "Animation", "Other")) +
        scale_fill_viridis_c()


#//////////////////////////////////////////////////////////////////////////////////////////////


# how to clean data to create list-column
data("gene_pathway_membership")
gene_pathway_membership

# tidy data
tidy_pathway_member <- gene_pathway_membership %>%
        as_tibble(rownames = "Pathway") %>%
        gather(Gene, Member, -Pathway) %>%
        filter(Member) %>%
        select(- Member)
tidy_pathway_member

# get list-column
tidy_pathway_member %>%
        group_by(Gene) %>%
        summarize(Pathways = list(Pathway))

tidy_pathway_member %>%
        group_by(Gene) %>%
        summarize(Pathways = list(Pathway)) %>%
        ggplot(aes(x = Pathways)) +
        geom_bar() +
        scale_x_upset(n_intersections = 20)


#//////////////////////////////////////////////////////////////////////////////////////////////


# nest tibbles
df <- tibble(x = c(1, 1, 1, 2, 2, 3), y = 1:6, z = 6:1)
df
df %>% nest(data = c(y, z))

# unnest tibbles
nested_tbl <- tibble(x = 1:3,
        y = list(NULL,
                tibble(a = 1, b = 2),
                tibble(a = 1:3, b = 3:1, c = 4)))
nested_tbl
nested_tbl %>% unnest(y)
nested_tbl %>% unnest(y, keep_empty = TRUE)
