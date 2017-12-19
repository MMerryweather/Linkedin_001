require(pacman)
pacman::p_load(tidyverse, stringr, RColorBrewer, ggthemr, ggrepel, hrbrthemes)

df = read_csv("data/01 data.csv") %>%
  select(-contains(".var")) %>%
  select(Datetime:`Elapsed Time Total`,
         `Live Time 1`:`Field 12`,
         everything()) %>%
  select(-contains("Field")) %>%
  mutate(run = str_extract(Reading, "\\d{1,2}"),
         duplicate = str_extract(Reading, "(?<=-)\\d")) %>% 
  filter(LocationID == "Concentrate") %>% 
  filter(!is.na(duplicate)) %>% 
  select(-(run:duplicate))

df_tidy = df %>%
  gather(element, assay, -(Datetime:comment)) %>% 
  mutate(assay = as.numeric(assay)) %>% 
  filter(!is.na(assay))

df_tidy %>%
  ggplot(aes(
    reorder(element, assay, mean),
    assay,
    fill = reorder(element, assay, mean)
  )) +
  geom_boxplot() +
  theme_ipsum() +
  scale_y_log10(breaks = 10 ^ (0:6),
                labels = scales::comma_format()) +
  coord_flip() +
  no_legend() +
  labs(title = "Distribution of Concentrations",
       x = "Element",
       y = "Assay (ppm)")
ggsave(
  "plot/01 Distributions.png",
  width = 7.5,
  height = 10,
  units = "cm",
  scale = 2
)

df_tidy %>%
  mutate(
    source = if_else(comment == "float con", 'Flotation', 'Gravity'),
    source = if_else(is.na(comment), 'Gravity', source)
  ) %>%
  ggplot(aes(
    x = reorder(element, assay, mean),
    y = assay,
    colour = source
  )) +
  theme_ipsum() +
  geom_point(alpha = 0.5) +
  scale_y_log10(breaks = 10 ^ (0:6),
                labels = scales::comma_format()) +
  coord_flip() +
  legend_bottom() +
  no_legend_title() +
  labs(title = "Distributions by Feed",
       x = "Element",
       y = "Assay (ppm)")
ggsave(
  "plot/02 Distributions by Feed.png",
  width = 7.5,
  height = 10,
  units = "cm",
  scale = 2
)

df_tidy %>%
  mutate(
    source = if_else(comment == "float con", 'Flotation', 'Gravity'),
    source = if_else(is.na(comment), 'Gravity', source)
  ) %>%
  group_by(source, element) %>%
  summarise(assay = mean(assay)) %>%
  ggplot(aes(
    x = source,
    y = assay,
    colour = reorder(element, assay, mean),
    group = element
  )) +
  theme_ipsum() +
  geom_point(size = 2) +
  geom_line() +
  scale_y_log10(labels = scales::comma_format()) +
  geom_text_repel(aes(label = element), point.padding = .5) +
  no_legend() +
  no_x_gridlines() +
  annotation_logticks(sides = "l") +
  labs(title = "Feed Comparison",
       x = "Feed Source",
       y = "Assay (ppm)")

ggsave(
  "plot/03 Feed Comparison.png",
  width = 7.5,
  height = 10,
  units = "cm",
  scale = 2
)
