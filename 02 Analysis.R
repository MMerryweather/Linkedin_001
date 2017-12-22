pacman::p_load(tidyverse, magrittr, ggthemr, hrbrthemes, stringr)

apply_partition_curve = function(float_sink_table, ep = 0.005, cutpoint = 1.5, t_0 = 0, t_100 = 100){
  # Requires a tibble with a floats column <dbl>
  float_sink_table %>%
    mutate(recovery_to_product = (100 - (t_0 + (t_100 - t_0) / (1 + exp((1.0986 * (cutpoint - floats) / ep))))) / 100,
           recovery_to_reject = 1 - recovery_to_product,
           cutpoint = cutpoint,
           ep = ep,
           t_0 = t_0,
           t_100 = t_100)
}
source("02 Apply Partition Curve.R")
#Demonstrate Apply Partition Curve

df = data.frame(floats = seq(from = 1, to = 2.2, by = 0.005))

partition_curve = df %>% 
  apply_partition_curve(ep = 0.05)

## Plot partition Curve
partition_curve %>% ggplot(aes(floats, recovery_to_product)) +
  theme_ipsum()+
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "JKMRC Partition Curve",
       subtitle = "D50 = 1.5 SG, Ep = 0.05",
       x = "SG",
       y = "Recovery to Product (%)")
ggsave(
  "plot/04 Partition Curve.png",
  width = 7.5,
  height = 5,
  units = "cm",
  scale = 3
)

## Demonstrate Mapping
Eps = seq(0.005, 0.2, 0.02)

partition_curves = Eps %>% map_df(~ apply_partition_curve(df, ep=.), .id = "Ep")
head(partition_curves)
# Note Ep column is just an identifier, not value used
partition_curves = Eps %>% map_df(~mutate(apply_partition_curve(df, ep=.),Ep = .))
head(partition_curves)

partition_curves %>% ggplot(aes(x = floats,
                                y = recovery_to_product,
                                colour = Ep,
                                group = Ep)) +
  theme_ipsum()+
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "JKMRC Partition Curves",
       subtitle = "D50 = 1.5 SG",
       x = "SG",
       y = "Recovery to Product (%)")+
  theme(
    legend.position = c(.95, .95),
    legend.direction = "horizontal",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.box.background = element_rect(colour = "white")
  )
ggsave(
  "plot/05 Partition Curves.png",
  width = 7.5,
  height = 5,
  units = "cm",
  scale = 3
)

float_sink = read_csv("data/02 float sink.csv") %>% 
  mutate(float_sink_fraction = str_replace_all(float_sink_fraction,"  - ","\n")) 

float_sink %>%
  ggplot(aes(x = float_sink_fraction, y = fraction_mass/100))+
  geom_col(fill="black")+
  theme_ipsum()+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Float Sink Mass Fractions",
       x = "SG",
       y = "Mass Fraction (%)")
ggsave(
  "plot/06 Float Sinks.png",
  width = 7.5,
  height = 5,
  units = "cm",
  scale = 3
)

partition_results = float_sink %>% apply_partition_curve(cutpoint = 1.6, ep = 0.05) %>% 
  mutate(Product = fraction_mass * recovery_to_product,
         Reject = fraction_mass * recovery_to_reject)

partition_results %>%
  gather(key,value, -(1:11)) %>%
  mutate(key = factor(key, c("Reject","Product"))) %>% 
  ggplot(aes(x = float_sink_fraction,
             y = value/100,
             fill = key))+
    geom_col()+
  scale_fill_ipsum()+
  theme_ipsum()+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Partition of Float Sinks",
       x = "SG",
       y = "Mass Fraction (%)")+
  no_legend_title()+
  theme(
    legend.position = c(.95, .95),
    legend.direction = "horizontal",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.box.background = element_rect(colour = "white")
  )
ggsave(
  "plot/07 Float Sinks Partitioned.png",
  width = 7.5,
  height = 5,
  units = "cm",
  scale = 3
)

# Write a summary function
partition_yields = function(tbl){
  tbl %>% mutate(product_mass = fraction_mass * recovery_to_product,
                 product_ash = fraction_ash * product_mass,
                 reject_mass = fraction_mass * recovery_to_reject,
                 reject_ash = fraction_ash * reject_mass) %>%
    select(product_mass, product_ash, reject_mass, reject_ash) %>%
    summarise_all(sum) %>%
    mutate(product_ash = product_ash / product_mass,
           reject_ash = reject_ash / reject_mass) %>% 
    bind_cols(tbl %>% select(cutpoint:t_100) %>% unique())
}

# works for one cutpoint
partition_results %>% partition_yields()

# Now try for many cutpoints
cutpoints = seq(1.2, 1.8, 0.01)

# Use map from purrr to run with various inputs
# use an anonymous function because the argument we want to change
# isn't the first one, otherwise we'd just call the function.
# last line outputs a dataframe and the map_df call binds them
df_partition_yields = cutpoints %>%
  map(~ apply_partition_curve(
    float_sink_table = float_sink,
    cutpoint = .,
    ep = 0.05
  )) %>% 
  map_df(partition_yields)

head(df_partition_yields)

# now we are plotting the summaries of many simulations
df_partition_yields %>%
  ggplot(aes(x = cutpoint,
             y = product_mass/100)) +
  geom_line() +
  theme_ipsum()+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,1))+
  labs(title = "Model Yield",
       subtitle = "Ep = 0.05",
       x = "Cutpoint",
       y = "Product Yield")
  ggsave(
    "plot/08 Model Yields.png",
    width = 7.5,
    height = 5,
    units = "cm",
    scale = 3
  )

  

df_partition_yields %>%
  ggplot(aes(x = product_ash,
             y = product_mass)) +
  geom_line() +
  theme_ipsum()

df_partition_yields %>%
  mutate(yield_delta = (product_mass - lag(product_mass)) / (product_ash - lag(product_ash))) %>%
  ggplot(aes(x = cutpoint, y = yield_delta)) +
  geom_line() +
  theme_ipsum()

conditions = list(cutpoint = seq(1.2, 1.8, 0.01),
                  ep = c(0.025,0.05,0.1)) %>% cross()

df_yields_ep = conditions %>%
  map(~ apply_partition_curve(
    float_sink_table = float_sink,
    cutpoint = .$cutpoint,
    ep = .$ep
  )) %>% 
  map_df(partition_yields)

df_yields_ep %>%
  mutate(ep = as.factor(ep)) %>% 
  ggplot(aes(x = product_ash/100,
             y = product_mass/100,
             group = ep,
             colour = ep)) +
  geom_line() +
  scale_colour_ipsum()+
  theme_ipsum()+
  geom_text(data = filter(df_yields_ep, cutpoint == 1.3) %>% mutate(ep = as.factor(ep)),
            aes(label = ep,x = product_ash/100, y = product_mass/100),
            hjust="left", 
            nudge_x = 0.001)+
  no_legend()+
  scale_x_continuous(labels = scales::percent_format())+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Sensitivity Analysis: Ep",
       subtitle = "Yield-Ash Curves",
       x = "Product Ash",
       y = "Product Yield")

df_yields_ep %>%
  mutate(ep = as.factor(ep)) %>% 
  ggplot(aes(x = cutpoint,
             y = product_mass/100,
             group = ep,
             colour = ep)) +
  geom_line() +
  scale_colour_ipsum()+
  theme_ipsum()+
  no_legend()+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Sensitivity Analysis: Ep",
       subtitle = "Yield-Ash Curves",
       x = "Cutpoint",
       y = "Product Yield")
