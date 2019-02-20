library(rvest)
library(tibble)
library(dplyr)
library(purrr)
library(stringr)

color_w <- read_html("https://www.colordic.org/w/")
#color_y <- read_html("https://www.colordic.org/y/")
tmp <-color_w %>% html_nodes("td")

color_jpn <- tibble(
  hex= tmp %>% html_attr("style") %>% str_remove(.,"background-color:"),
  yomi = tmp %>% html_nodes("span") %>% html_text(),
  name = tmp %>% html_nodes("a") %>% html_text()
)

## clean name name as name includes everything in td...
color_jpn <-color_jpn %>% 
  mutate(name=str_remove(str_remove(name,yomi),hex))

## I couldn't figure out how to do this in one step... Hex to HSV...
## convert Hexvalue to RGB first
color_jpn_df <-color_jpn %>% 
  mutate(rgb_list = map(hex,my_hex2rgb)) %>% unnest(rgb_list)
## Then convert RGB to HSV... 
color_jpn_df <-color_jpn_df %>% 
  mutate(hsv_list = pmap(list(r,g,b),my_rgb2hsv)) %>% unnest(hsv_list)


## Group into Hue Groups - I chose 10 groups.
color_jpn_df <- color_jpn_df %>% 
  mutate(hue_group=factor(cut_width(h,width=1/10, boundary=0),
                          labels=c("Red/Yellow","Yellow","Yellow/Green","Green","Green/Blue","Blue","Blue/Purple","Purple","Puple/Red","Red")))

## golden angle 
g_ang <- pi*(3-sqrt(5))
color_jpn_df <-color_jpn_df %>% 
  group_by(hue_group) %>%
  mutate(t=row_number(v),
         #t=row_number(s),
         x=sqrt(t)*cos(t*g_ang),
         y=sqrt(t)*sin(t*g_ang),
         g_size=n(),
         h_mean=mean(h)) %>% 
  ungroup()

color_jpn_df %>%  
  ggplot(aes(x=x,y=y,color=hex)) + 
  geom_point(aes(size=g_size)) + 
  scale_color_identity() +
  theme_void(base_family="Roboto Condensed") +
  facet_wrap(~hue_group,ncol=5) +
  coord_fixed() +
  scale_size_continuous(range=c(4,3), guide="none") +
  labs(caption="There's so many different types of orange colours... ",
       title="465 Japanese Traditional Colours")
