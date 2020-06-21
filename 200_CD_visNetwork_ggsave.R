library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(broom)
# library(officer)
library(tidygraph)
library(ggraph)
library(visNetwork)
library(ggimage)
library(ggpubr)

# lout <- create_layout(gd, layout = 'auto')
# head(lout)

source("999_libs.r")

set_graph_style() 

## get the data

cd_nodes <- read_csv("cd_vis_nodes.csv")

node <- cd_nodes %>%
  filter(group == "Main Chronic Disease")

cd_links <- read_csv("cd_vis_links.csv")

link <- cd_links %>%
  filter(gender == "All Sex",
         age_group == "All Age",
         develop_year == "All Years") %>%
  select(from, to, width, color, title, is_significant)


p1 <- visNetwork(node, link, width = "100%", height = 700) %>%
  visNodes(font = list(size = 11)) %>%
  visEdges(arrows = 'to') %>%
  visLayout(randomSeed = 123)


link <- cd_links %>%
  filter(gender == "All Sex",
         age_group == "All Age",
         develop_year == "All Years",
         is_significant == 1) %>%
  select(from, to, width, color, title)


p2 <- visNetwork(node, link, width = "100%", height = 700) %>%
  visNodes(font = list(size = 11)) %>%
  visEdges(arrows = 'to') %>%
  visLayout(randomSeed = 123)



###################################################################################################

cd_graph <- read_csv("cd_gggraph.csv")
# mutate(age_group = factor(age_group, 
#               levels = c( "All Age", "< 40", "40 to 60", "> 60")),
#        develop_year = factor(develop_year, 
#                          levels = c( "All Years", "<= 2 years",  "<= 5 years",  "<= 10 years")))


cd_graph_d <- cd_graph %>% 
  filter(gender == "All Sex",
         age_group == "All Age",
         develop_year == "All Years") 

l_ind <- sort(unique(cd_graph_d$width_color))

gd <- as_tbl_graph(cd_graph_d)
gd <- gd %>% 
  activate(nodes) %>% 
  inner_join(node, by = c("name" = "label"))

labels <- c("--", "*", "**", "***")
values <- c( "slategray", "#F4A582", "#0571B0", "#5CC96E")
names(labels) <- c("a", "b", "c", "d")
names(values) <- c("a", "b", "c", "d")


edge_labels <- labels[l_ind]
col_value <- values[l_ind]

set.seed(12221)   # 11111, 11222, 2222, 12221
lout <- create_layout(gd, layout = 'drl')

title_name <- paste0("Comorbidty Path for ", "All Age", ", All Sex", ", and All Developing Years")

# gd %>% 
g_all <- ggraph(lout) +
  geom_edge_link(aes(width = width, color = width_color, alpha = ifelse(width == 1, 0.01, 0.7)), 
                 arrow = arrow(length = unit(3, 'mm')), end_cap = circle(4, 'mm'),
                 show.legend = T) +
  geom_node_point(aes(size = value, color = color), alpha = 1) +
  geom_node_text(aes(label = name), size = 12, repel = T) +
  # geom_image(aes(x =x, y = y, image = image)) +
  # scale_edge_width(range = c(0.5, 3)) +
  scale_edge_width(range = c(1, 3)) +
  # scale_radius(range = c(3, 5)) +
  scale_size(range = c(5, 15)) +
  scale_edge_color_manual(name = "Link Significance", labels = edge_labels, values = col_value) +
  # scale_color_manual(name = "Node Color", labels = c("From Disease", "To Disease"), values = c("darkblue", "slategray3")) +
  guides(edge_width = "none", edge_alpha = "none", size = "none", color = "none") +
  # facet_edges(~ age_group) +
  # facet_graph(age_group ~ gender, scale = "free", space = "free") +
  labs(title = title_name) + 
  th_foreground(foreground = 'grey80', border = TRUE) + 
  theme(plot.title = element_text(color = "darkblue", size = 36, hjust = 0.5),
        legend.title = element_text(color = "darkblue", size = 30),
        legend.text = element_text(color = "darkblue", size = 30),
        legend.position = "top",
        strip.text = element_text(face = "plain", size = 8))


g_legend <- get_legend(g_all)
g_legend <- as_ggplot(g_legend)

#########################################
cd_graph_d <- cd_graph %>% 
  filter(gender == "All Sex",
         age_group != "All Age",
         develop_year == "All Years") 

l_ind <- sort(unique(cd_graph_d$width_color))
# View(cd_graph_d)

gd <- as_tbl_graph(cd_graph_d)
gd <- gd %>% 
  activate(nodes) %>% 
  inner_join(node, by = c("name" = "label")) %>% 
  activate(edges) %>% 
  mutate(age_group = factor(age_group, 
                            levels = c("< 40", "40 to 60", "> 60")))

edge_labels <- labels[l_ind]
col_value <- values[l_ind]

lout.2 <- create_layout(gd, layout = 'drl')

lout.2$x <- lout$x
lout.2$y <- lout$y

title_name <- paste0("Comorbidty Path for ", "All Sex", " and All Developing Years Separated by Age Group")

g_age <- gg_plot(lout.2, title_name, "age_group") 

#########################################
cd_graph_d <- cd_graph %>% 
  filter(gender != "All Sex",
         age_group == "All Age",
         develop_year == "All Years") 

l_ind <- sort(unique(cd_graph_d$width_color))
# View(cd_graph_d)

gd <- as_tbl_graph(cd_graph_d)
gd <- gd %>% 
  activate(nodes) %>% 
  inner_join(node, by = c("name" = "label")) %>% 
  activate(edges) %>% 
  mutate(gender = factor(gender, 
                         levels = c("F", "M")))

edge_labels <- labels[l_ind]
col_value <- values[l_ind]

lout.2 <- create_layout(gd, layout = 'drl')

lout.2$x <- lout$x
lout.2$y <- lout$y

title_name <- paste0("Comorbidty Path for ", "All Age Group", " and All Developing Years Separated by Gender")

g_sex <- gg_plot(lout.2, title_name, "gender") 


#########################################

cd_graph_d <- cd_graph %>% 
  filter(gender == "All Sex",
         age_group == "All Age",
         develop_year != "All Years") 

l_ind <- sort(unique(cd_graph_d$width_color))
# View(cd_graph_d)

gd <- as_tbl_graph(cd_graph_d)
gd <- gd %>% 
  activate(nodes) %>% 
  inner_join(node, by = c("name" = "label")) %>% 
  activate(edges) %>% 
  mutate(develop_year = factor(develop_year, 
                               levels = c("Within 2 years",  "Within 5 years",  "Within 10 years")))

edge_labels <- labels[l_ind]
col_value <- values[l_ind]

lout.2 <- create_layout(gd, layout = 'drl')

lout.2$x <- lout$x
lout.2$y <- lout$y

title_name <- paste0("Comorbidty Path for ", "All Age Group", " and All Gender Separated by Developing Years")

g_year <- gg_plot(lout.2, title_name, "develop_year")   


######Age group Breackdown######################################

cd_graph_d <- cd_graph %>% 
  filter(gender == "F",
         age_group != "All Age",
         develop_year == "All Years") 

l_ind <- sort(unique(cd_graph_d$width_color))
# View(cd_graph_d)

gd <- as_tbl_graph(cd_graph_d)
gd <- gd %>% 
  activate(nodes) %>% 
  inner_join(node, by = c("name" = "label")) %>% 
  activate(edges) %>% 
  mutate(age_group = factor(age_group, 
                            levels = c("< 40", "40 to 60", "> 60")))

edge_labels <- labels[l_ind]
col_value <- values[l_ind]

lout.2 <- create_layout(gd, layout = 'drl')

lout.2$x <- lout$x
lout.2$y <- lout$y

title_name <- paste0("Comorbidty Path for ", "Female", " and All Developing Years Separated by Age Group")

g_age_f <- gg_plot_1(lout.2, title_name, "age_group") 

g_age_f <- g_age_f + rremove("legend")

g_age_f <- annotate_figure(g_age_f,
                           left = text_grob("Female", color = "darkblue", rot = 90, face = "bold", size = 33)
                           # fig.lab = "Figure 1", fig.lab.face = "bold"
)


ggsave("image/all_age_femail.png", g_age_f, dpi = 300)
##

cd_graph_d <- cd_graph %>% 
  filter(gender == "M",
         age_group != "All Age",
         develop_year == "All Years") 

l_ind <- sort(unique(cd_graph_d$width_color))
# View(cd_graph_d)

gd <- as_tbl_graph(cd_graph_d)
gd <- gd %>% 
  activate(nodes) %>% 
  inner_join(node, by = c("name" = "label")) %>% 
  activate(edges) %>% 
  mutate(age_group = factor(age_group, 
                            levels = c("< 40", "40 to 60", "> 60")))

edge_labels <- labels[l_ind]
col_value <- values[l_ind]

lout.2 <- create_layout(gd, layout = 'drl')

lout.2$x <- lout$x
lout.2$y <- lout$y

title_name <- paste0("Comorbidty Path for ", "Male", " and All Developing Years Separated by Age Group")

g_age_m <- gg_plot_1(lout.2, title_name, "age_group") 

g_age_m <- g_age_m + rremove("legend")

g_age_m <- annotate_figure(g_age_m,
                           left = text_grob("Male", color = "darkblue", rot = 90, face = "bold", size = 33)
                           # fig.lab = "Figure 1", fig.lab.face = "bold"
)

g_age_fm <- ggarrange(g_legend, g_age_f, g_age_m, nrow = 3, 
                      heights = c(0.04, 0.48, 0.48),
                      common.legend = TRUE, align = "v")

#############################################################################

#######All Years############################################################

cd_graph_d <- cd_graph %>% 
  filter(gender == "F",
         age_group == "All Age",
         develop_year != "All Years") 

l_ind <- sort(unique(cd_graph_d$width_color))
# View(cd_graph_d)

gd <- as_tbl_graph(cd_graph_d)
gd <- gd %>% 
  activate(nodes) %>% 
  inner_join(node, by = c("name" = "label")) %>% 
  activate(edges) %>% 
  mutate(develop_year = factor(develop_year, 
                               levels = c("Within 2 years",  "Within 5 years",  "Within 10 years")))

edge_labels <- labels[l_ind]
col_value <- values[l_ind]

lout.2 <- create_layout(gd, layout = 'drl')

lout.2$x <- lout$x
lout.2$y <- lout$y

title_name <- paste0("Comorbidty Path for ", "Female", " and All Age Separated by Developing Years")

g_year_f <- gg_plot_1(lout.2, title_name, "develop_year") 

g_year_f <- g_year_f + rremove("legend")

g_year_f <- annotate_figure(g_year_f,
                            left = text_grob("Female", color = "darkblue", rot = 90, face = "bold", size = 33)
                            # fig.lab = "Figure 1", fig.lab.face = "bold"
)

#########

cd_graph_d <- cd_graph %>% 
  filter(gender == "M",
         age_group == "All Age",
         develop_year != "All Years") 

l_ind <- sort(unique(cd_graph_d$width_color))
# View(cd_graph_d)

gd <- as_tbl_graph(cd_graph_d)
gd <- gd %>% 
  activate(nodes) %>% 
  inner_join(node, by = c("name" = "label")) %>% 
  activate(edges) %>% 
  mutate(develop_year = factor(develop_year, 
                               levels = c("Within 2 years",  "Within 5 years",  "Within 10 years")))

edge_labels <- labels[l_ind]
col_value <- values[l_ind]

lout.2 <- create_layout(gd, layout = 'drl')

lout.2$x <- lout$x
lout.2$y <- lout$y

title_name <- paste0("Comorbidty Path for ", "Male", " and All Age Separated by Developing Years")

g_year_m <- gg_plot_1(lout.2, title_name, "develop_year") 

g_year_m <- g_year_m + rremove("legend")

g_year_m <- annotate_figure(g_year_m,
                            left = text_grob("Male", color = "darkblue", rot = 90, face = "bold", size = 33)
                            # fig.lab = "Figure 1", fig.lab.face = "bold"
)

g_year_fm <- ggarrange(g_legend, g_year_m, g_year_m, nrow = 3, 
                       heights = c(0.04, 0.48, 0.48),
                       common.legend = TRUE, align = "v")

#####################################################################

#######All Sex breakdown by age and year#########################################################


cd_graph_d <- cd_graph %>% 
  filter(gender == "All Sex",
         age_group != "All Age",
         develop_year == "Within 2 years") 

l_ind <- sort(unique(cd_graph_d$width_color))
# View(cd_graph_d)

gd <- as_tbl_graph(cd_graph_d)
gd <- gd %>% 
  activate(nodes) %>% 
  inner_join(node, by = c("name" = "label")) %>% 
  activate(edges) %>% 
  mutate(age_group = factor(age_group, 
                            levels = c("< 40", "40 to 60", "> 60")))

edge_labels <- labels[l_ind]
col_value <- values[l_ind]

lout.2 <- create_layout(gd, layout = 'drl')

lout.2$x <- lout$x
lout.2$y <- lout$y

title_name <- paste0("Comorbidty Path for ", "All Sex", " and Within 2 Years Separated by Age Group")

g_age_2y <- gg_plot_2(lout.2, title_name, "age_group") 

g_age_2y <- g_age_2y + rremove("legend")

g_age_2y <- annotate_figure(g_age_2y,
                            left = text_grob("Within 2 years", color = "darkblue", rot = 90, face = "bold", size = 33)
                            # fig.lab = "Figure 1", fig.lab.face = "bold"
)

#####

cd_graph_d <- cd_graph %>% 
  filter(gender == "All Sex",
         age_group != "All Age",
         develop_year == "Within 5 years") 

l_ind <- sort(unique(cd_graph_d$width_color))
# View(cd_graph_d)

gd <- as_tbl_graph(cd_graph_d)
gd <- gd %>% 
  activate(nodes) %>% 
  inner_join(node, by = c("name" = "label")) %>% 
  activate(edges) %>% 
  mutate(age_group = factor(age_group, 
                            levels = c("< 40", "40 to 60", "> 60")))

edge_labels <- labels[l_ind]
col_value <- values[l_ind]

lout.2 <- create_layout(gd, layout = 'drl')

lout.2$x <- lout$x
lout.2$y <- lout$y

title_name <- paste0("Comorbidty Path for ", "All Sex", " and Within 5 Years Separated by Age Group")

g_age_5y <- gg_plot_2(lout.2, title_name, "age_group") 

g_age_5y <- g_age_5y + rremove("legend")

g_age_5y <- annotate_figure(g_age_5y,
                            left = text_grob("Within 5 years", color = "darkblue", rot = 90, face = "bold", size = 33)
                            # fig.lab = "Figure 1", fig.lab.face = "bold"
)


#####

cd_graph_d <- cd_graph %>% 
  filter(gender == "All Sex",
         age_group != "All Age",
         develop_year == "Within 10 years") 

l_ind <- sort(unique(cd_graph_d$width_color))
# View(cd_graph_d)

gd <- as_tbl_graph(cd_graph_d)
gd <- gd %>% 
  activate(nodes) %>% 
  inner_join(node, by = c("name" = "label")) %>% 
  activate(edges) %>% 
  mutate(age_group = factor(age_group, 
                            levels = c("< 40", "40 to 60", "> 60")))

edge_labels <- labels[l_ind]
col_value <- values[l_ind]

lout.2 <- create_layout(gd, layout = 'drl')

lout.2$x <- lout$x
lout.2$y <- lout$y

title_name <- paste0("Comorbidty Path for ", "All Sex", " and Within 10 Years Separated by Age Group")

g_age_10y <- gg_plot_2(lout.2, title_name, "age_group") 

g_age_10y <- g_age_10y + rremove("legend")

g_age_10y <- annotate_figure(g_age_10y,
                             left = text_grob("Within 10 years", color = "darkblue", rot = 90, face = "bold", size = 33)
                             # fig.lab = "Figure 1", fig.lab.face = "bold"
)


g_age_yrs <- ggarrange(g_legend, g_age_2y, g_age_5y, g_age_10y, nrow = 4, 
                       heights = c(0.04, 0.32, 0.32,0.32),
                       common.legend = TRUE, align = "v")




#####################################################################






# pagedown::chrome_print("change_of_comorbidity_with_AGD.html",output="test.pdf")

























