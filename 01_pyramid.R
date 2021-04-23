require(tidyverse)
require(cowplot)
require(viridis)

pyramid_data_bars <- read.csv("data/pyramid_data.csv") %>%
  filter(measure_name %in% c("YLLs (Years of Life Lost)", "YLDs (Years Lived with Disability)"),
         age_name != "All Ages") %>%
  mutate(val = ifelse(sex_name == "Female", -1*val, val),
         upper = ifelse(sex_name == "Female", -1*upper, upper),
         lower = ifelse(sex_name == "Female", -1*lower, lower),
         age_id = ifelse(age_id == 28, 1, age_id))

pyramid_data_errors <- read.csv("data/pyramid_data.csv") %>%
  filter(measure_name == "DALYs (Disability-Adjusted Life Years)",
         age_name != "All Ages") %>%
  mutate(val = ifelse(sex_name == "Female", -1*val, val),
         upper = ifelse(sex_name == "Female", -1*upper, upper),
         lower = ifelse(sex_name == "Female", -1*lower, lower),
         age_id = ifelse(age_id == 28, 1, age_id))

p1 <- ggplot()+
  geom_bar(data = filter(pyramid_data_bars, sex_name == "Female"), aes(x = reorder(age_name, age_id), y = val, fill = measure_name),position="stack",  stat = "identity", alpha = 0.8) + 
  geom_errorbar(data = filter(pyramid_data_errors, sex_name == "Female"), aes(x = reorder(age_name, age_id), ymin = lower, ymax = upper), color = "red")+
  coord_flip()+
  scale_y_continuous(breaks = c(-150000, -100000, -50000), 
                     labels = c("150 000", "100 000", "50 000"))+
  facet_grid(. ~sex_name)+
  scale_fill_manual(values=c("cornflowerblue", "gold"))+
  theme_bw()+
  ylab("")+
  xlab("")+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"))

p2 <- ggplot()+
  geom_bar(data = filter(pyramid_data_bars, sex_name == "Male"), aes(x = reorder(age_name, age_id), y = val, fill = measure_name),position="stack", stat = "identity", alpha = 0.8)+
  geom_errorbar(data = filter(pyramid_data_errors, sex_name == "Male"), aes(x = reorder(age_name, age_id), ymin = lower, ymax = upper), color = "red")+
  coord_flip()+
  scale_y_continuous(breaks = c(50000, 100000, 150000), 
                     labels = c("50 000", "100 000", "150 000"))+
  facet_grid(. ~sex_name)+
  scale_fill_manual(values=c("cornflowerblue", "gold"))+
  theme_bw()+
  ylab("")+
  theme(legend.position = "top", legend.title = element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm"))

fig1 <- plot_grid(p1, p2, nrow = 1, ncol = 2, align = "h", axis = "tblr", scale = 1, labels = NA)
f1 <- ggdraw(add_sub(fig1, "DALYs per 100,000 general population", vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=4.5))

ggsave('figures/fig1.png', f1, scale = 2, width = 7, height = 4, units = "in", device='png', dpi=200)


