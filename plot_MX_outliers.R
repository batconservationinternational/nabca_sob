library(tidyverse)
library(readxl)
library(plotly)
library(htmlwidgets)

t <- read_csv("/Users/ngoodby/Desktop/nabca_sob/threat_sums_Mexico.csv", 
              col_names = c("expert","species", "sum_threats")) %>% 
  filter(expert != "linear pool")


my_labs <- t %>% filter(sum_threats>1) %>% group_by(expert) %>% 
  summarise(num_above_one = n())

my_plot <- ggplot(t, aes(x=expert, y=sum_threats, color = species)) + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, fill = "green", alpha = .2, color = NA) + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, fill = "red", alpha = .2, color = NA) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90), legend.position="none") + 
  geom_hline(yintercept=1, linetype='dotted', col = 'red') +
  labs(title = "Total estimated threat impact for each expert for each MX species", 
       caption = "*Red numbers are count of species where the expert's estimated impact total was >1.") +
  ylab("Sum of estimated threat impacts") +
  xlab("") +
  scale_y_continuous(breaks = seq(0, 6)) +
  geom_text(data=my_labs, aes(x=expert, y=-0.5, label=num_above_one),
            color="red", 
            size=4,fontface="bold")


my_plotly <- ggplotly(my_plot, tooltip = "colour")

# ggsave("MX_threat_sums.png", plot = my_plot)
htmlwidgets::saveWidget(my_plotly, "MX_threat_sums.html")
