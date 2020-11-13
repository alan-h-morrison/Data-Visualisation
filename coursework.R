library(fmsb)
library(ggplot2)
library(lattice)
library(MASS)
library(gridExtra)
library(ggthemes)

# Find five relationships and 2 outliers
data <- read.csv("dataset.csv")

data$Group <- factor(data$Group, levels = c("N", "E", "V", "P"))
data$Wind.Direction <- factor(data$Wind.Direction, levels = c("N", "E", "S", "W"))
data$Gender <- factor(data$Gender, levels = c("M", "F"))

data.Direction <- c("North", "East", "South", "West")
names(data.Direction) <- c("N", "E", "S", "W")

data.Gender <- c("Male", "Female")
names(data.Gender) <- c("M", "F")

data.Group <- c("Novice", "Experienced", "Very Experienced", "Professional")
names(data.Group) <- c("N", "E", "V", "P")

Group.Labels <- c("Novice", "Experienced", "Very Experienced", "Professional")
Wind.Labels <- c("North", "East", "South", "West")

# Relationship - Distance of the ball throwns(cts)
# better way to visualize this
distance_index <- 5

distance <- ggplot(data, aes(x=Distance)) + 
  geom_histogram( colour="#000000", fill="#66c2a5", binwidth=distance_index, boundary=0)

distance + 
  labs(title="Distance of the Ball Reached From a Straight Throw") +
  scale_x_continuous(expand= c(0,0), limits = c(0, 105), name = "Distance (metres)", breaks = seq(0,170,distance_index), labels= seq(0,170,distance_index)) +
  scale_y_continuous(expand= c(0,0), limits = c(0, 4600), name="Count (no. of throws)", breaks = seq(0,4600,250)) +
  theme(#plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    plot.title = element_blank(),
    axis.title = element_text(size = 15),
    axis.title.x = element_text(margin = margin(r=10)),
    axis.title.y = element_text(margin = margin(r=10)),
    axis.text = element_text(size = 10),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# Relationship - Average Height of the ball thrown (cts)
height_index <- 5

height <- 
  ggplot(data, aes(x=Height)) + 
  geom_histogram( colour="#000000", fill="#66c2a5", binwidth=height_index, boundary=0)

height + 
  labs(title="Height of the Ball Reached From a Straight Throw (Barplot)") +
  scale_x_continuous(expand= c(0,0), limits = c(0,170), name = "Height (metres)", breaks = seq(0,170,height_index), labels= seq(0,170,height_index)) +
  scale_y_continuous(expand= c(0,0), limits = c(0, 2800), name="Count (no. of throws)", breaks = seq(0,3000,100)) +
  theme(#plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    plot.title = element_blank(),
    axis.title = element_text(size = 15),
    axis.title.x = element_text(margin = margin(r=10)),
    axis.title.y = element_text(margin = margin(r=10)),
    axis.text = element_text(size = 10),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


# Outlier 1 - Height
boxplot_height <-
  ggplot(data, aes(x=Height)) + 
  geom_boxplot(fill="#66c2a5", outlier.shape = 21, outlier.size = 5, outlier.fill ="#f2766d")

boxplot_height +
  scale_x_continuous(expand= c(0,0), limits = c(0,170), name = "Height (metres)", breaks = seq(0,170,height_index), labels= seq(0,170,height_index)) +
  labs(title = "Height of the Ball Reached From a Straight Throw (Boxplot)") +
  theme(# plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    plot.title = element_blank(),
    axis.title = element_text(size = 15),
    axis.title.x = element_text(margin = margin(r=10)),
    axis.title.y = element_text(margin = margin(r=10)),
    axis.text = element_text(size = 10),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

# adjusted
adjusted_index <- 3

height_adjusted <- 
  ggplot(data, aes(x=Height)) + 
  geom_histogram( colour="#000000", fill="#66c2a5",  binwidth=adjusted_index, boundary=0) + 
  labs(title = "Average Height of the ball thrown ") 

height_adjusted + 
  labs(title="Height of the Ball Reached From a Straight Throw (Adjusted)") +
  scale_x_continuous(expand= c(0,0), name = "Height (metres)", breaks = seq(0,100,adjusted_index), labels= seq(0,100,adjusted_index), limits = c(20,85)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1800), name="Count (no. of throws)", breaks = seq(0,3000,100)) +
  theme(# plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    plot.title = element_blank(),
    axis.title = element_text(size = 15),
    axis.title.x = element_text(margin = margin(r=10)),
    axis.title.y = element_text(margin = margin(r=10)),
    axis.text = element_text(size = 10),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# Relationship = Male offset is uniform, Female offset is regressive
offset <- ggplot(data, aes(x=Group, y=Offset, fill=Wind.Direction)) + 
  geom_boxplot() +
  facet_wrap(~Gender, ncol = 2, labeller = labeller(Gender = data.Gender))

offset + labs(title ="Difference Between Genders Offset of the Ball From a Straight Throw") +
  scale_x_discrete(labels = Group.Labels) +
  scale_y_continuous(expand= c(0,0), limits = c(0,80), name="Offset (cm)", breaks = seq(0,80,5)) +
  scale_fill_manual(name="Wind Direction: ", labels=Wind.Labels, values = c("N" = "#66c2a5",
                                                                            "E" = "#8da0cb",
                                                                            "S" = "#fc8d62",
                                                                            "W" = "#e78ac3")) +
  theme(strip.text.x = element_text(size = 15, colour = "black", hjust = 0), 
        strip.text.y = element_text(size = 15, colour = "black", hjust = 0),
        # plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        plot.title = element_blank(),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(r=10)),
        axis.title.y = element_text(margin = margin(r=10)),
        axis.text = element_text(size = 10),
        legend.title = element_text(face = "bold"),
        legend.position = "top",
        legend.background = element_rect(colour = "black"),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# Outlier 2 - Group V (Very experienced) scores a higher score.1 than all of the other groups
# Relationship - Male have a uniform relation, Female have a negative regression
outlier2 <- ggplot(data, aes(x=Group, y=Score.1, fill=Wind.Direction)) +
  geom_boxplot() +
  facet_wrap(~Gender, ncol = 4, labeller = labeller(Gender=data.Gender)) +# Can be expanded more
  labs(title = "Outlier - Group V scoring higher")

outlier2 + labs(title ="Impact of Gender and Wind Direction on a Person's Second Score") +
  scale_x_discrete(labels = Group.Labels) +
  scale_y_continuous(expand= c(0,0), limits = c(0,95),name="Score 1 (out of 100 points)", breaks = seq(0,95,5)) +
  scale_fill_manual(name="Wind Direction: ", labels=Wind.Labels, values = c("N" = "#66c2a5",
                                                                            "E" = "#8da0cb",
                                                                            "S" = "#fc8d62",
                                                                            "W" = "#e78ac3")) +
  theme(strip.text.x = element_text(size = 15, colour = "black", hjust = 0), 
        strip.text.y = element_text(size = 15, colour = "black", hjust = 0),
        # plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
        plot.title = element_blank(),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(r=10)),
        axis.title.y = element_text(margin = margin(r=10)),
        axis.text = element_text(size = 10),
        legend.title = element_text(face = "bold"),
        legend.position = "top",
        legend.background = element_rect(colour = "black"),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# When the wind is blowing east, as you increase the Age of the participant so does the Score.2,
# When the wind is blowing west, as you increase the age of the participant so the Score.2 decreases.
age <- ggplot(data, aes(x=Age, y=Score.2, fill=Wind.Direction, shape=Wind.Direction)) + 
  geom_point(size=3) + 
  geom_smooth(colour="black", size=1) + 
  facet_grid( ~Wind.Direction, labeller = labeller(Wind.Direction=data.Direction))

age + labs(title ="Impact of Age and Wind Direction on a Person's Second Score") +
  scale_x_continuous(name = "Age (years)", breaks = seq(0,120,10)) +
  scale_y_continuous(name="Score 2 (out of 100 points)", breaks = seq(0,105,5)) +
  scale_fill_manual(name="Wind Direction: ", labels=Wind.Labels, values = c("N" = "#66c2a5",
                                                                            "E" = "#8da0cb",
                                                                            "S" = "#fc8d62",
                                                                            "W" = "#e78ac3")) +
  scale_shape_manual(name="Wind Direction: ", labels=Wind.Labels, values = c(22,21,24,23)) +
  labs(shape="Wind Direction: ", colour="Wind Direction: ") +
  theme(strip.text.x = element_text(size = 15, colour = "black", hjust = 0), 
        strip.text.y = element_text(size = 15, colour = "black", hjust = 0),
        # plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        plot.title = element_blank(),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(r=10)),
        axis.title.y = element_text(margin = margin(r=10)),
        axis.text = element_text(size = 10),
        legend.title = element_text(face = "bold"),
        legend.position = "top",
        legend.background = element_rect(colour = "black"),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# Relationship - Distance and angle (cts X cts)
distance.angle <- ggplot(data, aes(x=Angle, y=Distance, fill = Group, shape=Group)) +
  geom_point(size=3.5) +
  geom_smooth(formula = y ~ x + I(x^2),method="lm", colour="#000000", size=1) + 
  facet_wrap(~Group, labeller = labeller(Group=data.Group))

distance.angle + labs(title ="Effect of Person's Throwing Angle on the Distance of a Throw") +
  scale_x_continuous(expand= c(0,0), limits = c(0,90), name = "Angle (degrees)", breaks = seq(0,90,5)) +
  scale_y_continuous(expand= c(0,0), limits = c(0,120),name="Distance (metres)", breaks = seq(0,120,10)) +
  scale_fill_manual(name="Group: ", labels=Group.Labels, values = c("#8960b3", "#56ae6c", "#41b6c4", "#225ea8")) +
  scale_shape_manual(name="Group: ", labels=Group.Labels,values = c(21,22,23,24)) +
  theme(strip.text.x = element_text(size = 15, colour = "black", hjust = 0), 
        strip.text.y = element_text(size = 15, colour = "black", hjust = 0),
        # plot.title = element_text(size = 27, face = "bold", hjust = 0.5),
        plot.title = element_blank(),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(r=10)),
        axis.title.y = element_text(margin = margin(r=10)),
        axis.text = element_text(size = 10),
        legend.title = element_text(face = "bold"),
        legend.position = "top",
        legend.background = element_rect(colour = "black"),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        panel.spacing = unit(7, "mm"))

highlight_df <- data %>% 
  filter(Distance>=90) 

data %>% 
  
  distance.angle <- ggplot(data, aes(x=Angle, y=Distance, fill = Group, shape=Group)) +
  geom_point(size=3.5) +
  geom_point(data = highlight_df, aes(x=Angle, y=Distance), color='red') +
  geom_smooth(formula = y ~ x + I(x^2),method="lm", colour="#000000", size=1) + 
  facet_wrap(Wind.Direction~Group, labeller = labeller(Group=data.Group))
