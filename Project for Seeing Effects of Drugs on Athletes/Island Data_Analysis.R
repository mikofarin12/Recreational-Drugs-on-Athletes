## STATS 101B: Project
## Ryan Cheong, Miko Farin 

library(tidyverse)
library(agricolae)
library(car)
library(readxl)

island_data <- read_csv("Island_Data_Final.csv")

# rename column names
colnames(island_data)[6:9] <- c('Pre Coordination', 'Pre Performance', 'Post Coordination', 'Post Performance')

# paired t-tests for coordination
t.test(island_data$`Pre Coordination`, island_data$`Post Coordination`, paired = TRUE)
# paired t-tests for performance
t.test(island_data$`Pre Performance`, island_data$`Post Performance`, paired = TRUE)

# calculate differences in post and pre
island_data <- island_data %>% mutate(Diff_Coord = `Post Coordination` - `Pre Coordination`, Diff_Perf = `Post Performance` - `Pre Performance`) %>% 
  select(-c(`Diff Coord`, `Diff Perf`))

# coordination
model1 <- aov(Diff_Coord ~ as.factor(Gender) + as.factor(Drug)*as.factor(Dosage), data = island_data)
summary(model1)
anova(model1)
# performance
model2 <- aov(Diff_Perf ~ as.factor(Gender) + as.factor(Drug)*as.factor(Dosage), data = island_data)
summary(model2)
anova(model2)

# introduce age groups blocks
island_data2 <- island_data %>% arrange(Age) %>% mutate(Age_Group = if_else(between(Age, 15, 23), 1, if_else(between(Age, 24, 28), 2, 3)))

# for reapeated measures
island_data3 <- tibble(Participant = rep(island_data2$Participant, 2), Time = rep(1:2, each = nrow(island_data2)), 
                       Coordination = c(island_data$`Pre Coordination`, island_data$`Post Coordination`), 
                       Performance = c(island_data$`Pre Performance`, island_data$`Post Performance`), 
                       Drug = rep(island_data$Drug, 2), Dosage = rep(island_data$Dosage, 2))
# summary data 
island_data2 %>% group_by(Drug, Dosage) %>% summarise(mean_precoord = mean(`Pre Coordination`), mean_postcoord = mean(`Post Coordination`), 
                                                      sd_precoord = sd(`Pre Coordination`), sd_postcoord = sd(`Post Coordination`), 
                                                      mean_preperf = mean(`Pre Performance`), mean_postperf = mean(`Post Performance`), 
                                                      sd_preperf = sd(`Pre Performance`), sd_postperf = sd(`Post Performance`))

# two-way repeated measure summary
aov(Coordination ~ (Drug*Dosage*Time) + Error(Participant), 
    data = (island_data3 %>% select(-Performance) %>% convert_as_factor(Participant, Time, Drug, Dosage))) %>% summary()

aov(Performance ~ (Drug*Dosage*as.factor(Time)) + Error(Participant/(Time)), 
    data = (island_data3 %>% select(-Coordination) %>% convert_as_factor(Participant, Drug, Dosage))) %>% summary()


# coordination with two blocks (age and gender)
aov(Diff_Coord ~ as.factor(Age_Group) + as.factor(Gender) + as.factor(Drug)*as.factor(Dosage), data = island_data2) %>% anova()
# performance with two blocks (age and gender)
aov(Diff_Perf ~ as.factor(Age_Group) + as.factor(Gender) + as.factor(Drug)*as.factor(Dosage), data = island_data2) %>% anova()

# interaction plots for performance
with(island_data, interaction.plot(Dosage, Drug, Diff_Perf, type = 'o', col = 1:3), legend(fill = c('blue', 'red', 'green')))
# interaction plots for coordination
with(island_data, interaction.plot(Dosage, Drug, Diff_Coord, type = 'o'))

# means table for coordination and performance
model.tables(model1)
model.tables(model2)

# TukeyHSD for coordintion and performance
TukeyHSD(model1) %>% plot()
TukeyHSD(model2) %>% plot()

# LSD Fisher test 
LSD.test(model2, 'as.factor(Drug)', p.adj = 'bonferroni') %>% print()
LSD.test(model2, 'as.factor(Dosage)', p.adj = 'bonferroni') %>% print()
# plots for performance
LSD.test(model2, 'as.factor(Drug)', p.adj = 'bonferroni') %>% plot.group()
LSD.test(model2, 'as.factor(Dosage)', p.adj = 'bonferroni') %>% plot.group()

#ggplot code for interaction:


#creating data for interaction plot
island_data_i <-  island_data
island_data_i$Dosage <-  ifelse(island_data_i$Dosage == "Low", 0, ifelse(island_data_i$Dosage == "Med",1,2))
attach(island_data_i)

#axis labels for plot
axis_labels<- c("Low", "Med", "High")

#interaction plot for difference in performance
ggplot() +aes(x = factor(island_data_i$Dosage), color = Drug, group =Drug, y = island_data$Diff_Perf)+
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")+
  ggtitle("Interaction Plot for Performance")  +
  ylab("Mean of Difference" )+
  xlab("Dosage" )+
  scale_color_manual(values=c("red", "blue", "green"))+
  scale_x_discrete(labels = axis_labels)+
  guides(fill=guide_legend(title="New Legend Title"))+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13))

#interaction plot for difference in Coordination
ggplot() +aes(x = factor(island_data_i$Dosage), color = Drug, group =Drug, y = island_data$Diff_Coord)+
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")+
  ggtitle("Interaction Plot for Performance")  +
  ylab("Mean of Difference" )+
  xlab("Dosage" )+
  scale_color_manual(values=c("green", "red", "blue"))+
  scale_x_discrete(labels = axis_labels)+
  guides(fill=guide_legend(title="New Legend Title"))+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
         legend.position = "none")


#ggplot code boxplots:

#filtering data for only female observations
female_data_original <- island_data%>% filter(island_data$Gender == "F")

#making the female data only Control
female_data<- female_data_original %>% filter(female_data_original$Drug == "Control")

#box-plot for pre-treatment Coordination control 
ggplot(female_data, aes(x=factor(female_data$Dosage), y=female_data$`Pre Coordination`, fill = factor(female_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Pre-Treatment Coordination")+
  ylab("Pre-Coordination(dribbles)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13))


#box-plot for post-treatment post-Coordination control 
ggplot(female_data, aes(x=factor(female_data$Dosage), y=female_data$`Post Coordination`, fill = factor(female_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Post-Treatment Coordination")  +
  ylab("Post Cordination(dribbles)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#box-plot for pre-treatment performance control 
ggplot(female_data, aes(x=factor(female_data$Dosage), y=female_data$`Pre Performance`, fill = factor(female_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Pre-Treatment Performance")  +
  ylab("Pre-Performance(seconds)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#box-plot for post-treatment performance control 
ggplot(female_data, aes(x=factor(female_data$Dosage), y=female_data$`Post Performance`, fill = factor(female_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Post-Treatment Performance")  +
  ylab("Post-Performance(seconds)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#filtering data for female THC
female_data<- female_data_original %>% filter(female_data_original$Drug == "THC")

#box-plot for pre-treatment Coordination THC 
ggplot(female_data, aes(x=factor(female_data$Dosage), y=female_data$`Pre Coordination`, fill = factor(female_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Pre-Treatment Coordination")+
  ylab("Pre-Coordination(dribbles)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13))


#box-plot for post-treatment post-Coordination THC 
ggplot(female_data, aes(x=factor(female_data$Dosage), y=female_data$`Post Coordination`, fill = factor(female_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Post-Treatment Coordination")  +
  ylab("Post Cordination(dribbles)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#box-plot for pre-treatment performance THC 
ggplot(female_data, aes(x=factor(female_data$Dosage), y=female_data$`Pre Performance`, fill = factor(female_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Pre-Treatment Performance")  +
  ylab("Pre-Performance(seconds)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#box-plot for post-treatment performance THC 
ggplot(female_data, aes(x=factor(female_data$Dosage), y=female_data$`Post Performance`, fill = factor(female_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Post-Treatment Performance")  +
  ylab("Post-Performance(seconds)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )


#filtering data for female Alcohol
female_data<- female_data_original %>% filter(female_data_original$Drug == "Alcohol")

#box-plot for pre-treatment Coordination Alcohol 
ggplot(female_data, aes(x=factor(female_data$Dosage), y=female_data$`Pre Coordination`, fill = factor(female_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Pre-Treatment Coordination")+
  ylab("Pre-Coordination(dribbles)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13))


#box-plot for post-treatment post-Coordination Alcohol 
ggplot(female_data, aes(x=factor(female_data$Dosage), y=female_data$`Post Coordination`, fill = factor(female_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Post-Treatment Coordination")  +
  ylab("Post Cordination(dribbles)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#box-plot for pre-treatment performance Alcohol 
ggplot(female_data, aes(x=factor(female_data$Dosage), y=female_data$`Pre Performance`, fill = factor(female_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Pre-Treatment Performance")  +
  ylab("Pre-Performance(seconds)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#box-plot for post-treatment performance Alcohol 
ggplot(female_data, aes(x=factor(female_data$Dosage), y=female_data$`Post Performance`, fill = factor(female_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Post-Treatment Performance")  +
  ylab("Post-Performance(seconds)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )





#filtering data for only male observations
male_data_original <- island_data%>% filter(island_data$Gender == "M")

#making the male data only Control
male_data<- male_data_original %>% filter(male_data_original$Drug == "Control")

#box-plot for pre-treatment Coordination control 
ggplot(male_data, aes(x=factor(male_data$Dosage), y=male_data$`Pre Coordination`, fill = factor(male_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Pre-Treatment Coordination")+
  ylab("Pre-Coordination(dribbles)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13))


#box-plot for post-treatment post-Coordination control 
ggplot(male_data, aes(x=factor(male_data$Dosage), y=male_data$`Post Coordination`, fill = factor(male_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Post-Treatment Coordination")  +
  ylab("Post Cordination(dribbles)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#box-plot for pre-treatment performance control 
ggplot(male_data, aes(x=factor(male_data$Dosage), y=male_data$`Pre Performance`, fill = factor(male_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Pre-Treatment Performance")  +
  ylab("Pre-Performance(seconds)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#box-plot for post-treatment performance control 
ggplot(male_data, aes(x=factor(male_data$Dosage), y=male_data$`Post Performance`, fill = factor(male_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Post-Treatment Performance")  +
  ylab("Post-Performance(seconds)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )


#making the male data only THC
male_data<- male_data_original %>% filter(male_data_original$Drug == "THC")

#box-plot for pre-treatment Coordination THC 
ggplot(male_data, aes(x=factor(male_data$Dosage), y=male_data$`Pre Coordination`, fill = factor(male_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Pre-Treatment Coordination")+
  ylab("Pre-Coordination(dribbles)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13))


#box-plot for post-treatment post-Coordination THC 
ggplot(male_data, aes(x=factor(male_data$Dosage), y=male_data$`Post Coordination`, fill = factor(male_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Post-Treatment Coordination")  +
  ylab("Post Cordination(dribbles)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#box-plot for pre-treatment performance THC 
ggplot(male_data, aes(x=factor(male_data$Dosage), y=male_data$`Pre Performance`, fill = factor(male_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Pre-Treatment Performance")  +
  ylab("Pre-Performance(seconds)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#box-plot for post-treatment performance THC 
ggplot(male_data, aes(x=factor(male_data$Dosage), y=male_data$`Post Performance`, fill = factor(male_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Post-Treatment Performance")  +
  ylab("Post-Performance(seconds)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )



#making the male data only Alcohol
male_data<- male_data_original %>% filter(male_data_original$Drug == "Alcohol")

#box-plot for pre-treatment Coordination Alcohol 
ggplot(male_data, aes(x=factor(male_data$Dosage), y=male_data$`Pre Coordination`, fill = factor(male_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Pre-Treatment Coordination")+
  ylab("Pre-Coordination(dribbles)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13))


#box-plot for post-treatment post-Coordination Alcohol 
ggplot(male_data, aes(x=factor(male_data$Dosage), y=male_data$`Post Coordination`, fill = factor(male_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Post-Treatment Coordination")  +
  ylab("Post Cordination(dribbles)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#box-plot for pre-treatment performance Alcohol 
ggplot(male_data, aes(x=factor(male_data$Dosage), y=male_data$`Pre Performance`, fill = factor(male_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Pre-Treatment Performance")  +
  ylab("Pre-Performance(seconds)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )

#box-plot for post-treatment performance Alcohol 
ggplot(male_data, aes(x=factor(male_data$Dosage), y=male_data$`Post Performance`, fill = factor(male_data$Dosage))) + geom_boxplot()+
  ggtitle("Box-Plot Post-Treatment Performance")  +
  ylab("Post-Performance(seconds)" )+
  xlab("Dosage" )+
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size=20, face = "bold.italic"),
         axis.title.y.left = element_text(size = 13),
         axis.title.x.bottom = element_text(size=13),
  )


