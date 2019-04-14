#POISSON REGRESSION
library(sandwich)
library(ggplot2)

#read in data
Data2 = read.csv2("C://Gent/SLIP_extra_and_naming_control/SLIP_2/Final_Analyses/SLIP_combined_Poisson.csv", header = TRUE, sep = ";")

#set factor categories
Data2$Subject <- as.factor(Data2$Subject)
Data2$Experiment <- as.factor(Data2$Experiment)

#Poisson regression for the entire data set
model_comb <- glm(Num_Mistakes ~ Experiment*Outcome*Language, family = "poisson", data = Data2)
summary(model_comb) 

#subset data by language
Data2_L1 <- subset(Data2, Language == "L1")
Data2_L2 <- subset(Data2, Language == "L2")

#L1
L1_model <- glm(Num_Mistakes ~ Experiment*Outcome, family = "poisson", data = Data2_L1)
summary(L1_model)

#L2
L2_model <- glm(Num_Mistakes ~ Experiment*Outcome , family = "poisson", data = Data2_L2)
summary(L2_model)


#subset by block
L1_exp1 <- subset(Data2_L1, Experiment == "1")
L1_exp2 <- subset(Data2_L1, Experiment == "2")
L2_exp1 <- subset(Data2_L2, Experiment == "1")
L2_exp2 <- subset(Data2_L2, Experiment == "2")


#L1 exp 1
L1_exp1_model <- glm(Num_Mistakes ~ Outcome, family = "poisson", data = L1_exp1)
summary(L1_exp1_model)
#p < .001

#L1 exp 2
L1_exp2_model <- glm(Num_Mistakes ~ Outcome, family = "poisson", data = L1_exp2)
summary(L1_exp2_model)
#p = 0.0898

#L2 non-word target
L2_exp1_model <- glm(Num_Mistakes ~ Outcome, family = "poisson", data = L2_exp1)
summary(L2_exp1_model)
#p = 0.578

#L2 word target
L2_exp2_model <- glm(Num_Mistakes ~ Outcome, family = "poisson", data = L2_exp2)
summary(L2_exp2_model)
#p = .0168


#graph
Errors = read.csv2("C://Gent/SLIP_exp2/Analyses/Excel/SLIP_mistakes_nonword_mixed_exp1andexp2.csv", header = TRUE, sep = ";")

exp1_2 <- ggplot(data=Errors, aes(x=Lang_exp, y=Full_and_Partial_Exchanges, fill=Outcome)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(limits=c(0, 45)) +
  theme_bw() +
  theme (plot.title = element_text(hjust = 0.5)) +
  ggtitle("Number of Mistakes")

exp1_2 + scale_fill_grey(start = 0, end = .9)
