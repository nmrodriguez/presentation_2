#NEGATIVE BINOMIAL REGRESSION (NBR) by subject
library(MASS)
library(ggplot2)
library(bblme)

#read in data set
SLIP_1_error_data = read.csv2("C://Gent/SLIP_extra_and_naming_control/SLIP_1/Final_Analyses/SLIP_1_data_only_switches_Poisson.csv", header = TRUE, sep = ";")

#correctly categorise Subject
SLIP_1_error_data$Subject <- as.factor(SLIP_1_error_data$Subject)

#model for the entire data set
SLIP1_model <- glm.nb(Num_Mistakes ~ Context*Outcome*Language, data = SLIP_1_error_data)
#warning message: related to print.summary method trying to use the standard error of theta 
#to determine the format, but that's NA 
#(see http://r.789695.n4.nabble.com/R-Error-Warning-Messages-with-library-MASS-using-glm-td4556771.html)
#so here we use the package bblme to counter the problem

library(bbmle)
SLIP1_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                    data=SLIP_1_error_data, parameters = list(logmu~Context*Outcome*Language),
                    start=list(logmu=0,logk=0))

summary(SLIP1_model) 

#subset data by language
SLIP_1_L1_error_data <- subset(SLIP_1_error_data, Language == "L1")
SLIP_1_L2_error_data <- subset(SLIP_1_error_data, Language == "L2")

SLIP_1_L1_error_data$Subject <- as.factor(SLIP_1_L1_error_data$Subject)
SLIP_1_L2_error_data$Subject <- as.factor(SLIP_1_L2_error_data$Subject)
SLIP_1_L2_error_data$Lextale <- scale(SLIP_1_L2_error_data$Lextale,scale = FALSE)

#L1 model
SLIP1_L1model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                      data=SLIP_1_L1_error_data, parameters = list(logmu~Context*Outcome),
                      start=list(logmu=0,logk=0))

summary(SLIP1_L1model) 

#L2 model
SLIP1_L2model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                      data=SLIP_1_L2_error_data, parameters = list(logmu~Context*Outcome + Lextale),
                      start=list(logmu=0,logk=0))

summary(SLIP1_L2model) 

#subset by block
L1_mix <- subset(SLIP_1_L1_error_data, Context == "Mixed")
L1_nl <- subset(SLIP_1_L1_error_data, Context == "Non-lexical")
L2_mix <- subset(SLIP_1_L2_error_data, Context == "Mixed")
L2_nl <- subset(SLIP_1_L2_error_data, Context == "Non-lexical")


#L1 mixed block
L1_mix_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                     data=L1_mix, parameters = list(logmu~Outcome),
                     start=list(logmu=0,logk=0))

summary(L1_mix_model)
#Outcome -> p < .001

#L1 non-lexical block
L1_nl_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                    data=L1_nl, parameters = list(logmu~Outcome),
                    start=list(logmu=0,logk=0))

summary(L1_nl_model)
#Outcome -> p = .04

#L2 mixed block
L2_mix_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                     data=L2_mix, parameters = list(logmu~Outcome),
                     start=list(logmu=0,logk=0))

summary(L2_mix_model)
#Outcome -> p = .59
#Lextale -> p = .65

#L2 non-lexical block
L2_nl_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                    data=L2_nl, parameters = list(logmu~Outcome + Lextale),
                    start=list(logmu=0,logk=0))

summary(L2_nl_model)
#Outcome -> p = .85
#Lextale -> p = .57


#graph observed values
L1L2_mistakes = read.csv2("C://Gent/SLIP_extra_and_naming_control/SLIP_1/Analysis/L1L2_mistakes_table_extra.csv", header = TRUE, sep = ";")

p_L1L2 <- ggplot(data=L1L2_mistakes, aes(x=Context, y=Full_and_Partial_Exchanges, fill=Outcome)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(limits=c(0, 45)) +
  theme_bw() +
  ggtitle("Number of Mistakes")

p_L1L2 + scale_fill_grey(start = 0, end = .9)















#NEGATIVE BINOMIAL REGRESSION (NBR) by item -> same pattern of results as by subject
library(MASS)
library(ggplot2)
library(bblme)

#read in data set
SLIP_1_error_data = read.csv2("C://Gent/SLIP_Article/Final_Analyses/SLIP_1_by_item_nbr.csv", header = TRUE, sep = ";")


SLIP1_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                    data=SLIP_1_error_data, parameters = list(logmu~Context*Outcome*Language),
                    start=list(logmu=0,logk=0))

summary(SLIP1_model) 

#subset data by language
SLIP_1_L1_error_data <- subset(SLIP_1_error_data, Language == "L1")
SLIP_1_L2_error_data <- subset(SLIP_1_error_data, Language == "L2")


#L1 model
SLIP1_L1model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                      data=SLIP_1_L1_error_data, parameters = list(logmu~Context*Outcome),
                      start=list(logmu=0,logk=0))

summary(SLIP1_L1model) 

#L2 model
SLIP1_L2model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                      data=SLIP_1_L2_error_data, parameters = list(logmu~Context*Outcome),
                      start=list(logmu=0,logk=0))

summary(SLIP1_L2model) 

#subset by block
L1_mix <- subset(SLIP_1_L1_error_data, Context == "Mixed")
L1_nl <- subset(SLIP_1_L1_error_data, Context == "Non-lexical")
L2_mix <- subset(SLIP_1_L2_error_data, Context == "Mixed")
L2_nl <- subset(SLIP_1_L2_error_data, Context == "Non-lexical")


#L1 mixed block
L1_mix_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                     data=L1_mix, parameters = list(logmu~Outcome),
                     start=list(logmu=0,logk=0))

summary(L1_mix_model)
#Outcome -> p < .001

#L1 non-lexical block
L1_nl_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                    data=L1_nl, parameters = list(logmu~Outcome),
                    start=list(logmu=0,logk=0))

summary(L1_nl_model)
#Outcome -> p = .046

#L2 mixed block
L2_mix_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                     data=L2_mix, parameters = list(logmu~Outcome),
                     start=list(logmu=0,logk=0))

summary(L2_mix_model)
#Outcome -> p = .65

#L2 non-lexical block
L2_nl_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                    data=L2_nl, parameters = list(logmu~Outcome),
                    start=list(logmu=0,logk=0))

summary(L2_nl_model)
#Outcome -> p = .85




