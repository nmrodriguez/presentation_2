library(bbmle)

#NEGATIVE BINOMIAL REGRESSION (by subject)
#read in data file
Error_data = read.csv2("C://Gent/SLIP_extra_and_naming_control/SLIP_2/Final_Analyses/SLIP_2_data_only_switches.csv", 
                       header = TRUE, sep = ";")

#correctly categorise Subject
Error_data$Subject <- as.factor(Error_data$Subject)


#model for the entire data set
model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
              data=Error_data, parameters = list(logmu~Target*Outcome*Language),
              start=list(logmu=0,logk=0))

summary(model)  

#subset data by language
L1_error_data <- subset(Error_data, Language == "L1")
L2_error_data <- subset(Error_data, Language == "L2")

#subset by block
L1_nw_error_data <- subset(L1_error_data, Target == "Non-word")
L1_w_error_data <- subset(L1_error_data, Target == "Word")
L2_nw_error_data <- subset(L2_error_data, Target == "Non-word")
L2_w_error_data <- subset(L2_error_data, Target == "Word")


#L1
L1_model1 <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                  data=L1_error_data, parameters = list(logmu~Target*Outcome),
                  start=list(logmu=0,logk=0))

summary(L1_model1)

#L2
L2_model1 <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                  data=L2_error_data, parameters = list(logmu~Target*Outcome + Lextale),
                  start=list(logmu=0,logk=0))

summary(L2_model1)


#L1 non-word target
L1_nw_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                    data=L1_nw_error_data, parameters = list(logmu~Outcome),
                    start=list(logmu=0,logk=0))

summary(L1_nw_model)
#Outcome -> p = .15

#L1 word target
L1_w_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                   data=L1_w_error_data, parameters = list(logmu~Outcome),
                   start=list(logmu=0,logk=0))

summary(L1_w_model)
#Outcome -> p = .33

#L2 non-word target
L2_nw_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                    data=L2_nw_error_data, parameters = list(logmu~Outcome + Lextale),
                    start=list(logmu=0,logk=0))

summary(L2_nw_model)
#Outcome -> p = .02
#Lextale -> p = .57

#L2 word target
L2_w_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                   data=L2_w_error_data, parameters = list(logmu~Outcome + Lextale),
                   start=list(logmu=0,logk=0))

summary(L2_w_model)
#Outcome -> p < .001
#Lextale -> p = .015








#graph of observed values
L1L2_mistakes_2 = read.csv2("C://Gent/SLIP_exp2/Analyses/Excel/SLIP_2_L1L2_mistakes_table.csv", header = TRUE, sep = ";")

p_L1L2 <- ggplot(data=L1L2_mistakes_2, aes(x=Target, y=Full_and_Partial_Exchanges, fill=Outcome)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(limits=c(0, 45)) +
  theme_bw() +
  ggtitle("Number of Mistakes")

p_L1L2 + scale_fill_grey(start = 0, end = .9)













#NEGATIVE BINOMIAL REGRESSION (by item) -> same pattern of results as by subject
#read in data file
Error_data = read.csv2("C://Gent/SLIP_exp2/Final_Analyses/SLIP_2_by_item_nbr.csv", 
                       header = TRUE, sep = ";")


#model for the entire data set
model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
              data=Error_data, parameters = list(logmu~Target*Outcome*Language),
              start=list(logmu=0,logk=0))

summary(model)  

#subset data by language
L1_error_data <- subset(Error_data, Language == "L1")
L2_error_data <- subset(Error_data, Language == "L2")

#subset by block
L1_nw_error_data <- subset(L1_error_data, Target == "Non-lexical")
L1_w_error_data <- subset(L1_error_data, Target == "Lexical")
L2_nw_error_data <- subset(L2_error_data, Target == "Non-lexical")
L2_w_error_data <- subset(L2_error_data, Target == "Lexical")


#L1
L1_model1 <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                  data=L1_error_data, parameters = list(logmu~Target*Outcome),
                  start=list(logmu=0,logk=0))

summary(L1_model1)

#L2
L2_model1 <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                  data=L2_error_data, parameters = list(logmu~Target*Outcome),
                  start=list(logmu=0,logk=0))

summary(L2_model1)


#L1 non-word target
L1_nw_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                    data=L1_nw_error_data, parameters = list(logmu~Outcome),
                    start=list(logmu=0,logk=0))

summary(L1_nw_model)
#Outcome -> p = .22

#L1 word target
L1_w_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                   data=L1_w_error_data, parameters = list(logmu~Outcome),
                   start=list(logmu=0,logk=0))

summary(L1_w_model)
#Outcome -> p = .40

#L2 non-word target
L2_nw_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                    data=L2_nw_error_data, parameters = list(logmu~Outcome),
                    start=list(logmu=0,logk=0))

summary(L2_nw_model)
#Outcome -> p = .04

#L2 word target
L2_w_model <- mle2(Num_Mistakes ~ dnbinom(mu=exp(logmu),size=exp(logk)), 
                   data=L2_w_error_data, parameters = list(logmu~Outcome),
                   start=list(logmu=0,logk=0))

summary(L2_w_model)
#Outcome -> p = .002









