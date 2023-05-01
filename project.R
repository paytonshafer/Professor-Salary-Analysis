library(tidyverse)
library(ggplot2)

#read file
salary = as_tibble(read.table("/Users/paytonshafer/Desktop/STAT383/Project/salary.txt"))
#rename columns
salary = rename(salary, "sx" = V1, "rk" = V2, "yr" = V3, "dg" = V4, "yd" = V5, "sl" = V6)
#removes top column with column names
salary = salary[-c(1),]

#when it is read in it is read in as a char so we have to turn them to ints
salary$sl = as.integer(salary$sl)
#checks the class
class(salary$sl)
#does it again for yr and yd
salary$yr = as.integer(salary$yr)
class(salary$yr)
salary$yd = as.integer(salary$yd)
class(salary$yr)

#changing sex, rank and degree to numerical values
#sex
sex = salary$sx
salary$sx = as.integer(salary$sx)
count = 1
salary$sx[0] = 0
for (i in sex){
  if(i == "male"){
    salary$sx[count] = 0
  }else{
    salary$sx[count] = 1
  }
  count = count + 1
}
salary$sx = as.integer(salary$sx)


#rank
rank = salary$rk
salary$rk = as.integer(salary$rk)
count = 1
salary$rk[0] = 3
for (i in rank){
  if(i == "assistant"){
    salary$rk[count] = 1
  }else if(i == "associate"){
    salary$rk[count] = 2
  }else{
    salary$rk[count] = 3
  }
  count = count + 1
}
salary$rk = as.integer(salary$rk)
class(salary$rk)

#degree
degree = salary$dg
salary$dg = as.integer(salary$dg)
count = 1
salary$dg[0] = 1
for (i in degree){
  if(i == "masters"){
    salary$dg[count] = 0
  }else{
    salary$dg[count] = 1
  }
  count = count + 1
}
salary$dg = as.integer(salary$dg)
class(salary$dg)

salary

#Checking lm assumptions
#sl vs yr
#show scatter plot
ggplot(salary, aes(yr, sl)) + geom_point()
#Check for correlation
cor.test(salary$sl,salary$yr)
#create model
slVyr = lm(sl ~ yr, salary)

#hypothesis test
print("{H0:B1 = 0, H1:B1 not= 0}")
summary(slVyr)
print("At a level of alpha = .01, since 7.34e-09 < .01 there is enough evidence to reject the null")

#check linearity and constant variance
plot(slVyr, 1)
#check epsilon normally distributed
plot(slVyr,2)
#check for outliers
plot(slVyr,5)

#sl vs yd
#show scatter plot
ggplot(salary, aes(yd, sl)) + geom_point()
#Check for correlation
cor.test(salary$sl,salary$yd)
#create model
slVyd = lm(sl ~ yd, salary)

#hypothesis test
print("{H0:B1 = 0, H1:B1 not= 0}")
summary(slVyd)
print("At a level of alpha = .01, since 4.1e-08 < .01 there is enough evidence to reject the null")

#check linearity and constant variance
plot(slVyd, 1)
#check epsilon normally distributed
plot(slVyd,2)
#check for outliers
plot(slVyd,5)

#show lm of sl vs yr and sl vs yd
#lm of sl vs yr
ggplot(salary, aes(yr, sl)) + geom_point() + geom_abline(intercept = 18166.1, slope = 752.8) + 
    labs(title = "Salary vs Years at Rank", x = "Years at Rank", y = "Salary")

#lm of sl vs yd
ggplot(salary, aes(yd, sl)) + geom_point() + geom_abline(intercept = 17502.26, slope = 390.65, col = "blue") + 
    labs(title = "Salary vs Years at Degree", x = "Years at Degree", y = "Salary")

#summerize by sex, degree, and rank
#sl vs yr by sx
salary %>%
  filter(sx == 0) %>%
  ggplot(aes(yr, sl)) +  geom_point() + geom_abline(intercept = 18166.1, slope = 752.8) + 
  labs(title = "Male: Salary vs Years at Rank", x = "Years at Rank", y = "Salary")

salary %>%
  filter(sx == 1) %>%
  ggplot(aes(yr, sl)) +  geom_point() + geom_abline(intercept = 18166.1, slope = 752.8) + 
  labs(title = "Female: Salary vs Years at Rank", x = "Years at Rank", y = "Salary")

ggplot(salary, aes(yr, sl)) + geom_point() + geom_abline(intercept = 18166.1, slope = 752.8) + 
  labs(title = "Both Sexes: Salary vs Years at Rank", x = "Years at Rank", y = "Salary") + facet_wrap(vars(sx))

#sl vs yd by sx
salary %>% 
  filter(sx == 0) %>%
    ggplot(aes(yd, sl)) + geom_point() + geom_abline(intercept = 17502.26, slope = 390.65, col = "blue") + 
      labs(title = "Male: Salary vs Years at Degree", x = "Years at Degree", y = "Salary")

salary %>%
  filter(sx == 1) %>%
    ggplot(aes(yd, sl)) + geom_point() + geom_abline(intercept = 17502.26, slope = 390.65, col = "blue") + 
      labs(title = "Female: Salary vs Years at Degree", x = "Years at Degree", y = "Salary")

ggplot(salary, aes(yd, sl)) + geom_point() + geom_abline(intercept = 17502.26, slope = 390.65, col = "blue") + 
  labs(title = "Both Sexes: Salary vs Years at Degree", x = "Years at Degree", y = "Salary") + facet_wrap(vars(sx))

#sl vs yr by rank
salary %>%
  filter(rk == 1) %>%
  ggplot(aes(yr, sl)) +  geom_point() + geom_abline(intercept = 18166.1, slope = 752.8) + 
  labs(title = "Assistant: Salary vs Years at Rank", x = "Years at Rank", y = "Salary")

salary %>%
  filter(rk == 2) %>%
  ggplot(aes(yr, sl)) +  geom_point() + geom_abline(intercept = 18166.1, slope = 752.8) + 
  labs(title = "Associate: Salary vs Years at Rank", x = "Years at Rank", y = "Salary")

salary %>%
  filter(rk == 3) %>%
  ggplot(aes(yr, sl)) +  geom_point() + geom_abline(intercept = 18166.1, slope = 752.8) + 
  labs(title = "Full: Salary vs Years at Rank", x = "Years at Rank", y = "Salary")

ggplot(salary, aes(yr, sl)) + geom_point() + geom_abline(intercept = 18166.1, slope = 752.8) + 
  labs(title = "All Ranks: Salary vs Years at Rank", x = "Years at Rank", y = "Salary") + facet_wrap(vars(rk))

#sl vs yd by rank
salary %>% 
  filter(rk == 1) %>%
  ggplot(aes(yd, sl)) + geom_point() + geom_abline(intercept = 17502.26, slope = 390.65, col = "blue") + 
  labs(title = "Assistant: Salary vs Years at Degree", x = "Years at Degree", y = "Salary")

salary %>%
  filter(rk == 2) %>%
  ggplot(aes(yd, sl)) + geom_point() + geom_abline(intercept = 17502.26, slope = 390.65, col = "blue") + 
  labs(title = "Associate: Salary vs Years at Degree", x = "Years at Degree", y = "Salary")

salary %>%
  filter(rk == 3) %>%
  ggplot(aes(yd, sl)) + geom_point() + geom_abline(intercept = 17502.26, slope = 390.65, col = "blue") + 
  labs(title = "Full: Salary vs Years at Degree", x = "Years at Degree", y = "Salary")

ggplot(salary, aes(yd, sl)) + geom_point() + geom_abline(intercept = 17502.26, slope = 390.65, col = "blue") + 
  labs(title = "All Ranks: Salary vs Years at Degree", x = "Years at Degree", y = "Salary") + facet_wrap(vars(rk))

#sl vs yr by dg
salary %>%
  filter(dg == 0) %>%
  ggplot(aes(yr, sl)) +  geom_point() + geom_abline(intercept = 18166.1, slope = 752.8) + 
  labs(title = "Masters: Salary vs Years at Rank", x = "Years at Rank", y = "Salary")

salary %>%
  filter(dg == 1) %>%
  ggplot(aes(yr, sl)) +  geom_point() + geom_abline(intercept = 18166.1, slope = 752.8) + 
  labs(title = "Doctarate: Salary vs Years at Rank", x = "Years at Rank", y = "Salary")

ggplot(salary, aes(yr, sl)) + geom_point() + geom_abline(intercept = 18166.1, slope = 752.8) + 
  labs(title = "Both Degrees: Salary vs Years at Rank", x = "Years at Rank", y = "Salary") + facet_wrap(vars(dg))

#sl vs yd by dg
salary %>% 
  filter(dg == 0) %>%
  ggplot(aes(yd, sl)) + geom_point() + geom_abline(intercept = 17502.26, slope = 390.65, col = "blue") + 
  labs(title = "Masters: Salary vs Years at Degree", x = "Years at Degree", y = "Salary")

salary %>%
  filter(dg == 1) %>%
  ggplot(aes(yd, sl)) + geom_point() + geom_abline(intercept = 17502.26, slope = 390.65, col = "blue") + 
  labs(title = "Doctarate: Salary vs Years at Degree", x = "Years at Degree", y = "Salary")

ggplot(salary, aes(yd, sl)) + geom_point() + geom_abline(intercept = 17502.26, slope = 390.65, col = "blue") + 
  labs(title = "Both Degrees: Salary vs Years at Degree", x = "Years at Degree", y = "Salary") + facet_wrap(vars(dg))

#sl vs dg and rk

#sl vs dg
ggplot(salary, aes(dg, sl)) + geom_point() +
  labs(title = "Salary vs Degree", subtitle = "0 = Masters, 1 = Doctorate", x = "Degree", y = "Salary")
mast = salary %>% filter(dg == 0) 
ggplot(mast, aes(dg, sl)) + geom_point() +
  labs(title = "Salaries of Professors with Masters", x = "Professors with Masters", y = "Salary")
doct = salary %>% filter(dg == 1 ) %>% filter(yd > 5) #filter out bc they dragged salary down
ggplot(doct, aes(dg, sl)) + geom_point() +
  labs(title = "Salaries of Professors with Doctorate", x = "Professors with Doctorate", y = "Salary")
mean(mast$sl)
mean(doct$sl)

#all degrees by sx
ggplot(salary, aes(dg, sl)) + geom_point() + facet_wrap(vars(sx)) +
  labs(title = "Salary vs Degree", subtitle = "0 = Masters, 1 = Doctorate (0 Graph for Males, 1 Graph for Females)", x = "Degree", y = "Salary")

#sl vs mast males
mast = mast %>% filter(sx == 0)
ggplot(mast, aes(dg, sl)) + geom_point() + 
  labs(title = "Salaries of Male Professors with Masters", x = "Male Professors with Masters", y = "Salary")
mean(mast$sl)
#sl vs mast females
mast = salary %>% filter(dg == 0) %>% filter(sx == 1)
ggplot(mast, aes(dg, sl)) + geom_point() + 
  labs(title = "Salaries of Female Professors with Masters", x = "Female Professors with Masters", y = "Salary")
mean(mast$sl)

#sl vs doct males
doct = salary %>% filter(dg == 1) %>% filter(sx == 0)
ggplot(doct, aes(dg, sl)) + geom_point() + 
  labs(title = "Salaries of Male Professors with Doctorate", x = "Male Professors with Doctorate", y = "Salary")
mean(doct$sl)
#sl vs doct females
doct = salary %>% filter(dg == 1) %>% filter(sx == 1)
ggplot(doct, aes(dg, sl)) + geom_point() + 
  labs(title = "Salaries of Female Professors with Doctorate", x = "Female Professors with Doctorate", y = "Salary")
mean(doct$sl)

#sl vs rk
ggplot(salary, aes(rk, sl)) + geom_point() + 
  labs(title = "Salary vs Rank", subtitle = "1 = Assistant, 2 = Associate, 3 = Full", x = "Rank", y = "Salary")
assis = salary %>% filter(rk == 1)
ggplot(assis, aes(rk, sl)) + geom_point() + 
  labs(title = "Salary vs Rank", subtitle = "1 = Assistant, 2 = Associate, 3 = Full", x = "Rank", y = "Salary")
mean(assis$sl)
assoc = salary %>% filter(rk == 2)
ggplot(assoc, aes(rk, sl)) + geom_point() + 
  labs(title = "Salary vs Rank", subtitle = "1 = Assistant, 2 = Associate, 3 = Full", x = "Rank", y = "Salary")
mean(assoc$sl)
full = salary %>% filter(rk == 3)
ggplot(full, aes(rk, sl)) + geom_point() + 
  labs(title = "Salary vs Rank", subtitle = "1 = Assistant, 2 = Associate, 3 = Full", x = "Rank", y = "Salary")
mean(full$sl)

#All ranks by sex
ggplot(salary, aes(rk, sl)) + geom_point() + facet_wrap(vars(sx)) +
  labs(title = "Salary vs Rank", subtitle = "1 = Assistant, 2 = Associate, 3 = Full (0 Graph for Males, 1 Graph for Females)", x = "Rank", y = "Salary")

#sl vs assis males 
assis = assis %>% filter(sx == 0)
ggplot(assis, aes(rk, sl)) + geom_point() +
  labs(title = "Salaries of Male Assistant Professors", x = "Male Assistant Professors", y = "Salary")
mean(assis$sl)
#sl vs assis females 
assis = salary %>% filter(rk == 1) %>% filter(sx == 1)
ggplot(assis, aes(rk, sl)) + geom_point() +
  labs(title = "Salaries of Female Assistant Professors", x = "Female Assistant Professors", y = "Salary")
mean(assis$sl)

#sl vs assoc males 
assoc = assoc %>% filter(sx == 0)
ggplot(assoc, aes(rk, sl)) + geom_point() +
  labs(title = "Salaries of Male Associate Professors", x = "Male Associate Professors", y = "Salary")
mean(assoc$sl)
#sl vs assoc females 
assoc = salary %>% filter(rk == 2) %>% filter(sx == 1)
ggplot(assoc, aes(rk, sl)) + geom_point() +
  labs(title = "Salaries of Female Associate Professors", x = "Female Associate Professors", y = "Salary")
mean(assoc$sl)

#sl vs full males 
full = full %>% filter(sx == 0)
ggplot(full, aes(rk, sl)) + geom_point() +
  labs(title = "Salaries of Male Full Professors", x = "Male Full Professors", y = "Salary")
mean(full$sl)
#sl vs full females
full = salary %>% filter(rk == 3) %>% filter(sx == 1)
ggplot(full, aes(rk, sl)) + geom_point() +
  labs(title = "Salaries of Female Full Professors", x = "Female Full Professors", y = "Salary")
mean(full$sl)

#get the mean of salaries
mean = mean(salary$sl)