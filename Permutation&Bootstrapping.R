#1. procedures of permutation, permutation test also called randomization or re-randomization

#a. calculate the observed t statistic as in the parametric approach; call this t0
#b. palce all scores in a single group
#c. randomly assign half to Treatment A and another half to Treatment B
#d. calculate and record the new observed t statistic
#e. repeat step 3-4 for every possible way of assigning five scores to Treatment A and five scores to Treatment B. There are nC(0.5n)  such possible arrangement
#f. arrange the all t statistics in ascending order and this is the empirical distribution based on the sample data
#g. if t0 falls outside the middle 95% of the empirical distribution, reject the null hypothesis that the population means for the two groups are equal at the 0.05 level of significance

#this permutation test that was based on all possible permutations of the data is called an exact test
#as the sample size increases, the time required to form all possible permutations can become prohibitive, in this case, we can use Monte Carlo simulation to sample from all possible permutations
#doing so provides an approximate test

#if we are uncomfortable assuming that the data is normally distributed, concerned about the impact of outliers or feel that the dataset is too small for standard paramteric approaches
#the permutation test provides an excellemnt alternative

#there are so many excellent packages to do it, such as the coin package and the lmPerm package
#coin provides a comprehensive framework for permuatation tests applied to independence problems whereas the lmPerm provides permutation tests for ANOVA and regression designs

#2. coin package and lmPerm package

install.packages("coin")

#lmPerm shoud be downloaded from the file lmPerm_1.1-2.tar.gz from http://cran.r-project.org/src/contrib/Archive/lmPerm/
install.packages(file.choose(), repos=NULL, type="source")
#from within R when a dialog box pops up, find and choose the lmPerm_1.1-2.tar.gz file and will install the package on our machine

#it is importnat to remember that permuattion tests use pseudo random numbers to sample from all possible permuatations, therefore the results will change each time the test is performed, so we should set the random number seed 

#3. permutation tests with the coin package

#coin package provides to test the independence problems
#are two numeric variables or categorical variables independent or are responses independent of group assignment?

#coin functions providing permutation test alternatives to tradition tests through functions like:
#two- and k-sample permutation test  oneway_test(y~A)
#Wilcoxon-Mann-Whitney rank-sum test wilcox_test(y~A)
#Kruskal-Wallis test                 kruskal_test(y~A)
#Pearson's chi-square test           chisq_test(A~B)
#cochran-Mantel-Haenszel test        cmh_test (A~B|C)
#Linear-by-linear association test   lbl_test(D~E)
#Spearman's test                     spearman_test(y~x)
#Friedman test                       friedman_test(y~A |C)
#Wilcoxon signed-rank test          wilcoxsign_test(y1~y2)

#y and x are numeric variables, A and B are C is a categorical blocking variable, D and E are ordred factors and y1 and y2 are matched numeric variables

function_name(formula, data, distribution=) #distribution specifies how the empirical distribution under the null hypothesis should 

#if ditribution="exact, the distribution under the null hypothesis is computed exactly(that is from all possible permutations)
#it also can be approximated by its asymptotic distribution or via Monte Carlo resampling(distribution="approximate(B=#)") where # indicates the number of replications used to approximate the exact distribution
#distribution="exact" now is only available for two-sample problems

#in the coin package, categorical variables and ordinal variables must be coded as factors and ordered factors, respectively. Additionally, the data must be stored in a data frame


#4. independent two-sample and k-sample tests

library(coin)
score <-c(40,57,45,55,58,57,64,55,62,65)
treatment <-factor(c(rep("A",5), rep("B",5)))
mydata <-data.frame(treatment, score)
t.test(score~treatment, data=mydata, var.equal=T)
#the traditional t-test indicates a significant group difference(p<.05) whereas the exact test doesn;t(p>.072). With only 10 observations, we would believe the later
oneway_test(score~treatment, data=mydata, distribution+"exact")

#consider the Wilcoxon-Mann-Whitney which examine the difference in the probability of imprisonment in Southern versus non-Southern US states using the wilcox.yesy() function

library(MASS)
UScrime <-transform(UScrime, So=factor(So))
wilcox_test(Prob ~So, data=UScrime, distribution="exact")

#suggesting that incarceration is more likely in southern states

library(multcomp)
set.seed(1234)
oneway_test(response~trt, data=cholesterol,
            distribution=approximate(B=9999))
#the reference distribution is based on 9,999 permutations of the data.
#there is clearly a difference in response amopng patients in the various groups

#5. independence in contingency tables
# we can use permuatation tests to assess the independence of two categorical variables using either the chisq_test() or cmh_tetst() function
#the latter function is used when data is stratified on a third categorical variable
#if both variables are ordinal, we can use the lbl_test() function

library(coin)
library(vcd)
Arthritis <- transform(Arthritis, Improved=as.factor(as.numeric(Improved)))
set.seed(1234)
chisq_test(Treatment ~ Improved, data=Arthritis,
           distribution=approximate(B=9999))
#we can perform a permutation version of the chi-square test like above
#we need to transfer ordered factor to a categorical factor since if we left an ordered factor, coin() would have generated a 
#linear x linear trend test instead of a chi-square test 
#although a trend test would be a good choice in this situation

#6. indepencdence between numeric variables
#spearman_test() provides a permutation test of the independence of two numeric variables
#we have examined the correlation between illiteracy rates and murder rates for US states, we can thus test the association via permuatation
states<-as.data.frame(state.x77)
set.seed(1234)
spearman_test(Illiteracy~Murder, data=states, distribution=approximate(B=9999))
#9,999 replicatons, the p value makes that the independence can be rejected 
#x77 is a matrix, it had to be converteed into a data frame for use in the coin package

#7. dependent two-sample and k-sample tests

#wilcoxsign_test() function can be used for two-paired groups permutation test
#for more than two groups, use the friedman_test() function

library(coin)
library(MASS)
wilcoxsign_test(U1~U2,data=UScrime, distribution="exact")
#based on the results, we would conclude that the unemployment rates differ

#8. advances in coin package
#coin package provides a general framework for testing that one group of variables is independent of a second group of variables
#with optional stratification on a blocking variable against arbitrary alternatives, via approximate permutation tests

independence_test()#function lets us approach most traditional tests from a permutation perspective and create new and novel statistical test for situations not covered by traditional methods
#vignette("coin) for more novel statistical tests

#9. permutation tests with the lmPerm package
#lmPerm package provides support for linear models

#lmp() and aovp() are the lm() and aov() functions modified to perform permutation tests rather than normal theory tests
#perm= option can take the value Exact, Prob, or SPR.Exact which produces an exact test, based on all possible permytations. 
#Prob samples from all possible permutations, sampling continues untial the estimated sd falls below .1 of the estimated p value. the stopping rule is controlled by an optional Ca parameter
#SPR uses a sequential probability ratio test to dicide when to stop sampling
#if the number of observations is greater than 10, perm="Exact will automatically default to perm="Prob"; exact tests are only available for small problems


#10. simple and polynomial regression permutation test
install.packages("lmPerm")
library(lmPerm)
set.seed(1234)
fit<-lmp(weight~height, data=women, perm="Prob")
summary(fit)


#11. permutation tests for polynomial regression
library(lmPerm)
set.seed(1234)
fit <-lmp(weight~height + I(height^2), data=women, perm="Prob")
summary(fit)
#a simple matter to test these regressions using permutation tests and requires little change in the underlying code
#the output is also similar to that produced by the lm() function
#Iter column added for how many iterations were required to reach the stopping rule


#12. Multiple regression

library(lmPerm)
set.seed(1234)
states <-as.data.frame(state.x77)
fit <-lmp(Murder ~Population +Illiteracy+Income+Frost, data=states, perm="Prob")
summary(fit)
#both Population and Illiteracy are significant(p<.05) when normal theory is used but based on this permutation test, the population variable is no longer significant

#13. One-way ANOVA and ANCOVA permutation test


#permutation test for one-way ANOVA
library(lmPerm)
library(multcomp)
set.seed(1234)
fit <-aovp(response~trt, data=cholesterol, perm="Prob")
anova(fit)
#the results suggest that the treatment effects are not all equal


#permutation test for one-way ANCOVA

fit<-aovp(weight~gesttime+dose, data=litter, perm="Prob")
anova(fit)
#based on the p values, the four drug doses dont equally impact litter weights, controlling for gestation tme


#14. two way ANOVA permutation test

fit<-aovp(len~supp*dose, data=ToothGrowth, perm="Prob")
anova(fit)
#as the .05 level of significance, all three effects are statistically different from zero. at the .01 level only the main effects are significant

#when aovp() applied to ANOVA designs, it defaults to unique sums of squares(also called SAS Type III sums of squares)
#each effect is adjusted for every other effect
#the default for parametric ANOVA designs in R is sequential sums of squares(SAS Type I sums of squares)
#for balanced designs, the two approaches will agree, but for greater the imbalance, the greater the disagreement
#if desired, specifying seqs=T in the aovp() function will produce sequential sums of squares

#15. additional comments on permutation tests
#corrperm package provides permutation tests of correlations with repeated meaures
#the logregperm package offers a permutation test for logistic regression
#glmperm package extends permutation tests to generalized linear models 

#in each of the permutation tests described, we were able to test statistical hypotheses without recourse to the normal, t, F, or chi-square distributions


#if the original sample is a poor representation of the population of interest, no test, including permutation tests will improve the inferences generated

#16. bootstrapping
#generate an empirical distribution of a test statistic or set of test statistics by repeated random sampling with replacement from the orginal sample
# it allows to generate confidence intervals and test statistical hypotheses without having to assume a pecific underlying theoretical distribution

#for example, if we arent willing to assume that the sampling distribution of the mean is normally distributed
#we can use a bootstrapping approach instead
#randomly select 10 observations from the sample, with replacement after each selection
#calculate and record the sample mean
#repeat the first two steps 1,000 times
#order the 1,000 sample means representing the 2.5th and 97.5th percentiles. In this case, its the 25th number from the bottom and top. These are the 95% confidence limits


#17. bootstrapping with the boot package
# we can bootstrap a single statistic or a vector of statistics

install.packages("boot")
library(boot)

bootobject <-boot(data=, statistic=, R=,...)#statistic means that produces the k statistic to be bootstrapped(K=1 if bootstrapping a single statistic). The function should include an indices parameter that the boot() function can use to select cases for each replication
#R is Number of bootstrap replicates

#boot() function calls the statistic function R times. Each time it generates a set of random indices with replacement, from thei ntegers 1:nrow(data)
#these indices are used in the statistic function to select a sample. the statistics are calculated on the sample and the results are accumulated in bootobject

#elements of the object returened by the  boot() function: t0 is the observed values of k statistics applied to the original data, t is an RxK matrix, where each row is a bootstrapp replicate of the k statistics
#we can access these elements as bootobject$t0 and bootobject$t
#once generating, we can use print() and plot() to examine the results, if the result looks reasonable, we can use the boot.ci() to obtain confidence intervals for the statistics
boot.ci(bootobject, conf=, type=)#conf is the desried confidence interval, type is possible value like norm, basic, stud, perc, bca, and all(default is all)

#the perc method was demonstrated in the sample mean example. bca provides an interval that makes simple adjustments for bias

#18. bootstrapping a single statistic

#for obtaining the R-squared value
rsq <-function(formula, data, indices){
  d<-data[indices,]
  fit<-lm(formula, data=d)
  return(summary(fit)$r.square)
}

library(boot)
set.seed(1234)
results <-boot(data=mtcars, statistic=rsq,
               R=1000,formula=mpg~wt+disp)
print(results)
plot(results)

#we can see that the distribution of bootstrapped R -squared values isnt normally distributed
#a 95% confidence interval for the R-squared values can be obtained using
boot.ci(results, type=c("perc","bca"))


#19. Bootstrapping several statistics

#not only estimate the ci for R^2 but also other matters like the model regression coefficient

#create a function that returns the vector of regression coefficients

bs <-function(formula, data, indices){
  d<-data[indices,]
  fit<- lm(formula, data=d)
  return(coef(fit))
}

library(boot)
set.seed(1234)
result <-boot(data=mtcars, statistic=bs,
              R=1000, formula=mpg~wt+disp)
print(result)
plot(result, index=2)
#when bootstrapping multiple statistics, add an index parameter to the plot() and boot.ci() to indicate which column of bootobject$t to analyze
#in this example, index 1 refers to the intercept, index 2 is car weight and index 3 is the egngine displacement

#to get the 95% confidence intervals for car weight and engine displacement, use
boot.ci(result, type="bca", index=2)

boot.ci(result, type="bca",index=3)

#the previous example resamples the entire sample of data each time
# we can assume that the predictor variables have fixed levels, we would do bettwer to only resample residual ter,s
#see Mooney and Duval(1993, pp. 16-17) for a simple explanation and logorithm

#20. other thoughts about bootstrapping

#a. how large does the original sample need to be
#no exact answer, some say 20-20 is sufficient for good results as long as the sample is representative of the population

#b. how many replications are needed
#find that 1,000 replications are more than adequate in most cases

#other sources
#Yu(2003). Good(2006) provides a comprehensive overview of resampling in general and includes R code
#introduction to bootstrapping is provided by Mooney and Duval (1993)
#Simon(1997), Canty(2002), and Fox(2002)
