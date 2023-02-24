#Problem Statement:
#Generate an optimal design to study the effect of 3 factors on response.
#Initially generate a design to identify the significant factors out of 3.
#then augment the design with additional runs to develop a 
#full polynomial model with the significant factors.


#Augmented design 
#the problem statement is given in Notepad++
#Idea is to design an experiment with 3 factors in two stages
#stage first is screening set where we only study the linear effect of 3 factors on response
#then after once we have defined the test program we gather the results of tests
#then Augment design with additional run to develop a full polynomial model
#with quadratic and interaction effects of significant factors

#Import the R packages

library(skpr)

#Specify factors and levels (3 FActors given and 3 levels chosen for a Quadratic design)

myfactor= expand.grid(x1= c(-1,0,1), x2= c(-1,0,1), x3= c(-1,0,1))

# Factor table with total 27 observation will generate for 3 variables

#Generate the screening design

set.seed(35)

mydesign= gen_design(myfactor, model = ~ x1 + x2 + x3, trials = 6, optimality = 'D')

#DOE of 6 optimal combinations of all 3 factors has been generated  in mydesign
mydesign

#TO check th optimality and Power of test
get_optimality(mydesign)

# D- optimallity criteria is the maximum = 94%

# We can check the power of DOE also
eval_design(mydesign)

#Performing one set of test give only 26.75% power of the DOE
# to increase it let do the repeatability of test

myscreendesign= rbind(mydesign, mydesign)

#now power of repaeated test is 
eval_design(myscreendesign)

#SO repeating the test increses the power of test to 81%.
#After the screening step, test has been conducted and 
#A repeated two set of results has been generated for each combinations
# Screening test results are given in 'Screening_Design_result.xlsx'

# Now Perform a Linear regression on the screening test results to check the validity of factors

screen_result= Screening_Design_result[,-1]

#Screen results have total 12 observations with 3 Features and as response column
#to check for Correlation/multicolllinearity
round(cor(screen_result), 3)
#So there is No issue of Multicollinearity

#check the linear model 
myscreenmodel= lm(Response ~ . , data = screen_result)

summary(myscreenmodel)

#Adj. R value is > 0.85, its very good result considering the data points
#The p value for X2 is >> 5%, indicating X2 is insignificant factor

#NOw the design will be augmented with additional runs to study the 
#quadratic and interaction effect of the significant terms

#Augmented design
set.seed(35)

myfinaldesign= gen_design(candidateset = myfactor,
                          model = ~x1 + x2 + x3 + I(x1^2) + I(x3^2) + x1*x3,
                          trials = 12,
                          augmentdesign = mydesign,
                          optimality = 'D')
myfinaldesign

#the additional 6 set of combinations has been generated
#Now for the addinoal block set , get the results in 'Augment_Design_result.xlsx'

Augment_result= Augment_Design_result[,-1]
round(cor(Augment_result), 3)
myaugmentmodel= lm(Response ~ . , data = Augment_result)

summary(myaugmentmodel)

#SO, p value for x2, x1^2, x3^2 is more than 5%, so they are insignificant

#New model will be

mynewaugmentmodel= lm(Response ~ x1 + x3 + x1*x3, data = Augment_result )

summary(mynewaugmentmodel)

#adj rsq= 0.82

#SO the Augment design 

