# R script for Appendix S1
# Title: Applying causal reasoning to investigate multicausality in microbial systems
#Journal: Ecosphere

#create a data frame with two casual factors, fac1 and fac2

fac1 = sample(0:100, 1000, replace = T)

fac2 = sample(0:100, 1000, replace = T)

#create outcomes based on different models of causality

#set true effect sizes
#effect size for factor 1
a = 2

#effect size for factor 2
b = 2
#additive causality

out1 = a*fac1 + b*fac2  + runif(length(fac1),-10,10)

#can we get accurate estimates of parameters using linear models if we only know about factor 1?

model1 = lm(out1~fac1)
summary(model1)
confint(model1)
plot(fac1, out1, xlab = "Factor 1", ylab = "Outcome", main = "Additive Causality" )


model1b = lm(out1~fac1 + fac2)
summary(model1b)
confint(model1b)




#synergistic causality - effect of factor 1 is enhanced by level of factor 2
c = (1/mean(fac2))*a

out2 = (0)*fac1 + (b)*fac2 + c*fac1*fac2 + runif(length(fac1),-10,10)

model2 = lm(out2~fac1)
summary(model2)
confint(model2)
plot(fac1, out2, xlab = "Factor 1", ylab = "Outcome", main = "Synergistic Causality" )


model2b = lm(out2~fac1 + fac2)
summary(model2b)
confint(model2b)


model2c = lm(out2~fac1*fac2)
summary(model2c)
confint(model2c)

#parallel causality - either factor1 or factor2 yield equivalent outcome
out3 = rep(0,length(fac1))

for (i in 1:length(fac1)){
out3[i] = if(fac1[i]*2 +  fac2[i]*2 > 200) 200 else (fac1[i]*2 + fac2[i]*2)}

out3 = out3 + runif(length(fac1), -10,10)

plot(fac1, out3, xlab = "Factor 1", ylab = "Outcome", main = "Parallel Causality" )
  

model3 = lm(out3~fac1)
summary(model3)
confint(model3)
   

model3b = lm(out3~fac1 + fac2)
summary(model3b)
confint(model3b)


model3c = lm(out3~fac1*fac2)
summary(model3c)
confint(model3c)

#mediated causality

fac2 = fac1 + runif(length(fac1), -20,20)
out4 = a*fac2 + runif(length(fac1), -10,10)

plot(fac1, out4, xlab = "Factor 1", ylab = "Outcome", main = "Mediated Causality" )
  
model4 = lm(out4~fac1)
summary(model4)
confint(model4)

model4b = lm(out4~fac1 + fac2)
summary(model4b)
confint(model4b)

model4c = lm(out4~fac1*fac2)
summary(model4c)
confint(model4c)



#spurious causality

fac1 = fac2 + runif(length(fac1), -20,20)
out5 = a*fac2 + runif(length(fac1), -10,10)

plot(fac1, out5, xlab = "Factor 1", ylab = "Outcome", main = "Spurious Causality" )

model5 = lm(out5~fac1)
summary(model5)
confint(model5)

model5b = lm(out5~fac1 + fac2)
summary(model5b)
confint(model5b)

model5c = lm(out5~fac1*fac2)
summary(model5c)
confint(model5c)







