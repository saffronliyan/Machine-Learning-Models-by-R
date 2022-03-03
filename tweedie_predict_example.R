###Data
y <- rgamma(20,shape=5)
x <- 1:20

###Tweedie model
# Fit a poisson generalized linear model with identity link
fit<-glm(y~x,family=tweedie(var.power=1,link.power=1))

### they are all the same because q = 1
fit$linear

fit$fitted ###fit$linear since q = 1

y_hat <- fit$coefficients[1]+fit$coefficients[2]*x
y_hat ##==fit$linear 

pred1 <- predict(fit, data = x, family=tweedie(var.power=1,link.power=1))
pred1


pred2<- predict(fit, data = x, family=tweedie(var.power=1,link.power=1), type = "response")
pred2



##########################################################################

fit2<-glm(y~x,family=tweedie(var.power=3,link.power=0)) #link = 0 is log link
fit2

fit2$linear # linear. predictors

fit2$fitted #fitted values; expected values

exp(fit2$linear) ###==fit2$fitted since log(fitted) = linear.predictors

y_hat2 <- fit2$coefficients[1]+fit2$coefficients[2]*x
y_hat2 ###This is the manual calculation of linear
exp(y_hat2) ###==fitted since taking exp of the above equation obtains fitted

pred3 <- predict(fit2, data = x, family=tweedie(var.power=1,link.power=1))
pred3 ###==linear since type = link


pred4<- predict(fit2, data = x, family=tweedie(var.power=1,link.power=1), type = "response")
pred4 ###== fitted since type = "response" whcih means taking inverse function of link.


##############################################################################
fit3 <- glm(y~x,family=tweedie(var.power=3,link.power=1.5))
fit3

fit3$linear

fit3$fitted

q = 1.5
(fit3$linear)^(1/q)###==fitted since fitted^q = linear

(fit3$fitted)^q ###==linear since the above eq

y_hat3 <- fit3$coefficients[1]+fit3$coefficients[2]*x
y_hat3 ##==linear  

y_hat3^(1/q)

pred5 <- predict(fit3, data = x, family=tweedie(var.power=1,link.power=1.5))
pred5 ###==linear type = link

pred6<- predict(fit3, data = x, family=tweedie(var.power=1,link.power=1.5), type = "response")
pred6

##############################################################################
fit4 <- glm(y~x,family=tweedie(var.power=3,link.power=-0.3))
fit4

fit4$linear

fit4$fitted

(fit4$linear)^(1/q4)

y_hat4 <- fit4$coefficients[1]+fit4$coefficients[2]*x
y_hat4 ##==linear  

y_hat4^(1/q4)

pred7 <- predict(fit4, data = x, family=tweedie(var.power=1,link.power=1.5))
pred7 ###==linear type = link

pred8<- predict(fit4, data = x, family=tweedie(var.power=1,link.power=1.5), type = "response")
pred8
