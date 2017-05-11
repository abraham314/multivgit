datasets::HairEyeColor
help("HairEyeColor",package = "datasets")
summary(datasets::HairEyeColor)
Haireye<-apply(HairEyeColor,c(1,2),sum)
Haireye

test<-chisq.test(Haireye)

plot(function(x) dchisq(x,df=9),xlim=c(0,150))

test$observed
test$expected