library(psych)

data(sat.act)
head(sat.act, 100)
describe(sat.act)
describeBy(sat.act, group=sat.act$gender)
describeBy(sat.act, group=list(sat.act$gender, sat.act$education))
head(describeBy(sat.act, group=list(sat.act$gender, sat.act$education), mat=TRUE), 20)


pairs.panels(sat.act)

data(affect)
head(affect)
pairs.panels(affect[14:17], bg=c("red","black","white","blue")[affect$Film], pch=21, main="Affect varies by movies ") 


data(epi.bfi)
head(epi.bfi)
error.bars.by(x = epi.bfi, group = epi.bfi$epilie==5, )


data(bfi)
with(bfi, {bi.bars(age, gender, ylab="Age", main="Age by males and females",)})
# bi.bars(bfi$age, bfi$gender, ylab="Age", main="Age by males and females")


lowerCor(sat.act)


female <- subset(sat.act, sat.act$gender==2)
male <- subset(sat.act, sat.act$gender==1)
lower <- lowerCor(male[-1])
upper <- lowerCor(female[-1])
both <- lowerUpper(lower, upper) 
round(both, 2)


head(Thurstone)
cor.plot(Thurstone, numbers=TRUE, main="9 cognitive variables from Thurstone")


circ <- sim.circ(24)
r.circ <- cor(circ)
cor.plot(r.circ, main='24 variables in a circumplex') 


op <- par(mfrow=c(2, 2)) 
spider(y=c(1, 6, 12, 18), x=1:24, data=r.circ, fill=TRUE, main="Spider plot of 24 circumplex variables")
op <- par(mfrow=c(1, 1)) 



(cor_ex <- as.data.frame(round(r.circ[c(1,13), ],2), row.names=c("V1", "V2")))
rownames(cor_ex) <- c("V1", "V2")
cor_ex