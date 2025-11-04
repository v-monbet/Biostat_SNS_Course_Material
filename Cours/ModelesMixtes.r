require("mdhglm")
require("lattice")
require("lme4")


data("Orthodont")
ort1=lm(distance~age+age:factor(Sex)+factor(Sex),data=Orthodont)
summary(ort1)

ort2=lm(distance~age+factor(Sex),data=Orthodont)
summary(ort2)

plot(ort2)

# Residuals not iid
xyplot(ort2$res~Subject,groups=Subject,data=Orthodont)


ortss=lm(distance~-1+Subject+age+age:factor(Sex)+factor(Sex),data=Orthodont)
summary(ortss)


ort4=lmer(distance~age+Sex+(1|Subject),data=Orthodont)
summary(ort4)