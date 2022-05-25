

#FIGURE 2 GRAPH LANCET RCS FROM 3modelsLC

regplot(rcs_male, mod="rcs(dose, knots)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, xlim = c(0,150), pch=NA_integer_, 
        ylim = c(0, 15), pred = pred_rcs_male, xvals = m)
regplot(rcs_female, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, xlim = c(0,150), pch=NA_integer_,
        ylim = c(0, 15), pred = pred_rcs_female, xvals = f)

###FIGURES SUBGROUP
regplot(male_mort, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, xlim = c(0,150), shade =FALSE, ci=FALSE,pch=NA_integer_, 
        ylim = c(0, 50), pred = pred_male_mort, xvals = m)
lines(exp(pred_male_morb$pred), lwd = "4", lty = "dotted", col = "black")
legend("topleft",inset =0.1, legend=c("Mortality", "Morbidity"), lty=1:2, cex=1)

pred_female_mort <- predict(female_mort, cbind(f))
regplot(female_mort, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, xlim = c(0,150), shade =FALSE, ci=FALSE,pch=NA_integer_, 
        ylim = c(0, 50), pred = pred_female_mort, xvals = m)
lines(exp(pred_female_morb$pred), lwd = "4", lty = "dotted", col = "black")
legend("topleft",inset =0.1, legend=c("Mortality", "Morbidity"), lty=1:2, cex=1)


#GRAPHIC LANCET

regplot(male_type5, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, xlim = c(0,150), shade =FALSE, ci=FALSE,pch=NA_integer_, 
        ylim = c(0, 50), pred = pred_male5, xvals = m)
lines(exp(pred_male1$pred), lwd = "4", lty = "dashed", col = "black")
lines(exp(pred_male3$pred), lwd = "4", lty = "dotted", col = "black")
legend("topleft",inset =0.1, legend=c("All type LC", "ALC", "HCV"), lty=1:3, cex=1)

regplot(female_type5, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, xlim = c(0,150), shade =FALSE, ci=FALSE,pch=NA_integer_,
        ylim = c(0, 50), pred = pred_female5, xvals = f)
lines(exp(pred_female1$pred), lwd = "4", lty = "dashed", col = "black")
lines(exp(pred_female3$pred), lwd = "4", lty = "dotted", col = "black")
legend("topleft",inset =0.1, legend=c("All type LC", "ALC", "HCV"), lty=1:3, cex=1)


regplot(male_mort, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, xlim = c(0,150), shade =FALSE, ci=FALSE,pch=NA_integer_, 
        ylim = c(0, 50), pred = pred_male_mort, xvals = m)
lines(exp(pred_male_morb$pred), lwd = "4", lty = "dotted", col = "black")
legend("topleft",inset =0.1, legend=c("Mortality", "Morbidity"), lty=1:2, cex=1)

##OPTION 1
##FIGURE RCS TOTAL
regplot(rcs, mod="rcs(dose, knots)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_, 
        ylim = c(0, 30), pred = pred_rcs, xvals = s)
abline(v=knots, lty="dotted")

##FIGURE RCS MALE
regplot(rcs_male, mod="rcs(dose, knots)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_, 
        ylim = c(0, 30), pred = pred_rcs_male, xvals = m)

regplot(rcs_female, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_, 
        ylim = c(0, 30), pred = pred_rcs_female, xvals = f)

##OPTION 2
regplot(rcs, mod="rcs(dose, knots)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE, ci=FALSE, pch=NA_integer_, 
        ylim = c(0, 30), pred = pred_rcs, xvals = s)
lines(exp(pred_rcs_female$pred), lwd = "4", lty = "dashed", col = "black")
lines(exp(pred_rcs_male$pred), lwd = "4", lty = "dotted", col = "black")
legend("topleft",inset =0.1, legend=c("Overall", "Women", "Men"), lty=1:3, cex=1)


##MORBIDITY AND MORTALITY

#QUADRATIC
regplot(quad_mort, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE, ci=FALSE, pch=NA_integer_, 
        ylim = c(0, 30), pred = pred_quad_mort, xvals = s, main="Quadratic Regression")
lines(exp(pred_quad_morb$pred), lwd = "4", lty = "dotted", col = "black")
legend("topleft",inset =0.1, legend=c("Mortality", "Morbidity"), lty=1:2, cex=1)

#RCS
regplot(rcs_mort, mod="rcs(dose, knots2)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE, ci=FALSE, pch=NA_integer_, 
        ylim = c(0, 30), pred = pred_rcs_mort, xvals = s, main="RCS Regression")
lines(exp(pred_rcs_morb$pred), lwd = "4", lty = "dotted", col = "black")
legend("topleft",inset =0.1, legend=c("Mortality", "Morbidity"), lty=1:2, cex=1)



regplot(rcs_mort, mod="rcs(dose, knots2)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l",xlim = c(0,100), shade =FALSE, pch=NA_integer_,
        ylim = c(0, 20), pred = pred_rcs_mort, xvals = s)
lines(exp(pred_rcs_morb$pred), lwd = "4",  col = "blue")
lines(exp(pred_rcs_morb$ci.lb), lwd = "1", lty = "dashed", col = "blue")
lines(exp(pred_rcs_morb$ci.ub), lwd = "1", lty = "dashed", col = "blue")
legend("topleft",inset =0.1, legend=c("Mortality", "Morbidity"), lty=1:1, cex=1, col=c("black", "blue"))


