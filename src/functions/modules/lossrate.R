# Calculate observed loss rate CI with glm
fLossCI <- function(hives_lost_e, hives_spring_e) {
    resGLM <- glm(
        cbind(hives_lost_e, hives_spring_e) ~ 1,
        family = quasibinomial(link = "logit"),
        na.action = na.omit
    )
    resMean <- resGLM$fitted.values[1]
    resDF <- resGLM$df.residual
    resSE <- summary(resGLM)$coefficients[, 2]
    resCI <- boot::inv.logit(coef(resGLM) + c(-1, 1) * qt(0.975, df = resDF) * resSE)
    names(resCI) <- c("loss_lower_ci", "loss_upper_ci")
    return(resCI * 100)
}
