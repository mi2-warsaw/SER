
## defing various stochastic mortality models ##

# Lee-Carter
LC <- lc(link = "logit")
LC$gnmFormula

# CBD
CBD <- cbd()

# Age-Period-Cohort (APC)
APC <- apc(link = "logit")

# RH (Renshaw and Haberman)
RH <- rh(link = "logit", cohortAgeFun = "1")

# M7
M7 <- m7()

# Plat model
f2 <- function(x, ages) mean(ages) - x # for Bx(2) calculation
constPlat <- function(ax, bx, kt, b0x, gc, wxt, ages){
  nYears <- dim(wxt)[2]
  x <- ages # age vector
  t <- 1:nYears # over years
  c <- (1 - tail(ages, 1)):(nYears - ages[1])
  xbar <- mean(x)
  #nsum g(c)=0, nsum cg(c)=0, nsum c^2g(c)=0
  phiReg <- lm(gc ~ 1 + c + I(c^2), na.action = na.omit) # fitting gamma par (gc)
  phi <- coef(phiReg) # gamma par extraction
  gc <- gc - phi[1] - phi[2] * c - phi[3] * c^2 # transforming gamma par
  kt[2, ] <- kt[2, ] + 2 * phi[3] * t # transforming kt(2) par
  kt[1, ] <- kt[1, ] + phi[2] * t + phi[3] * (t^2 - 2 * xbar * t) # transforming kt(1) par
  ax <- ax + phi[1] - phi[2] * x + phi[3] * x^2 # transforming ax par
  #nsum kt[i, ] = 0
  ci <- rowMeans(kt, na.rm = TRUE)
  ax <- ax + ci[1] + ci[2] * (xbar - x) # second transformation of ax par
  kt[1, ] <- kt[1, ] - ci[1] # second transformation of kt(1) par
  kt[2, ] <- kt[2, ] - ci[2] # second transformation of kt(2) par
  list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
}
PLAT <- StMoMo(link = "logit", staticAgeFun = TRUE,
               periodAgeFun = c("1", f2), cohortAgeFun = "1",
               constFun = constPlat)

# extended PLAT
f2 <- function(x, ages) mean(ages) - x
f3 <- function(x, ages) pmax(mean(ages)-x,0)

constPLAT_extended <- function(ax, bx, kt, b0x, gc, wxt, ages){
  nYears <- dim(wxt)[2]
  x <- ages
  t <- 1:nYears
  c <- (1 - tail(ages, 1)):(nYears - ages[1])
  xbar <- mean(x)
  #nsum g(c)=0, nsum cg(c)=0, nsum c^2g(c)=0
  phiReg <- lm(gc ~ 1 + c + I(c^2), na.action = na.omit)
  phi <- coef(phiReg)
  gc <- gc - phi[1] - phi[2] * c - phi[3] * c^2
  
  kt[2, ] <- kt[2, ] + 2 * phi[3] * t
  kt[1, ] <- kt[1, ] + phi[2] * t + phi[3] * (t^2 - 2 * xbar * t)
  ax <- ax + phi[1] - phi[2] * x + phi[3] * x^2
  #nsum kt[i, ] = 0
  ci <- rowMeans(kt, na.rm = TRUE)
  ax <- ax + ci[1] + ci[2] * (xbar - x) + 
    ci[3] * pmax(xbar-x,0) 
  kt[1, ] <- kt[1, ] - ci[1]
  kt[2, ] <- kt[2, ] - ci[2]
  kt[3, ] <- kt[3, ] - ci[3]
  list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
}
extended_PLAT <- StMoMo(link = "logit", staticAgeFun = TRUE,
                        periodAgeFun = c("1", f2, f3), cohortAgeFun = "1",
                        constFun = constPLAT_extended)

# O'Hare-Li model
f2 <- function(x, ages) mean(ages) - x
f3 <- function(x, ages) pmax(mean(ages)-x,0) + pmax(mean(ages)-x,0)^2 

constOhare <- function(ax, bx, kt, b0x, gc, wxt, ages){
  nYears <- dim(wxt)[2]
  x <- ages
  t <- 1:nYears
  c <- (1 - tail(ages, 1)):(nYears - ages[1])
  xbar <- mean(x)
  #nsum g(c)=0, nsum cg(c)=0, nsum c^2g(c)=0
  phiReg <- lm(gc ~ 1 + c + I(c^2), na.action = na.omit)
  phi <- coef(phiReg)
  gc <- gc - phi[1] - phi[2] * c - phi[3] * c^2
  kt[2, ] <- kt[2, ] + 2 * phi[3] * t
  kt[1, ] <- kt[1, ] + phi[2] * t + phi[3] * (t^2 - 2 * xbar * t)
  ax <- ax + phi[1] - phi[2] * x + phi[3] * x^2
  #nsum kt[i, ] = 0
  ci <- rowMeans(kt, na.rm = TRUE)
  ax <- ax + ci[1] + ci[2] * (xbar - x) + 
    ci[3] * (pmax(xbar-x,0) + pmax(xbar-x,0)^2)
  kt[1, ] <- kt[1, ] - ci[1]
  kt[2, ] <- kt[2, ] - ci[2]
  kt[3, ] <- kt[3, ] - ci[3]
  list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
}
OHARE <- StMoMo(link = "log", staticAgeFun = TRUE,
                periodAgeFun = c("1", f2, f3), cohortAgeFun = "1",
                constFun = constOhare)
