library("dplyr")
library("ggplot2")
library("plotly")
library("ggh4x")
library("circular")
library("rstan")
library("cmdstanr")
library("rstanarm")
library("bayesplot")

dat <- read.csv("../../data/R/datR1.csv")
datFx <- read.csv("../../data/R/datR2_fixations.csv")

# descriptive stat --------------------------------------------------------

DSC <- function(dat, item){
  conditions <- unique(dat$condition)
  vecM <- numeric(0)
  vecSD <- numeric(0)
  for (i in 1:length(conditions)){
    tmp <- subset(dat, condition==conditions[i])
    vecM <- c(vecM, mean(tmp[[item]]))
    vecSD <- c(vecSD, sd(tmp[[item]]))
  }
  res <- rbind(vecM, vecSD)
  colnames(res) <- conditions
  return(res)
}

DSC(dat, "liking")
DSC(dat, "admiration")
DSC(dat, "empathy")
DSC(dat, "imagination")
DSC(dat, "isp01")
DSC(dat, "isp02")
DSC(dat, "isp03")
DSC(dat, "isp04")
DSC(dat, "isp05")
DSC(dat, "beauty")
DSC(dat, "boredom01")
DSC(dat, "boredom02")
DSC(dat, "interest01")
DSC(dat, "interest02")
DSC(dat, "nostalgia01")
DSC(dat, "nostalgia02")

# MANOVA ------------------------------------------------------------------
options(contrasts = c("contr.sum", "contr.poly"))
MNV <- function(datCompared){
  model_MNV <- lm(cbind(admiration, beauty, boredom01+boredom02, empathy, imagination, (interest01+interest02)/2,
                        liking, nostalgia01+nostalgia02, isp01+isp02+isp03+isp04+isp05) ~ condition,
                        data=datCompared)
  res1 <- car::Anova(model_MNV)
  res2 <- effectsize::eta_squared(res1, alternative = "two.sided")
  res3 <- summary.aov(model_MNV)
  return(list(res1,res2,res3))
}
MNV(dat)
MNV(subset(dat,condition!="I"))
MNV(subset(dat,condition!="W"))
MNV(subset(dat,condition!="P"))

effectsize::eta_squared(car::Anova(lm(interest01+interest02 ~ condition, data=subset(dat,condition!="P"))), alternative = "two.sided")
effectsize::eta_squared(car::Anova(lm(empathy ~ condition, data=subset(dat,condition!="P"))), alternative = "two.sided")
effectsize::eta_squared(car::Anova(lm(empathy ~ condition, data=subset(dat,condition!="I"))), alternative = "two.sided")
effectsize::eta_squared(car::Anova(lm(imagination ~ condition, data=subset(dat,condition!="I"))), alternative = "two.sided")


# distance ----------------------------------------------------------------

ANCV <- function(datCompared){
  model_ANCV <- lm(log_travelDistance ~ condition+log_travelDistancePre, datCompared) 
  res1 <- car::Anova(model_ANCV, type="III")
  res2 <- effectsize::eta_squared(res1, alternative = "two.sided")
  return(list(res1,res2))
}

psych::describe(subset(datFinal,condition=="W")$log_travelDistance)
psych::describe(subset(datFinal,condition=="P")$log_travelDistance)
psych::describe(subset(datFinal,condition=="I")$log_travelDistance)
ANCV(datFinal)
ANCV(subset(datFinal, condition!="W"))
ANCV(subset(datFinal, condition!="P"))
ANCV(subset(datFinal, condition!="I"))

# view-angles -------------------------------------------------------------

dfAngle <- subset(datFx, aa < -0.0001 | aa > 0.0001)

map <- function(z){ 
  density(z)$x[which.max(density(z)$y)] 
}
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
vW <- as.numeric(subset(dfAngle,condition=="W")$IDnum)
vW2 <- as.numeric((vW-head(c(0,vW),-1))>0)
pW <- purrr::accumulate(vW2, ~.x + .y)
vP <- as.numeric(subset(dfAngle,condition=="P")$IDnum)
vP2 <- as.numeric((vP-head(c(0,vP),-1))>0)
pP <- purrr::accumulate(vP2, ~.x + .y)
vI <- as.numeric(subset(dfAngle,condition=="I")$IDnum)
vI2 <- as.numeric((vI-head(c(0,vI),-1))>0)
pI <- purrr::accumulate(vI2, ~.x + .y)
stanData <- list(
  N_W=length(pW),
  N_P=length(pP),
  N_I=length(pI),
  K_W=max(pW),
  K_P=max(pP),
  K_I=max(pI),
  Y_W=subset(dfAngle,condition=="W")$aa,
  Y_P=subset(dfAngle,condition=="P")$aa,
  Y_I=subset(dfAngle,condition=="I")$aa,
  person_W=pW,
  person_P=pP,
  person_I=pI)

modelN <- cmdstan_model("stan/H0.stan")
fitN <- modelN$sample(stanData, iter_warmup=1000,
                      iter_sampling=3000,
                      chains=4,
                      parallel_chains=4,
                      refresh=200)
modelA <- cmdstan_model("stan/H1.stan")
fitA <- modelA$sample(stanData, iter_warmup=1000,
                        iter_sampling=3000,
                        chains=4,
                        parallel_chains=4,
                        refresh=200)
bayesplot::rhat(fitN) %>% hist()
bayesplot::rhat(fitA) %>% hist()

fitA$print(c("kappa0","kappa_PWdif","kappa_IWdif","sig_kappa","mu0","mu_PWdif","mu_IWdif","sig_mu"), digits=3)

q95 <- function(x) quantile(x, probs=c(0.025, 0.5, 0.975))
fitN$summary(variables=c('sig_kappa'), q95, 'sd') %>% data.frame
fitN$summary(variables=c('kappa0'), q95, 'sd') %>% data.frame
fitN$summary(variables=c('sig_mu'), q95, 'sd') %>% data.frame
fitN$summary(variables=c('mu0'), q95, 'sd') %>% data.frame
fitA$summary(variables=c('sig_kappa'), q95, 'sd') %>% data.frame
fitA$summary(variables=c('kappa0'), q95, 'sd') %>% data.frame
fitA$summary(variables=c('kappa_PWdif'), q95, 'sd') %>% data.frame
fitA$summary(variables=c('kappa_IWdif'), q95, 'sd') %>% data.frame
fitA$summary(variables=c('sig_mu'), q95, 'sd') %>% data.frame
fitA$summary(variables=c('mu0'), q95, 'sd') %>% data.frame
fitA$summary(variables=c('mu_PWdif'), q95, 'sd') %>% data.frame
fitA$summary(variables=c('mu_IWdif'), q95, 'sd') %>% data.frame

# saveRDS(fitN, file = "MCMCresult/fitN.obj")
# saveRDS(fitA, file = "MCMCresult/fitA.obj")

# figures -----------------------------------------------------------------
figFontFamily  <- tryCatch(quartzFonts()$sans[1], finally="sans")

# Fig 3B
mu0W <- as.vector(fitA$draws("mu0"))
mu0P <- mu0W+as.vector(fitA$draws("mu_PWdif"))
mu0I <- mu0W+as.vector(fitA$draws("mu_IWdif"))
kappa0W <- as.vector(fitA$draws("kappa0"))
kappa0P <- kappa0W+as.vector(fitA$draws("kappa_PWdif"))
kappa0I <- kappa0W+as.vector(fitA$draws("kappa_IWdif"))
mat_muW <- matrix(numeric(0), ncol=length(mu0W))
mat_muP <- matrix(numeric(0), ncol=length(mu0W))
mat_muI <- matrix(numeric(0), ncol=length(mu0W))
mat_kappaW <- matrix(numeric(0), ncol=length(mu0W))
mat_kappaP <- matrix(numeric(0), ncol=length(mu0W))
mat_kappaI <- matrix(numeric(0), ncol=length(mu0W))
for(i in 1:max(pW)){
  str <- sprintf("mu_W[%d]",i)
  tmp <- fitN$draws(str)
  mat_muW<-rbind(mat_muW,as.vector(tmp))
  str <- sprintf("kappa_W[%d]",i)
  tmp <- fitN$draws(str)
  mat_kappaW<-rbind(mat_kappaW,as.vector(tmp))
}
for(i in 1:max(pP)){
  str <- sprintf("mu_P[%d]",i)
  tmp <- fitN$draws(str)
  mat_muP<-rbind(mat_muP,as.vector(tmp))
  str <- sprintf("kappa_P[%d]",i)
  tmp <- fitN$draws(str)
  mat_kappaP<-rbind(mat_kappaP,as.vector(tmp))
}
for(i in 1:max(pI)){
  str <- sprintf("mu_I[%d]",i)
  tmp <- fitN$draws(str)
  mat_muI<-rbind(mat_muI,as.vector(tmp))
  str <- sprintf("kappa_I[%d]",i)
  tmp <- fitN$draws(str)
  mat_kappaI<-rbind(mat_kappaI,as.vector(tmp))
}
makeEcdf <- function(y_group, y_individual){
  ppc_ecdf_overlay(y = y_group, yrep = y_individual) +
    scale_x_continuous(
      breaks = seq(0, 6, by = 1), 
      minor_breaks = seq(0, 6, by = 0.5),
      limits = c(0, 6)
    ) + 
    
    theme(legend.position = "none") + 
    theme(text = element_text(size = 90, family = figFontFamily)) + 
    theme(axis.ticks.length = unit(20, "points")) +
    guides(x = "axis_minor", y = "axis_minor") +
    theme(ggh4x.axis.ticks.length.minor = rel(0.25))
}
narrative_color_g <- c("#B9D9B9","#96C796","#73B573","#4AA34A","#259125","#008000")
color_scheme_set(narrative_color_g)
png("fig/3B_narrative.png", width = 1000, height = 1000) 
makeEcdf(kappa0W, mat_kappaW)
dev.off()     
process_color_r <- c("#D9B9B9","#E09696","#E87373","#EF4A4A","#F72525","#FF0000")
color_scheme_set(process_color_r)
png("fig/3B_process.png", width = 1000, height = 1000) 
makeEcdf(kappa0P, mat_kappaP)
dev.off()      
creator_color_b <- c("#B9B9D9","#9696E0","#7373E8","#4A4AEF","#2525F7","#0000FF")
color_scheme_set(creator_color_b)
png("fig/3B_creator.png", width = 1000, height = 1000) 
makeEcdf(kappa0I, mat_kappaI)
dev.off()      

# Fig 3C, 3D
cutByPi <- function(num){
  while(num>pi){
    num <- num-2*pi
  }
  while(num<(-1*pi)){
    num <- num+2*pi
  }
  return(num)
}
v_dat <- circular(dfAngle$aa)
v_dW <- circular(subset(dfAngle,condition=="W")$aa)
v_dP <- circular(subset(dfAngle,condition=="P")$aa)
v_dI <- circular(subset(dfAngle,condition=="I")$aa)
yW <- fitA$draws("y_W")
yP <- fitA$draws("y_P")
yI <- fitA$draws("y_I")
v_yW <- numeric(0)
v_yP <- numeric(0)
v_yI <- numeric(0)
for(i in 1:dim(yW)[3]){
  v_yW <- c(v_yW, yW[,,i][3000,1,]) 
}
for(i in 1:dim(yP)[3]){
  v_yP <- c(v_yP, yP[,,i][3000,1,])
}
for(i in 1:dim(yI)[3]){
  v_yI <- c(v_yI, yI[,,i][3000,1,])
}
v_yW <- sapply(v_yW, cutByPi) %>% sort
v_yP <- sapply(v_yP, cutByPi) %>% sort
v_yI <- sapply(v_yI, cutByPi) %>% sort
addLineFromCircularDat <- function(fig, tmp, color){
  fig <- fig %>% 
    add_trace(
      data = data.frame(tmp$x,tmp$y) %>% `colnames<-`(c("x","y")),
      r = ~y,
      theta = ~as.numeric(x),
      thetaunit = "radians",
      line = list(
        color = color,
        width = 0.3
      ),
      showlegend = F
    ) 
  return(fig)
}
addLineToHist <- function(fig, dens, color){
  dens$x2 <- sapply(dens$x, cutByPi)
  df <- data.frame(dens$x2, dens$y) %>% `colnames<-` (c("x","y"))
  fig <- fig %>% 
    add_trace(
      x=df[order(df$x),]$x,
      y=df[order(df$x),]$y,
      type = "scatter",
      mode = "lines",  
      line = list(
        color = color,
        width = 0.3
      ),
      showlegend = F,
      yaxis = "y2",
      xaxis = "x",
      fillcolor = "transparent"
    ) %>%
    layout(
      yaxis2 = list(
        range=list(0,0.85),
        overlaying="y",
        side="right"))
  return(fig)
}

t <- list(
  family = figFontFamily,
  size = 20)

initHistWidthDens <- function(vec, breaks, color){
  m <- list(
    l = 80,
    r = 80,
    b = 100,
    t = 100,
    pad = 4
  )
  fig <- plot_ly(
    type = 'histogram'
  )
  fig <- fig %>% 
    add_trace(
      x=vec,
      type="histogram",
      marker = list(color = "white",
                    line = list(color = "grey",
                                width = 1)),
      xbins = list(
        start = -1*pi,
        end = pi,
        size = pi/(breaks/2))) %>%
    layout(margin = m,
           showlegend = F,
           xaxis = list(
             # dtick = pi/(breaks/2),
             # tick0 = -1*pi,
             # tickmode = "linear",
             ticks = "inside",
             tickwidth = 4,
             tickvals = as.list(seq(from=-1*pi,to=pi,by=pi/4)),
             ticktext = list("-π","-3π/4","-π/2","-π/4","0","π/4","π/2","3π/4","π")),
           yaxis = list(
             range = list(0,850)
           ),
           font = t)
  d <- density(vec,bw=40)
  fig <- addLineToHist(fig, d, "grey")
  return(fig)
}
figCW <- initHistWidthDens(v_dW, 16, "grey")
figCP <- initHistWidthDens(v_dP, 16, "grey")
figCI <- initHistWidthDens(v_dI, 16, "grey")

l1 <- as.list(sqrt(seq(from=0,to=0.6,by=0.1)*100))
l2 <- as.list(seq(from=0,to=0.6,by=0.1))
m <- list(
  l = 40,
  r = 40,
  b = 40,
  t = 40,
  pad = 4
)
figD <- plot_ly(
  type = 'scatterpolar',
  mode = 'lines'
) 
figD <- fig %>%
  layout(
    polar = list(
      radialaxis = list(
        tickfont = list(
          size = 8,
          color = "grey"
        ),
        angle = 90,
        ticks = "outside",
        tickvals = l1,
        ticktext = l2,
        tickangle = 90
      ),
      angularaxis = list(
        tickvals = as.list((0:7)*45),
        ticktext = list("0","π/4","π/2","3π/4","π","-3π/4","-π/2","-π/4")
      )
    ),
    font = t,
    margin = m
  )
vDraw <- sample(1:3000.,size=100)
for(i in 1:10){
  tmp <- numeric(0)
  for(j in 1:dim(yW)[3]){
    tmp <- c(tmp, yW[,,j][vDraw[i],1,])
  }
  tmp <- density(circular(tmp),bw=40)
  figCW <- addLineToHist(figCW, tmp, "green")
  tmp$y <- sqrt(tmp$y * 100)
  figD <- addLineFromCircularDat(figD, tmp, "green")
}
for(i in 1:10){
  tmp <- numeric(0)
  for(j in 1:dim(yP)[3]){
    tmp <- c(tmp, yP[,,j][vDraw[i],1,])
  }
  tmp <- density(circular(tmp),bw=40)
  figCP <- addLineToHist(figCP, tmp, "red")
  tmp$y <- sqrt(tmp$y * 100)
  figD <- addLineFromCircularDat(figD, tmp, "red")
}
for(i in 1:10){
  tmp <- numeric(0)
  for(j in 1:dim(yI)[3]){
    tmp <- c(tmp, yI[,,j][vDraw[i],1,])
  }
  tmp <- density(circular(tmp),bw=40)
  figCI <- addLineToHist(figCI, tmp, "blue")
  tmp$y <- sqrt(tmp$y * 100)
  figD <- addLineFromCircularDat(figD, tmp, "blue")
}

save_image(figCW,scale=5, file = "fig/3C_narrative.png")
save_image(figCP,scale=5, file = "fig/3C_process.png")
save_image(figCI,scale=5, file = "fig/3C_creator.png")
save_image(figD,scale=5, file = "fig/3D.png")

