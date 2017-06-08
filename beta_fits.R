setwd("/Users/peterkuriyama/School/Research/hlsimulator")

#Source hlfig2
library(stats4)

#Try to fit model to quantify degree of hyper-
temp <- plot2 %>% filter(ind == 13, type == 'preferential')
input <- temp$med_cpue

#------------------------------------------------------------
#Functions
hyper <- function(q, beta, input){
  #Observations
  obs <- input
  
  #Relative abundance levels
  xx <- seq(0, 1, .1)

  preds <- q * xx ^ beta

  #Sum of squares
  ss <- sum((preds - obs) ^ 2)
  return(ss)
}

hyper_preds <- function(q, beta){
  #Observations
  obs <- temp$med_cpue
  
  #Relative abundance levels
  xx <- seq(0, 1, .1)

  preds <- q * xx ^ beta

  return(preds)
}

#------------------------------------------------------------
#Analysis
vals <- temp$med_cpue
res <- mle(hyper, start = list(beta = .1), fixed = list(q = 1, input = input))

best_fit <- hyper_preds(q = coef(res)[1], beta = coef(res)[2])

#------------------------------------------------------------
#Run this for all values the indices
#For loop to estimate this shit
all_outs <- data.frame(prefq = 1:16, prefbeta = 1:16, randq = 1:16, randbeta = 1:16)

for(ii in 1:16){
  preftemp <- plot2 %>% filter(ind == ii, type == 'preferential')
  prefres <- mle(hyper, start = list(beta = .1, q = 1), 
    fixed = list(input = preftemp$med_cpue))

  randtemp <- plot2 %>% filter(ind == ii, type == 'random')
  randres <- mle(hyper, start = list(beta = .1, q = 1), fixed = list(input = randtemp$med_cpue))

  out <- c(coef(prefres)[1], coef(prefres)[2], coef(randres)[1], coef(randres)[2])
  # names(out) <- c('pref_beta', 'rand_beta')
  all_outs[ii, ] <- out
}

#Refit the one with a fixed q at 1
fix_thing <- plot2 %>% filter(type == 'preferential', ind == 13)

fix_res <- mle(hyper, start = list(beta = .1), fixed = list(q = 1, 
  input = fix_thing$med_cpue))

#Replace in the thing
c(coef(fix_res)[1], coef(fix_res)[2])
all_outs[13, c('prefq', 'prefbeta')] <- c(coef(fix_res)[1], coef(fix_res)[2])

#Histogram of 2
hist(ests$beta, breaks = 30)
#------------------------------------------------------------
#Reformat and reshape data for plots
ests <- rbind(data.frame(ind = 1:16, type = 'random', 
               q = all_outs[, 3], beta = all_outs[, 4]),
             data.frame(ind = 1:16, type = 'preferential', 
               q = all_outs[, 1], beta = all_outs[, 2]))

#Save plot2 data with q and beta estimates
plot2 <- inner_join(plot2, ests, by = c('ind', 'type'))
save(plot2, file = 'output/plot2.Rdata')


#plot2 might already be loaded
#q
qq <- plot2 %>% select(nsites, init_dist, type, init_dist_plot, q, beta) %>% 
  distinct() %>% dcast(nsites + init_dist_plot ~ type, value.var = "q")
qq$preferential <- round(qq$preferential, digits = 2)
qq$random <- round(qq$random, digits = 2)
names(qq)[c(3, 4)] <- c('prefq', 'randq')

bb <- plot2 %>% select(nsites, init_dist, type, init_dist_plot, q, beta) %>% 
  distinct() %>% dcast(nsites + init_dist_plot ~ type, value.var = "beta")
bb$preferential <- round(bb$preferential, digits = 2)
bb$random <- round(bb$random, digits = 2)
names(bb)[c(3, 4)] <- c('prefbeta', 'randbeta')

table2 <- inner_join(qq, bb, by = c('nsites', 'init_dist_plot'))
table2 <- table2 %>% arrange(init_dist_plot, nsites)

table2$pref <- paste0(table2$prefq, " - ", table2$prefbeta)
table2$rand <- paste0(table2$randq, " - ", table2$randbeta)

table21 <- table2 %>% select(nsites, init_dist_plot, prefq, prefbeta) %>% 
  melt(id.vars = c('nsites', 'init_dist_plot'))
table21$type <- 'Preferential'

table22 <- table2 %>% select(nsites, init_dist_plot, randq, randbeta) %>% 
  melt(id.vars = c('nsites', 'init_dist_plot'))
table22$type <- 'Random'

table2 <- rbind(table21, table22)
table2$variable2 <- c(rep("q", 16), rep('beta', 16))

table2 %>% filter(variable2 == "q") %>% dcast(init_dist_plot + type ~ nsites, value.var = 'value') %>%
  write.csv('output/table21.csv', row.names = FALSE)

table2 %>% filter(variable2 == "beta") %>% dcast(init_dist_plot + type ~ nsites, value.var = 'value') %>%
  write.csv('output/table22.csv', row.names = FALSE)


table2 %>% select(nsites, init_dist_plot, pref, rand) %>% melt(id.vars = c("nsites", "init_dist_plot"))

table2 <- rbind(dcast(table2, init_dist_plot ~ nsites, value.var = "pref"),
    dcast(table2, init_dist_plot ~ nsites, value.var = "rand"))
table2$type <- c(rep('Preferential', 4), rep('Random', 4))


write.csv(table2, file = 'output/table2.csv', row.names = FALSE)

to_loop <- expand.grid(ind = 1:16, type = c('preferential', 'random'))

# png(width = 15, height = 15, units = 'in', res = 150, 
#   file = 'figs/hyper.png')
par(mfcol = c(8, 4), mar = c(.5, .5, .5, .5))

for(ii in 1:nrow(to_loop)){
  obs <- plot2 %>% filter(ind == to_loop[ii, 'ind'], 
    type == to_loop[ii, 'type'])
  obs <- obs$med_cpue

  preds <- ests %>% filter(ind == to_loop[ii, 'ind'], 
    type == to_loop[ii, 'type'])
  preds <- hyper_preds(q = preds$q, beta = preds$beta)

  plot(seq(0, 1, by = .1), obs, pch = 19, col = 'red', axes = F, 
    type = 'n', xlim = c(0, 1), ylim = c(0, 1))
  points(seq(0, 1, by = .1), obs, pch = 19, col = 'red')
  box()
  # axis(side = 1)
  # axis(side = 2)
  lines(seq(0, 1, by = .1), preds, type = 'b', lwd = 1.5, pch = 19)
}

# dev.off()

#index, type, q, beta