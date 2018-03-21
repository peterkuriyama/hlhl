


propfish1 <- seq(0, 1, by = .01)

#.7
# prob7 <- data.frame(x = propfish1, y = prob7 <- 1 - exp(-2 * propfish1))
prob7 <- data.frame(x = propfish1, y = 1 - exp(-2 * propfish1))
prob7$cc <- .7

#.3
prob3 <- data.frame(x = propfish1, y = log(1 - propfish1) / -2)
prob3$cc <- .3
prob3$y[101] <- 1

#.5
prob5 <- data.frame(x = propfish1, y = propfish1)
prob5$cc <- .5

comp <- rbind(prob3, prob5, prob7)
comp$cc <- as.character(comp$cc)
names(comp)[3] <- 'comp_coeff'

#combine comp type
comp_types <- data.frame(comp_coeff = unique(comp$comp_coeff), 
  comp_type = as.character(c('Spp1 < Spp2', 'Spp1 = Spp2', 'Spp1 > Spp2')))

comp <- inner_join(comp, comp_types, by = 'comp_coeff')
comp$comp_type <- as.character(comp$comp_type)


png(file = 'figs/hlfigS1.png', width = 7, height = 7, units = 'in', res = 150 )
ggplot(comp, aes(x = x, y = y)) + geom_line(aes(linetype = comp_type, group = comp_type)) + 
  xlab("Proportion fish 1") + ylab("c, comp coefficient")
dev.off()

propfish1

# Relative catchability
# p1 but for cs

c1 <- .5

n1 <- 50
n2 <- 100

p1 <- (c1 * n1) / ((c1 * n1) + ((1 - c1) * n2))
p1


(1 - exp(propfish1))
plot(propfish1, (1 - exp(propfish1)))
