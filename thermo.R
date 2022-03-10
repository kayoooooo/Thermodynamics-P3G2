library("ggplot2")

dat = data.frame(temp = c(24,61,3), lvl = c(0,79,-35))

LSRL = lm(dat$lvl ~ dat$temp)
print(LSRL)
print(cor(dat$lvl, dat$temp))
b = cor(dat$lvl, dat$temp) * sd(dat$lvl) / sd(dat$temp)
print(b)
a = mean(dat$lvl) - b * mean(dat$temp)
ggplot(dat, aes(x = temp, y = lvl)) + 
  geom_abline(slope = b, intercept = a) + 
  geom_point()

dat$temp = dat$temp + 273.15

LSRL = lm(dat$lvl ~ dat$temp)
print(LSRL)
summary(LSRL)
print(cor(dat$lvl, dat$temp))
b = cor(dat$lvl, dat$temp) * sd(dat$lvl) / sd(dat$temp)
print(b)
a = mean(dat$lvl) - b * mean(dat$temp)
ggplot(dat, aes(x = temp, y = lvl)) + 
  geom_abline(slope = b, intercept = a) + 
  geom_point()