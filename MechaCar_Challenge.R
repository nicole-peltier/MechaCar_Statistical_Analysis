library(dplyr)
mechacar <- read.csv('MechaCar_mpg.csv', header=TRUE)
fit <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mechacar)
summary(fit)

suspension <- read.csv('Suspension_Coil.csv', header=TRUE)

total_summary %>% summarize(mean = mean(suspension.PSI), median = median(suspension.PSI), variance = var(suspension.PSI), SD = sd(suspension.PSI))
lot_summary <- suspension %>% group_by(Manufacturing_Lot) %>% summarize(mean = mean(PSI), median = median(PSI), variance = var(PSI), SD = sd(PSI))


t.test(suspension$PSI, mu=1500)
lot1 <- t.test(subset(suspension, Manufacturing_Lot == "Lot 1")$PSI, mu=1500)
lot2 <- t.test(suspension$PSI, data=suspension, mu=1500, subset=suspension$PSI %in% Manufacturing_Lot == "Lot 2")