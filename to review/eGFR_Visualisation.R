library(ggplot2)
library(hrbrthemes)

eGFR <- read.csv(file("eGFR.csv"))

eGFR$firstBookedIn <- 0 

eGFR$Date.Booked.In = as.Date(eGFR$Date.Booked.In, format="%d/%m/%Y")

for (i in 1:nrow(eGFR)) {
  eGFR$firstBookedIn[i] <- (eGFR$Date.Booked.In[which(eGFR$uob_ID==eGFR$uob_ID[i])][1])
}

eGFR$Days = as.integer(eGFR$Date.Booked.In - eGFR$firstBookedIn) 

ggplot(eGFR, aes(Days, uob_ID, fill=Numeric.Result)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdPu") +
  theme_ipsum()