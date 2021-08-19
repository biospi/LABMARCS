library(ggplot2)
library(hrbrthemes)

CRP <- read.csv(file("CRP.csv"))

CRP$firstBookedIn <- 0 

CRP$Date.Booked.In = as.Date(CRP$Date.Booked.In, format="%d/%m/%Y")

for (i in 1:nrow(CRP)) {
  CRP$firstBookedIn[i] <- (CRP$Date.Booked.In[which(CRP$uob_ID==CRP$uob_ID[i])][1])
}

CRP$Days = as.integer(CRP$Date.Booked.In - CRP$firstBookedIn) 

ggplot(CRP, aes(Days, uob_ID, fill=Numeric.Result)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdPu") +
  theme_ipsum()

