## Calibration plot for models without cross validation

#initialize
calibration_df = data.frame()
calibration_n_df = data.frame()

#model calibration bin each set of probabilites and then count the percent of our
#patients that fall in that category
cal_interval <- 0.1
zz = 0
for (kk in seq(0,1 - cal_interval,cal_interval)) {
  
  zz = zz + 1
  
  kk2 = kk
  if (kk == 1 - cal_interval) {kk2 = kk + 0.1}
  #find which patients are in this bin
  tmp_prob_bin_idx <- (probabilities >= kk) & (probabilities < (kk2 + cal_interval))
  
  #of the patients in the bin what % had the outcome
  cal_p = sum(SelectedData$outcome[tmp_prob_bin_idx])/sum(tmp_prob_bin_idx)
  
  #save number of interval and %cases in this bin
  #calibration_df[zz,1] = kk
  #what is the % true outcome for this particular bin
  calibration_df[zz,1] = cal_p
  calibration_n_df[zz,1] = sum(tmp_prob_bin_idx)
  
}


#plot calibration of model expected vs. observed probabilities
calplot_x <- vector()
for (kk in 1:dim(calibration_df)[1]) {
  calplot_x[kk] = kk*cal_interval
}  

calplot_df = data.frame(calplot_x,calibration_df)

p1 = ggplot(calplot_df, aes(x = calplot_x , y = V1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 1)) +
  xlab("Predicted Probability (Bin Width 0.1)") +
  ylab("Observed Proportion of Population") +
  geom_line(linetype = "dashed") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")
p1                    


ggsave(paste(save_path, mod_str, SelectedData_str, '_CalibrationPlot.pdf',sep = ''), device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)

