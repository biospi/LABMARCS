#Plot and save calibration distirbution for crossvalidated trials

#plot calibration of model expected vs. observed probabilities
calplot_y <- vector()
calplot_y_2_5 <- vector()
calplot_y_97_5 <- vector()
calplot_x <- vector()

for (kk in 1:dim(calibration_df)[2]) {
  tmp = calibration_df[!is.nan(calibration_df[,kk]),kk]
  if (!is_empty(tmp)) {
    calplot_y[kk] = median(calibration_df[,kk], na.rm = TRUE)
    calplot_y_2_5[kk] = sort(tmp)[ceiling(length(tmp)*0.025)]
    calplot_y_97_5[kk] = sort(tmp)[round(length(tmp)*0.975)]
    calplot_x[kk] = kk*cal_interval
  } else {
    calplot_y[kk] = NaN
    calplot_y_2_5[kk] = NaN
    calplot_y_97_5[kk] = NaN
    calplot_x[kk] = kk*cal_interval
    
  }
}  

calplot_df = data.frame(calplot_x,calplot_y,calplot_y_2_5, calplot_y_97_5)

p1 = ggplot(calplot_df, aes(x = calplot_x , y = calplot_y)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = calplot_y_2_5, ymax = calplot_y_97_5), alpha = 0.2) +
  xlab("Predicted Probability (Bin Width 0.1)") +
  ylab("Observed Proportion of Population") 

p1                    

ggsave(paste(save_path,  'CV_', mnum, '_', model_desc_str, '_Median_Calibration_',
             as.character(n_models), '_models.pdf',sep = ''), device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)

