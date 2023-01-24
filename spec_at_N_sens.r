
specificity90_store <- list()
specificity95_store <- list()

####
ij_idx = outsidefolds*(j - 1) + i
###

    #we wnat to save specificity at 90 & 95% sensitivity - the ROC curve stores sensitivities
    #so we need to find the index matching the above, senistivity is stored decreasing from one
    #so if we subtract our threshold sensitivity we can take the first minimum 
    s_thresh = 0.90
    pos_min = roccurve[[ij_idx]]$sensitivities[ (roccurve[[ij_idx]]$sensitivities - s_thresh) > 0 ] - s_thresh
    #we need the first threhold that is at least 90/95% sensitivity (but also need to exclude
    #values just below threshold that might be a smaller min)
    pos_min_rev_idx = seq(length(pos_min),1) #reverse index
    sens_idx_90 = pos_min_rev_idx[which.min(rev(pos_min))]
  
    s_thresh = 0.95
    pos_min = roccurve[[ij_idx]]$sensitivities[ (roccurve[[ij_idx]]$sensitivities - s_thresh) > 0 ] - s_thresh
    pos_min_rev_idx = seq(length(pos_min),1) #reverse index
    sens_idx_95 = pos_min_rev_idx[which.min(rev(pos_min))]
    
    #now we can assign values for specifity at 90 & 95% sensitivity
    #after finishing the loops these lists need to be converted to matrices
    specificity90_store[[ij_idx]] <- c(roccurve[[ij_idx]]$thresholds[sens_idx_90], 
      roccurve[[ij_idx]]$sensitivities[sens_idx_90], 
      roccurve[[ij_idx]]$specificities[sens_idx_90])
    
    specificity95_store[[ij_idx]] <- c(roccurve[[ij_idx]]$thresholds[sens_idx_95], 
                                    roccurve[[ij_idx]]$sensitivities[sens_idx_95], 
                                    roccurve[[ij_idx]]$specificities[sens_idx_95])


###
#get quantiles for specificity at particlular sensitiivity
spec90_mat = do.call(cbind,specificity90_store)
specificity90_thresh_quantile <- as.numeric(quantile(spec90_mat[1,], c(.025, .50, .975)))
specificity90_sens_quantile <- as.numeric(quantile(spec90_mat[2,], c(.025, .50, .975)))
specificity90_spec_quantile <- as.numeric(quantile(spec90_mat[3,], c(.025, .50, .975)))

spec95_mat = do.call(cbind,specificity95_store)
specificity95_thresh_quantile <- as.numeric(quantile(spec95_mat[1,], c(.025, .50, .975)))
specificity95_sens_quantile <- as.numeric(quantile(spec95_mat[2,], c(.025, .50, .975)))
specificity95_spec_quantile <- as.numeric(quantile(spec95_mat[3,], c(.025, .50, .975)))

start cross val 12:41 - july 21
