### 6.17 ###
## (a) ##
n <- 340
mean <- 5.4
sd <- 2.3
critical_t <- qt(.975, df <- n-1)
margin_error <- critical_t*(sd/sqrt(n))
margin_error
# The margin of error is 0.2453521.
lower_bound <- mean - margin_error
lower_bound 
upper_bound <- mean + margin_error
upper_bound
# The 95% confidence interval is (5.154648, 5.645352).
## (b) ##
critical_t <- qt(.995, df <- n-1)
margin_error <- critical_t*(sd/sqrt(n))
margin_error
# The margin of error is 0.3221148.
lower_bound <- mean - margin_error
lower_bound 
upper_bound <- mean + margin_error
upper_bound
# The 95% confidence interval is (5.076885, 5.723115).
# The interval in (b) is wider than the interval in (a).

### 6.27 ###
## (a) ##
n <- 1200
mean <- 11.5
sd <- 8.3
critical_t <- qt(.975, df <- n-1)
margin_error <- critical_t*(sd/sqrt(n))
margin_error
lower_bound <- mean - margin_error
lower_bound 
upper_bound <- mean + margin_error
upper_bound
# The 95% confidence interval is (11.02992, 11.97008).
## (b) ##
# No. There are 204 students who have times of 0, as given in the problem.
## (c) ##
# The sample is large enough, with a high enough response rate of 83% that indicates that the use of normality is okay. 

### 6.28 ###
## (a) ##
n <- 1200
mean_min <- 60*11.5
mean_min
sd_min <- 60*8.3
sd_min
# The mean is 690 minutes and the sd is 498 minutes. 
## (b) ##
critical_t <- qt(.975, df <- n-1)
margin_error <- critical_t*(sd_min/sqrt(n))
margin_error
lower_bound <- mean_min - margin_error
lower_bound 
upper_bound <- mean_min + margin_error
upper_bound
# The 95% confidence interval is (661.795, 718.205).
## (c) ##
# You could have multiplied the confidence interval in the previous exercise, (11.02992, 11.97008), by 60.

