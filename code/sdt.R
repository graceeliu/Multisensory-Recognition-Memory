# Signal detection theory (SDT): compute c, d', Beta, ln(Beta) [equal or unequal variance (least squares fit of zROC slope)]
#
# Created: 10/01/2019 By: Evan Layher (sdt.R)
# Revised: 10/14/2019 By: Evan Layher (unequal variance calculations)
# Revised: 02/22/2020 By: Evan Layher (Beta and c' calculations)
# Revised: 04/10/2020 By: Evan Layher (fixed dMinBeta & dMinCprime errors)
# Revised: 04/20/2020 By: Evan Layher (fixed custom_slope adjustment with 1 input)
# Revised: 07/18/2020 By: Evan Layher (take absolute value of d' for c'/Beta measures; updated for online upload)
#
## --- LICENSE INFORMATION --- ##
## Modified BSD-2 License - for Non-Commercial Use Only

## Copyright (c) 2019-20, The Regents of the University of California
## All rights reserved.

## Redistribution and use in source and binary forms, with or without modification, are 
## permitted for non-commercial use only provided that the following conditions are met:

## 1. Redistributions of source code must retain the above copyright notice, this list 
##    of conditions and the following disclaimer.

## 2. Redistributions in binary form must reproduce the above copyright notice, this list 
##    of conditions and the following disclaimer in the documentation and/or other 
##    materials provided with the distribution.

## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY 
## EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
## OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT 
## SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
## INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED 
## TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
## OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
## CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY 
## WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

## For permission to use for commercial purposes, please contact UCSB’s Office of 
## Technology & Industry Alliances at 805-893-5180 or info@tia.ucsb.edu.
## --------------------------- ##
#
# Input an array of hit and false alarm rates to get d' and c values
# ------------------------------------------------------------------ #
# REQUIRED INPUTS: hrs and fars must be equal length
# [1] hrs: hit rates (values from 0 - 1)
# [2] fars: false alarm rates (values from 0 - 1)
# OPTIONAL INPUTS:
# [3] adj: prevent infinite normalized values by adjusting 0/1 values (default = 0)
# [4] uvsd: TRUE/FALSE, TRUE = unequal variance (least squares fit of zROC slope), FALSE = equal variance (default = FALSE; equal variance SDT model)
# [5] custom_slope: manually input slope value
# [6] dMinBeta: correct for low d' in Beta calucations (converts any d' < dMinBeta to dMinBeta)
# [7] dMinCprime: correct for low d' in c' calucations (converts any d' < dMinCprime to dMinCprime)
# **NOTE** for Beta and c' calculations; takes absolute value of d' BEFORE converting values from dMinBeta/dMinCprime to avoid reversing sign of criterion
# ^see Waldmann, M. R., & Göttert, R. (1989); https://doi.org/10.1037/0033-2909.106.2.338^
#
# OUTPUTS:
# [1] c: criterion value(s) (c; ca if unequal variance)
# [2] d: discriminability value(s) (d'; da if unequal variance)
# [3] slope: slope of the standardized cumulative sum of hit and false alarm rates
# [4] c_prime: c'; (c / d')
# [5] beta: likelihood ratio; exp(c x d')
# [6] lnBeta: log-likelihood ratio; c x d' = ln(Beta)

sdt <- function(hrs = NULL, fars = NULL, adj = 0, uvsd = FALSE, custom_slope = NULL, dMinBeta = NULL, dMinCprime = NULL) {

  # Ensure valid inputs (1) At least 1 hr/far value (2) hr/far values have equal length (3) Adjust from 0 to <1
  if (is.null(hrs) | is.null(fars)) {stop('Must specify hit (0 - 1) and false alarm rate(s) (0 - 1)')}
  if (length(hrs) != length(fars)) {stop('Input(s) for "hrs" and "fars" must be equal length')}
  if (adj < 0 | adj >= 1) {stop('Input for "adj" must range from 0 to <1')}
  if (!is.null(dMinBeta)) {if (dMinBeta < 0) {stop('Input for "dMinBeta" must be positive')}} # minimum d' value must be positive (negative values flip criterion)
  if (!is.null(dMinCprime)) {if (dMinCprime < 0) {stop('Input for "dMinCprime" must be positive')}} # minimum d' value must be positive (negative values flip criterion)
  if (length(hrs) == 1 & is.null(custom_slope)) {uvsd = FALSE} # must have at least 2 points for unequal variance calculations
  
  c <- rep(NaN, length(hrs)) # preallocate c (or ca) array
  d <- rep(NaN, length(hrs)) # preallocate d' (or da) array
  slope <- rep(NaN, length(hrs)) # preallocate slope array
  c_prime <- rep(NaN, length(hrs)) # preallocate slope array
  beta <- rep(NaN, length(hrs)) # preallocate Beta array
  lnBeta <- rep(NaN, length(hrs)) # preallocate ln(Beta) array
  
  for (i in 1:length(hrs)) { # loop thru hit rates; check for rates of 0 or 1
    hr <- hrs[i] # hit rate array
    far <- fars[i] # false alarm array

    if (adj > 0) { # adjust rates of 0 or 1 (prevent infinite values)
      if (hr < 0 | hr > 1 | far < 0 | far > 1) {stop('All inputs must range from 0 to 1')}
      if (hr == 0) {hrs[i] <- adj} else if (hr == 1) {hrs[i] <- 1 - adj} # adjust hit rate if 0/1
      if (far == 0) {fars[i] <- adj} else if (far == 1) {fars[i] <- 1 - adj} # adjust false alarm rate if 0/1
    } else {
      if (hr == 0 | hr == 1 | far == 0 | far == 1) { # warn user of infinite values
        message('INFINITE STANDARDIZED VALUES: Consider adding adjustment value with "adj" option')
        break()
      } # if (hr == 0 | hr == 1 | far == 0 | far == 1)
    } # if (adj > 0)
  } # for (i in 1:length(hrs))
  
  if (uvsd) { # compute unequal variance slope (least squares fit of zROC slope)
    if (is.null(custom_slope)) { # find slope
      x <- qnorm(fars) # normalize false alarm rates
      y <- qnorm(hrs) # normalize hit rates
      lm <- lm(y ~ x) # linear regression
      s <- as.numeric(coef(lm)["x"]) # slope of best fit line
    } else { # manually input slope
      s = custom_slope
    } # if (slope == 0)
  } else { # equal variance SDT model (slope = 1)
    s <- 1
  } # if (uvsd)
  
  for (i in 1:length(hrs)) { # loop thru hit rates; compute discriminability and criterion measures
    hr <- hrs[i] # hit rate array
    far <- fars[i] # false alarm array
    
    d[i] <- sqrt(2 / (1 + s ^ 2)) * (qnorm(hr) - s * qnorm(far)) # compute d' (or da)
    c[i] <- (-s / (1 + s)) * (qnorm(hr) + qnorm(far)) # compute c (or ca)
    slope[i] <- s # add slope to array
    
    iDb <- abs(d[i]) # d' for Beta (take absolute value to avoid reversing sign <see Waldmann, M. R., & Göttert, R. (1989); https://doi.org/10.1037/0033-2909.106.2.338>)
    if (!is.null(dMinBeta)) { # correct for low d' values
      if (iDb < dMinBeta) { # if below d' correction value
        iDb <- dMinBeta # convert to minimum d' for Beta
      }
    } # if (!is.null(dMinBeta))
    
    iDc <- abs(d[i]) # d' for c' (take absolute value to avoid reversing sign <see Waldmann, M. R., & Göttert, R. (1989); https://doi.org/10.1037/0033-2909.106.2.338>)
    if (!is.null(dMinCprime)) { # correct for low d' values
      if (iDc < dMinCprime) { # if below d' correction value
        iDc <- dMinCprime # convert to minimum d' for c'
      }
    } # if (!is.null(dMinCprime))
    
    c_prime[i] <- c[i] / iDc # c'
    beta[i] <- exp(iDb * c[i]) # Beta
    lnBeta[i] <- log(beta[i]) # ln(Beta); c x d'
  } # for (i in 1:length(hrs))
  
  # output matrix: (1) c (or c2); (2) d' (or da); (3) slope; (4) c'; (5) Beta; (6) ln(Beta)
  return(cbind(c, d, slope, c_prime, beta, lnBeta)) # return output matrix
} # sdt <- function(hrs = NULL, fars = NULL, adj = 0, uvsd = FALSE, custom_slope = NULL)