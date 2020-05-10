
###############################################################
# function to return pump flow rate as a function of shaft power, efficiency, head and pipe diameter
###############################################################

# rearranged equations from https://www.pumpsandsystems.com/pump-efficiency-what-efficiency
# conversion in function to provide input / output in SI units, bit bushleague but wth

# required head loss adjustment model
hdloss_lm <- read_rds("model-data/head-loss-model.rds")

# Ps = shaft power (Watts), Ef = efficiency (0-1), H = head (m), D = pipe int. diameter, (mm), L = pipe length (m)
# returns flow rate in litres per second
pumpflow <- function(Ps, Ef, H, D, L){
  
  Ps <- Ps * 0.00134102 # Watts / BHP
  H = H * 1/0.3048 # feet / metre
  
  Pw <- Ps * Ef
  Q_est <- (Pw * 3960) / H
  
  hdloss_100ft <- predict(hdloss_lm, newdata = tibble(flowrate_gpm = Q_est,
                                                d_mm = D,
                                                area_mm2 = pi * (D / 2) ^ 2,
                                                flowrate_gpm2 = Q ^ 2)) %>%
    as.numeric()
  
  hdloss = L / 0.3048 * 1/100 * hdloss_100ft
  
  Q <- (Pw * 3960) / (H + hdloss)
  
  Q <- Q * 4.5461 * 1/60 # gpm to litres / second
  
  return(Q)
}

###############################################################
# function to return top pond outflow rate as a function of outflow orifice diameter and pond head
###############################################################

# adapted from https://www.mcnallyinstitute.com/13-html/13-12.htm
# D = outflow orifice diameter (mm), H = pond head (m)
# returns outflow rate in litres per second
outflow <- function(D, H){
  
  # approximate K value
  K <- 0.7
  
  # orifice area in inches ^2
  A <- pi * (D * 1/25.4 * 1/2) ^ 2
  
  # outflow rate
  Q <- 25 * A * K * H^2
  
  Q <- Q * 4.5461 * 1/60 # gpm to litres / second
  return(Q)
  
}

###############################################################
# function to build stochastic simulation nested dataframe
###############################################################

build_sim <- function(Dat_base, mcN, seed){

  Dat_sim <- Dat_base %>%
    mutate(simN = 1) %>%
    nest(data = -simN) %>%
    slice(rep(1, mcN)) %>%
    mutate(simN = 1:mcN)
  
  # simulate hourly value for kwh per kwp
  set.seed(seed)
  
  Dat_sim <- Dat_sim %>%
    mutate(
      data = data %>%
        map(function(df){
          df %>%
            mutate(solar_alt = ifelse(solar_alt < 0, 0, solar_alt),
                   is_cloudy = runif(n = nrow(df), min = 0, max = 1) <= cld_pc_av,
                   cloudy_adj_fac = ifelse(is_cloudy, runif(n = nrow(df), min = 0.1, max = 0.25), 1), # https://cleantechnica.com/2018/02/08/solar-panels-work-cloudy-days-just-less-effectively/
                   hourly_kwh_kwp = solar_alt * cloudy_adj_fac) %>%
            group_by(yday) %>%
            mutate(hourly_kwh_kwp = hourly_kwh_kwp / sum(hourly_kwh_kwp) * kwh_kwp) %>%
            ungroup()
        })
    )
  return(Dat_sim)
}

