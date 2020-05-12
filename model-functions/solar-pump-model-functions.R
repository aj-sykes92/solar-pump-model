
###############################################################
# function to return pump flow rate as a function of shaft power, efficiency, head and pipe diameter
###############################################################

# head loss equation pirated from https://www.pumpsandsystems.com/pumps/april-2015-calculating-head-loss-pipeline
# Q = flow rate (gpm), L = length of pipe (ft), d = pipe diameter (inches)
hdloss <- function(Q, L, d){
  Re <- 50.66 * ((Q * 62.4) / (d * 1.1))
  f <- 0.25 / (log10(0.0018 / (3.7 * d) + 5.74 / Re ^ 0.9)) ^ 2
  hL <- 0.0311 * (f * L * Q ^ 2) / d ^ 3
  return(hL)
}


# rearranged equations from https://www.pumpsandsystems.com/pump-efficiency-what-efficiency
# conversion in function to provide input / output in SI units, bit bushleague but wth

# Ps = shaft power (Watts), Ef = efficiency (0-1), H = head (m), d = pipe int. diameter, (mm), L = pipe length (m)
# returns flow rate in litres per second
pumpflow <- function(Ps, Ef, H, d, L){
  
  Ps <- Ps * 0.00134102 # Watts / BHP
  H = H * 1/0.3048 # feet / metre
  L = L * 1/0.3048 # feet / metre
  d = d * 1/25.4 # inches / mm
  
  Pw <- Ps * Ef
  Qmax <- (Pw * 3960) / H # flow rate w/o head loss, gpm
  
  hL <- hdloss(Qmax, L, d)
  Qmin <- (Pw * 3960) / (H + hL)
  
  Q = mean(c(Qmax, Qmin)) # linear approximation which trial/error shows is broadly ok
  
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
  
  # convert H to feet
  H <- H * 1 / 0.3048
  
  # outflow rate
  Q <- 25 * A * K * H^2
  
  Q <- Q * 4.5461 * 1/60 # gpm to litres / second
  return(Q)
  
}

equal_flow_head <- function(Q, D){
  
  # approximate K value
  K <- 0.7
  
  # orifice area in inches ^2
  A <- pi * (D * 1/25.4 * 1/2) ^ 2
  
  # convert Q to gpm
  Q <- Q / 4.5461 * 60
  
  # head at equal outflow
  H <- sqrt(Q / (25 * A * K))
  
  return(H * 0.3048)
  
}


###############################################################
# function to iteratively calculate battery level
###############################################################

battery_level <- function(battery_cap, power_in, power_out){
  
  battery_level <- rep(battery_cap, length(power_in))
  
  for(i in 2:length(power_in)){
    battery_level[i] <- battery_level[i - 1] + power_in[i] - power_out[i]
    if(battery_level[i] > battery_cap) battery_level[i] <- battery_cap
    if(battery_level[i] < 0) battery_level[i] <- 0
  }
  return(battery_level)
}

###############################################################
# function to build stochastic simulation nested dataframe
###############################################################

build_sim <- function(Dat_core, mcN, seed){

  Dat_sim <- Dat_core %>%
    mutate(simN = 1) %>%
    nest(base_data = -simN) %>%
    slice(rep(1, mcN)) %>%
    mutate(simN = 1:mcN)
  
  # simulate hourly value for kwh per kwp
  set.seed(seed)
  
  Dat_sim <- Dat_sim %>%
    mutate(
      base_data = base_data %>%
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


###############################################################
# function to run simulation
###############################################################

run_sim <- function(Dat_sim, # simulation base data returned by build_sim()
                    solar_pv_eff = 0.125, # solar panel efficiency (kWp per m2)
                    solar_pv_m2 = 1, # solar pv size (m2)
                    control_eff = 97, # controller efficiency, %
                    battery_cap = 500, # battery capacity (Wh)
                    pump_rating = 20, # pump max power draw (Watts)
                    pump_eff = 80, # pump efficiency, %
                    pump_head = 2, # pumping head in metres
                    pipe_diameter = 25, # pipe internal diameter in mm
                    pipe_length = 16.2, # pipe length in m
                    outflow_height = 0.1, # outflow height above pond bottom, m
                    outflow_diameter = 50, # outflow diameter, mm
                    bottom_m3 = 0.51, # bottom pond capacity (m3)
                    top_L = 1.5, # top pond length
                    top_W = 0.8, # top pond width
                    top_D = 0.3 # top pond depth
){
  
  # simulation run
  Dat_sim <- Dat_sim %>%
    mutate(sim_data = base_data %>%
             map(function(df){
               df %>%
                 mutate(kwh = hourly_kwh_kwp * solar_pv_eff * solar_pv_m2 * control_eff/100,
                        pv_watts = kwh * 1000,
                        pv_to_pump = ifelse(pv_watts > pump_rating, pump_rating, pv_watts),
                        pv_to_battery = pv_watts - pv_to_pump,
                        battery_to_pump = pump_rating - pv_to_pump,
                        battery_level = battery_level(battery_cap, pv_to_battery, battery_to_pump),
                        battery_to_pump = ifelse(battery_to_pump < lag(battery_level, default = 0), battery_to_pump, lag(battery_level, default = 0)), # adjustment following battery level calculation
                        pump_power = battery_to_pump + pv_to_pump,
                        pump_flow = pmap_dbl(list(pump_power,
                                                  pump_eff/100,
                                                  pump_head,
                                                  pipe_diameter,
                                                  pipe_length),
                                             pumpflow),
                        equilibrium_head = equal_flow_head(Q = pump_flow, D = outflow_diameter),
                        top_water_level = ifelse(top_D < (outflow_height + equilibrium_head), top_D, (outflow_height + equilibrium_head)),
                        water_vol_top = top_L * top_W * top_water_level,
                        water_vol_bottom = bottom_m3 - water_vol_top,
                        av_head = (equilibrium_head + lag(equilibrium_head, 1, default = 0)) / 2,
                        river_flow_rate = outflow(D = outflow_diameter, H = av_head)
                 )
             }))
  return(Dat_sim)
}


###############################################################
# simulation plots and tables
###############################################################

# daily hours with river flow plot
flow_hours_plot <- function(Dat_sim){
  
  Dat_sim %>%
    unnest(cols = c(sim_data)) %>%
    mutate(is_flow = river_flow_rate > 0) %>%
    filter(is_flow) %>%
    group_by(simN, month) %>%
    summarise(flow_hours = sum(is_flow),
              flow_rate = mean(river_flow_rate)) %>%
    group_by(month) %>%
    summarise(flow_hours = mean(flow_hours),
              flow_rate = mean(flow_rate)) %>%
    mutate(days_in_month = lubridate::days_in_month(month),
           flow_hours_per_day = flow_hours / days_in_month) %>%
    ggplot(aes(x = as.ordered(month), y = flow_hours_per_day, fill = flow_rate)) +
    geom_col() +
    labs(#title = "Daily hours with river flow",
         x = "Month",
         y = "Hours per day with flow",
         fill = "Mean flow rate\n(litres per second)") +
    scale_y_continuous(breaks = seq(0, 24, by = 6)) +
    theme_classic()
  
}

# mean hourly flow rate
hourly_flow_plot <- function(Dat_sim){
  Dat_sim %>%
    unnest(cols = c(sim_data)) %>%
    group_by(month, hour) %>%
    summarise(flow_rate = mean(river_flow_rate)) %>%
    ggplot(aes(x = hour, y = flow_rate)) +
    geom_line(colour = "darkblue") +
    facet_wrap(~ month, nrow = 3) +
    labs(#title = "Hourly flow rate by month",
         x = "Hour of day",
         y = "Mean hourly flow rate (litres per second)") +
    theme_classic()
}

# water volume plot
water_vol_plot <- function(Dat_sim){
  Dat_sim %>%
    unnest(cols = c(sim_data)) %>%
    group_by(month, hour) %>%
    summarise(Top = mean(water_vol_top),
              Bottom = mean(water_vol_bottom)) %>%
    gather(-month, -hour, key = "pond", value = "water_vol") %>%
    mutate(water_vol = water_vol * 1000) %>%
    ggplot(aes(x = hour, y = water_vol, colour = pond, group = pond)) +
    geom_line() +
    facet_wrap(~month, nrow = 3) +
    labs(#title = "Hourly water volume in ponds",
         x = "Hour of day",
         y = "Water volume (litres)",
         colour = "Pond") +
    theme_classic()
}

# hourly means table
hourly_means_table <- function(Dat_sim){
  Dat_sim %>%
    unnest(cols = c(sim_data)) %>%
    select(month,
           `Solar power generation (W)` = pv_watts,
           `Solar power to pump (W)` = pv_to_pump,
           `Solar power to battery (W)` = pv_to_battery,
           `Battery level (Wh)` = battery_level,
           `Total power to pump (W)` = pump_power,
           `River flow (litres/second)` = river_flow_rate) %>%
    gather(-month, key = "metric", value = "value") %>%
    group_by(month, metric) %>%
    summarise(mean = mean(value),
              sd = sd(value)) %>%
    mutate(conc = paste0(round(mean, 1), " (", round(sd, 1), ")")) %>%
    select(Month = month, metric, conc) %>%
    spread(key = metric, value = conc) %>%
    ungroup() %>%
    mutate(Month = as.integer(Month))
}
