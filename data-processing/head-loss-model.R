# head loss adjustment model
# dataset based on Darcy-Weisbach equation -- parameterised for water / water pipe and imperial measurements
# weird mix of metric and imperial is deliberate -- to be used in flow_out() function
# data from https://www.backwoodshome.com/build-your-own-solar-powered-water-pumping-station/
# head loss units are ft head per 100ft pipe length
head_loss <- tibble(flowrate_gpm = c(0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 6, 8),
                    d0.5 = c(0.27, 0.99, 2.09, 3.56, 5.38, NA, NA, NA, NA, NA),
                    d0.75 = c(NA, NA, NA, 0.90, 1.37, 1.92, 3.26, 4.39, NA, NA),
                    d1 = c(NA, NA, NA, NA, NA, NA, 1.01, 1.52, 2.13, 3.64)) %>%
  gather(-flowrate_gpm, key = "key", value = "hdloss_ft") %>%
  mutate(d_inches = str_replace(key, "d", "") %>% as.numeric(),
         d_mm = d_inches * 25.4) %>%
  select(flowrate_gpm, d_mm, hdloss_ft) %>%
  drop_na()

head_loss <- head_loss %>%
  mutate(area_mm2 = pi * (d_mm / 2) ^ 2,
         flowrate_gpm2 = flowrate_gpm ^ 2)

head_loss %>%
  ggplot(aes(x = flowrate_gpm, y = hdloss_ft, colour = area_mm2, group = area_mm2)) +
  geom_line()

head_loss_lm <- lm(hdloss_ft ~ (flowrate_gpm + flowrate_gpm2) * (d_mm + area_mm2), data = head_loss)
summary(head_loss_lm)

head_loss$hdloss_ft_pred <- predict(head_loss_lm, newdata = head_loss %>% select(-hdloss_ft))

head_loss %>%
  ggplot() +
  geom_line(aes(x = flowrate_gpm, y = hdloss_ft, colour = area_mm2, group = area_mm2)) +
  geom_line(aes(x = flowrate_gpm, y = hdloss_ft_pred, group = area_mm2), colour = "darkred")

write_rds(head_loss_lm, "model-data/head-loss-model.rds")
