# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---- --- ---
# Author:   Andreas Halgreen Eiset
# Title:    Crowding in the emergency department in the absence of boarding 
#           – a transition regression model to predict departures and waiting 
#           time (https://doi.org/10.1186/s12874-019-0710-3)
# System: R version 3.3.2 (2016-10-31); x86_64-pc-linux-gnu (64-bit)
# Packages: rms_4.5-0; Hmisc_3.17-4; ggplot2_2.1.0; tidyr_0.6.0; dplyr_0.5.0
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# -*- -*- -*- -*- -*- -*- -*- -*- -*- -*- -*- -*- -*- -*-
# Please be aware: the code below has not been tested on the provided 
# synthetic data example. As a minimum it requires the data sets created 
# in the "RCode_analysis_crowding.R" file up to and including the capacity 
# data set ("cpsty").
# -*- -*- -*- -*- -*- -*- -*- -*- -*- -*- -*- -*- -*- -*-

#load nessesary packages
tmps <- new.env()
tmps$pckg <- c("Hmisc", "rms", "tidyverse", "lubridate")
lapply(tmps$pckg, library, character.only = TRUE)
print(sapply(tmps$pckg, packageVersion))

# Create the data set for the transition regression model ------------

dta <- left_join(sumad, qblack, by = "period.id") %>%
  left_join(., dttmSft, by = "period.id") %>%
  left_join(., kpct, by = "period.id")

vrbls <- c("dicdy.sft", "season", "shift", "nrs")
for(i in vrbls) {
  dta[, i] <- as.factor(dta[, i])
}

vrbls <- c("arrivals", "departures", "queue")
for(i in vrbls) {
  dta[, i] <- as.integer(dta[, i])
}

dta <- filter(dta, ! dte.sft == "2014-01-01")

dta <- dta %>%
  mutate(lag.q = Lag(queue, shift = 1)) %>%
  mutate(lag.d1 = Lag(departures, shift = 1)) %>%
  mutate(lag.d2 = Lag(departures, shift = 2)) %>%
  mutate(lag.d3 = Lag(departures, shift = 3)) %>%
  mutate(lag.d4 = Lag(departures, shift = 4))

dta <- droplevels(dta)


# Set up and run the transition model --------------------------------

# since we know that
# -0.025Q_t + 0.018Q_t-1 <=>
# -0.025(Q_t - Q_t-1 + Q_t-1) + 0.018Q_t-1 =
# -0.025 * deltaQ_t + (-0.025 + 0.018)Q_t-1 =
# -0.025 * deltaQ_t - 0.007Q_t-1
# Thus, it seems that including the DIFFERENCE between present and lagged 
# queue gives a better interpretation of both parameter (does not go in 
# opposite direction). Please refer to the article linked to above for 
# further details

dta2 <- mutate(dta, delta.q = queue - lag.q)
N <- dta2$arrivals + dta2$queue

m1 <- glm(cbind(departures, N) ~ lag.d1 + lag.d2 + lag.d3 + arrivals + 
            delta.q + lag.q + shift + dicdy.sft + shift * dicdy.sft,
          data = dta2, 
          family = binomial(link = "logit"))

summary(m1)
confint(m1) # CIs using profiled log-likelihood

prob.dep <- m1$fitted
# = probability = exp(predict(m1)) / (1 + exp(predict(m1)))

# p-value of the interaction
m1.reduc <- update(m1, .~. - shift * dicdy.sft)
anova(m1.reduc, m1, test = "Chisq")

# Plot of observed and expected (fig. 1 in cited article) ---------------

dta3 <- dta2 %>%
  mutate(departures = departures * -1) # For at få dep som negativ på graf

for(h in 1:8) {
  fnal <- NULL
  for(i in 1:12) {
    mnth <- i
    set.seed(h + i)
    dy <- sample(1:28, 1) # 28 da dette er max i februar
    yr <- 2013
    dte <- paste(yr, "-", formatC(mnth, width = 2, flag = "0"), "-",
                 formatC(dy, width = 2, flag = "0"), sep = "")
    # paste0(X) er genvej for paste(X, sep = "")
    
    temp <- filter(dta3, dte.sft == dte)
    temp1 <- cbind(temp, predict(m1, newdata = temp, type = "link", se = TRUE))
    temp1 <- within(temp1, {
      prob <- plogis(fit)
      CIlow <- plogis(fit - (1.96 * se.fit))
      CIhigh <- plogis(fit + (1.96 * se.fit))
    })
    temp1$expt.dep <- temp1$prob * c(temp1$arrivals + temp1$queue) * -1
    # arr + dep = N; "* -1" så depart er på neg skala
    temp1$ordr <- 1:48
    temp1$tm <- factor(temp1$tmin1, levels = c("07:00", "07:30", "08:00", "08:30",
                                               "09:00", "09:30", "10:00", "10:30",
                                               "11:00", "11:30", "12:00", "12:30",
                                               "13:00", "13:30", "14:00", "14:30",
                                               "15:00", "15:30", "16:00", "16:30",
                                               "17:00", "17:30", "18:00", "18:30",
                                               "19:00", "19:30", "20:00", "20:30",
                                               "21:00", "21:30", "22:00", "22:30",
                                               "23:00", "23:30", "00:00", "00:30",
                                               "01:00", "01:30", "02:00", "02:30",
                                               "03:00", "03:30", "04:00", "04:30",
                                               "05:00", "05:30", "06:00", "06:30"))
    
    dta4 <- temp1 %>%
      gather(vari, nbr, departures, expt.dep, arrivals, queue)
    fnal <- rbind(fnal, dta4)
  }
  
  # df with total number of arrivals
  temp2 <- fnal %>%
    filter(vari == "arrivals") %>%
    group_by(dte.sft) %>%
    summarise(tot.arr = sum(nbr))
  
  temp3 <- suppressMessages(
    fnal %>%
      group_by(dte.sft) %>%
      transmute(dy.sft = dy.sft) %>%
      unique()
  )
  
  #temp4 <- fnal %>%
  #  filter(vari == "arrivals") %>%
  #  group_by(dte.sft) %>%
  #  summarise(vmr = round(var(nbr)/mean(nbr), 1))
  ### taget ud af tidsskriftsartikel jf. Mogens' kommentar
  
  g1 <- ggplot(fnal, aes(tm, nbr))
  print(g1 + geom_point(aes(tm, nbr, group = vari, colour = vari, shape = vari)) +
          scale_colour_manual(labels = c("Arrivals (obs.)", "Departures (obs.)",
                                         "Departures (expt.)", "Queue (obs.)"),
                              values = c("grey60", "red", "blue", "grey30")) +
          scale_shape_manual(labels = c("Arrivals (obs.)", "Departures (obs.)",
                                        "Departures (expt.)", "Queue (obs.)"),
                             values = c(20, 4, 1, 20)) +
          geom_line(aes(tm, nbr, group = vari, colour = vari)) +
          geom_line(data = fnal[fnal$vari %in% c("expt.dep", "departures"), ],
                    aes(group = tm), colour = "blue", alpha = 0.4, size = 2) +
          scale_x_discrete(breaks = c("07:00", "12:00", "18:00", "00:00", "06:30")) +
          theme(legend.title = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(x = "Time of day",
               y = "Arrivals & departures") +
          facet_wrap(~ dte.sft) +
          geom_text(x = 40, y = 25,
                    aes(label = paste0("", dy.sft)),
                    size = 3,
                    data = temp3) +
          geom_text(x = 40, y = 22,
                    aes(label = paste0("Total arr.: ", tot.arr)),
                    size = 3,
                    data = temp2) #+
        #  geom_text(x = 40, y = 19,
        #            aes(label = paste0("VMR = ", vmr)),
        #            size = 3,
        #            data = temp4)
        
  )
  
  cat("looped", h, "\n")
  
}


rm(list = ls(pattern = "temp"))



# Residual diagnostics -----------------------------------------------
par(mfrow = c(3, 2)) # 2-by-2 grid of plots
par(oma = c(3,3,3,3)) 
par(mar = c(1.5,1,1.5,1))

for(w in c("weekday", "weekend")){
  fnal <- NULL
  for(s in c("day", "evening", "night")){
    rw.nbr <- as.character(which(dta2$shift %in% s & dta2$dicdy.sft %in% w))
    res <- (residuals(m1, type = "pearson"))[rw.nbr]
    fit <- predict(m1)[rw.nbr]
    
    plot(plogis(fit), res,
         xlab = "Fitted values",
         ylab = "Residuals",
         main = paste(s, "shift", split = " "), adj = 0)
    abline(h = 0, col = "red")
    mtext(toupper(w), side = 3, line = 1, cex = 1.5, outer = TRUE)
    mtext('Fitted values', side = 1, adj = 0, line = 1, cex = 0.8, outer = TRUE)
    mtext('Residuals', side = 2, line = 1, cex = 0.8, outer = TRUE)
    mtext('Normal Q-Q plot', side = 3, adj = 1, line = -1, cex = 1, outer = TRUE)
    qqnorm(res, main = "", xaxt = "n", xlab = "", yaxt = "n", ylab = "")
    axis(side = 1, labels = FALSE)
    axis(side = 2, labels = FALSE)
    #qqline(res)
  }
  cat("looped", w, "\n")
}
