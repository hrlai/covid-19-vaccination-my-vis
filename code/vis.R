library(tidyverse)
library(lubridate)
library(viridis)
library(emojifont)
library(ggtext)
library(magick)

# import data
date_min <- ymd("2021-04-01")
vax <- 
    read_csv("citf-public/vaccination/vax_malaysia.csv") %>% 
    filter(date >= date_min)

# data for annotations
monthly_chase <- 
    vax %>% 
    mutate(mday = mday(date)) %>% 
    filter(mday == 1) %>% 
    mutate(date_full_vax = ymd(NA)) %>% 
    # remove last row because unlikely to have both doses
    slice(n = 1:(n()-1))
for (i in seq_len(nrow(monthly_chase))) {
    monthly_chase$date_full_vax[i] <- 
        vax$date[which(abs(monthly_chase$dose1_cumul[i] - vax$dose2_cumul) == 
                           min(abs(monthly_chase$dose1_cumul[i] - vax$dose2_cumul)))]
}
monthly_chase <- 
    monthly_chase %>% 
    mutate(day_diff = date_full_vax - date)
textbox <- 
    monthly_chase %>% 
    filter(date == ymd("2021-05-01")) %>% 
    mutate(label = "In May, it took 23 days for the <span style='color:#477CF3FF'>2nd dose</span> to match the number of <span style='color:#DD3D08FF'>1st dose</span>")

# visualise as PDF to render aesthetics better
# (maybe a linux shortcoming? png worked fine on PC)
pdf("fig/vax_doses.pdf", width = 6, height = 5)
ggplot(vax) +
    # guidelines
    geom_hline(yintercept = seq(1, 15, 1)*1e6, 
               colour = "lightgrey",
               size = 0.2,
               lty = 2) +
    # geom_hline(yintercept = c(1, 5, 10, 15)*1e6, 
    #            colour = "grey", 
    #            size = 0.2) +
    geom_segment(data     = monthly_chase,
                 aes(x    = date, 
                     y    = rep(min(vax$dose2_cumul), nrow(monthly_chase)),
                     xend = date,
                     yend = dose1_cumul),
                 lineend = "butt",
                 colour = "lightgrey",
                 size = 0.2) +
    # data
    geom_ribbon(aes(date, 
                    ymax = dose1_cumul,
                    ymin = dose2_cumul),
              fill = turbo(1, 1, 0.85),
              alpha = 0.2) +
    geom_ribbon(aes(date, 
                    ymax = dose2_cumul, 
                    ymin = rep(min(vax$dose2_cumul), nrow(vax))),
                fill = turbo(1, 1, 0.15),
                alpha = 0.2) +
    geom_line(aes(date, dose1_cumul),
              colour = turbo(1, 1, 0.85),
              size = 1,
              lineend = "round") +
    geom_line(aes(date, dose2_cumul),
              colour = turbo(1, 1, 0.15),
              size = 1,
              lineend = "round") +
    # annotations
    annotate("text", 
             x = ymd("2021-08-01"), 
             y = 9e6,
             label  = emoji("syringe"),
             family = "EmojiOne", 
             vjust = 0,
             size = 12,
             colour = turbo(1, 1, 0.85)) +
    annotate("text", 
             x = ymd("2021-07-26"), 
             y = 3e6,
             label  = paste0(emoji("syringe"), emoji("syringe")),
             family = "EmojiOne", 
             vjust = 0,
             size = 12,
             colour = turbo(1, 1, 0.15)) +
    geom_segment(data     = monthly_chase,
                 aes(x    = date, 
                     y    = dose1_cumul,
                     xend = date_full_vax,
                     yend = dose1_cumul),
                 arrow = arrow(15, length = unit(0.3, "cm")),
                 lineend = "round") +
    geom_text(data = monthly_chase,
              aes(x = date_full_vax,
                  y = dose1_cumul,
                  label = paste(as.numeric(day_diff), "days")),
              hjust = 1, vjust = -0.5,
              size = 4,
              fontface = "italic") +
    geom_textbox(data = textbox,
                 aes(date, 1.9e6, 
                     label = label),
                 size = 3.5,
                 vjust = 0,
                 width = unit(1.5, "in")) +
    annotate("curve",
             x = ymd("2021-04-20"), y = 1.9e6,
             xend = ymd("2021-05-01"), yend = 1e6,
             size = 0.3,
             arrow = arrow(25, length = unit(0.2, "cm")),
             lineend = "round") +
    # formats
    scale_y_log10(
        breaks = c(1, 5, 10, 15)*1e6,
        labels = c(1, 5, 10, 15),
        # Add a second axis (% population)
        sec.axis = sec_axis(trans  = ~./32.7e6*100,
                            breaks = c(1, 5, seq(10, 50, 10)),
                            name   = "% population")) +
    coord_cartesian(expand = FALSE, clip = "off") +
    labs(x = "", 
         y = "Cumulative vaccination (millions)",
         title = "**Lag in full vaccination**",
         subtitle = "Daily and cumulative vaccination in Malaysia at the country level as of 7 August 2021. Data prior to 1 April 2021 is not shown. Note the logarithmic scale on the vertical axes. Data from the National Covid-19 Immunisation Programme, see <span style='color:blue'>github.com/CITF-Malaysia/citf-public</span>.",
         caption = "Reproducible code for the figure on <span style='color:blue'>github.com/hrlai/covid-19-vaccination-my-vis</span>.") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 14, colour = "black"),
          axis.text = element_text(size = 12, colour = "black"),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.title = element_textbox_simple(
              size = 16,
              padding = margin(5.5, 5.5, 5.5, 5.5),
              margin = margin(0, 0, 0, 0)
          ),
          plot.subtitle = element_textbox_simple(
              size = 9,
              padding = margin(0, 5.5, 5.5, 5.5),
              margin = margin(0, 0, 5.5, 0)
          ),
          plot.caption = element_textbox_simple(
              size = 5,
              padding = margin(5.5, 5.5, 0, 5.5),
              hjust = 1,
              halign = 1
          ))
dev.off()

# convert to PNG
p <- image_read_pdf("fig/vax_doses.pdf")
image_write(p, 
            path = "fig/vax_doses.png", 
            format = "png",
            quality = 100) 
