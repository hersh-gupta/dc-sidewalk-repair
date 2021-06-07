library(tidyverse)
library(geojsonsf)
library(sf)
library(hrbrthemes)
library(anytime)
library(nord)

# Download street and sidewalk repairs

repair_requests_2020 <- geojson_sf("data/311_City_Service_Requests_in_2020.geojson")

# Summarize count of requests

repair_requests_2020_sum <- repair_requests_2020 %>%
  st_drop_geometry() %>%
  filter(SERVICEORDERSTATUS %in% c("In-Progress","Closed")) %>%
  group_by(SERVICECODEDESCRIPTION, SERVICEORDERSTATUS) %>% 
  summarize(n = n()) %>%
  mutate(pct = n/sum(n))

repair_requests_2020_sum %>%
  ggplot(aes(fct_reorder(SERVICECODEDESCRIPTION,n), n, fill = fct_rev(SERVICEORDERSTATUS))) +
  geom_col(position = position_stack(reverse = F)) +
  geom_text(aes(label = paste0(n," (", scales::percent(pct, accuracy = 1), ")")), 
            position = position_stack(vjust = .5, reverse = F), 
            color = "white", fontface = "bold", data = . %>% filter(n > 10)) +
  scale_fill_manual(values = c('#f4777f','#93003a'), name = NULL) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Sidewalk Repair Request Closure Lags Behind Pothole Request Closure",
       subtitle = "Days to Resolve DC 311 Repair Requests, 2020",
       caption = "'In-Progress' Submissions as of June 6, 2021\nSource: OpenData DC, 311 Requests in 2020",
       fill = NULL) +
  theme_ipsum_pub(base_size = 12) +
  theme(legend.position = "right")

ggsave(filename = "output/repair_requests_2020_sum.png", device = "png", width = 11, height = 6, units = "in", dpi = 320)


# Clean and add format dates

repair_requests_2020_cl <- repair_requests_2020 %>%
  st_drop_geometry() %>%
  mutate(SERVICEORDERDATE = anydate(SERVICEORDERDATE),
         RESOLUTIONDATE = anydate(RESOLUTIONDATE),
         SERVICEDUEDATE = anydate(SERVICEDUEDATE),
         RESOLUTIONDATE = if_else(RESOLUTIONDATE == "1970-01-01", Sys.Date(), RESOLUTIONDATE),
         days_to_resolve = RESOLUTIONDATE- SERVICEORDERDATE
         )

# Save data
write_rds(repair_requests_2020_cl, "output/repair_requests_2020_cl.rds")

# Boxplot

repair_requests_2020_cl %>%
  filter(SERVICEORDERSTATUS %in% c("In-Progress","Closed")) %>%
  ggplot(aes(fct_reorder(SERVICECODEDESCRIPTION,days_to_resolve,median), 
             days_to_resolve, color = SERVICECODEDESCRIPTION,
             fill = SERVICECODEDESCRIPTION)) +
  geom_boxplot(lwd = 1.01, alpha = .2)  +
  #geom_jitter(alpha = .05) +
  coord_flip() +
  labs(y = "Days to Resolve", x = NULL, color = NULL, fill = NULL,
       title = "Sidewalk Repair Requests Usually Take 100+ Days to Resolve",
       subtitle = "Days to Resolve DC 311 Repair Requests, 2020",
       caption = "'In-Progress' Submissions as of June 6, 2021\nSource: OpenData DC, 311 Requests in 2020") +
  scale_color_manual(values = nord(palette = "victory_bonds", 4, reverse = T)) +
  scale_fill_manual(values = nord(palette = "victory_bonds", 4, reverse = T)) +
  facet_wrap(~SERVICEORDERSTATUS) +
  theme_ipsum_pub(base_size = 12)+
  theme(legend.position = "none")

ggsave(filename = "output/repair_boxplot.png", device = "png", width = 11, height = 6, units = "in", dpi = 320)

# Table

repair_requests_2020_cl %>%
  filter(SERVICEORDERSTATUS %in% c("Closed")) %>% 
  group_by(SERVICECODEDESCRIPTION) %>% 
  summarize(med = median(days_to_resolve),
            avg = mean(days_to_resolve) %>% round(0),
            count = n())
  
