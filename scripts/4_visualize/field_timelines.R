# create a visual timeline for site
library(timeline)

timeline <- read.csv('data_raw/SW1_field_activity.csv', stringsAsFactors = FALSE, strip.white = TRUE)
timeline$date <- as.Date(timeline$date, format = '%m/%d/%Y')
timeline.events <- timeline[,c('date', 'activity_group')]
timeline.events <- timeline.events[!is.na(timeline.events$date), ]
timeline.events <- timeline.events[grep('application|fertilizer', timeline.events$activity_group),]
timeline.events$activity_group <- gsub("planting \\+ fertilizer", "fertilizer application", timeline.events$activity_group)
timeline.events$activity_group <- gsub(" application", "", timeline.events$activity_group)

cleaned.timeline <- data.frame(
  content = c("Pre-Grass Waterway", "Post-Grass Waterway", rep('corn', 4), '','alfalfa', rep('', 3)),
  id = c("Intervention", "Intervention", rep('crop', 9)),
  StartDate = c(timeline$date[1], as.Date('2015-06-01'), timeline$date[c(5,11,17,23)], timeline$date[28:32]),
  EndDate = c(as.Date('2015-05-09'), as.Date('2017-09-14'), timeline$date[c(8,13, 19, 25)], timeline$date[29:33])
)

# can add events
# see example: https://github.com/jbryer/timeline
#pdf('figures/SW1_timeline.pdf', height = 4, width = 10)

p <- timeline(cleaned.timeline, timeline.events, text.size = 6, event.col = 'date', 
         event.label.col = 'activity_group', event.above = FALSE, border.color = 'black', 
         border.linetype = 1,
         num.label.steps = 2, event.label = "Field Activity",
         event.text.vjust = 1, event.text.hjust = 0.5) +
  theme_classic() +
  scale_fill_manual(values = c(rep(rgb(135,193,137,max = 255), 2), rgb(206,200,69,max = 255), 
                               rgb(27,113,68,max = 255),  rgb(158,101,32,max = 255))) +
  theme(legend.position="none")

ggsave('figures/SW1_timeline.pdf', p, height = 4, width = 10)
