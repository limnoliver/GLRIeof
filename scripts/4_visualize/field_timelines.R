# create a visual timeline for site
#library(timeline)

timeline <- read.csv('H:/Projects/GLRIeof/data_raw/SW1_field_activity.csv', stringsAsFactors = FALSE, strip.white = TRUE)
timeline$date <- as.Date(timeline$date, format = '%m/%d/%Y')
timeline.events <- timeline[,c('date', 'activity_group')]
timeline.events <- timeline.events[!is.na(timeline.events$date), ]
timeline.events <- timeline.events[grep('application', timeline.events$activity_group),]

cleaned.timeline <- data.frame(
  content = c("Pre-Grass Waterway", "Post-Grass Waterway", "Corn", "Corn", "Corn", "Corn", "Alfalfa"),
  id = c("Intervention", "Intervention", 'Crop', 'Crop', "Crop", 'Crop', "Crop"),
  StartDate = c( timeline$date[1], as.Date('2015-06-01'), timeline$date[5], timeline$date[11], timeline$date[17], timeline$date[23], timeline$date[28]),
  EndDate = c(as.Date('2015-05-09'), as.Date('2017-09-14'), timeline$date[8],as.Date('2013-10-15'), timeline$date[19],  as.Date('2015-10-01'), as.Date('2017-09-14'))
)

# can add events
# see example: https://github.com/jbryer/timeline
#pdf('figures/SW1_timeline.pdf', height = 4, width = 10)

#dev.off()
