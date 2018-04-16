#load paired data
paired_filename <- file.path("data_cached", paste0(site, "_", site_paired, "_prepped_WQbystorm.csv"))
paired <- read.csv(paired_filename, stringsAsFactors = F)

#Make linear models of the difference after treatment:
m.TPE = lm((log10(OT.load)) ~ (log10(FS.load))*trt, data=TPEvent.17)#.vegon)
m.SSE = lm((log10(OT.load)) ~ (log10(FS.load))*trt, data=SSEvent.17)

m.TPE = lm((log10(OT.load)) ~ (log10(FS.load))*veggieTime, data=TPEvent.17)
m.SSE = lm((log10(OT.load)) ~ (log10(FS.load))*veggieTime, data=SSEvent.17)
