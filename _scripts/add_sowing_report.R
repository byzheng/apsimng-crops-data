# Temp script to add sowing report to all get_experiments

rm(list = ls())

library(rapsimng)
file <- "C:\\APSIM\\ApsimX/Tests/Validation/Chickpea/Chickpea.apsimx"

apsimx <- read_apsimx(file)
sowing_report <- search_path(apsimx, "[Replacements].SowingReport")

all_zones <- search_node(apsimx, "$type" = "Models.Core.Zone, Models", all = TRUE)
i <- length(all_zones)
for (i in rev(seq(along = all_zones))) {
    zone <- all_zones[[i]]
    existing_report <- search_path(zone$node, "[SowingReport]")
    if (length(existing_report) > 0) {
        next
    }
    apsimx <- insert_model(apsimx, zone$path, sowing_report$node)
}

write_apsimx(apsimx, file.path(dirname(file), "Chickpea.apsimx"))
