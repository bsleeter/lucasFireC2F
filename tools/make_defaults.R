# =============================================================================
# make_defaults.R - builds the default project tables shipped with lucasFireC2F
# =============================================================================
# Writes defaults/*.csv. tools/inline_defaults.R copies them into package.xml as <record> rows, so a new project
# opens with a working configuration. Run from the repo root:  Rscript tools/make_defaults.R
# Sources:
#   FuelModel         Scott & Burgan (2005) 40 standard fire behavior fuel models, metric (R package Rothermel,
#                     data SFM_metric: loads t/ha, SAV 1/m, depth cm, dead extinction moisture %, heat kJ/kg)
#   MoistureScenario  Scott & Burgan D1L1-D4L4 (the four C2F-W moisture scenarios), %
#   FuelType          LANDFIRE FBFM40 grid codes, with non-burnable NB1-NB9
#                     plus the crosswalk columns: FuelModel, EngineFuelModel (the C2F-W v1.0.1 lookup has no GR9, SH1,
#                     TL4, TL7 or SB1-4, so those are remapped to the nearest modelled type, as script 200), Lifeform
#   Lifeform (with response functions), FlameLengthClass   Wildfire Risk to Communities V2 (Scott et al. 2024)
# =============================================================================
suppressMessages(library(Rothermel))
dir.create("defaults", showWarnings = FALSE)
w <- function(x, f) { write.csv(x, file.path("defaults", f), row.names = FALSE, na = ""); cat("wrote defaults/", f, " (", nrow(x), " rows)\n", sep = "") }

data(SFM_metric); data(scenarios)
codes <- c(sprintf("GR%d", 1:9), sprintf("GS%d", 1:4), sprintf("SH%d", 1:9), sprintf("TU%d", 1:5), sprintf("TL%d", 1:9), sprintf("SB%d", 1:4))
ids   <- c(101:109, 121:124, 141:149, 161:165, 181:189, 201:204)
grp   <- c(rep("GR", 9), rep("GS", 4), rep("SH", 9), rep("TU", 5), rep("TL", 9), rep("SB", 4))
grpname <- c(GR = "Grass", GS = "Grass-Shrub", SH = "Shrub", TU = "Timber-Understory", TL = "Timber Litter", SB = "Slash-Blowdown")
life  <- c(GR = "Grass", GS = "Shrub", SH = "Shrub", TU = "Tree", TL = "Tree", SB = "Tree")
S <- SFM_metric[codes, ]
w(data.frame(Name = codes, ID = ids, FuelGroup = unname(grpname[grp]), Dynamic = S$Fuel_Model_Type == "D",
             Load1h = S$Load_1h, Load10h = S$Load_10h, Load100h = S$Load_100h, LoadLiveHerb = S$Load_Live_Herb, LoadLiveWoody = S$Load_Live_Woody,
             SAV1h = S$`SA/V_1h`, SAVLiveHerb = S$`SA/V_Live_Herb`, SAVLiveWoody = S$`SA/V_Live_Woody`,
             DepthCm = S$Fuel_Bed_Depth, DeadExtinctionMoisture = S$Mx_dead, HeatContent = S$Heat_1h), "FuelModel.csv")

m <- scenarios[c("D1L1", "D2L2", "D3L3", "D4L4"), ]
w(data.frame(Name = rownames(m), ID = 1:4, Moisture1h = m$Moist_1h, Moisture10h = m$Moist_10h, Moisture100h = m$Moist_100h,
             MoistureLiveHerb = m$Moist_Live_Herb, MoistureLiveWoody = m$Moist_Live_Woody, Description = m$Description), "MoistureScenario.csv")

gcol <- c(GR = "255,255,190,160", GS = "255,211,127,200", SH = "205,137,102,230", TU = "115,178,115,230", TL = "56,168,0,230", SB = "137,90,68,230")
nb <- data.frame(Name = c("NB1", "NB2", "NB3", "NB8", "NB9"), ID = c(91, 92, 93, 98, 99),
                 Description = c("Urban / developed", "Snow / ice", "Agricultural", "Open water", "Bare ground"),
                 Color = c("255,156,156,156", "255,225,225,225", "255,245,202,122", "255,151,219,242", "255,204,204,204"))
# The crosswalk lives ON the fuel type row (fuel model codes as text): SyncroSim default records cannot reference rows
# of another datasheet (their IDs do not exist until the project is created), and stage 1 validates the codes.
# Spread remaps (P27 calibration, 2026-09-28; package default from 1.9.2): C2F-W's timber litter spreads too slowly against the
# record, so TL2 -> TL3 and TL3/TL4 -> TL5 (~1.65x timber-litter spread, target ~1.8x; TL1/TL8 kept for the recent-fire rules),
# and GS2 -> GS1 slows grass-shrub. Only spread changes: effects (stage 6) and fuel groups use FuelModel, not EngineFuelModel.
REMAP <- c(GR9 = "GR8", GS2 = "GS1", SH1 = "SH2", TL2 = "TL3", TL3 = "TL5", TL4 = "TL5", TL7 = "TL8", SB1 = "TL6", SB2 = "SH5", SB3 = "SH5", SB4 = "SH5")
ft <- rbind(nb, data.frame(Name = codes, ID = ids, Description = paste(grpname[grp], codes), Color = paste0("255,", gsub(",[0-9]+$", "", gcol[grp]))))
ft$FuelModel <- ifelse(ft$ID < 100, NA, ft$Name)
ft$EngineFuelModel <- ifelse(ft$Name %in% names(REMAP), REMAP[ft$Name], ft$FuelModel)
ft$NonBurnable <- ft$ID < 100
ft$Lifeform <- ifelse(ft$ID < 100, NA, unname(life[substr(ft$Name, 1, 2)]))
w(ft, "FuelType.csv")

# response functions on the lifeform row (same reason): % change by FSim flame-length class 1-6
rf <- rbind(Grass = -c(10, 25, 40, 55, 70, 85), Shrub = -c(20, 35, 50, 65, 80, 95), Tree = -c(25, 40, 55, 70, 85, 100))
w(data.frame(Name = rownames(rf), setNames(as.data.frame(rf), sprintf("Response%d", 1:6))), "Lifeform.csv")
fl <- data.frame(Name = sprintf("FIL%d", 1:6), ID = 1:6, LowerFt = c(0, 2, 4, 6, 8, 12), UpperFt = c(2, 4, 6, 8, 12, NA),
                 Description = c("< 2 ft", "2-4 ft", "4-6 ft", "6-8 ft", "8-12 ft", "> 12 ft"))
w(fl, "FlameLengthClass.csv")
w(data.frame(Name = c("Spring", "Summer", "Autumn", "Winter"), FirstMonth = c(3, 6, 10, 12), LastMonth = c(5, 9, 11, 2),
             LiveMoistureClass = c(2, 1, 1, 3)), "Season.csv")
w(data.frame(Name = c("Human", "Lightning")), "Cause.csv")
w(data.frame(Class = 1:3, MaximumEMC = c(4.5, 7.5, 10.5)), "MoistureDeadClass.csv")
