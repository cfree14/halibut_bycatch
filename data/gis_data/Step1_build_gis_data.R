

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/Chris/UCSB/data/california/gis_data/raw"
outdir <- "data/gis_data/processed"
plotdir <- "data/gis_data/figures"

# Read data
data_orig <- sf::st_read(file.path(indir, "management/MAN_SCSR_HalibutTrawlGrounds/MAN_SCSR_HalibutTrawlGrounds.shp"))
state_waters <- sf::st_read(file.path(indir, "management/StateWaterJurisdiction/MAN_CA_StateWaterLine.shp"))


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(ccr_section=ccr_sectio, description=descriptio) %>%
  # Add sub area
  mutate(sub_area=recode(description,
                         "Sub-Area (1)(B) Santa Barbara Point to Pitas Point:"="1b",
                         "Sub-Area (2)(A) Point Conception to Gaviota:"="2a",
                         "Sub-Area(1)(A) Rocky Point (near Point Arguello) to Point Conception:"="1a",
                         "Sub-Area(1)(C) Hueneme Canyon to Laguna Point:"="1c")) %>%
  # Add short name
  mutate(sub_area=recode(description,
                         "Sub-Area (1)(B) Santa Barbara Point to Pitas Point:"="Santa Barbara Point to Pitas Point",
                         "Sub-Area (2)(A) Point Conception to Gaviota:"="Point Conception to Gaviota",
                         "Sub-Area(1)(A) Rocky Point (near Point Arguello) to Point Conception:"="Rocky Point to Point Conception",
                         "Sub-Area(1)(C) Hueneme Canyon to Laguna Point:"="Hueneme Canyon to Laguna Point"))



# Plot data
################################################################################

# Landmarks
point_arguello <- c(-120.650602, 34.577201)
# point_conception <- c(-120.472066, 34.448312)
point_sur <- c(-121.902584, 36.307328)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.2, 0.1),
                   legend.key = element_rect(fill=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot stock central/southern stock boundary
  # geom_hline(yintercept=point_conception[2], linetype="dotted") +
  # Plot gillnet line
  geom_hline(yintercept=point_arguello[2]) +
  geom_hline(yintercept=point_sur[2], linetype="dotted", color="grey40") +
  annotate(geom="text", x=-124.5, y=point_arguello[2]+0.08, hjust=0, vjust=0,
           label="Gillnet fishery south of here", size=2.5) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot state waters
  geom_sf(data=state_waters, color="grey30", lwd=0.2) +
  # Plot SCSRs
  geom_sf(data=data, mapping=aes(fill=status), color=NA) +
  # Plot points
  geom_point(mapping=aes(x=point_arguello[1], y=point_arguello[2]), size=1.5) +
  geom_text(mapping=aes(x=point_arguello[1], y=point_arguello[2]), label="Point Arguello", hjust=0, nudge_x=0.1, size=1.8) +
  geom_point(mapping=aes(x=point_sur[1], y=point_sur[2]), size=1.5) +
  geom_text(mapping=aes(x=point_sur[1], y=point_sur[2]), label="Point Sur", hjust=0, nudge_x=0.1, size=1.8) +
  # Labels
  labs(x="", y="") +
  scale_fill_discrete(name="CHTG areas") +
  scale_y_continuous(breaks=seq(32,42, 1)) +
  # Crop
  coord_sf(xlim = c(-124.25, -117.1), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "halibut_mgmt_map.png"),
       width=4, height=6.5, units="in", dpi=600)





