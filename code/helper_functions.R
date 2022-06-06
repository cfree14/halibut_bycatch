

# Plot bycatch by species
# Stats: comm_name, psets,
# data <- data3; plot_title <- "CDFW landings receipts: northern small mesh gillnets"
plot_bycatch_spp <- function(data, plot_title){

  # Compute p(occurence)
  nsets_tot <- n_distinct(data$set_id)
  stats <- data %>%
    group_by(comm_name) %>%
      summarize(nsets=n_distinct(set_id),
                psets=nsets/nsets_tot,
                ratio_med=median(ratio)) %>%
      arrange(desc(psets))

  # Theme
  theme1 <-  theme(axis.text=element_text(size=5),
                   axis.title=element_text(size=7),
                   axis.title.y=element_blank(),
                   plot.title=element_text(size=8),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

  # Determine set type
  set_type <- ifelse(grepl("gillnet", tolower(plot_title)), "gillnet sets", "trawl tows")
  xaxis_title <- paste0("Bycatch occurence\n(percent of ", set_type, ")")

  # Plot data
  g1 <- ggplot(stats, aes(y=factor(comm_name, levels=comm_name), x=psets)) +
    # Boxplots
    geom_bar(stat="identity") +
    # Labels
    labs(x=xaxis_title, y="", tag="A", title=plot_title) +
    # Axis
    scale_x_continuous(labels=scales::percent) +
    # Theme
    theme_bw() + theme1
  g1

  # Plot data
  g2 <- ggplot(data, aes(y=factor(comm_name, levels=stats$comm_name), x=ratio)) +
    # Boxplots
    geom_boxplot(fill="grey90", lwd=0.2, outlier.size = 0.3) +
    # Reference line
    geom_vline(xintercept = 1) +
    # Labels
    labs(x="Bycatch ratio\n(bycatch / halibut catch)", y="", tag="B", title="  ") +
    # Axis
    scale_x_continuous(trans="log10",
                       breaks=c(0.1, 1, 10, 100),
                       labels=c("0.1", "1", "10", "100")) +
    # Theme
    theme_bw() + theme1 +
    theme(axis.text.y=element_blank())
  g2

  # Merge plots
  g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.55, 0.45))
  g

  # Return plot
  return(g)


}



plot_bycatch_spp_over_time <- function(data, plot_title){

  # Compute p(occurence)
  nsets_tot <- n_distinct(data$set_id)
  stats <- data %>%
    group_by(comm_name) %>%
    summarize(nsets=n_distinct(set_id),
              psets=nsets/nsets_tot,
              ratio_med=median(ratio)) %>%
    arrange(desc(psets))

  # Identify top-20 species
  top20spp <- stats$comm_name[1:20]

  # Theme
  theme2 <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=8),
                  strip.text=element_text(size=7),
                  plot.title=element_text(size=8),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.background = element_rect(fill=alpha('blue', 0)))

  # Plot ratio over time
  g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=year, y=ratio, group=year)) +
    facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
    geom_boxplot(lwd=0.3, outlier.size = 0.5, color="grey40", fill="grey90") +
    # Reference line
    geom_hline(yintercept=1) +
    # Labels
    labs(y="Bycatch ratio\n(bycatch / halibut catch)", x="Year", title=plot_title) +
    # Axis
    scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
    # Theme
    theme_bw() + theme2 +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g

  # Return plot
  return(g)

}


plot_bycatch_spp_by_block <- function(data, plot_title){

  # Number of sets in dataset
  nsets_tot <- n_distinct(data$set_id)

  # Determine species order
  stats <- data %>%
    group_by(comm_name) %>%
    summarize(nsets=n_distinct(set_id),
              psets=nsets/nsets_tot,
              ratio_med=median(ratio)) %>%
    arrange(desc(psets))

  # Top 20 species
  top20spp <- stats$comm_name[1:20]

  # Blocks
  blocks <- wcfish::blocks %>%
    mutate(block_id=as.character(block_id))

  # Data by block
  data_block <- data %>%
    mutate(block_id=as.character(block_id)) %>%
    group_by(comm_name, block_id) %>%
    summarize(ratio_med=median(ratio)) %>%
    ungroup()

  # Merge
  data_block_sf <- blocks %>%
    left_join(data_block, by="block_id")

  # Filter
  data_use <- data_block_sf %>%
    filter(comm_name%in%top20spp & !is.na(ratio_med) & block_type=="Inshore")

  # USA
  usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
  mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf", scale="small")

  # Theme
  theme_map <- theme(axis.text=element_text(size=6),
                     axis.title=element_blank(),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     strip.text=element_text(size=7),
                     plot.title = element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.position = "bottom",
                     legend.background = element_rect(fill=alpha('blue', 0)))

  # Plot
  g <- ggplot(data_use,  aes(fill=ratio_med)) +
    facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
    geom_sf(lwd=0.1) +
    # Plot Point Arguello
    geom_hline(yintercept = 34.577201, lwd=0.2) +
    # USA
    geom_sf(data=usa, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
    geom_sf(data=mexico, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
    # Labels
    labs(x="", y="", title=plot_title) +
    # Legend
    scale_fill_gradientn(name="Median\nbycatch ratio",
                         colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                         trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Axis
    scale_x_continuous(breaks=seq(-120, -116, 2)) +
    scale_y_continuous(breaks=seq(32, 42, 2)) +
    # Crop
    coord_sf(xlim= sf::st_bbox(data_use)[c(1, 3)] %>% as.numeric(),
             ylim= sf::st_bbox(data_use)[c(2, 4)] %>% as.numeric()) +
    # Theme
    theme_bw() + theme_map
  g

  # Return plot
  return(g)

}



plot_bycatch_spp_by_yday <- function(data, plot_title){

  # Number of sets in dataset
  nsets_tot <- n_distinct(data$set_id)

  # Determine species order
  stats <- data %>%
    group_by(comm_name) %>%
    summarize(nsets=n_distinct(set_id),
              psets=nsets/nsets_tot,
              ratio_med=median(ratio)) %>%
    arrange(desc(psets))

  # Top 20 species
  top20spp <- stats$comm_name[1:20]

  # Theme
  theme3 <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=8),
                  strip.text=element_text(size=7),
                  plot.title = element_text(size=8),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.background = element_rect(fill=alpha('blue', 0)))

  # Bycatch ratio by day of year
  g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=date_dummy, y=ratio)) +
    facet_wrap(~factor(comm_name, levels=top20spp), ncol=5, scales="free_y") +
    geom_point(pch=21, color="grey60", alpha=0.5, size=0.7) +
    geom_smooth(fill="grey30", color="black", alpha=0.7, lwd=0.5) +
    # Horizontal line
    geom_hline(yintercept=1, linetype="dotted") +
    # Labels
    labs(x="Day of year", y="Bycatch ratio\n(bycatch / halibut catch)", title=plot_title) +
    # Axis
    scale_x_date(date_breaks = "2 months", date_labels =  "%b") +
    scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
    # Theme
    theme_bw() + theme3 +
    theme(axis.text.x=element_text(size=6))
  g

  # Return plot
  return(g)

}

