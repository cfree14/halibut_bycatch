

# species <- "White Croaker"
plot_tot_catch <- function(species){

  # Build data
  spp_do <- species
  sdata <- data %>%
    # Reduce to species of interest
    filter(species==spp_do) %>%
    # Summarize by year
    group_by(year) %>%
    summarize(landings_mt=sum(landings_mt, na.rm=T)) %>%
    ungroup()

  # Plot data
  g <- ggplot(sdata, aes(x=year, y=landings_mt/1e3)) +
    geom_bar(stat="identity") +
    labs(x="Year", y="Landings (1000s mt)") +
    theme_bw()
  g

  # Return
  return(g)

}
