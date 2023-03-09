library('move2')
library('dplyr')
library('magrittr')
library('lubridate')
library('circular')
library('ggplot2')
library('units')
library('grid')
library('gridExtra')

## The parameter "data" is reserved for the data object passed on from the previous app

## to display messages to the user in the log file of the App in MoveApps one can use the function from the logger.R file: 
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

rFunction = function(data) {
  
  ids <- unique(mt_track_id(data))
  
  data %<>% mutate(azimuth = mt_azimuth(.), speed = mt_speed(.), month = month(mt_time(.),label=TRUE,abbr=FALSE), trackid = mt_track_id(.))
  
  data %<>% mutate(speed_categorical = cut(speed,breaks = c(0,0.5,1,2,5,10,15,ceiling(max(speed,na.rm=TRUE)))), speed = speed %>% set_units("m/s"))
  
  g <- list()
  for(i in seq(along=ids))
    {
    logger.info(ids[i])  
    data |>
        filter(!is.na(speed), trackid==ids[i]) |>
        ggplot() +
        coord_polar(start = pi) +
        geom_histogram(aes(
          x = set_units(azimuth,"degrees"),
          fill=speed_categorical
        ),
        breaks = seq(-180, 180, by = 10),
        position = position_stack(reverse = T)
        ) +
        scale_x_units(
          name = NULL,
          limits = set_units(c(-180, 180), "degrees"),
          breaks = (-2:2) * 90
        ) +
        labs(title=paste("Speed Direction Plots",id,sep=", ")) +
        facet_wrap(~month) +
        scale_fill_ordinal("Speed") +
        theme_linedraw() -> g[[i]]
    }

  data |>
    filter(!is.na(speed)) |>
    ggplot() +
    coord_polar(start = pi) +
    geom_histogram(aes(
      x = set_units(azimuth,"degrees"),
      fill=speed_categorical
    ),
    breaks = seq(-180, 180, by = 10),
    position = position_stack(reverse = T)
    ) +
    scale_x_units(
      name = NULL,
      limits = set_units(c(-180, 180), "degrees"),
      breaks = (-2:2) * 90
    ) +
    labs(title="Speed Direction Plots, all Tracks") +
    facet_wrap(~month) +
    scale_fill_ordinal("Speed") +
    theme_linedraw() -> g[[length(ids)+1]]
  
  gp  <- marrangeGrob(g, nrow = 1, ncol = 1)
  ggsave(appArtifactPath("speed_direction_plots.pdf"), plot = gp, width = 21, height = 29.7, units = "cm")
  
  return(data)
}
  

