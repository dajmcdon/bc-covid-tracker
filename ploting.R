plotcovid <- function (data,
                       scale_y = c("none", "log_scale", "per_capita"),
                       subgroups = c("all", "health_authority", "age"),
                       time = c("all", "recent")) {
  scale_y <- match.arg(scale_y)
  subgroups <- match.arg(subgroups)
  
  
  smoother <- switch (
    scale_y,
    none = data$s,
    log_scale = data$ls,
    per_capita = data$ps
  )
  
  scale_y <- switch (
    scale_y,
    none = "s",
    log_scale = "ls",
    per_capita = "ps"
  )
  if (scale_y == "ps") {
    if (subgroups == "all")
      data <- data %>% mutate(Cases = Cases / pop_bc * 1e5)
    if (subgroups == "health_authority" || subgroups == "age") 
      data <- data %>% mutate(Cases = Cases / pop * 1e5)
  }
  
  p <- switch (subgroups,
    all = ggplot(data, aes(Date, Cases)) +
      geom_point(aes(y = Cases, text = paste0("Cases: ", round(Cases,2))),
                 colour = "darkblue", shape = 16, alpha = .4) +
      geom_line(aes_string(y = scale_y, group = 1,
                text = "paste0(\"Smoother: \", round(smoother,2))"),
                size = 1.5, colour = "darkblue"),
    
    health_authority = ggplot(data, aes(Date, Cases, colour = HA)) +
      geom_point(aes(y = Cases, text = paste0("Cases: ", round(Cases,2))),
                 shape = 16, alpha = .4) +
      geom_line(aes_string(y = scale_y, group = "1",
                           text = "paste0(\"Smoother: \", round(smoother,2))"),
                size = 1.5) +
      scale_colour_brewer(palette = "Set1") +
      theme(legend.title = element_blank(), legend.position = "bottom"),
    
    age = ggplot(data, aes(Date, Cases, colour = Age)) +
      geom_point(aes(y = Cases, text = paste0("Cases: ", round(Cases,2))),
                 shape = 16, alpha = .4) +
      geom_line(aes_string(y = scale_y, group = "1",
                           text = "paste0(\"Smoother: \", round(smoother,2))"),
                size = 1.5) +
      scale_colour_viridis_d() +
      theme(legend.title = element_blank(),
            legend.position = "bottom")
  )
  
  # Add y aesthetics based on `scale_y`
  p <-  switch (scale_y,
    s = p,
    ls = p +
      scale_y_continuous(trans = pseudo_log_trans(base = 10),
                         breaks = log10_breaks) +
      ylab("Cases (log scale_y)"),
    ps = p +
      ylab("Cases per 100K population")
  )
  
  # Add x aesthetics based on `time`
  p <-  switch (time,
    all = p + scale_x_date(date_breaks = "3 months", date_labels = "%b %Y"),
    recent = p
  )
  
  switch(subgroups,
         all = p %>% ggplotly(tooltip = c("Date","text")),
         health_authority = p %>% ggplotly(tooltip = c("Date","HA","text")),
         age = p %>% ggplotly(tooltip = c("Date","Age","text")))
}
