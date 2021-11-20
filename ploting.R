plotcovid <- function (data, 
                       scale = c("s", "ls", "ps"), 
                       by = c("all", "ha","age"), 
                       time = c("all", "recent")) {
  scale <- match.arg(scale)
  by <- match.arg(by)
  if(scale == "ps") {
    if(by == "all") data <- data %>% mutate(Cases = Cases / pop_bc * 1e5)
    if(by == "ha" || by == "age") {
      data <- data %>% mutate(Cases = Cases / pop * 1e5)
      }
  }

  # TODO: Could use a if statement if desired
  # TODO: could potentially pull out some more code using do.call if desired
  p <- switch(by,
              all = ggplot(data, aes(Date, Cases))+
                geom_point(colour = "darkblue", shape = 16, alpha = .4) +
                geom_line(aes_string(y = scale), size = 1.5, colour = "darkblue"),
              
              ha = ggplot(data, aes(Date, Cases, colour = HA)) +
                geom_point(shape = 16, alpha = .4) +
                geom_line(aes_string(y = scale), size = 1.5) +
                scale_colour_brewer(palette = "Set1") +
                theme(legend.title = element_blank(), legend.position = "bottom"),
              
              age = ggplot(data, aes(Date, Cases, colour = Age)) +
                geom_point(shape = 16, alpha = .4) +
                geom_line(aes_string(y = scale), size = 1.5) +
                scale_colour_viridis_d() +
                theme(legend.title = element_blank(),
                      legend.position = "bottom")
  )
  

  # For different ylab and y scales
  p <-  switch (scale,
                s = p,
                ls = p + 
                  scale_y_continuous(trans = pseudo_log_trans(base = 10), 
                                     breaks = log10_breaks) +
                  ylab("Cases (log scale)"),
                ps = p +
                  ylab("Cases per 100K population")
  )
  
  # scale x based on time
  p <-  switch(time,
               all = p + scale_x_date(date_breaks = "3 months", date_labels = "%b %Y"),
               recent = p
  )
  p 
}