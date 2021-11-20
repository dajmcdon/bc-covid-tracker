popbc = 5147712

plotcovid <- function (data, scale = c("s", "ls", "ps")) {
  scale <- match.arg(scale)
  if(scale == "ps") {
    data <- data %>% mutate(Cases = Cases / pop_bc * 1e5)
    }
  
  p <- ggplot(data, aes(Date, Cases)) +
    geom_point(colour = "darkblue", shape = 16, alpha = .4) +
    geom_line(aes_string(y=scale), size = 1.5, colour = "darkblue") 
  
  p = switch (scale,
          s = p,
          ls = p + 
            scale_y_continuous(trans = pseudo_log_trans(base = 10), 
                                  breaks = log10_breaks) +
            ylab("Cases (log scale)"),
          ps = p +
            ylab("Cases per 100K population")
  )
  p 
}