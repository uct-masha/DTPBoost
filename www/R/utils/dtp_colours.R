# define the colours
dtp_colours <- list(
  tol3budget=c("#4477AA", "#88CCEE", "#44AA99"),
  tol1dtp=c("#4477AA"),
  tol2dtp=c("#4477AA", "#44AA99"),
  tol3dtp=c("#4477AA", "#44AA99", "#CC6677"),
  tol4dtp=c("#4477AA", "#44AA99", "#DDCC77","#CC6677"),
  tol5dtp=c("#4477AA", "#88CCEE", "#44AA99", "#DDCC77","#CC6677"),
  tol6dtp=c("#4477AA", "#88CCEE", "#44AA99", "#DDCC77","#CC6677","#AA4499"),
  tol7dtp=c("#4477AA", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499"),
  tol8dtp=c("#332288", "#4477AA", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499"),
  tol9dtp=c("#332288", "#4477AA", "#88CCEE", "#44AA99", "#117733",  "#DDCC77", "#CC6677","#AA4499", "#882255"),
  tol10dtp=c("#332288", "#4477AA", "#88CCEE", "#44AA99", "#117733",  "#999933", "#DDCC77", "#CC6677", "#AA4499", "#882255"),
  # If we have more than 10 scenarios we will default to the rainbow palette
  tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
)

# function to generate a colour palette
dtp_palettes <- function(name, n, all_palettes = dtp_colours, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}

# function to handle discrete values (colour)
scale_colour_dtp_d = function(name) {
  ggplot2::scale_colour_manual(values = dtp_palettes(name, type = "discrete"))
}

# function to handle discrete values (fill)
scale_fill_dtp_d = function(name) {
  ggplot2::scale_fill_manual(values = dtp_palettes(name, type = "discrete"))
}

# function to handle continuous values (colour)
scale_colour_dtp_c = function(name) {
  ggplot2::scale_colour_gradientn(colours = dtp_palettes(name = name, type = "continuous"))
}

# function to handle continuous values (fill)
scale_fill_dtp_c = function(name) {
  ggplot2::scale_fill_gradientn(colours = dtp_palettes(name = name, type = "continuous"))
}



























#
