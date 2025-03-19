# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(OECD)
library(ggdist)
library(rcartocolor)


# Load data ---------------------------------------------------------------

# Download from: https://data-explorer.oecd.org/vis?fs[0]=Topic%2C0%7CEducation%23EDU%23&pg=20&fc=Topic&bp=true&snb=38&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_EAG_SAL_ACT%40DF_EAG_SAL_ACT_TCH&df[ag]=OECD.EDU.IMEP&df[vs]=1.1&dq=AUS%2BAUT%2BBFL%2BBFR%2BCAN%2BCHL%2BCOL%2BCRI%2BCZE%2BDNK%2BEST%2BFIN%2BFRA%2BDEU%2BGRC%2BHUN%2BISL%2BIRL%2BISR%2BITA%2BJPN%2BKOR%2BLVA%2BLTU%2BLUX%2BMEX%2BNLD%2BNZL%2BNOR%2BPOL%2BPRT%2BSVK%2BSVN%2BESP%2BSWE%2BCHE%2BTUR%2BUKB%2BUKM%2BUSA%2BARG%2BBRA%2BBGR%2BCHN%2BHRV%2BIND%2BIDN%2BPER%2BROU%2BSAU%2BZAF%2BEU25%2BOECD..USD_PPP...Y25T64._T.&ly[rw]=REF_AREA&ly[cl]=EDUCATION_LEV&to[TIME]=false&vw=tb
salaries <- readr::read_csv("2024/data/salaries.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()


# Data wrangling ----------------------------------------------------------

plot_data <- salaries |>
  filter(`Unit of measure` == "US dollars, PPP converted") |>
  select(`Education level`, OBS_VALUE) |>
  drop_na() |>
  mutate(
    `Education level` = factor(`Education level`,
      levels = c(
        "Pre-primary education",
        "Primary education",
        "Lower secondary general education",
        "Upper secondary general education"
      )
    )
  )


# Define colours and fonts-------------------------------------------------

bg_col <- "grey95"
text_col <- "black"
cols_vec <- rcartocolor::carto_pal(5, "Bold")[1:4]
names(cols_vec) <- unique(plot_data$`Education level`)
highlight_col <- cols_vec[1]

body_font <- "roboto"
title_font <- "roboto_slab"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "recording"),
  device = "png",
  width = 5,
  height = 7,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Teachers' salaries increase with level of education"
st <- "Data from OECD shows there is a lot of variability in teachers' annual
salaries (converted to US Dollars) across the world. There is a slight upward trend
in salary as the level of education increases, with upper secondary general education
teachers paid the most on average."
cap <- paste0(
  "**Data**: OECD<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(
    x = OBS_VALUE,
    y = `Education level`,
    fill = `Education level`
  )
) +
  geom_slab(
    aes(
      colour = `Education level`
    ),
    scale = 0.52,
    linewidth = 0.5,
    alpha = 0.5,
    stat = "slab"
  ) +
  stat_dotsinterval(side = "bottom", scale = 0.52, slab_linewidth = NA) +
  geom_text(
    data = data.frame(x = seq(25000, 100000, 25000),
                      y = rep(0.5, 4)),
    mapping = aes(
      x = x, y = y, label = paste0("$", format(x,big.mark=",",scientific=FALSE))
    ),
    family = body_font,
    size = 7,
    inherit.aes = F
  ) +
  geom_segment(
    data = data.frame(x = seq(25000, 100000, 25000)),
    mapping = aes(x = x, y = 0.55, yend = 4.6),
    colour = alpha(text_col, 0.2),
    inherit.aes = F,
    linewidth = 0.5
  ) +
  labs(
    x = "", y = "",
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_fill_manual(values = cols_vec) +
  scale_colour_manual(values = cols_vec) +
  scale_y_discrete(labels = scales::label_wrap(12)) +
  theme_minimal(base_size = 25, base_family = body_font) +
  theme(
    plot.margin = margin(5, 10, 5, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.3,
      family = title_font,
      face = "bold",
      size = rel(1.6)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = -25, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    axis.text.x = element_text(family = body_font,
                               margin = margin(b = -50)),
    axis.text.y = element_text(family = body_font,
                               lineheight = 0.5,
                               margin = margin(r = -5)),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "viz", "gifs", paste0("day_06", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

unlink("2024/recording/", recursive = TRUE)
