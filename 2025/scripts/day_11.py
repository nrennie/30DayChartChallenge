import pandas as pd
import numpy as np
import plotnine as gg
import matplotlib.pyplot as plt

# Data
income = pd.read_csv("2025/data/income-share-of-the-richest-1.csv")

# Wrangling
plot_data = income[(income["Country"] == "World") & (income["Year"] >= 1980)].rename(columns={"Income share of the richest 1% (before tax) (World Inequality Database)": "Income"})
plot_data['y'] = 1

# Colours
bg_col = "#FAFAFA"
text_col = "#000000"

# Plot
g = (gg.ggplot()
    + gg.geom_tile(gg.aes(x="Year", y="y", fill="Income"), data=plot_data)
    + gg.scale_fill_continuous(cmap_name="bwr")
    + gg.theme_void()
    + gg.theme(
        legend_position="none",
        plot_margin=0,
        plot_caption=gg.element_text(color=text_col, ha='left'),
        figure_size = (8, 4)
    )
)
g.draw()

gg.ggsave(g, "2025/viz/day_11.png")