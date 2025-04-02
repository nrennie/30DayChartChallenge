import pandas as pd
import numpy as np
import plotnine as gg
import matplotlib.pyplot as plt
import textwrap

# Functions
def wrapping_func(text):
    return [textwrap.fill(wrapped_text, 12, break_long_words=False) for wrapped_text in text]

# Data
income = pd.read_csv("2025/data/income-share-of-the-richest-1.csv")

# Colours
bg_col = "#262626"
text_col = "#e5e5e5"

# Text
title = "Most unequal countries according to wealth distribution"
st = "Income is measured before payment of taxes and non-pension benefits but after the payment of public and\nprivate pensions."
cap = "Data: World Inequality Database (WID). Processed by Our World in Data\nGraphic: Nicola Rennie"

# Wrangling
plot_data = income[
    ~income["Country"].str.contains("(WID)", regex=True) &
    (income["Country"] != "World") &
    (income["Year"] >= 2010)
].rename(columns={"Income share of the richest 1% (before tax) (World Inequality Database)": "Income"}).dropna()

plot_data2 = (plot_data
    .groupby("Year", group_keys=False)
    .apply(lambda x: x.nlargest(4, "Income"))
    .sort_values(by=["Year", "Income"], ascending=[True, False])
    .reset_index(drop=True)
)
plot_data2["Rank"] = plot_data2.groupby("Year").cumcount() + 1
plot_data2["Rank"] = plot_data2["Rank"].astype(str)

label_data = plot_data2.groupby('Country').apply(lambda x: x.loc[x['Year'].idxmax()]).reset_index(drop=True)
label_data['blabel'] = "                   \n                   "
label_data['country_label'] = wrapping_func(label_data['Country'])

# Plot
g = (gg.ggplot()
    + gg.geom_point(gg.aes(x="Year", y="Rank", color="Country"), data=plot_data2, size=2.5)
    + gg.geom_line(gg.aes(x="Year", y="Rank", color="Country", group="Country"), data=plot_data2)
    + gg.geom_label(gg.aes(x="Year", y="Rank", color="Country", label="blabel"), fill=bg_col, data=label_data, size=5)
    + gg.geom_text(gg.aes(x="Year", y="Rank", color="Country", label="country_label"), data=label_data, size=5)
    + gg.scale_x_continuous(expand=(0, 0.1), limits=(2009.5, 2023.5), breaks=np.arange(2010, 2024), minor_breaks=np.arange(2009.75, 2023.5, 0.25))
    + gg.scale_y_discrete(limits=reversed)
    + gg.labs(title = title, subtitle = st, caption = cap)
    + gg.theme_light()
    + gg.theme(
        legend_position="none",
        plot_margin=0.01,
        axis_title=gg.element_blank(),
        axis_text=gg.element_text(color=text_col),
        axis_text_x=gg.element_text(margin={'t': 40, 'units':'pt'}),
        axis_text_y=gg.element_text(margin={'r': 40, 'units':'pt'}),
        plot_title=gg.element_text(color=text_col, weight='bold'),
        plot_subtitle=gg.element_text(color=text_col, size=10),
        plot_caption=gg.element_text(color=text_col, ha='left'),
        plot_background=gg.element_rect(fill=bg_col, color=bg_col),
        panel_background=gg.element_rect(fill=bg_col, color=bg_col),
        panel_grid_major_y=gg.element_line(color=text_col, alpha=0.5, size=0.1),
        panel_grid_major_x=gg.element_line(color=text_col, alpha=0.5, size=0.1),
        panel_grid_minor_x=gg.element_line(color=text_col, alpha=0.2, size=0.1),
        panel_border=gg.element_rect(color=text_col, alpha=0.7, fill=None, size=0.1),
        axis_ticks=gg.element_line(color=bg_col),
        axis_ticks_minor=gg.element_blank(),
        figure_size = (8, 4.5)
    )
)
g.draw()

gg.ggsave(g, "2025/viz/day_05.png")