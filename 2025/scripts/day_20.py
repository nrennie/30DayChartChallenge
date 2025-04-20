import pandas as pd
import plotnine as gg
import matplotlib.pyplot as plt

# Data
income = pd.read_csv("2025/data/income-share-of-the-richest-1.csv")

# Wrangling
plot_data = income[(income["Country"] == "World")].rename(columns={"Income share of the richest 1% (before tax) (World Inequality Database)": "Income"})
End1 = plot_data["Year"].iloc[1:len(plot_data["Year"])].tolist()
End1.append(2024)
plot_data["Year_End"] = End1

# Plot
g = (gg.ggplot()
    + gg.geom_rect(gg.aes(xmin="Year", xmax="Year_End", ymin=0, ymax="Income"), data=plot_data, fill="black")
    + gg.annotate("text", x=1922, y=35, label= "Income received by the richest 1%", size = 22)
    + gg.annotate("text", x=1922, y=1, label= "Data: World Inequality Database (WID). Processed by Our World in Data", size = 8, colour="white")
    + gg.scale_y_continuous(limits=[0,45])
    + gg.coord_cartesian(expand=False)
    + gg.theme_void()
    + gg.theme(
        plot_background=gg.element_rect(fill="white"),
        legend_position="none",
        plot_margin=0,
        figure_size = (7, 5)
    )
)
g.draw()

gg.ggsave(g, "2025/viz/day_20.png")
