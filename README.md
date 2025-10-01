# Normal Distribution App

This Shiny app provides an interactive way to explore the **Normal distribution**.  
You can adjust the mean (μ), standard deviation (σ), and sample size to see how the histogram and density curve change.  
The app also shades selected probability regions and calculates their values in real time.

---

## Live App
[Click here to try the app on shinyapps.io](https://moer4.shinyapps.io/lab_3/)

---

## Features
- **Widgets:** sliders (μ, σ, sample size), numeric inputs (a, b), checkbox (shading), radio buttons (area type), and tab panels.
- **Interactive plot:** Histogram with density curve, rendered using **ggplot2 + plotly** for hover, zoom, and pan.
- **Probability tab:** Shows calculated values for:
  - P(a < X < b)
  - P(X ≤ a)
  - P(X ≥ b)
- **Cool feature:** As you move the sliders or inputs, the shaded probability region and the calculated probability update instantly, making it a dynamic visualization of how the Normal distribution behaves.
