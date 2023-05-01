<a href="url"><img src="/www/forecast_baq.png" align="left" height="80" width="80" ></a>

# Crime Forecasting Dashboard for the City of Barranquilla
This repository contains the code for a crime forecasting [Shiny](https://shiny.rstudio.com/) dashboard that utilizes time series analysis to predict crime trends in the city of Barranquilla. The dashboard provides insights into criminal activity and enables law enforcement agencies to make data-driven decisions about crime prevention and response efforts.

## Features
- Crime forecasting based on time series analysis techniques
- Interactive dashboard to visualize crime patterns, trends over time and predict future crime activity
- Crime data sourced from the [National Police of Colombia public database](https://www.policia.gov.co/grupo-informacion-criminalidad/estadistica-delictiva)

## Live App
[Barranquilla Crime Forecasting App](https://mariaaroca.shinyapps.io/BAQ_CFD/)

*About*  

![1 about](/assets/screenshot_about.png)

*Explore Time Series*  

![2 explore](/assets/screenshot_explore_1.png)

![3 explore](/assets/screenshot_explore_3.png)

*Forecast with different Models*  

![4 allmodels](/assets/screenshot_all_models.png)

*Ensemble Forecast*
![5 ensemble](/assets/screenshot_ensemble.png)

## Acknowledgments
[feasts](https://cran.r-project.org/web/packages/feasts/index.html) package was used for seasonal summaries and autocorrelation plots.

[anomalize](https://business-science.github.io/anomalize/) package was used for anomaly detection.

[modeltime](https://business-science.github.io/modeltime/) and [parsnip](https://parsnip.tidymodels.org/index.html) were used for modeling.

App was created using [shiny](https://shiny.rstudio.com/), and [shinydashboardPlus](https://rinterface.github.io/shinydashboardPlus/). 

Theming was done with [fresh](https://github.com/dreamRs/fresh). Other components used: [shinybusy](https://github.com/dreamRs/shinybusy) and, [summaryBox](https://github.com/deepanshu88/summaryBox).

Icons from [Font Awesome](https://fontawesome.com/v4/icons/).

## Contact

For more information please contact:  

| Name            |            Email                       |
|-----------------|:--------------------------------------:|
| Maria Aroca     | mparoca@iu.edu or mp.arocav2@gmail.com |
