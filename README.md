# hospital_forecasting_tool
Forecasting tool for hospital demand during heat periods.

This tool has been developed by researchers at ISPM, University of Bern, in the context of the NCCS project about impacts of climate change on health in Switzerland.

This forecasting tool allows to:
(i) quantify the association between visits to emergency department and daily temperature, based on historical data provided;
(ii) generate short-term forecasts of the expected number of patient visits on new data, given temperature and number of visits observed in the past week and temperature forecasts for the next days.

We showcase an application of the tool to data from the emergency department at Bern University Hospital, and mean temperature data for the city of Bern.
You can upload your own data to test the tool for your case study. Check this [user guide](https://hospital-forecasting-tool.ispm.unibe.ch/USER-GUIDE.html) for detailed instructions on how to use this tool.

If you use this tool, please refer to the following manuscript:
Di Domenico L, Wohlfender MS, Hautz WE, Vicedo-Cabrera AM, Althaus CL,
A forecasting tool of hospital demand during heat periods: a case study in Bern, Switzerland

This app makes use of the following R libraries: DT, tidyverse, ggpubr, zoo, timeDate, splines, MASS, Epi, matrixStats, Metrics, ciTools, viridis.

To run this R shiny app locally on your machine, please install R and the above packages, and download the source code from the GitHub repository https://github.com/ISPMBern/hospital_forecasting_tool/tree/main.
Alternatively, you can use this app online at the following link: https://hospital-forecasting-tool.ispm.unibe.ch/. The app is hosted on the ISPM server, in Bern, Switzerland.

This app is licensed under the [GNU General Public License v3 license](https://www.gnu.org/licenses/gpl-3.0.html), under the [“Commons Clause” condition](https://commonsclause.com/).

For any questions or issues, you can contact laura.didomenico@uhasselt.be.





