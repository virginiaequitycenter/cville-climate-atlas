# Charlottesville Regional Climate Equity Atlas 

The Regional Climate Equity Atlas sheds light on relationships between environmental, social, economic, and health-related patterns of inequity in the Charlottesville Region. The Democratization of Data Initiative team at the Equity Center at UVA built this resource to provide community advocates, residents, and leaders with data as well as interactive maps and visualizations to examine the data to aid in the pursuit of climate equity throughout the region. 

Using numerous publicly available datasets to generate a collection of measures for Albemarle County, the City of Charlottesville, Fluvanna County, Greene County, Louisa County, and Nelson County, this data visualization tool merges these data into a single interactive web app. Data sets are organized in six groupings: 

* Social and economic characteristics 
* Climate measures 
* Environmental risk factors 
* Community assets and infrastructure 
* Health factors 
* Transportation 

The data and code for this work is available on GitHub

* Data stories and Regional Climate Equity Atlas:  https://github.com/virginiaequitycenter/cville-climate-atlas/
* Data acquisition, processing, and data files:  https://github.com/virginiaequitycenter/summer-sandbox/tree/main/cville_region_collection 

## Climate Equity Dashboard

App deployed at [https://virginiaequitycenter.shinyapps.io/climate-app/](https://virginiaequitycenter.shinyapps.io/climate-app/)

### climate-app

Files to create app

* app.R: the file to create the app
* climate-app-template.html: HTML app template file
* deploy.R
* functions/utils.R: Utility functions for custom HTML styled elements in dashboard
* data/combine_data.R: Data prep
* data/cvl_dat.RData: app data file, created in combine_data.R
* www/styles.css: Additional styles for dashboard

## The Picturing Climate Justice Exhibit
These data were used in more narrative storytelling as part of the Picturing Climate Justice exhibit in collaboration with the Jefferson School African American Heritage Center (March 7, 2022-May 28, 2022).

* See the data stories at Picturing Climate Justice-Data Stories site: https://virginiaequitycenter.github.io/picturing-climate-stories/
* View and listen to the ~~Global~~Local Warming data sonification: https://local-warming.vercel.app


## Project Contributions
This project was built by many hands.

* Community partners and the Climate Justice Coalition members spurred this work by sharing their deep knowledge, ongoing needs, and future goals.

* The measures and metrics on which the larger project relies were researched, collected, processed and derived by the Equity Center Data team: the Equity Center Data Fellows -- Chase Dawson, Jacob Goldstein-Greenwood, Jordan House, Khalila Karefa-Kargbo, Lee LeBoeuf, Marisa Lemma, Helena Lindsay, Tolu Odukoya -- and Michele Claibourn, the Equity Center's Director of Equitable Analysis.

* Michael Salgueiro, the Equity Center's Research Programs Manager, provided conceptual guidance, identified key questions and sources, helped to frame and edit all components, constructed designs and use cases, and ensured the project stayed on track and in sync with the work of our community partners.

* The Regional Climate Equity Atlas platform was programmed and created by Michele Claibourn, Jacob Goldstein-Greenwood, Lee LeBoeuf, and Elizabeth Mitchell.

* Data stories were conceived, analyzed and written by Equity Center Data Fellows Lee LeBoeuf, Marisa Lemma, and Chase Dawson with support from Michele Claibourn and Michael Salguiero.

* The data sonification work was conceived, researched, and programmed by Equity Center Data Fellow Chase Dawson.

* The data work was funded with support from UVA's Office of Sustainability and the Environmental Resilience Institute, and by a grant from the Public Interest Technology University Network Challenge Grant

README updated 3-15-2024
