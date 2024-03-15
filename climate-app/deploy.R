rsconnect::deployApp(appName = "climate-app", 
                     appFiles = c("app.R", 
                                  "climate-app-template.html",
                                  "data/cvl_dat.RData", 
                                  "functions/utils.R",
                                  "www/styles.css",
                                  "www/message-handler.js",
                                  "www/cville_climate_update.html",
                                  "www/bivariate_legend_static.svg",
                                  "www/ec_climate_logo.png"
                                  ),
                     account = "virginiaequitycenter") 
