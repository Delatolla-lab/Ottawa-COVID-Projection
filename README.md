# Ottawa COVID-19 projections

Projections for daily new COVID-19 cases and hospitalizations for residents in the Ottawa area. Case projections are created using the [Bayesian SEIR model](https://github.com/seananderson/covidseir) developed by Simon Fraser University. Hospital projections are based on the [Bayesian extension to the CHIME model](https://github.com/pennsignals/chime_sims) developed by the University of Pennsylvania Health System. The figures in this repository, however, can be used to present projections for other models.

# To build the website

1. Follow the instructions here to install blogdown and hugo https://bookdown.org/yihui/blogdown/installation.html
2. Once installed, run `blogdown::serve_site()` and navigate to http://127.0.0.1:4450/613-COVID/ to view the site. This command will also continuously watch your files for changes so that the website will be automatically rebuilt and refreshed whenever you change any of the files.

## Updates

See [changelog](https://github.com/Big-Life-Lab/Ottawa-COVID-Projection/blob/master/content/en/change-log.Rmd) for updates.
