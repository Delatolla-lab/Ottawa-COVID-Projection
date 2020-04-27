# Ottawa COVID-19 Hospital and ICU projections

Projections for hospital and ICU use for patients with COVID-19.
Current projections are based on PENN CHIME model, but the figures can be used to present projections for other models.

This repository is not public. Contact Doug Manuel dmanuel@ohri.ca if you would like access.

# To build the website

1. FOllow the instructions here to install blogdown and hugo https://bookdown.org/yihui/blogdown/installation.html
2. Once installed, run `blogdown::serve_site()` and navigate to http://127.0.0.1:4450/613-COVID/ to view the site. This command will also continuously watch your files for changes so that the website will be automatically rebuilt and refreshed whenever you change any of the files.

## Updates

See [changelog](https://github.com/Big-Life-Lab/Ottawa-COVID-Projection/blob/master/docs/index.Rmd#L140) for updates.

## Long-term care line listing

**Case ID** - should be a unique identifier for each row.

**Accurate Episode Date** - date corresponding to _Episode Date_

**Episode Date** Best or earliest date for onset of illness.
In order: onset of illness > specimen date > run date > report date

**Hcw_new** health care work = 1. No other valid entries.

**Exposure Setting**
-RH = Retirement Home
-LTCH = Long-term care facility
-Hosp Acute = care care facility

**Expsoure Setting** All LTC outbreaks are consider facility wide. Hospt Acute are for wing, floor, etc.

**Outbreak Name** unique name for each outbreak

**Reported Date** Reported date for Onset of outbreak.

**Date Outbreak** Date outbreak reported.
