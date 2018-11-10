# Inequality Project

This repository is used for organising the creation of income inequality profiles of several European countries with EU-SILC data.

**Please take the time to read this short introduction before starting with your project.**

### Legal

Make sure to not let plain EU-SILC data end up on GitHub! This includes the credentials to the Ineq-Institute's PostgreSQL instance.


### Folder Structure

Please stick with the existing folder structure and **create an issue** if something was not accounted for. The current structure is:

- **reports**: Use a subfolder named after your country's [ISO 3166 ALPHA-3 code](https://de.wikipedia.org/wiki/ISO-3166-1-Kodierliste), for files *directly pertaining to your country*, i.e. your report, scripts for your data, your *BibTeX* file, images (in `img`), tables (in `tables`), et cetera.
- **R**: The folder should be used for scripts that might be helpful to your colleagues (e.g. functions for preparing or plotting data). Be sure to name these files concisely and provide some documentation for the code.
- **data**: This folder should be used for processed data that will be required later on. Use the *RDS* or *RData* formats and *tidy up your outputs* beforehand!


### Coding Style

To keep code readable and thus easy to share and verify, try to adhere to some basic rules:

- **File Names** are meaningful, end in `.R` and do not contain spaces (use `_` or `-`)
- **Objects** are named in a meaningful and readable way (e.g. `read_csv()` or `table_qsr`)
- **Lines** should not exceed 80 characters (use *Show Margin* in RStudio and linebreaks or functions)
- **Spaces** are put around all operators and after commas (e.g. `c("this", 1 + 1)`)
- **Comment** your code!

If you want to go further have a look at this [short guide](http://adv-r.had.co.nz/Style.html) or the [extended tidyverse style guide](https://style.tidyverse.org/).

### Starting off

1. Create a fork of this repository by clicking the *Fork* button in the top right
2. Clone your forked repository with RStudio
3. Add this repository as *upstream* by running:
´git remote add upstream https://github.com/nk027/ineq_project.git´
4. Start

To update your fork with changes from this main repository run: `git pull upstream` or use GitHub's online interface for pull requests.