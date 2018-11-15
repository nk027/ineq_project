# Inequality Project

This repository is used for organising the creation of income inequality profiles of several European countries with EU-SILC data.

**Please take the time to read this short introduction before starting with your project.**

### Legal

**Make sure to not let plain EU-SILC data end up on GitHub!**

This includes the credentials to the Ineq-Institute's database. To connect to the database use the script `R/_connection.R` which requires you to input the password by hand (directly or via variable). Do no access this script from R Markdown - instead prepare your data in a separate script and store it locally in the `data` folder.


### Folder Structure

Please stick with the existing folder structure and **create an issue** if something was not accounted for. The current structure is:

- `reports`: Create a subfolder (named after your country's [ISO 3166 ALPHA-3 code](https://de.wikipedia.org/wiki/ISO-3166-1-Kodierliste)) for files directly pertaining to your country, i.e. your report, scripts for your data, your *BibTeX* file, images (in `img`), tables (in `tables`), et cetera.
- `R`: The folder should be used for scripts that might be helpful to your colleagues (e.g. functions for preparing or plotting data). Be sure to name these files concisely and provide some documentation for the code.
- `data`: This folder should be used for your data and will be ignored by Git.


### Coding Style

To keep code readable and thus easy to share and verify, try to adhere to some **basic rules**:

- *File Names* are meaningful and do not contain spaces (use `_` or `-`).
- *Objects* are named in a meaningful and readable way (e.g. `read_csv()` or `table_qsr`).
- *Lines* should not exceed 80 characters (use *Show Margin* in RStudio).
- *Spaces* are put around all operators and after commas (e.g. `c("this", 1 + 1)`).
- *Comment* your code.
- *Avoid* clutter by organising your workflow (separate scripts for separate tasks, [functions](http://adv-r.had.co.nz/Functions.html), etc.)

If you want to go further have a look at this [short guide](http://adv-r.had.co.nz/Style.html) or the [extended tidyverse style guide](https://style.tidyverse.org/).

### Starting off

1. Create a fork of this repository by clicking the *Fork* button in the top right
2. Clone your forked repository with RStudio
3. Add this repository as *upstream* by running: `git remote add upstream https://github.com/nk027/ineq_project.git`
4. Start off

To update your fork with changes from this main repository run: `git pull upstream master` or use GitHub's online interface for pull requests.
