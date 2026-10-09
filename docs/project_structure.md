### Project Structure

```
\carbonviewer
├── .github
    ├── workflows
        deploy.yml          # Github action for deployment
├── appScripts
    ├── custom_theme.R      # Custom theme for the app 
    ├── contribute.R        # Optional "Contribute data" module (standardize + download peat depths)
    ├── dependancies.R      # Required packages and translation object (i18n)
    ├── global.R            # Global variables
    ├── server.R            # Server side of the app
    ├── standard.R          # Loads the peat survey standard (YAML) and validates data against it
    ├── ui.R                # User interface of the app
├── data                    # Data folder
    ├── gran_dataset.zip    # gran dataset with peat properties for norwegian mires
├── docs
    ├── changelog.md         # Changelog for the app
    ├── contribute_plan.md   # Plan for the "Contribute data" module (phases 1-3)
    ├── project_structure.md # Project structure
    ├── instructions.md            # instructions for the app (main body app)
├── man
    ├── figures             # Figures for the documentation
        ├── interpolation
        ├── ln.png
        ├── logo_nina.png
        ├── ntnu.png
        ├── statnett.png
├── Rscripts 
    ├── exploration.R       # IDW interpolation (stand-alone)
├── standard                # Data Exchange Standard for Peat Surveys v1.0 (read by the app at runtime)
    ├── Data Exchange Standard for Peat Surveys v1.0.pdf  # Source document (Natural England)
    ├── peat_standard_v1.0.yaml          # Corrected machine-readable profile (1.0-corr.2), authoritative
    ├── peat_standard_no_profile.yaml    # Norwegian overlay: NO/EN labels, defaults, licence choices
    ├── peat_data_attributes.csv         # Flat view of the peat data attributes
    ├── peat_metadata_terms.csv          # Flat view of the metadata terms
    ├── peat_standard_changes.csv        # Changes C1-C7 against the published v1.0
├── test                    # Test data
    ├── csv_only.zip
    ├── invalid_csv.zip
    ├── invalid_shp.zip
    ├── shp_no_proj_file.zip
    ├── test_dataset_cm.zip
├── .gitignore              # Files to ignore
├── app.R                   # Main file for the app: runApp('/home/rstudio/app')
├── carbonviewer.Rproj      # R project file
├── docker-compose.yaml     # Docker compose file for the app
├── Dockerfile              # Dockerfile for the app
├── LICENSE                 # License for the app
├── README.md               # Readme file for the app (exact same as instructions.md)
├── rstudio-prefs.json      # Rstudio preferences
├── translation.json        # Language translation dict 
```

### Development in Rstudio with docker:
- Comment last line out in the Dockerfile `CMD Rscript ./home/rstudio/app/app.R`
- Build the docker image: `docker compose build`
- Run the docker container: `docker compose up`
- Open the Rstudio in a browser: `localhost:8787`
- Set working directory to app dir: `setwd(/home/rstudio/app)`
- Make changes, save and run the app in the Rstudio terminal: `shiny::runApp()`



### "Contribute data" module
- Optional; switch it off with `options(carbonviewer.contribute = FALSE)` in `app.R`.
- Code in `appScripts/contribute.R` (module + helpers) and `appScripts/standard.R` (standard loader + validator).
- The app reads `standard/peat_standard_v1.0.yaml` and `standard/peat_standard_no_profile.yaml` at runtime. With docker compose, `./standard` is mounted into the container.
- The module reads the uploaded zip itself with `read_dataset_zip()` (`appScripts/global.R`, also used by the volume calculation), so it works without the volume calculation.
- Phase 1 only standardizes and offers a download; `store_submission()` is a stub for a later submission step. See `docs/contribute_plan.md`.
