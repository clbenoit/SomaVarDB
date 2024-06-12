# :clipboard: Requirements

R 4.2.0

# :hammer_and_wrench: Installation

`git clone git@github.com:clbenoit/GermlineVarDB.git`

# :gear: Configuration

Edit the [config file](config.yml)

  - **cache_directory** : directory to store GermlineVarDB cache files in. Necessary for good app performances. If NULL, a temporary directory will be used and cache will be lost on computer restart
  - **multiqc** : Path where multiqc run reports are stored. Will be soon compatible with Apache server URL
  - **db_path** : Path where you genomic variation database is stored
  - **prefix** :  Name of you Genomic variation stuctured database
  - **use_browser** : Do you want to activate genome browser functionnality to visualize bam files ? (TRUE|FALSE)
  - **[Optional]** **browser_server_path** :  In the case of a [local JBrowseR installation](https://gmod.github.io/JBrowseR/articles/creating-urls.html#using-local-data), not intended for production use
  - **[Optional]** **browser_client_url**: Url to your [JBrowseR](https://github.com/GMOD/JBrowseR) server, files should be accessible from the WebBrowser used to access the Shiny app
  
For example 
`
prefix = "lung_variations"
db_path = "/home/my_databases/"
`
Will set up GermlineVarDB to use the following SQLite database : /home/my_databases/lung_variations.db 

# :rocket: Run the application

Go to the app root directory and run

`Sys.setenv(MY_VARIABLE = "default"); shiny::runApp()`

# Demo app

##  :computer: Run demo locally

Go to the app root directory and run

`Sys.setenv(MY_VARIABLE = "demo"); shiny::runApp()` Skip references download and disable genome browser on demo app

`Sys.setenv(MY_VARIABLE = "demo_browser"); shiny::runApp()` Will download genome reference file on first call before to start the app

## :globe_with_meridians: Live demo App

<a href="https://omicsverse.fr/app/GermlineVarDB" target="_blank">See live demo</a>

![](app/static/germlinevardb.gif)

# :dna: Manage your genomic variations database

See https://github.com/clbenoit/GermlineVarDBTools

# :soon: Incoming features

- Possibility for the user to save filters and parameters presets and choose a default one 
- Add in silico panel lists usable as filtering option
- Possibility for the user to upload a list of preferential transcripts, default use the canonical one in sample variants table -> cleaner view

# :warning: Troubleshouting


