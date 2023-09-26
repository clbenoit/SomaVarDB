
# Requirements

R 4.2.0

# Installation

`devtools::install_github("Plateforme-Data-NGS-CHUGA/SomaVarDB")`

# Configuration

Save locally and edit the [config file](inst/golem-config.yml)

  - **[Optional]** **browser_server_path** :  In the case of a [local JBrowseR installation](https://gmod.github.io/JBrowseR/articles/creating-urls.html#using-local-data), not intended for production use
  - **browser_client_url**: Url to your [JBrowseR](https://github.com/GMOD/JBrowseR) server, files should be accessible from the WebBrowser used to access the Shiny app
  - **cache_directory** : directory to store SomaVarDB cache files in. Necessary for good app performances.

# Running the application

`SomaVarDB::run_app(prefix = "yourdatabasename", db_path = "path_to_your_database_location", config = "path_to_your_config_file")`

For example 

`SomaVarDB::run_app(prefix = "lung_variations", db_path = "/home/my_databases/", config =  "/home/configs/mycurrentconfir.yml")`

Will launch SomaVarDB on the following SQLite database : /home/my_databases/lung_variations.db

## Running Demo app

  `SomaVarDB::run_app(prefix = "test", db_path = system.file("extdata","testdata", package = "SomaVarDB"))`

# Manage your genomic variations database

The SomaVarDB package comes with a tools suite to manage your database. Here are the more common R functions : 

- `SomaVarDB::buildDB_seqone(prefix = prefix, db_path = db_path, vcf_name = "pathtovcf")` Will import genomic variations found on the specified vcf to the selected database. [More information abouth the vcf specifications]()


# Notes

6:109,689,067..109,690,010


data_server <- JBrowseR::serve_data("/home/ptngs/JBrowseR_data_server")

data_server$stop_server()

