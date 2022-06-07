# Anomaly Detection UI

## Prerequisites

1. Download and install [R](https://www.r-project.org/).
2. Install the required R packages.
```sh
R
install.packages("config", "data.table", "DT", "jsonlite", "lubridate", "plotly", "RODBC", "shiny")
q()
```

## Usage

First, edit `config.yml` according to your specifications.
| Config Variable Name | Variable Type | Definition |
| :---: | :---: | :---: |
| idcol | string | ID column name |
| predcol | string | Predicted label column name |
| truthcol | string | Truth label column name |
| datacol | string | Data column name |
| datafields | vector | All data fields present in data column |
| tablename | string | Table name |
| vertsettings | string | Vertica settings following [odbcDriverConnect()](https://www.rdocumentation.org/packages/RODBC/versions/1.3-19/topics/odbcConnect) documentation |

Once `config.yml` is correct, run the following command to launch the Anomaly Detection UI:
```
Rscript app.R
```

By default, the launched application can be accessed in a standard browser at `localhost:1234`.
