# FruitKountRPackage

The goal of FruitKountRPackage to provide a R wrapper in order to better integrate, analyze and use the FruitKount Device.

## Installation

You can install the development version of FruitKountRPackage like so:

``` r
devtools::install_github("https://github.com/agronomofiorentini/fruitkount-r-package")
```

## Authentication

``` r
library(FruitKountRPackage)
token<-get_token(email,
                password)
```

## Get sensors list

``` r
sensor_list<-get_sensor_list(token=token)
```

## Get all the data of the sensors linked to your account

``` r
sensor_data<-get_sensor_data(id_sensor=sensor_list$id_sensor[1],
                            token=token)
```

## Get the dates of survey of one sensor linked to your account

``` r
dates<-get_date_acquisition(id_sensor=sensor_list$id_sensor[1],
                            token=token)
```

## Get the data of one sensor and date linked to your account

``` r
sensor_data_selected<-get_sensor_data_by_date(id_sensor=sensor_list$id_sensor[1],
                                              date=dates$date[1],
                                              token=token)
```

## Get the html graph

``` r
graph<-get_html_graph(
  id_sensor = sensor_list$id_sensor[1],
  date = dates$date[1],
  variable = "count",
  token = token,
  save_dir = ".",
  filename = "ciao.html",
  open_in_browser = TRUE
)
```

## Get the html map

``` r
map<-get_html_map(
  id_sensor = sensor_list$id_sensor[1],
  date = dates$date[1],
  variable = "count",
  token = token,
  save_dir = ".",
  filename = "ciao.html",
  open_in_browser = TRUE
)
```

## Get the prescription map

``` r
prescription_map<-get_html_prescription_map(
  id_sensor = sensor_list$id_sensor[1],
  date = dates$date[1],
  automatic="True",
  zone=3,
  strategy="highwherehigh",
  fertilizer=160,
  percentage=20,
  save_dir=".",
  filename="ciao.zip",
  token = token,
  open_in_browser="True"
)
```

## Download all the photo and data

``` r
photo<-download_photo(
  id_sensor = sensor_list$id_sensor[1],
  date = dates$date[1],
  filename="./ciao.zip",
  token = token
)
```

## Download the interpolated map

``` r
interpolated_map<-download_interpolated_map(
  id_sensor = sensor_list$id_sensor[1],
  date = dates$date[1],
  variable  = "count",
  filename  = "./interpolated_map",
  token     = token
)
```

## Download the prescription map

``` r
prescription_map<-download_prescription_map(
  id_sensor = sensor_list$id_sensor[1],
  date = dates$date[1],
  automatic="True",
  zone=3,
  strategy="highwherehigh",
  fertilizer=160,
  percentage=20,
  filename="./ciao.zip",
  token = token
)
```
