# waterloggerQAQC

<!-- badges: start -->
<!-- badges: end -->

An R package for Quality Assurance/Quality Control (QA/QC) of hydrometric time series data from HOBO water-level loggers. Developed by Fisheries and Oceans Canada for processing data from aquatic monitoring networks.

## Overview

**waterloggerQAQC** automates the detection, flagging, and correction of anomalies in water monitoring data. The package implements a comprehensive pipeline from raw logger CSV ingestion through multi-stage processing with parameter-specific validation rules, spatial data association, unit conversions, and detailed audit logging.

### Supported Parameters

- **Water Level (WL)** - Pressure-based water level measurements
- **Barometric Pressure (BARO)** - Atmospheric pressure for water level correction
- **Dissolved Oxygen (DO)** - Concentration (mg/L) and percent saturation
- **Conductivity (COND)** - Electrical conductivity (µS/cm)
- **Temperature (WT/AT)** - Water and air temperature

### Key Features

- **Automated QA/QC rules** for detecting spikes, flatlines, out-of-range values, dry sensors, and ice conditions
- **Spatial interpolation** of barometric pressure to water level stations
- **Unit conversions** (kPa to meters, DO mg/L to percent saturation)
- **Manual adjustment tools** for offsets, spikes, and zero establishment
- **Comprehensive audit logging** of all QA/QC operations
- **Interactive and static visualizations** of time series with QA/QC flags
- **Versioned data outputs** (v0.1 → v0.2 → v0.3 → v1.0)
- **Temporal aggregation** (hourly, daily, weekly, monthly, yearly)


## Installation

You can install the development version of waterloggerQAQC from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("pausoto7/waterlogged_QAQC")

```

## Load and Update Package During Development 

You can install the development version of waterloggerQAQC from [GitHub](https://github.com/) with:

``` r

# Restart R session and clear memory (reccomended)
# rm(list = ls()) # reccomended
# Navigate to working directory where the package is located

# Remove any previous installations of the package
remove.packages("waterloggerQAQC")

# Loading unfinished package to memory for testing
devtools::load_all()

# Update R function documentation
devtools::document()

# Run tests - several tests are failing and functions need to 
# need to be updated accordingly.
devtools::test()

# Check package will run properly
devtools::check()

# Install package from source (you may need to restart your R session)
# install.packages(getwd(), repos = NULL, type = "source")
# devtools::install_github("pausoto7/waterlogged_QAQC")
# library(waterloggerQAQC)
# ?bind_hobo_files


```



## Typical Workflow

### 1. Data Ingestion - Bind HOBO Files

Read raw HOBO CSV files, link metadata, and create v0.1 standardized output:

```r
library(waterloggerQAQC)


# bind_hobo_files() with DO data
DO_bound <- bind_hobo_files(
...
)

```

### 2. Barometric Pressure QA/QC

Check for unusual temperatures, pressures, spikes, or flatlines:

```r
all_checked_baro <- barometric_qaqc_all(
...)

```

### 3. Add Nearest Barometric Station

Associate barometric pressure from the nearest station to water level and DO data:

```r
wl_with_baro <- add_nearest_baro(
...
)

DO_with_baro <- add_nearest_baro(
...
)
```

### 4. Unit Conversions

Convert water level from pressure (kPa) to depth (meters):

```r
converted_wl <- convert_waterlevel_kPa_m(
...
)
```

Convert dissolved oxygen between mg/L and percent saturation:

```r
converted_DO <- convert_do_mgl_percsat(
...
)
```

### 5. Water Level QA/QC

Apply automated QA/QC for disturbances, dry sensors, and ice conditions:

```r
wl_qaqc <- waterlevel_qaqc(
...
)

plot_qaqc_timeseries(
...
)
```

### 6. Manual Adjustments (as needed)

Adjust for spikes (e.g., ice formation):

```r
wl_adjusted <- adjust_waterlevel_spike(
...
)
```

Adjust for logger offsets (e.g., sensor movement):

```r
wl_adjusted <- adjust_WL_offset(
...
)
```

Adjust water level zero reference:

```r
wl_adjusted <- adjust_WL_zero(
...
)
```

Handle missing data (NAs):

```r
wl_adjusted <- adjust_logger_NA(
...
)
```

### 7. Dissolved Oxygen QA/QC

Apply automated QA/QC for DO data:

```r
do_qaqc <- dissox_qaqc(
...
)

plot_qaqc_timeseries(
...
)
```

### 8. Conductivity QA/QC

Apply automated QA/QC for conductivity data:

```r
cond_qaqc <- conductivity_qaqc_all(
...
)

plot_qaqc_timeseries(
...
)
```

Temperature compensation (optional):

```r
compensated_cond <- conductivity_temp_compensation(cond_qaqc)
```

### 9. Write Clean Data

Export finalized v1.0 clean data split by station and year:

```r
clean_data <- write_clean_data(
...
)
```

### 10. Retrieve and Aggregate Data

Retrieve logger data with optional temporal aggregation:

```r
monthly_wl <- get_logger_data(
...
)
```

## Data Versioning

The package uses a versioned workflow to track data processing stages:

- **v0.1** - Raw data bound with metadata
- **v0.2** - Barometric pressure added to water level/DO data
- **v0.3** - Unit conversions applied (kPa to m, mg/L to %sat)
- **v1.0** - Finalized clean data after all QA/QC

## Audit Logging

All QA/QC operations are automatically logged with:
- Timestamps affected
- Station and metric identifier
- Field modified and QA/QC code/action
- Manual notes and reason for adjustment
- Function name, user, and runtime

Logs are stored in the `logs/` directory within your output folder.
