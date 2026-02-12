# BATD: Batch Analysis of Tactile Data

<!-- badges: start -->
![R Version](https://img.shields.io/badge/R-%E2%89%A5%204.0-blue)
![Version](https://img.shields.io/badge/version-1.5.0-green)
[![License](https://img.shields.io/badge/license-NA-lightgrey)]()
<!-- badges: end -->

**BATD** is an R package for extracting, analyzing, and visualizing vibrotactile data from Cortical Metrics Brain Gauge devices. It provides comprehensive tools for processing data from both newer Brain Gauge devices (NF format) and older Research Edition devices (OF format).

## Features

### 🆕 Version 1.5.0 - Major Refactoring

- **Automatic Format Detection**: Intelligently detects NF vs OF formats
- **Unified API**: Single `BATD_extract()` function handles both formats
- **Modular Architecture**: Clean separation of concerns with helper functions
- **Configuration System**: YAML-based site protocol configuration
- **Comprehensive Testing**: 57+ passing tests for reliability
- **Better Documentation**: Improved function documentation and examples

### Core Capabilities

- **Data Extraction**: Parse raw Brain Gauge output files into structured dataframes
- **Protocol Recognition**: Automatically label protocols by site-specific configurations
- **Performance Analysis**: Calculate thresholds, accuracy, reaction times, and reversals
- **Visualization**: Generate publication-ready plots for individual or batch analysis
- **Multi-Session Support**: Handle participants with multiple testing sessions
- **Run Detection**: Identify repeated protocol runs within sessions

## Installation

### From GitHub (Recommended)

```r
# Install devtools if needed
if (!require("devtools")) install.packages("devtools")

# Install BATD
devtools::install_github("HeJasonL/BATD")
```

### From Source

```bash
# Clone the repository
git clone https://github.com/HeJasonL/BATD.git

# Install in R
R CMD INSTALL BATD
```

## Quick Start

### Automatic Format Detection (Recommended)

```r
library(BATD)

# Extract data from mixed or unknown formats
files <- list.files(pattern = "\\.txt$")
data <- BATD_extract(files, site = "UCLA")

# Analyze all participants
results <- BATD_analyze_all(data)

# Generate plots for all participants
BATD_plot_all(data)
```

### Specific Format Extraction

```r
# For newer Brain Gauge devices (NF format)
data_nf <- BATD_extract_NF(list_of_filenames, site = "KKI")

# For older Research Edition devices (OF format)
data_of <- BATD_extract_OF(list_of_folders, Site = "JHU")

# Explicitly specify format if auto-detection fails
data <- BATD_extract(files, site = "UCLA", format = "NF")
```

### Analysis and Visualization

```r
# Analyze single participant
participant_data <- data[data$id == "001", ]
analysis <- BATD_analyze(participant_data)

# Analyze all participants
all_analysis <- BATD_analyze_all(data)

# Plot single participant
plot <- BATD_plot(participant_data)

# Batch plot all participants (saves PDFs)
BATD_plot_all(data)
```

## Supported Sites

BATD includes protocol configurations for the following research sites:

- **KKI**: Kennedy Krieger Institute
- **CCH**: Cincinnati Children's Hospital
- **UCLA**: University of California, Los Angeles
- **JHU**: Johns Hopkins University
- **EU-AIMS**: European Autism Interventions
- **SPIN**: Social Pediatric Innovation Network
- **CARE**: CARE Network
- **STES**: STES Study
- **ARBA1-4**: ARBA Study Sites

Don't see your site? Use `site = "NA"` for generic protocol labeling or contact the maintainer to add your site.

## Main Functions

### Data Extraction

| Function | Description |
|----------|-------------|
| `BATD_extract()` | **Recommended**: Unified extraction with automatic format detection |
| `BATD_extract_NF()` | Extract data from newer Brain Gauge format (NF) |
| `BATD_extract_OF()` | Extract data from older Research Edition format (OF) |
| `detect_file_format()` | Detect whether file is NF or OF format |

### Analysis

| Function | Description |
|----------|-------------|
| `BATD_analyze()` | Analyze single participant data |
| `BATD_analyze_all()` | Batch analyze multiple participants |

### Visualization

| Function | Description |
|----------|-------------|
| `BATD_plot()` | Generate plots for single participant |
| `BATD_plot_all()` | Generate plots for all participants (saves PDFs) |

### Configuration

| Function | Description |
|----------|-------------|
| `load_config()` | Load YAML configuration files |
| `get_site_protocols()` | Get protocol configurations for a site |
| `get_protocol_name()` | Look up protocol name by ID and site |
| `get_defaults()` | Access default analysis parameters |

## Data Structure

BATD returns dataframes with the following key columns:

### Participant Information
- `id`: Participant identifier
- `race`, `gender`, `handedness`, `birthYear`: Demographics

### Session Information
- `date`, `time`: When protocol was completed
- `session`: Session number (multiple testing dates)
- `run`: Run number (repeated protocols within session)
- `site`: Data collection site

### Protocol Information
- `protocol`: Protocol number (e.g., 100, 900)
- `protocolName`: Human-readable protocol name
- `numberofPracticeTrials`, `numberofTestTrials`: Trial counts
- `ISI`: Inter-stimulus interval

### Stimulus Parameters
- `stim1amplitude`, `stim2amplitude`: Stimulus amplitudes
- `stim1frequency`, `stim2frequency`: Stimulus frequencies
- `stim1duration`, `stim2duration`: Stimulus durations
- `astim*`: Adapting stimulus parameters (for adaptation protocols)

### Performance Data
- `trialNumber`: Trial index
- `value`: Stimulus value on this trial
- `response`: Participant's response
- `correctResponse`: Whether response was correct (0/1)
- `responseTime`: Reaction time in milliseconds

### Metadata
- `format`: "NF" or "OF"
- `extractedBy`: BATD version used for extraction
- `originalFilename`: Source filename

## Protocols Supported

BATD recognizes and analyzes the following protocol types:

### Reaction Time
- Simple Reaction Time (SRT)
- Choice Reaction Time (CRT)

### Detection Thresholds
- Static Detection Threshold (SDT)
- Dynamic Detection Threshold (DDT)
- Static Detection with Adaptation (ISI 30, ISI 100)

### Amplitude Discrimination
- Simultaneous Amplitude Discrimination (SMAD)
- Sequential Amplitude Discrimination (SQAD)
- Amplitude Discrimination without Adaptation (ADT)
- Amplitude Discrimination with Single Site Adaptation (ADTssa)
- Amplitude Discrimination with Dual Site Adaptation (ADTdsa)
- Dual Staircase Amplitude Discrimination (up/down)

### Frequency Discrimination
- Simultaneous Frequency Discrimination (SMFD)
- Sequential Frequency Discrimination (SQFD)

### Temporal Processing
- Temporal Order Judgement (TOJ)
- Temporal Order Judgement with Carrier (TOJwc)
- Duration Discrimination (DD)

## Advanced Usage

### Custom Site Configuration

Add your own site protocols by editing `config/site_protocols.yaml`:

```yaml
sites:
  YOUR_SITE:
    name: Your Site Name
    location: City, State
    protocols:
      - id: 100
        name: Static Detection Threshold
        parameters:
          num_test_trials: 20
```

### Format Detection

```r
# Check file format before extraction
detection <- detect_file_format("path/to/file.txt")
cat("Format:", detection$format,
    "\nConfidence:", detection$confidence,
    "\nReason:", detection$reasoning)

# Validate format matches expectation
validate_file_format("path/to/file.txt", expected_format = "NF")
```

### Working with Results

```r
# Filter by protocol
sdt_data <- data[data$protocolName == "Static Detection Threshold", ]

# Calculate group statistics
library(dplyr)
group_stats <- all_analysis %>%
  group_by(site) %>%
  summarise(
    mean_threshold_SDT = mean(threshold_SDT, na.rm = TRUE),
    mean_accuracy_SDT = mean(accuracy_SDT, na.rm = TRUE)
  )

# Export results
write.csv(all_analysis, "batd_results.csv", row.names = FALSE)
```

## File Formats

### Newer Format (NF)
- Single `.txt` file per participant
- Colon-delimited key-value pairs
- Used by current Brain Gauge devices
- Example: `participant_001.txt`

### Older Format (OF)
- Nested folder structure
- Tab-delimited files
- Used by Brain Gauge Research Edition
- Structure: `ParticipantID/YYYY-MM-DD/ProtocolNumber/`

## Dependencies

BATD requires the following R packages:
- `dplyr`: Data manipulation
- `stringr`: String operations
- `ggplot2`: Plotting
- `ggpubr`: Plot arrangement
- `data.table`: Efficient data binding
- `plyr`: Data splitting and combining
- `yaml`: Configuration file parsing

These will be automatically installed when you install BATD.

## Citation

If you use BATD in your research, please cite:

```
He, J. L. (2026). BATD: Batch Analysis of Tactile Data (Version 1.5.0)
[Computer software]. GitHub. https://github.com/HeJasonL/BATD
```

## Development History

- **v1.5.0** (2026-02): Major refactoring - modular architecture, automatic format detection, comprehensive testing
- **v1.4.0**: Previous stable release
- Earlier versions: Initial development and site-specific customizations

## Contributing

Contributions are welcome! Please feel free to:
- Report bugs via [GitHub Issues](https://github.com/HeJasonL/BATD/issues)
- Suggest new features
- Submit pull requests
- Add support for additional research sites

## Support

For questions, issues, or site-specific protocol configurations, please contact:
- **Jason L. He**: jasonhe93@gmail.com
- **GitHub Issues**: https://github.com/HeJasonL/BATD/issues

## Related Resources

- [Cortical Metrics Brain Gauge](https://www.corticalmetrics.com/braingauge)
- [Brain Gauge Research Edition](https://www.corticalmetrics.com/braingaugeresearch)

## License

License information to be determined. Please contact the maintainer for usage permissions.

---

**Maintainer**: Jason L. He (jasonhe93@gmail.com)  
**Repository**: https://github.com/HeJasonL/BATD
