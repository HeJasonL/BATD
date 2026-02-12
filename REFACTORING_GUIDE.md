# BATD v1.5.0 Refactoring Guide

This document describes the changes and improvements in BATD v1.5.0, with focus on the new unified extraction workflow and configuration system.

## What's New in v1.5.0

### 1. Unified BATD_extract() Function

The new `BATD_extract()` function provides a simplified interface with automatic format detection:

```r
# Automatic format detection (recommended)
data <- BATD_extract(
  list_of_filenames = my_files,
  site = "UCLA"
)

# Explicit format specification (optional)
data <- BATD_extract(
  list_of_filenames = my_files,
  site = "UCLA",
  format = "NF"  # or "OF" for older format
)

# Mixed-format batch processing (automatic routing)
data <- BATD_extract(
  list_of_filenames = c(nf_files, of_files),
  site = "UCLA"
)
```

### 2. Automatic Format Detection

The system now automatically detects whether input files are in:
- **NF (Newer Format):** Single text file with colon-delimited data (date:, protocol:, gender:, birthYear:, etc.)
- **OF (Older Format):** Nested folder structure with tab-delimited files (Subject_Number, Gender, Handedness, Birthdate, etc.)

Detection uses confidence scoring (0-1 scale) and provides detailed reasoning:

```r
format_result <- detect_file_format("path/to/file.txt")
# Returns: list(format = "NF", confidence = 0.95, reasoning = "Found 5 NF markers")
```

### 3. YAML Configuration System

#### Site Protocols Configuration (`config/site_protocols.yaml`)

Site-specific protocol information is now centralized in YAML format:

```yaml
sites:
  UCLA:
    name: University of California, Los Angeles
    location: Los Angeles, CA
    protocols:
      - id: 100
        name: Static Detection Threshold
        parameters:
          num_test_trials: 20
      - id: 900
        name: Simultaneous Amplitude Discrimination
        parameters:
          num_test_trials: 30
```

#### Default Configuration (`config/defaults.yaml`)

Global analysis parameters are configured here:

```yaml
analysis:
  session_gap_threshold: 24  # hours
  min_trials_threshold: 5
  
extraction:
  min_confidence_threshold: 0.7
  default_site: "NA"
  
plot:
  theme: "classic"
  dpi: 300
```

### 4. Configuration API

New functions for accessing configuration:

```r
# Load site protocols
kki_protocols <- get_site_protocols("KKI")

# Get specific protocol name
protocol_name <- get_protocol_name(protocol_id = 100, site = "UCLA")

# Access analysis defaults
analysis_config <- get_defaults("analysis")

# Load entire configuration
all_defaults <- get_defaults()
```

### 5. Backward Compatibility

All existing function calls continue to work without changes:

```r
# v1.4.0 code still works in v1.5.0
data_nf <- BATD_extract_NF(my_nf_files, site = "UCLA")
data_of <- BATD_extract_OF(my_of_files, site = "UCLA")

# These internally now use the refactored implementation
# with shared helpers and improved code organization
```

## Architecture Changes

### Code Organization

```
R/
├── BATD_extract_NF.R          # Public API wrapper (backward compat)
├── BATD_extract_OF.R          # Public API wrapper (backward compat)
├── BATD_analyze.R             # Existing analysis functions
├── BATD_plot.R                # Existing plotting functions
├── extract/
│   ├── extract.R              # New unified wrapper function
│   ├── extract_nf.R           # Refactored NF extraction (388 lines, -35%)
│   ├── extract_of.R           # Refactored OF extraction (254 lines, -30%)
│   └── helpers/
│       ├── common_extraction.R # Shared helper functions
│       └── format_detection.R  # Format detection logic
├── config/
│   └── load_config.R          # Configuration loading functions
└── utils/
    └── operators.R            # Utility operators (%ni%)

config/
├── site_protocols.yaml        # Site-specific protocol definitions
└── defaults.yaml              # Global analysis defaults
```

### Code Reduction

- **BATD_extract_NF:** 595 lines → 388 lines (-35%, 207 lines eliminated)
- **BATD_extract_OF:** 361 lines → 254 lines (-30%, 107 lines eliminated)
- **Total duplication eliminated:** 314 lines

### Refactored Shared Helpers

The following helper functions now replace duplicated code:

1. **normalize_demographics()** - Converts demographic formats (M→Male, F→Female, etc.)
2. **account_for_repeated_runs()** - Identifies and numbers repeated protocol runs
3. **standardize_column_types()** - Ensures consistent column data types
4. **extract_participant_metadata()** - Format-aware demographic extraction
5. **validate_extracted_data()** - Quality validation for extracted data

## Migration Guide

### For End Users

No migration needed! All existing code continues to work. To use new features:

```r
# Instead of specifying format:
# Old approach
data_nf <- BATD_extract_NF(files, "UCLA")
data_of <- BATD_extract_OF(files, "UCLA") 

# New approach (auto-detection)
data <- BATD_extract(files, "UCLA")  # Auto-detects format
```

### For Developers

The new modular structure makes development easier:

```r
# Access shared helpers in custom code
source(system.file("R/extract/helpers/common_extraction.R", package = "BATD"))

# Normalize demographics in your function
my_data <- normalize_demographics(my_data)

# Validate extracted data
validate_extracted_data(my_data)
```

## Configuration Customization

### Adding a New Site

Edit `config/site_protocols.yaml`:

```yaml
sites:
  NEW_SITE:
    name: Your Institution Name
    location: City, State
    contact: contact@institution.edu
    protocols:
      - id: 100
        name: Static Detection Threshold
        description: Optional description
        parameters:
          num_test_trials: 20
```

### Modifying Analysis Defaults

Edit `config/defaults.yaml`:

```yaml
analysis:
  session_gap_threshold: 48  # Changed from 24 hours
  min_trials_threshold: 8     # Changed from 5
```

## Dependencies

v1.5.0 adds the following dependencies:

- **yaml** (NEW) - For YAML configuration parsing
- dplyr, stringr - Data manipulation
- ggplot2, ggpubr - Visualization
- data.table, plyr - Data processing

Development dependency:
- **testthat** (NEW) - For comprehensive test suite (37 tests)

## Testing

Run the new test suite to verify functionality:

```r
# Install test dependencies if needed
devtools::install(dependencies = TRUE)

# Run all tests
devtools::test()

# Run specific test file
devtools::test(filter = "config")

# Check coverage
covr::code_coverage("BATD")
```

Test categories:
- Operators (6 tests)
- Demographics normalization (4 tests)
- Format detection (6 tests)
- Common extraction helpers (8 tests)
- Unified wrapper (7 tests)
- Backward compatibility (8 tests)
- Configuration loading (18 tests)

**Total: 57 tests**

## Performance Notes

Refactoring improvements:
- **Code maintainability:** 314 fewer duplicated lines (-25% average)
- **Development velocity:** Shared helpers reduce new-feature development time
- **Quality:** Format detection prevents user errors from format misspecification
- **Backward compatibility:** 100% - no breaking changes

## Support & Issues

For issues or feature requests:
- Email: jasonhe93@gmail.com
- Report issues with specific format problems to enable better format detection
- Share site-specific protocol information to expand configuration coverage

## Version History

- **v1.5.0** (Jan 2024) - Major refactoring with modular architecture, YAML configuration, auto-detection, 57 tests
- **v1.4.0** (Previous) - Original monolithic implementation
