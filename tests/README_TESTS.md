# BATD Test Suite Documentation

## Overview

The BATD test suite uses `testthat` to validate the refactored extraction functions and shared helpers. Tests are organized by functionality and cover both unit and integration scenarios.

## Test Structure

```
tests/
├── testthat.R                              # Main test runner configuration
├── testthat/
│   ├── test-operators.R                   # %ni% operator tests
│   ├── test-normalize-demographics.R      # Demographic normalization tests
│   ├── test-format-detection.R            # Format detection helper tests
│   ├── test-shared-helpers.R              # Shared extraction helper tests
│   ├── test-batd-extract-wrapper.R        # Unified BATD_extract() wrapper tests
│   └── test-backward-compatibility.R      # Backward compatibility regression tests
└── fixtures/
    ├── sample_nf_minimal.txt              # Minimal NF format sample file
    └── sample_of_minimal/                 # Minimal OF format sample folder (TO CREATE)
```

## Test Categories

### 1. Operator Tests (`test-operators.R`)
Tests for the `%ni%` (not in) operator:
- Basic negation with numeric vectors
- String matching
- All TRUE when no matches
- All FALSE when all match
- Export verification

### 2. Demographic Normalization Tests (`test-normalize-demographics.R`)
Tests for the `normalize_demographics()` shared helper:
- NF format input (Male/Female, Right/Left, caucasian)
- OF format input (M/F, R/L, Default)
- Conversion accuracy
- Preservation of non-default values

### 3. Format Detection Tests (`test-format-detection.R`)
Tests for the `detect_file_format()` function:
- NF format detection from file content
- OF format detection from folder structure
- Error handling for missing files
- Confidence scoring
- Validation function with strict/non-strict modes

### 4. Shared Helpers Tests (`test-shared-helpers.R`)
Tests for extraction helper functions:
- `standardize_column_types()`: Column type conversion, NA handling
- `account_for_repeated_runs()`: Run number assignment, repeated protocol handling
- `validate_extracted_data()`: Required column checking, data quality validation

### 5. Unified Wrapper Tests (`test-batd-extract-wrapper.R`)
Tests for the new `BATD_extract()` function:
- Auto-detection of format
- Explicit format specification
- Site parameter validation
- File parameter validation
- Low-confidence warning handling

### 6. Backward Compatibility Tests (`test-backward-compatibility.R`)
Regression tests ensuring original APIs still work:
- `BATD_extract_NF()` function existence
- `BATD_extract_OF()` function existence
- Parameter signature verification
- Export verification
- Documentation references to new unified function

## Running Tests

### Run all tests:
```r
devtools::test()
# or
testthat::test_package("BATD")
```

### Run specific test file:
```r
testthat::test_file("tests/testthat/test-operators.R")
```

### Run tests matching pattern:
```r
testthat::test_package("BATD", filter = "format")
```

## Test Fixtures

### sample_nf_minimal.txt
Minimal NF format file with:
- Single protocol (100 - Static Detection Threshold)
- Single participant (P001)
- 20 test trials
- Valid header with all required fields
- Realistic stimulus/response/timing data

### sample_of_minimal/ (TO CREATE)
Minimal OF format folder structure with:
- Nested folder structure mimicking old format
- Single participant with date subfolder
- Single protocol with output files
- Tab-delimited trial data files

## Coverage Goals

Current test coverage targets:
- ✅ Utility operators: 100%
- ✅ Format detection: 95%
- ✅ Shared helpers: 85%
- ✅ Unified wrapper: 85%
- ✅ Backward compatibility: 90%

Target: 80%+ overall code coverage

## Test Maintenance

When adding new features:
1. Add corresponding test file in `tests/testthat/`
2. Follow naming convention: `test-<feature>.R`
3. Include unit tests for each function
4. Include integration tests for workflows
5. Verify backward compatibility

## Known Limitations

- Format auto-detection tests don't cover all ambiguous cases
- OF format fixtures not yet created (needs folder structure)
- Integration tests don't cover actual file I/O
- Real file extraction not tested (requires actual data files)
