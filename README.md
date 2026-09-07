# NSC

Tools for preparing and summarizing National Student Clearinghouse data.

This package is currently private and is intended for local development. Its main
workflow is for StudentTracker Subsequent Enrollment (`SE`) data: prepare the raw
export, identify enrollment and graduation patterns, classify degrees, and create
tables for reporting.

## Current status

The package source parses successfully, but there are currently no package tests.
The functions below should be treated as useful development tools rather than a
fully validated production workflow. Always inspect the generated data before
submitting it to the National Student Clearinghouse.

## Functions

| Function | Purpose | Recommendation |
| --- | --- | --- |
| `NSC()` | Backward-compatible wrapper around `format_data()`. | Keep for compatibility. |
| `format_data()` | Reads a seven-column student file or data frame, creates NSC header/detail/trailer rows, and writes TXT and CSV files. | Keep; this is the core function. |
| `prepare_data()` | Reads an NSC export and adds cohort, year/month, and approximate day/month/year indicators. | Keep, but validate dates and limit replacements to `SearchDate`. |
| `your_college()` | Classifies students associated with the target institution as higher-education-system dropout, returned later, or graduated. | Keep if the target-college workflow is needed; review selection logic. |
| `college_length()` | Collapses less-than-two-year, two-year, and four-year enrollment into one row per requester. | Keep if length-of-college summaries are needed; fix edge cases. |
| `one_from_Graduated()` | Creates overall and college-length-specific graduation indicators. | Keep if graduation summaries are needed; fix empty-group handling. |
| `highest_degree()` | Classifies degree titles as certificate, associate, bachelor, master, or doctoral and selects the highest level. | Keep if degree summaries are needed; improve matching rules. |
| `for_NSC_crosstab()` | Combines the enrollment, college-length, graduation, and degree summaries into one student-level table. | Keep as an orchestrator after its dependencies are stabilized. |
| `table_fp2()` | Adds frequency, percentage, and total columns to a frequency table. | Keep if reporting tables are needed; fix the confirmed rounding bug. |

## Important findings

### `format_data()`

- Input must contain seven columns, in this order: first name, middle initial,
	last name, suffix, date of birth, search date, and requester-return identifier.
- CSV, TXT, and Excel file inputs are read as text so leading zeros are preserved.
- In-memory `school_code`, `branch_code`, and requester-return identifiers must be
	character values. Zeros lost before the function receives a numeric value cannot
	be recovered.
- Punctuation is removed from every input field, including identifiers. This can
	change identifiers that contain punctuation.
- The function does not yet validate column count, query-code values, code lengths,
	directory existence, or all date formats.
- Invalid dates can produce `NA` comparison errors. The current date rule uses
	`< 59` days even though its message says the date must be at least 60 days old.
- Blank names are removed, but whitespace-only names are not.
- Repeated calls using the same date, query code, and suffix overwrite files.

### `college_length()`

Useful for enrollment summaries. It expects NSC columns including
`RequesterReturnField`, `X2year4year`, `PublicPrivate`, `EnrollmentBegin`,
`Graduated`, and `SearchDate`.

Known issues:

- `exclude` values other than `NULL` and `"W"` leave the working object undefined.
- Missing enrollment status is excluded when `exclude = "W"`.
- The `all_colleges` construction removes the text `"NA"` broadly and can be
	fragile for missing values.
- Empty input and no matching college types need explicit handling.

### `for_NSC_crosstab()`

Useful as the high-level workflow, but currently fragile:

- When `x` is `NULL`, it reads from `getwd()` instead of using `file_dir`.
- It depends on all other summary functions, so their defects affect this result.
- `target_college <- college_name` should be a normal named argument.
- Unexpected `RecordFoundYN` or enrollment values become `NA`.

### `highest_degree()`

Useful, but its degree-title rules are institution-specific and should be treated
as heuristics. Matching is case-sensitive, several patterns use regular-expression
metacharacters unintentionally, and replacements can affect columns beyond the
degree title. The undergraduate classification also references the unfiltered
input rather than the normalized data. Empty input is not handled safely.

### `one_from_Graduated()`

Useful for student-level graduation flags. It assumes that sorting places `"Y"`
before `"N"`; unexpected or missing values can produce incorrect results. Its
indexing loops and empty college groups need safer handling. Missing college types
are represented in concatenated strings such as `LNA_2NA_4Y`.

### `prepare_data()`

Useful for creating cohort and timing variables. The month and year indicators are
approximations based on 30-day months and 360-day years. `replace_dates` currently
replaces matching values across every column, which can alter identifiers or other
fields. The argument should contain exactly two valid dates.

### `your_college()`

Potentially useful for identifying return and higher-education-system patterns, but
its result depends strongly on the search-date design. The target-college null check
is unsafe, the example documents obsolete arguments, target-name replacement affects
the whole data frame, and alphabetical ordering influences which record is retained.
Empty input and incomplete status combinations are not handled robustly.

### `table_fp2()`

Useful for report-ready frequency tables. There is a confirmed bug: supplying a
non-`NULL` `rounding_dec` produces `object 'rounding_desc' not found`. Zero totals
can also produce `NaN` or `Inf`, and input validation is missing.

## Suggested maintenance order

1. Add tests for `format_data()`, including leading zeros, invalid dates, and output
	 row widths.
2. Fix `table_fp2()` because the failure is immediate and reproducible.
3. Fix `for_NSC_crosstab()` file-path handling and add an end-to-end fixture.
4. Replace unsafe empty-data loops with `seq_len()` and `seq_along()`.
5. Add explicit input validation to all public functions.
6. Review the domain rules in `your_college()` and `highest_degree()` against real
	 NSC exports before relying on their classifications.

## Local installation

Because the package is private, install it from the local package directory:

```r
remotes::install_local("path/to/NSC")
```

For development, load the package with `devtools::load_all("path/to/NSC")`.

## License

This package is licensed under the MIT License. See `LICENSE` and `LICENSE.md`.
