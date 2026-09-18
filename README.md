# NSC

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22822594.svg)](https://doi.org/10.5281/zenodo.22822594)

This is an R package for formatting student data for the National Student
Clearinghouse. The package currently supports one function: `format_data()`.

This package is not affiliated with, endorsed by, sponsored by, or officially
connected with the National Student Clearinghouse.

It prepares a seven-column student dataset for submission by creating the NSC
header, detail, and trailer rows, then writing tab-delimited TXT and CSV files.
Always inspect the generated files before submitting them to the National Student
Clearinghouse.

## `format_data()`

### Input

- Input must contain seven columns, in this order: first name, middle initial,
	last name, suffix, date of birth, search date, and requester-return identifier.
- The input may be a data frame or a headerless CSV, TXT, XLS, or XLSX file.
- File inputs are read as text so leading zeros are preserved.
- Birth dates are accepted in common formats, normalized internally to
	`yyyy-mm-dd`, and written to the output as `yyyymmdd`.
- In-memory `school_code`, `branch_code`, and requester-return identifiers must be
	character values. Zeros lost before the function receives a numeric value cannot
	be recovered.

### Output

`format_data()` returns a 12-column data frame containing the NSC header, student
detail rows, and trailer row. It also writes one `.txt` file and one `.csv` file
to `file_dir`. Output filenames use the current date, query option, and `suffix`.

### Important behavior

- Punctuation is removed from every input field, including identifiers. This can
	change identifiers that contain punctuation.
- Birth-date values that are not recognized are rejected.
- For query option `CO`, multiple search dates display a message and return
  `NULL` without creating output files; use `SE` for multiple search dates.
- The function does not yet validate column count, query-code values, code lengths,
	directory existence, or all search-date formats.
- Invalid dates can produce `NA` comparison errors. The current number of days from the search date and today's date must be at least 60 days. So, it gives a message if the length is 59 days or less.
- Blank names are removed, but whitespace-only names are not.
- Repeated calls using the same date, query code, and suffix overwrite files.

## Example

```r
students <- data.frame(
	first_name = "Jane",
	middle_initial = "Q",
	last_name = "Example",
	suffix = "",
	date_of_birth = "2000-01-01",
	search_date = "2020-01-01",
	requester_return_id = "0001",
	stringsAsFactors = FALSE
)

formatted <- format_data(
	school_code = "001234",
	branch_code = "00",
	school_name = "Example College",
	file_creation_date = "20260907",
	query_option = "SE",
	file_dir = tempdir(),
	input_file_name = students,
	suffix = "example"
)
```

The `school_code`, `branch_code`, and requester-return identifier are quoted so
leading zeros remain available to the function.

## Citation

To obtain the package citation, run:

```r
cite <- format(citation("NSC"), style = "text")
cat(gsub("_", "", cite, fixed = TRUE), "\n")
```

The console displays:

```text
Klauth B (2026). NSC: Format Student Data for the National Student Clearinghouse.
doi:10.5281/zenodo.22822594 <https://doi.org/10.5281/zenodo.22822594>,
R package version 1.1.8, <https://github.com/Boklauth/NSC>.
```

## Installation

In addition to CRAN, to install the package from GitHub, type the following:

```r
install.packages("remotes")
remotes::install_github("Boklauth/NSC")
```

## License

This package is licensed under the MIT License. See `LICENSE` and `LICENSE.md`.

## Warranty Disclaimer

This package is provided free of charge and "as is," without warranty of any
kind. To the fullest extent permitted by applicable law, the author disclaims
all express or implied warranties, including warranties of merchantability,
fitness for a particular purpose, accuracy, and non-infringement. Users are
responsible for evaluating the package, verifying its output, and determining
whether it is suitable for their intended use. The author is not responsible
for errors, omissions, data loss, or consequences resulting from the use of
this package.

