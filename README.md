# NSC

Private R package for formatting student data for the National Student
Clearinghouse. The package currently supports one function: `format_data()`.

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

## Local installation

Because the package is private, install it from the local package directory:

```r
remotes::install_local("path/to/NSC")
```

For development, load the package with `devtools::load_all("path/to/NSC")`.

There are currently no package tests. The first tests should cover leading zeros,
invalid dates, empty or malformed input, output row widths, and repeated output
filenames.

## License

This package is licensed under the MIT License. See `LICENSE` and `LICENSE.md`.
