# Reproduction Materials for "Forgotten but not gone: A multi-state analysis of modern-day debt imprisonment."

## Bulk data downloads

Install [python 3](https://www.python.org) if you do not have it. Then, run the following commands from the command line:

```bash
git clone git@github.com:stanford-policylab/debt
cd debt

# Example uses:
./download.py --all                                           # Download all data
./download.py --state wi --subgeo milwaukee_county            # Download data for a specific county
./download.py --all --download-dir /tmp/debt                  # Download the data to /tmp/debt
./download.py --state wi --subgeo milwaukee_county --type csv # Download as CSV instead of RDS
```

## Reproduction

To reproduce our analysis, download all of the data into the `data/repro`
subdirectory:
```bash
./download.py --all --download-dir data/repro
```
Once the data have downloaded, ensure that you have `groundhog` installed and
are using R version `4.3.1`. Then, simply knit the `main.Rmd` file in the root
of this directory.
```bash
Rscript -e "knitr::knit('main.Rmd')"
```

## Reprocessing Data

Data processing is carried out in a number of steps. A large number of helper
functions for the seven major steps—rectangularization, classification,
parsing, deduplication, imputation, standardization, and sanitization—can be
found in the `lib` directory under the corresponding `R` script. Higher level
functions for managing data processing are contained in `lib/debt.R`.

When a new county is initialized with the `init` script, a number of
sub directories within the `data` directory are created for it:

* `raw`: This directory holds the raw data, as originally given to us by the
  county jail.
* `raw_txt`: This directory holds text extracted from the raw data, usually
  using tools such as `pdftotext` or `tabula`, or the `to_txt.py` utility.
* `raw_csv`: This directory holds data in CSV format that can be ingested by
  `R`, either because it was directly extracted in a usable format, e.g., using
  our `to_csv.py` utility, or one of the scripts in `lib/extract/`, or, in some
  cases, location-specific cleaning scripts.
* `clean`: This directory holds data in CSV and RDS formats that have been
  cleaned and standardized.

Converting raw data into CSV files that can be manipulated in `R` is the first
step of the pipeline (i.e., going from the `raw` directory to the `raw_csv`
directory), which we term "rectangularization":

1. **Rectangularization:** First, raw data must be converted to CSV format.
   This is accomplished by converting the raw data to a CSV using one of the
   conversion utilities in `lib/utils` or `lib/pdf.R`, or with
   [tabula](https://tabula.technology). In some cases, e.g., due to special
   visual formatting, additional conversion is required. Scripts for converting
   specific report types can be found in `lib/xtract`. In some cases, due to
   unusual visual formatting or structure, additional location-specific
   processing is performed on a location-by-location basis to rectangularize
   the data.

Data processing (i.e., going from the `raw_csv` directory to the `clean`
directory`) is undertaken at the location level, according to the following
steps:

2. **Classification:** Once data have been rectangularized, the columns must be
   classified according to the overall data schema. First, an automatic pass
   using cosine similarity is attempted by calling `debt_classify()` from
   `lib/debt.R`. This classification is confirmed (and modified, as
   appropriate) using the `shiny` app in `lib/audit/app.R`.

The remaining steps are carried out automatically by calling `debt_process()` or
`debt_process_all()` after sourcing `debt.R`.

3. **Parsing:** The raw contents of the classified columns are parsed according
   to a standard schema (i.e., "WF" is converted to "white" in the race column
   and "female" in the gender column). (See `lib/parse.R`.)
4. **Deduplication:** Each charge present at the time of booking is separated
   into a distinct row, and rows representing the same charge at the time of
   booking are combined. (See `lib/dedupe.R`.)
5. **Imputation:** Features not included in the data but which can be
   reasonably inferred from the data—most notably, whether the booking
   represents a failure-to-pay booking, but also ages, length of stay,
   ethnicity, etc.—are imputed. (See `lib/impute.R`.)
6. **Standardization:** Parsed values are coerced to the correct types, and
   values are checked to ensure that incorrect values resulting from clerical
   errors (such as ages greater than 100 or less than 15) are removed. (See
   `lib/standardize.R` and `lib/standards.R`.)
7. **Sanitization:** Unparsed personally identifying information is removed
   from free text fields. (See `lib/sanitize.R`.)

Due to the dependency on the [`poster`](https://github.com/Ironholds/poster)
`R` library, which in turn has a dependency on the
[`libpostal`](https://github.com/openvenues/libpostal) `C` library, the
data cleaning code cannot be run in a virtual environment with `renv` or
`groundhog`. We are working on a solution using
[Spack](https://github.com/spack/spack), and will update this repository with
instructions for recreating the data cleaning environment once that process is
complete.

Raw data and location-specific processing scripts are available only by
request.
