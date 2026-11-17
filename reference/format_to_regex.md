# Convert a datetime format string into a regular expression

Internal helper for
[`birdnet_add_datetime()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_add_datetime.md).
Translates a [`strptime()`](https://rdrr.io/r/base/strptime.html)-style
format (e.g. `"%Y%m%d_%H%M"`) into a regular expression that matches
text of the same shape, so users can describe a file name pattern with a
familiar format instead of writing a regex.

## Usage

``` r
format_to_regex(fmt)
```

## Arguments

- fmt:

  A single character string giving a
  [`strptime()`](https://rdrr.io/r/base/strptime.html)-style format.

## Value

A single character string containing a regular expression (using
lookarounds, so it needs the ICU engine used by stringr).

## Details

Supported codes are `%Y` (4-digit year, 1900-2099), `%y`, `%m`, `%d`,
`%H`, `%M` and `%S` (2 digits each). All other characters are matched
literally, with regex special characters escaped. The regex only checks
the *shape* of the text (the number of digits in each position), not
whether the values are valid dates. That check is left to
[`strptime()`](https://rdrr.io/r/base/strptime.html). The match is
wrapped in lookarounds so it only matches a digit run of exactly this
shape and never part of a longer run of digits (e.g. the prefix in
`00014530_20230518T133500`).

## Examples

``` r
if (FALSE) { # \dontrun{
format_to_regex("%Y%m%d_%H%M")

stringr::str_extract(
  "00014530_20230518T133500.WAV",
  format_to_regex("%Y%m%dT%H%M%S")
)
} # }

```
