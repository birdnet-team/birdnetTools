# Allocate samples in proportion to bin size

Internal helper for
[`birdnet_subsample()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_subsample.md).
Decides how many detections to draw from each confidence bin when
sampling in proportion to the number of detections in each bin
(`method = "proportional_stratified"`).

## Usage

``` r
allocate_proportional(counts, n)
```

## Arguments

- counts:

  A numeric vector of whole numbers giving the number of detections in
  each bin.

- n:

  A single positive whole number giving the total number of detections
  to sample.

## Value

An integer vector the same length as `counts`, giving the number of
detections to sample from each bin. No value exceeds the matching value
in `counts`, and the values sum to `min(n, sum(counts))`.

## Details

Each bin's share is `n * counts / sum(counts)`. Shares are rounded down,
and the samples left over from rounding go to the bins with the largest
fractional parts (the largest-remainder method). A bin can never give
more than it has, so if a bin's share exceeds its size, the shortfall is
re-spread in proportion across the bins that still have detections. The
total is therefore always the full number of samples when enough
detections exist. Bins with very few detections can receive none.

## See also

[`allocate_even()`](https://birdnet-team.github.io/birdnetTools/reference/allocate_even.md),
[`birdnet_subsample()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_subsample.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# allocation follows bin size; the two smallest bins get none
allocate_proportional(counts = c(2, 5, 1000, 3000), n = 100)
#> 0 0 25 75
} # }
```
