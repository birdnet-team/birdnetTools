# Allocate samples evenly across bins

Internal helper for
[`birdnet_subsample()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_subsample.md).
Decides how many detections to draw from each confidence bin when
sampling evenly across bins (`method = "even_stratified"`).

## Usage

``` r
allocate_even(counts, n)
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

The function works in passes. In each pass, every bin that still has
unsampled detections gets an equal share of the samples left. A bin with
fewer detections than its share gives everything it has, and the unused
part is carried into the next pass and shared among the remaining bins.
When fewer samples are left than there are open bins, one extra sample
goes to each of a randomly chosen subset of bins, so the result can
depend on the random number generator (set a seed beforehand for
reproducible results). Unlike a fixed `floor(n / n_bins)` quota, this
always returns the full number of samples when enough detections exist.

## See also

[`allocate_proportional()`](https://birdnet-team.github.io/birdnetTools/reference/allocate_proportional.md),
[`birdnet_subsample()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_subsample.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# small bins give everything they have; the rest is shared by the others
allocate_even(counts = c(2, 5, 1000, 1000), n = 99)
#> 2 5 46 46

# fewer detections than n: everything is kept
allocate_even(counts = c(3, 4), n = 100)
#> 3 4
} # }
```
