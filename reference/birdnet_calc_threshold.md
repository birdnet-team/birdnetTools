# Calculate species-specific confidence thresholds for BirdNET detections

Computes species-specific confidence thresholds for BirdNET detections
using either a target `precision` or a target predicted `probability`
from a logistic regression model. The function returns one threshold per
species. The `precision`-based approach follows [Tseng et al.
(2025)](https://link.springer.com/article/10.1007/s10336-025-02260-w),
while the `probability`-based method is adapted from [Wood and Kahl
(2024)](https://link.springer.com/article/10.1007/s10336-024-02144-5).

## Usage

``` r
birdnet_calc_threshold(
  validated_data,
  full_data = NULL,
  probability = NULL,
  precision = NULL
)
```

## Arguments

- validated_data:

  A data frame of validated BirdNET detections with columns
  `common_name`, `confidence`, and `validation`. The `validation` column
  must contain 1 (true positives) and 0 (false positives).

- full_data:

  Optional. A data frame of all BirdNET detections with at least
  `common_name` and `confidence` columns. If `NULL`, `validated_data` is
  used instead.

- probability:

  Numeric. A target predicted probability (between 0 and 1) used to
  calculate thresholds from the logistic regression model.

- precision:

  Numeric. A target precision (between 0 and 1) used to select the
  lowest threshold that achieves the desired precision.

## Value

A tibble with two columns:

- common_name:

  Species common name.

- threshold:

  The calculated confidence threshold for that species.

## Details

You must supply exactly one of `precision` or `probability`. If both or
neither are provided, the function will throw an error.

When using the precision method, the function predicts probabilities for
each detection using a logistic regression model fit to
`validated_data`. It then identifies the lowest confidence threshold
that meets or exceeds the target precision.

When using the probability method, the function calculates the
confidence threshold corresponding to the inverse logit of the target
predicted probability from the regression model.

All thresholds are clamped to fall between the minimum observed
confidence in `validated_data` and 1.

## References

Tseng, S., Hodder, D. P., & Otter, K. A. (2025). Setting BirdNET
confidence thresholds: Species-specific vs. universal approaches.
*Journal of Ornithology*. https://doi.org/10.1007/s10336-025-02260-w
Wood, C. M., & Kahl, S. (2024). Guidelines for appropriate use of
BirdNET scores and other detector outputs. *Journal of Ornithology*.
https://doi.org/10.1007/s10336-024-02144-5
