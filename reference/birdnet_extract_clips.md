# Extract audio clips for BirdNET detections

Cuts a short audio clip for each detection in a BirdNET output table and
saves all clips in one folder, for example to validate detections
without needing the original recordings. Each clip runs from the start
of the detection to its end, plus a buffer before and after.

## Usage

``` r
birdnet_extract_clips(
  data,
  output_dir,
  audio_dir = NULL,
  buffer = 3,
  file = NULL,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- data:

  A data frame containing BirdNET output, with columns for the file
  path, start time, end time, species common name, and confidence
  (detected automatically).

- output_dir:

  Character. Folder where the clips are saved. It is created if it does
  not exist.

- audio_dir:

  Character or `NULL`. Folder containing the original audio recordings,
  searched including subfolders. Only used for files that are not found
  at the path stored in `data`. Defaults to `NULL`, which uses the
  stored paths only.

- buffer:

  Numeric. Seconds added before the start and after the end of each
  detection. Defaults to `3`.

- file:

  Character or `NULL`. Where to save the table as a CSV file (must end
  in `.csv`). Defaults to `NULL`, which saves it as
  `detections_clips.csv` in `output_dir`, next to the clips.

- overwrite:

  Logical. If `FALSE`, the function stops before extracting anything
  when the table file already exists, so that a table you have already
  validated is not replaced by accident. Defaults to `FALSE`.

- quiet:

  Logical. If `TRUE`, suppresses the status messages about how many
  clips were extracted or could not be created. Defaults to `FALSE`.

## Value

The input data frame with additional columns:

- clip_file:

  The name of the clip file in `output_dir`, or `NA` if the clip could
  not be created.

- clip_start:

  Start of the clip in the original recording, in seconds.

- clip_end:

  End of the clip in the original recording, in seconds.

## Details

The detection table, with the clip columns added (see Value), is saved
as a CSV file in the same folder, so the folder holds everything needed
to validate the detections.

**File names.** Clips are saved in the same audio format as the original
recording (e.g. WAV stays WAV, MP3 stays MP3) and are named
`<ID>_<recording name>_<start>_<end>_<species common name>_<confidence>.<extension>`,
where:

- `ID` is the row number of the detection in `data`, with leading zeros
  (`01`, `02`, ...), so the clips sort in the same order as the table.

- `start` and `end` are the detection times in seconds (without the
  buffer).

- the species common name has characters that are unsafe in file names
  replaced with `_`.

- `confidence` is the detection confidence, rounded to three decimals.

For example, `01_20230518_133500_12_15_Barred_Owl_0.873.wav`. Because
every name starts with its own ID, names are always unique.

**Finding the audio.** For each detection, the function first uses the
file path stored in the table. If that file no longer exists (for
example because the recordings were moved to another drive), it looks
for a file with the same name inside `audio_dir`, including all
subfolders. If several files have that name, the one whose parent
folders best match the original path is used.

**Clips near the edges.** A clip is trimmed to fit inside the recording,
so a detection near the start or end of a file gets a shorter buffer on
that side. The times actually used are returned in `clip_start` and
`clip_end`.

## See also

[`birdnet_subsample()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_subsample.md),
[`birdnet_launch_validation()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_launch_validation.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sampled <- birdnet_subsample(my_data, n = 100, seed = 1)

clips <- birdnet_extract_clips(
  sampled,
  output_dir = "validation_audio",
  audio_dir = "path/to/recordings",
  buffer = 3
)
# "validation_audio" now holds the clips and detections_clips.csv
} # }
```
