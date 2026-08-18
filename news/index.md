# Changelog

## screenshot 0.9.2.9000

- Bug fixes
  - [`index2xy()`](https://matutosi.github.io/screenshot/reference/index2xy.md)
    returned a column shifted by one for an index on the last row of a
    matrix, which could make
    [`locate_image()`](https://matutosi.github.io/screenshot/reference/locate_image.md)
    report a wrong position.
  - [`hex2little_endian()`](https://matutosi.github.io/screenshot/reference/hex2little_endian.md)
    padded an odd number of digits in the middle, so the file size in
    the BMP header of
    [`create_header()`](https://matutosi.github.io/screenshot/reference/create_header.md)
    was wrong.
  - [`save_clipboard_image()`](https://matutosi.github.io/screenshot/reference/save_clipboard_image.md)
    raised an error instead of returning `NULL` when the clipboard held
    no image.
  - [`locate_image()`](https://matutosi.github.io/screenshot/reference/locate_image.md)
    applied [`round()`](https://rdrr.io/r/base/Round.html) and
    [`floor()`](https://rdrr.io/r/base/Round.html) to the wrong operand
    because of the precedence of the native pipe.
  - [`hay2needle()`](https://matutosi.github.io/screenshot/reference/crop_image.md)
    lost the defaults of `w` and `h`.
  - [`screenshot_exists()`](https://matutosi.github.io/screenshot/reference/screenshot_exists.md)
    raised an error when `bin_dir` did not exist.
- Other changes
  - [`display_size()`](https://matutosi.github.io/screenshot/reference/display_size.md)
    falls back to PowerShell where `wmic` is not available.
  - [`crop_image()`](https://matutosi.github.io/screenshot/reference/crop_image.md)
    reports an area outside of the image.
  - [`display_corner()`](https://matutosi.github.io/screenshot/reference/display_corner.md)
    reports an unknown corner name.
  - `tests/testthat.R` now runs the tests, and unit tests were added.

## screenshot 0.9.2

CRAN release: 2025-08-27

- 2025-08-27
  - [`reset_transparent()`](https://matutosi.github.io/screenshot/reference/reset_transparent.md)
    to reset transparent color.

## screenshot 0.9.1

CRAN release: 2024-05-30

- 2024-05-30
  - [`display_corner()`](https://matutosi.github.io/screenshot/reference/display_corner.md)
    to locate image in the display corner.
  - [`save_clipboard_image()`](https://matutosi.github.io/screenshot/reference/save_clipboard_image.md)
    to save clipboard image as a png file.

## screenshot 0.9.0

CRAN release: 2023-08-14

- 2023-08-11

- First release

  - [`screenshot()`](https://matutosi.github.io/screenshot/reference/screenshot.md)
    to take a screenshot.
  - [`locate_image()`](https://matutosi.github.io/screenshot/reference/locate_image.md)
    to locate a image position on a screenshot image.
