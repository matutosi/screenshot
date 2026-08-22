# Changelog

## screenshot 0.9.3

CRAN release: 2026-08-22

- 2026-08-20

- Bug fixes

  - [`screenshot()`](https://matutosi.github.io/screenshot/reference/screenshot.md)
    failed with `'C:/Program' not found` when a path held a space, such
    as a package installed under `C:/Program Files`
    ([\#1](https://github.com/matutosi/screenshot/issues/1)). The
    command is now quoted with
    [`shQuote()`](https://rdrr.io/r/base/shQuote.html) for the shell of
    the platform. The `quote` argument of
    [`screenshot()`](https://matutosi.github.io/screenshot/reference/screenshot.md)
    did not help, because it wrapped the whole command in single quotes,
    which `cmd.exe` does not treat as quoting. It is deprecated and
    ignored now.
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

- Search speed

  - [`locate_ndl_in_hay()`](https://matutosi.github.io/screenshot/reference/locate_ndl_in_hay.md)
    counts only the values of the needle image with
    [`match()`](https://rdrr.io/r/base/match.html) and
    [`tabulate()`](https://rdrr.io/r/base/tabulate.html), instead of
    building a frequency table of the whole haystack image with `dplyr`.
    The frequency table was where almost all of the time went.
  - One position of the rarest shared value is now used as an anchor, so
    the candidates follow directly from it and the repeated
    [`intersect()`](https://rdrr.io/r/base/sets.html) is no longer
    needed. A few pixels spread over the needle image reject a candidate
    before the whole block is compared.
  - On a haystack image of 1920 x 1280 the search went from about 42
    seconds to about 0.25 seconds, and a needle image that is absent
    from the haystack image from about 110 seconds to about 0.3 seconds.

- New features

  - [`locate_image()`](https://matutosi.github.io/screenshot/reference/locate_image.md)
    and
    [`locate_ndl_in_hay()`](https://matutosi.github.io/screenshot/reference/locate_ndl_in_hay.md)
    gain `tol` to allow a difference of a few grayscale levels. The
    default 0 keeps the exact match, which is right for a screenshot
    saved as PNG; a positive value helps for an image that has been
    through a lossy format such as JPEG.

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
