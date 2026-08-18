# screenshot 0.9.2.9000

* Bug fixes
    * `index2xy()` returned a column shifted by one for an index on the last
      row of a matrix, which could make `locate_image()` report a wrong position.
    * `hex2little_endian()` padded an odd number of digits in the middle,
      so the file size in the BMP header of `create_header()` was wrong.
    * `save_clipboard_image()` raised an error instead of returning `NULL`
      when the clipboard held no image.
    * `locate_image()` applied `round()` and `floor()` to the wrong operand
      because of the precedence of the native pipe.
    * `hay2needle()` lost the defaults of `w` and `h`.
    * `screenshot_exists()` raised an error when `bin_dir` did not exist.
* Search speed
    * `locate_ndl_in_hay()` counts only the values of the needle image with
      `match()` and `tabulate()`, instead of building a frequency table of
      the whole haystack image with `dplyr`. The frequency table was where
      almost all of the time went.
    * One position of the rarest shared value is now used as an anchor, so
      the candidates follow directly from it and the repeated `intersect()`
      is no longer needed. A few pixels spread over the needle image reject
      a candidate before the whole block is compared.
    * On a haystack image of 1920 x 1280 the search went from about 42
      seconds to about 0.25 seconds, and a needle image that is absent from
      the haystack image from about 110 seconds to about 0.3 seconds.
* New features
    * `locate_image()` and `locate_ndl_in_hay()` gain `tol` to allow a
      difference of a few grayscale levels. The default 0 keeps the exact
      match, which is right for a screenshot saved as PNG; a positive value
      helps for an image that has been through a lossy format such as JPEG.
* Other changes
    * `display_size()` falls back to PowerShell where `wmic` is not available.
    * `crop_image()` reports an area outside of the image.
    * `display_corner()` reports an unknown corner name.
    * `tests/testthat.R` now runs the tests, and unit tests were added.

# screenshot 0.9.2

* 2025-08-27
    * `reset_transparent()` to reset transparent color.

# screenshot 0.9.1

* 2024-05-30
    * `display_corner()` to locate image in the display corner.
    * `save_clipboard_image()` to save clipboard image as a png file.

# screenshot 0.9.0

* 2023-08-11

* First release
    * `screenshot()` to take a screenshot.
    * `locate_image()` to locate a image position on a screenshot image.
