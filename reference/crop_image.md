# Cut off a part of image from a whole image.

Cut off a part of image from a whole image.

## Usage

``` r
crop_image(image, pos_x, pos_y, w = 50, h = 20)

hay2needle(image, pos_x, pos_y, w = 50, h = 20)
```

## Arguments

- image:

  An image of cimg.

- pos_x, pos_y:

  A numeric to indicate the top left corner of cutting image. When NULL,
  position will be randomly sampled.

- w, h:

  A numeric for width or height of the cutting image.

## Value

              An image of cimg object.

## Examples

``` r
image <- imager::load.example("parrots")
croped_image <- crop_image(image, 200, 250, 100, 50)
layout(c(1:2))
plot(image)
plot(croped_image)

```
