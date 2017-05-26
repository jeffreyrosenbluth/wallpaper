# wallpaper
## Symmetry Patterns
### Wallpaper and Frieze
To make a wallpaper or frieze image, run the wallpaper function with a yaml
file like the following. There are many
![examples](https://github.com/jeffreyrosenbluth/wallpapeer/exampels/)

```yaml
# The wallpaper or frieze group in short IUC notation.
# If the group lattice takes parameters then they follow tagged
# by there names, for example:
#   Group:
#     Name: p1
#     xi: 1.5
#     eta: 2.1
# If the group lattice takes no parameters you can use either
#   Group:
#     Name: p4
# or, just directly:
#   Group: p4
Group: p3m1

# A list of doubly indexed coefficients for the wave functions.
Coefficients:
- A(n,m): [0.75, 0.25] # 0.75 + 0.25i
  n: 1                 # first index of coefficient
  m: 0                 # second index of coefficient
- A(n,m): [0.2, -0.2]
  n: -2
  m: 2
- A(n,m): [0.6, 0.1]
  n: 1
  m: -1

# Optional field for setting the style, choices are: plain, morph (which takes
# one parameter: cutoff), and blend (which takes as an argument a group object).
# morph will interpolate between the color wheel and its 180 degree rotation,
# leaving the proportion specified by cutoff at the beginning and end.
# Blend interpolates horizontally between two different wallpaper groups.
# If left unspecified type will default to style: plain.
Type:
  style: blend
    group: cmm
    b: 1.5

Options:
  width: 380        # width in pixels of the output image
  height: 270       # height in pixels of the output image
  repeat-length: 90 # used to convert pixel coordinates to real numbers
  scale-factor: 0.5 # to scale the color wheel so that it contains the domain values

# Optional, defualts to the standard artists color wheel of infinite size.
Colorwheel-path: rose.png

# Optional, Preprocess the image, choices are: none, fliphorizontal,
# flipvertical, flipboth, invert,  antisymmvertical, and antisymmhorizontal.
# The default is none.
Pre-process: AntiSymmHorizontal

# File to write the result. The file type will match the extension.
# Choices for extenstion are: png, jpg, tif, and bmp.
Output-path: result.png
```
