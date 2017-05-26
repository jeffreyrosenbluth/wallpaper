# wallpaper
## Symmetry Patterns
Create wallpaper patterns using the domain coloring algorithm as described in

[Creating Symmetry], by Frank A. Farris, (c) 2015 Princeton University Press.

[Creating Symmetry]: https://www.amazon.com/Creating-Symmetry-Mathematics-Wallpaper-Patterns/dp/0691161739/ref=sr_1_1?ie=UTF8&qid=1495813829&sr=8-1&keywords=creating+symmetry

## Installation
Install [stack](https://docs.haskellstack.org/en/stable/README/)

```
git clone https://github.com/jeffreyrosenbluth/wallpaper.git
stack build
stack exec wallpaper myconfig.yaml
```

### Wallpaper and Frieze
To make a wallpaper or frieze image, run the wallpaper function with a yaml
file like the following. There are many [examples].

![example](https://github.com/jeffreyrosenbluth/wallpaper/blob/master/examples/readme.png)

[examples]: https://github.com/jeffreyrosenbluth/wallpaper/tree/master/examples

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
# flipvertical, flipboth, invert, antisymmvertical, and antisymmhorizontal.
# The default is none.
Pre-process: AntiSymmHorizontal

# File to write the result. The file type will match the extension.
# Choices for extenstion are: png, jpg, tif, and bmp.
Output-path: result.png
```

### Rosette
To make a rosette, use a yaml file like the following. There are also some
[examples], look at rosetteP.yaml and rosettePM.yaml.

```yaml
P-fold: 5    # the number of rotational symmetries
Mirror: true # symmetric about the horizontal axis
Coefficients:
- A(n,m): [0.5, 0]
  n: 5
  m: 0
- A(n,m): [0.3, 0]
  n: 4
  m: -6
Options:
  width: 1080
  height: 1080
  repeat-length: 270
  scale-factor: 0.5
Colorwheel-path: rose.png
Pre-process: invert
Output-path: result.png
```
