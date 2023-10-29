# VizConnectome

### **1. Introduction**

This R markdown document will show you how to generate connectograms and
chord diagrams to illustrate your brain connectivity-related results,
both SC and FC. Currently, in our neuroimaging database, we have
connectivity data that are derived from different atlas parcellation
schemes, as a result, the lengths of the input vectors (N x N FC/SC
matrix stringed out into a single vector) are contingent upon the atlas
parcellation schemes used. Previously, I had written out many R
codes/markdown documents to deal with these different parcellation
schemes, which have been somewhat confusing and messy.

To this end, I decided to implement a unified approach to generate these
figures, regardless of the parcellation schemes used to generate the
FC/SC matrices. This approach is also a lot simpler and cleaner because
most of the code is placed in the cloud (i.e., github) and you will only
need to enter one line of code to generate the figures

### **2. Connectogram**

**a. Install and load the required packages**

If you have not previously installed these packages, run the following
line without the \#

    #install.packages(c("ggplot2","ggraph","igraph","grid","gridExtra"))

then load the required packages

    require(ggplot2)
    require(ggraph)
    require(igraph)
    require(grid)
    require(gridExtra)

**b. Plotting the connectogram**

The `vizConnectogram()` function can take input vectors of the following
lengths:

-   30135 — derived from 246x246 FC matrices generated using the
    [Brainnetome](https://atlas.brainnetome.org/bnatlas.html) atlas

-   23871 — derived from 219x219 FC matrices generated using the
    [Schaefer](https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal)-200
    atlas + 19 subcortical regions from the freesurfer subcortical
    segmentations. This is typically used in the NIMH datasets

-   7021 — derived from 119x119 FC matrices generated using the
    [Schaefer](https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal)-100
    atlas + 19 subcortical regions from the freesurfer subcortical
    segmentations. This is typically used in the ABCD dataset

-   4005 — derived from 90x90 SC matrices generated using the
    [AAL](https://www.sciencedirect.com/science/article/abs/pii/S1053811901909784?via%3Dihub)-90
    atlas

The number of nodes to display in the connectogram plot will be
automatically determined from the length of the input vector.

For the first example, we will generate a sparse vector of 30135 random
numbers ranging from -1 to 1 and use this vector as the input vector to
simulate the results derived from running the network-based statistics
on the 219x219 FC matrices. The vector is ‘sparse’ because it contains
very few non-zero values. You will see why this is important later

    results=sample(c(1,0, -1), 23871, replace = TRUE, prob = c(0.001, 0.998,0.001))

In the above line, the frequency of the numbers `c(1,0, -1)` appearing
in the vector corresponds to the specified probabilities
`prob = c(0.001, 0.998,0.001)`. Next, we will call upon the
`vizConnectogram()` function from the cloud, and run it on the `results`
vector

    source("https://github.com/CogBrainHealthLab/VizConnectome/blob/main/vizConnectome.R?raw=TRUE")

    vizConnectogram(data=results, filename="FC_219.png")

The above code will output the following `FC_219.png` image in your
working directory. Do note that the distribution of nodes in the
Schaefer-100 and Schaefer-200 parcellation schemes is not symmetrical
across both hemispheres.

![](examples/FC_219.png)

**b. Customizing optional parameters**

The `vizConnectogram()` allows you to customize the following visual
parameters

-   `hot` : color code or name for the color of positive edges

-   `cold` : color code or name for the color of positive edges

-   `edgethickness`: thickness of the edge links; default thickness is
    set to `0.8`

-   `colorscheme`: a vector of 8 color codes/names for all FC matrices
    or 7 color codes/names for SC matrices

In the next example, we will simulate results derived from the
brainnetome atlas (246x246 FC matrices; length of input vector=30135)
and also play around with the color-related parameters,

    results=sample(c(1,0, -1), 30135, replace = TRUE, prob = c(0.001, 0.998,0.001))

    ## to generate color codes for the color scheme
    library(paletteer) 
    colorcodes=c(paletteer_c("ggthemes::Sunset-Sunrise Diverging", 8))

    vizConnectogram(data=results, filename="FC_246.png", hot="red", cold="blue", colorscheme=colorcodes)

The above code will output the following `FC_246.png` image in your
working directory. ![](FC_246.png) As you can see it can be a little
difficult to differentiate which network the nodes belong to because the
colors are too similar. So do pay attention to the choice of colors if
you were to adjust the `colorscheme` parameter

**b. Excessive cluttering**

The connectograms are ideal for visualizing sparse FC/SC matrices. If
there are too many non-zero values in these matrices, the connectogram
can become too cluttered and consequently difficult to interpret. Here’s
an example:

    results=sample(c(1,0, -1), 7021, replace = TRUE, prob = c(0.01, 0.98,0.01))
    vizConnectogram(data=results, filename="FC_119.png")

![](FC_119.png)

You can make it look a little less cluttered by reducing the thickness
of the edge links. Note that the default value is `0.8`

    vizConnectogram(data=results, filename="FC_119_thinedges.png",edgethickness = 0.5)

![](FC_119_thinedges.png) This will be very effective if the edges are
nicely bundled up, which isn’t the case for the above. So if you tried
to reduce the thickness of the edge links and it doesn’t help much, then
perhaps you should use a chord diagram instead

### **3. Chord diagram**

The `vizChord()` function can take input vectors of various lengths as
described in the connectogram section and additionally, vectors of 68
values — derived from 12x12 functional network matrices generated using
the
[Gordon](https://academic.oup.com/cercor/article/26/1/288/2367115?login=falseb)
atlas. The 12x12 functional network matrices are only available in the
ABCD dataset.

**a. Install and load the required packages**

If you have not previously installed these packages, run the following
line without the \#

    #install.packages(c("circlize","ggplotify","cowplot"))

then load the required packages

    require(circlize)
    require(ggplotify)
    require(ggplot2)
    require(cowplot)

**b. Plotting the chord diagram**

Here’s a basic use of the `vizChord()` function on an input vector of
7021 <u>non-zero values</u> ranging from -1 to 1.

    results=runif(7021, min = -1, max = 1)

    source("https://github.com/CogBrainHealthLab/VizConnectome/blob/main/vizChord.R?raw=TRUE")
    vizChord(data=results, filename="FC_chord119.png")

![](FC_chord119.png)

Just like the `vizConnectogram()` function, you can customize the `hot`
and `cold` colors, as well as the colors for the networks. Note that if
your input vector has a length of 78 (derived from the 12x12 functional
network matrices), you will need to specify 12 colors (because 12
networks) for the `colorscheme`.

**c. Customizing optional parameters**

    results=runif(78, min = -1, max = 1)

    ## to generate color codes for the color scheme
    library(paletteer) 
    colorcodes=c(paletteer_c("grDevices::Temps", 12))

    vizChord(data=results, filename="FC_chord12.png", hot="red", cold="blue", colorscheme = colorcodes)

![](FC_chord12.png)
