# iSEE_app
Customization of the iSEE shiny app [package](https://www.bioconductor.org/packages/release/bioc/html/iSEE.html) to visualize scRNAseq results.

The goal is to merge all the add-on from iSEEu and vignettes in one place.

# Customization

## Landing page

### Object input format

* Ability to load Seurat or sce object: if Seurat it will be converted into sce before starting the app.
* Object can be imported from RDS format, qs format, or HDF5 array format as suggested in the [iSEE vignette](https://bioconductor.org/packages/release/bioc/vignettes/iSEE/inst/doc/bigdata.html)

### Allow user to choose color palette

Modified landing page UI to allow the user to choose a color palette by modifying the default shiny interface generated by `iSEE::createLandingPage`. It plots the color palette on the side. The way the chosen color palette is fed to iSEE is a bit hacky: there is no argument to pass `colormap` to `FUN` so I use `assign` to update the colormap choices within the `FUN` environment.

TODO: create a list of palette to chose from and link with plot.
TODO: have several menu choices for assays vs colData and discrete vs continuous.

## Custom color palettes

As described in [iSEE documentation](https://bioc.ism.ac.jp/packages/3.7/bioc/vignettes/iSEE/inst/doc/iSEE_vignette.html#512_defining_color_maps)


## Saving the app state function

The function only saves the memory slot so that one can restore the preivous state of the plots from session to session


# Plot class modification

## hexplots

* I added a slider bar for the saturation of the color palette to control the min and the max of the color palette.
* I added the ability to control the color palette in relation with the colormap.
* I added a color mapping for the edge of the hexes so that there is no gap between the hexes.
* I hide the second colorbar that is created for the color as it is redundant with fill.