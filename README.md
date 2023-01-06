# iSEE_app
Customization of the iSEE shiny app [package](https://www.bioconductor.org/packages/release/bioc/html/iSEE.html) to visualize scRNAseq results.

The goal is to merge all the add-on from iSEEu and vignettes in one place.

# Customization

## Landing page

* Ability to load Seurat or sce object: if Seurat it will be converted into sce before starting the app.
* Object can be imported from RDS format, qs format, or HDF5 array format as suggested in the [iSEE vignette](https://bioconductor.org/packages/release/bioc/vignettes/iSEE/inst/doc/bigdata.html)

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