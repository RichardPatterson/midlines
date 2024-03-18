# midlines 0.0.0.9002

- removed dplyr dependency
- added igraph dependency
- replaced wonky bespoke code that grouped intersecting lines with igraph function. This effects midlines_group(), midlines_check(), midlines_dedensify(), midlines_debit(), midlines_smooth(). The new code is ~2.5x fast than the previous version.
- Vignette added
