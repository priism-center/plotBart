# plotBart 0.1.27
- new function `table_moderator_c_bin()`

# plotBART 0.1.26
- new features for `plot_balance()`

# plotBart 0.1.25
- update SATE with bartCause changes

# plotBart 0.1.24
- update SATE with bartCause changes

# plotBart 0.1.23
- added `plot_trank()` function
- added additional paramaters to `plot_trace()`

# plotBart 0.1.22
- minor bug fix

# plotBart 0.1.21
- Added `view_overlap` argument to `plot_SATE()`function. 
- Allows user to view what results would have been under alternative common support rules. 
- Improvements to `plot_common_support()`
- Added `trim` feature to `plot_overlap_pScores()` this allows users to zoom in on relevant area of the plot for overlap checks

# plotBart 0.1.20
- Added `plot_moderator_c_bin()` function. 

# plotBart 0.1.10
- Documentation updates for CRAN

# plotBart 0.1.9
- Documentation updates for CRAN

# plotBart 0.1.8
- Documentation updates for CRAN

# plotBart 0.1.7
- Documentation updates for CRAN

# plotBart 0.1.6
- Documentation updates for CRAN

# plotBart 0.1.5
- Documentation is now included for each function

# plotBart 0.1.4
- propensity_scores() no longer exported
- Removed legend argument from `plot_moderator_c_pd()`. Legend can be modified using `p + theme(legend.position = "<position>")`
- `plot_overlap_pScores()` and `propensity_scores()` now pass additional arguments to `bartCause::bartc()` which enables seed setting

# plotBart 0.1.3
- Homogenized function argument names across functions. This is code breaking as previous argument names are not supported.
- `plot_moderator_search()` now returns a ggplot object instead of an `rpart.plot` object

# plotBart 0.1.2
- Fixed bug in waterfall plot

# plotBart 0.1.1
- Fixed bug in pate plot

# plotBart 0.1.0
- Initial release

