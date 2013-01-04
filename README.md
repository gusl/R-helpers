R-helpers
=========

R-helpers is a jumble of functions that I load by default when I start up R (to see how this is done, look at .Rprofile).  I have not packaged them as an R library.  If you see anything that you want to use, just copy the file, and source it.  Some of them require having external libraries installed (which you can see by looking for calls to `library`).  If this still doesn't work, there may be dependencies on other R-helper files, so you might want to copy everything and try again. :-p

I have tried to make the files work independently of each other, and document the functions, but I am sure there are ommissions and bugs.

I hope that R-helpers can be useful to other R users.  Here are some highlights:

* inspect.R: I expect this will be the most useful function here.  In the process of debugging, it is common for R programmers to write code like: `cat("x = "); print(x)`  but this is repetitive and inflexible.  `inspect` handles objects that can be printed inline (e.g. integers and strings) as well as objects that can't be (e.g. matrices, functions).  Furthermore, we make it easy to turn off all calls to inspect in one go (by setting NO_INSPECT), or to turn on only a subset of inspects (by setting ACTIVE_TAGS).

* plotting.R: `pplot` is useful for debugging when you have overlapping plots.  It figures out when to call `plot` vs `points`.

* plotting.R: There is also some code for generating color functions (i.e. functions from [0,1] to RGB colors) from RColorBrewer palettes as well as pairs of colors.

* string.R: function to compute the Levenshtein edit distance between two strings or two character vectors.

* meta-programming.R: for any distribution following the R naming convention, `distributionFuns` wraps together the `p`,`q`,`r`,`d` functions (i.e. cdf, quantile, random sample, pdf) as "methods" of a Distribution object.  This is one place in R where a little Object-Orientation would be welcome, and `distributionFuns` is one step in that direction.

* meta-programming.R: I have a reimplementation of `with`, called `getProperty`.



To help you use my functions, I have included some examples behind comments (##), usually right after the function's implementation.

Gustavo Lacerda, 4 Jan 2013
http://www.optimizelife.com