# Wormtrails CSV

*Wormtrails CSV* is a Common Lisp library for generating streamgraphs that resemble [Zach Beane][1]‘s famous [movie box office visualizations][2]. It provides a wrapper around Beane‘s [*Wormtrails*][3] library to allow graphs to be generated from simple CSV files.

## Demonstration

Examples of *Wormtrails CSV*’s output can be found in my blog post, “[Charting US city and state growth over time][4]“. Other examples of “wormtrails”-style streamgraphs can be found on Zach Beane‘s [movie box office charts microsite][2].

## Installation

### Environment

*Wormtrails CSV* is a [Quicklisp][5]-based [ASDF][6] system. If you know what that means you’re probably already familiar with Common Lisp and have a working environment. Otherwise, if you’re new to Lisp, I wrote a blog post, “[Setting up a Common Lisp environment on OS X with Sublime Text 3][7]”, that will get you started on OS X by setting up a development environment and the [SBCL][8] compiler.

### Dependencies

Besides *Wormtrails CSV* itself, you will need to manually clone or download a copy of Beane’s [*Wormtrails*][3] and [*Geometry*][9] libraries and place them somewhere Quicklisp/ASDF will look for them. By default, Quicklisp looks for local libraries in `~/quicklisp/local-projects`:

```sh
cd ~/quicklisp/local-projects
git clone git@github.com:/itafroma/wormtrails-csv.git
git clone git@github.com:/xach/wormtrails.git
git clone git@github.com:/xach/geometry.git
```

All other dependencies should be handled automatically when *Wormtrails CSV* is loaded through Quicklisp using `(ql:quickload "wormtrails-csv")`.

## Usage

The main functionality can be accessed through the `GENERATE` function:

```lisp
(ql:quickload "wormtrails-csv")
(wormtrails-csv::generate "/foo/data/states.csv"
                          "/foo/output/states.png"
                          :scale 0.00001
                          :height 100
                          :label "10M PEOPLE"
                          :top-n 25)
```

For more information about what parameters and options can be set, check out the files in the `examples` folder. The files there can also be run run as-is:

```sh
cd ~/quicklisp/local-projects/wormtrails-csv
sbcl --load examples/states.lisp --quit
sbcl --load examples/cities.lisp --quit
```

The example implementations should be easily modifiable to work with most other CSV use cases. Just copy one and edit it to your liking.

## Copyright and license

The `data` folder contains a number of spreadsheets and CSV files containing US census data. Facts generally do not receive protection under US copyright law, and even if they did, the individual data points were created by US census workers acting in their official capacity, thereby placing them in the public domain. Because it is trivially easy to recreate the files in the `samples` folder with that data, the files themselves are also placed into the public domain and any automatic copyright I may have on them is forsworn.

All other files copyright © 2014 Mark Trapp where applicable. They are licensed under the MIT license: a copy can be found in the `LICENSE` file. All other rights reserved.

## Acknowledgements

* [Zach Beane][1], for the original concept, the Wormtrails library, and Quicklisp
* [Kevin Fox][10], for the [prompt][11] that led to the creation of this library

[1]: http://www.xach.com "Zach beane’s website"
[2]: http://www.xach.com/moviecharts/ "Movie box office charts"
[3]: https://github.com/xach/wormtrails "Wormtrails repository on GitHub"
[4]: http://marktrapp.com/2014/02/01/american-population-growth/ "Charting US city and state growth over time"
[5]: http://quicklisp.org/ "Quicklisp website"
[6]: http://common-lisp.net/project/asdf/ "ASDF website"
[7]: http://marktrapp.com/blog/2014/01/20/lisp-with-os-x-sublime-text/ "Setting up a Common Lisp environment on OS X with Sublime Text 3"
[8]: http://www.sbcl.org "SBCL website"
[9]: https://github.com/xach/geometry "Geometry repository on Github"
[10]: http://fury.com "Kevin Fox’s website"
[11]: https://twitter.com/kfury/status/424368704948686848 "Kevin Fox’s tweet about Peakbagger.com’s charts" 
