;;;; Example implementation for CSV Wormtrails

;;;; Copyright (c) 2014 Mark Trapp

;;;; Permission is hereby granted, free of charge, to any person obtaining a
;;;; copy of this software and associated documentation files (the "Software"),
;;;; to deal in the Software without restriction, including without limitation
;;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;; and/or sell copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included in
;;;; all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;; DEALINGS IN THE SOFTWARE.

;;; Load Wormtrails CSV. Note: you must install the Wormtrails and Geometry
;;; libraries or this will fail! See the "Dependencies" section of the README
;;; for more information.
(ql:quickload "wormtrails-csv")

;;;; Configuration options for Wormtrails and Wormtrails CSV
;;;; Feel free to change anything here to your liking.

;;; CSV file to use for data:
;;;   - The first row is reserved for the header. Column 1 is ignored but the
;;;     rest of the columns are used for time period, or "bucket", names. They
;;;     must be numeric (e.g., use "201401" not "2014-01-01").
;;;   - The reset of the rows correspond to each ribbon/worm, or "thing", on the
;;;     chart. The first column is the thing name, while the rest of the columns
;;;     correspond to the values, or "samples", for each bucket. Samples can be
;;;     blank, but otherwise must be numeric.
;;; See data/states.csv for an example.
;;; Also accepts an absolute path: (defparameter *infile* "/foo/input.csv")
(defparameter *infile* (asdf:system-relative-pathname 'wormtrails-csv "data/states-original-13.csv"))

;;; Output PNG for chart. A companion HTML file with the same name will be
;;; created alongside the PNG.
;;; Also accepts an absolute path: (defparameter *outfile* "/foo/output.png")
(defparameter *outfile* (asdf:system-relative-pathname 'wormtrails-csv "output/states-original-13.png"))

;;; Scale for chart. The correct value to use here is an aesthetic judgment and
;;; should be found via trial-and-error. A basic rule of thumb is to use a scale
;;; that when multiplied by the largest sample value is ~100-200.
(defparameter *scale* 0.00001)

;;; Height of each Y-axis segment in pixels.
;;; Also accepts an integer: (defparameter *metric-height* 100)
(defparameter *metric-height* (* *scale* 10000000))

;;; Cut-off point for worms to show on chart. If a worm is not in the top-n for
;;; a specific time period, it will not show up on the chart, but may show up
;;; in earlier or later time periods if it gets into the top-n then.
(defparameter *top-n* 13)

;;; Label for the Y-axis segmenting.
(defparameter *metric-label* "10M PEOPLE")

;;; Label for the overall chart.
(defparameter wormtrails:*default-name* "Original 13 States")

;;; Width of each time period in pixels.
(defparameter wormtrails:*bucket-width* 100)

;;; Horizontal gap between each time period in pixels.
(defparameter wormtrails:*bucket-gap* 50)

;;; Vertical gap between each worm in pixels.
(defparameter wormtrails:*sample-gap* 2)

;;;; SETQ is used for the next set of options because Wormtrails's vecto.lisp
;;;; uses DEFVAR instead of DEFPARAMETER.

;;; Font size of all labels in points.
(setq wormtrails:*font-size* 13)

;;; Horizontal padding to add to chart in pixels.
(setq wormtrails:*canvas-padding* 25)

;;; Padding on wormtrail labels in pixels.
(setq wormtrails:*text-padding* 2)

;;; Path to the TTF font file to use for labels.
;;; Also acceps an absolute path: (setq wormtrails:*font-file* "/fonts/foo.ttf")
(setq wormtrails:*font-file*
      (asdf:system-relative-pathname 'wormtrails "font.ttf"))

;;; Mouseover label formatting.
;;; This example has two values passed to the mouseover label:
;;; the thing name (a string) and the sample value (a number). You can use any
;;; valid FORMAT specifier for the string: see
;;; http://psg.com/~dlamkins/sl/chapter24.html for a list.
;;; Examples:
;;;   - "~A (pop. ~:D)" => "New York (pop. 12,345,321)"
;;;   - "~A (~D)"       => "New York (12345321)"
;;;   - "~:@(~a~) ~:x"  => "NEW YORK BC5,FE9"
(defmethod wormtrails::mouseover-banner-html ((sample wormtrails-csv::cell))
  (format nil
          "~A (pop. ~:D)"
          (wormtrails:name (wormtrails:thing sample))
          (wormtrails:value sample)))

;;;; End Wormtrails and Wormtrails CSV configuration options

;;; Pass all parameters to Wormtrails CSV and generate the chart.
;;; wormtrails-namespaced parameters are pulled in automatically and should not
;;; be passed here.
(wormtrails-csv::generate *infile*
                          *outfile*
                          :scale *scale*
                          :height *metric-height*
                          :label *metric-label*
                          :top-n *top-n*)
