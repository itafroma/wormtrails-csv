;;;; Loads a CSV file and runs it through Wormtrails

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

(in-package #:wormtrails-csv)

;;;; Boilerplate class and method declarations for Wormtrails

(defclass csv-data (wormtrails:chart wormtrails::mouseover-mixin
                                     wormtrails::rainbow-colored-mixin) ()
  (:default-initargs))

(defclass column (wormtrails:bucket) ())

(defclass cell (wormtrails::sample wormtrails::mouseover-mixin) ())

(defclass row (wormtrails:thing) ())

(defmethod wormtrails:create-bucket (index (chart csv-data))
  (make-instance 'column :index index))

(defmethod wormtrails:create-sample (thing (column column))
  (make-instance 'cell :thing thing :bucket column))

(defmethod wormtrails:create-thing (name (chart csv-data))
  (make-instance 'row :name name))

;;;; Chart generation functions.
;;;; Usually, calling GENERATE is what you want.

(defun generate (infile outfile
                        &key (scale 1)
                        (height 100)
                        (label "")
                        (top-n 10))
  "Generate a wormtrails chart at OUTFILE using the CSV file INFILE with
  optional SCALE, HEIGHT, and LABEL. Can be limited to the top TOP-N samples per
  bucket."
  (ensure-directories-exist outfile)
  (wormtrails:output-html (load-chart-from-csv infile)
                          outfile
                          :scaler (wormtrails:linear-scaler scale)
                          :metric-height height
                          :metric-label label
                          :top-n top-n))

(defun load-chart-from-csv (infile)
  "Load a CSV file INFILE and create a wormtrails chart object from it."
  (let ((chart (make-instance 'csv-data)))
    (with-open-file (stream infile)
      ;; Bucket names are defined by the CSV header row
      (let ((buckets (cdr (fare-csv:read-csv-line stream))))
        (loop for row = (fare-csv:read-csv-line stream)
              until (null row)
              do (add-thing-data (car row) (cdr row) chart buckets))))
    chart))

(defun add-thing-data (thing samples chart buckets)
  "Add a sequence of SAMPLES for a THING to a CHART keyed by BUCKETS."
  (loop for sample in samples
        for c from 0
        ;; Skip buckets for thing when no data is present
        when (> (length sample) 0)
        do (wormtrails:add-data chart
                                ;; wormtrails expects bucket to be a number
                                (parse-integer (nth c buckets))
                                thing
                                (parse-integer sample)))
  chart)
