;;;; Copyright (c) 2019-2020 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.cold-generator.ppc64le
  (:use :cl)
  (:import-from #:mezzano.cold-generator
                #:configure-system-for-target)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)
                    (#:lap #:mezzano.lap.ppc64le)))

(in-package :mezzano.cold-generator.ppc64le)
