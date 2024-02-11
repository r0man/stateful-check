(ns stateful-check.nrepl
  (:require [stateful-check.nrepl.middleware]))

(def middleware
  '[stateful-check.nrepl.middleware/wrap-stateful-check])
