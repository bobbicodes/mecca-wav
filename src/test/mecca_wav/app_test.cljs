(ns mecca-wav-test
  (:require [cljs.test :refer [deftest is run-tests]]
            [shadow-reagent.app :as app]))

#_(deftest square-root-test
  (is (= (app/square-root 4) 2)))

(comment
  (run-tests)
  )