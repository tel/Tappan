(ns tappan.test.util
  (:use tappan.util)
  (:use clojure.test))

(deftest take-while2:test
  (testing "Simple examples"
    (is (= (take-while2 #(> (- 1 (/ %1 %2)) 0.2) (range 20))
           (range 6))
        "terminates improperly on short lists")
    (is (= (take-while2 #(> (- 1 (/ %1 %2)) 0.02) (range 20))
           (range 20))
        "filter not working")))