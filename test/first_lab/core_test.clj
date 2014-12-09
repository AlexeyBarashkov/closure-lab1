(ns first-lab.core-test
  (:require [clojure.test :refer :all]
            [first-lab.core :refer :all :as lab]))

(deftest check-arguments-test
  (testing "function 'check-arguments'"
    (testing "Not valid if 'distance-calculation-type' is empty"
      (is (not (#'lab/check-arguments "" "./files/irises.txt"))))
    (testing "Not valid if 'filepath' is empty"
      (is (not (#'lab/check-arguments "euclidean" ""))))
    (testing "Not valid if 'distance-calculation-type' value is not 'euclidean' or 'hamming'"
      (is (not (#'lab/check-arguments "something" "./files/irises.txt"))))
    (testing "Not valid if 'filepath' is not path to file"
      (is (not (#'lab/check-arguments "euclidean" "./files/irises"))))
    (testing "Check with valid arguments"
      (is (#'lab/check-arguments "euclidean" "./files/irises.txt")))
    ))


(deftest get-points-test
  (testing "function 'get-points'"
    (testing "returns correct value"
      (let [file-path "test/first_lab/samples/1.txt"]
        (is (= (#'lab/get-points file-path)
               [{:index 0, :value [5.5 6.6]}
                {:index 1, :value [4.4 3.4]}]))))
    (testing "returns correct value even file contains empty lines"
      (let [file-path "test/first_lab/samples/2.txt"]
        (is (= (#'lab/get-points file-path)
               [{:index 0, :value [5.5 6.6]}
                {:index 1, :value [4.4 3.4]}
                {:index 2, :value [4.1 3.1]}]))))
    ))

(deftest distance-functions-test
  (testing "distance functions"
    (testing "'euclidean-distance' returns correct value"
      (is (= (#'lab/get-euclidean-distance [3 4] [3 3])
             1.0)))
    (testing "'hamming-distance' returns correct value"
      (is (= (#'lab/get-hamming-distance [1 1 1 1 1 1] [2 1 1 2 1 2])
             3)))
    ))

(deftest calculate-points-potentials-test
  (testing "correct potintial calculation"
    (testing "function 'get-potential'"
      (let [get-distance #'lab/get-hamming-distance
            p {:index 1, :value [3.0 4.0]}
            points [{:index 0, :value [1.0 5.0]} p]]
        (is (= (-> (#'lab/get-potential get-distance points p) :potential)
               1.4111122905071873))))
    (testing "'calculate-point-potential'"
      (let [get-distance #'lab/get-hamming-distance
            center {:index 1, :value [3.0 4.0], :potential 1.4111122905071873}
            point {:index 0, :value [1.0 5.0], :potential 1.4111122905071873}]
        (is (= (-> (#'lab/calculate-point-potential get-distance center point) :potential)
               0.4605327868119663))))
    ))