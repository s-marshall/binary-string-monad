(ns binary-string-monad.core-test
  (:require [clojure.test :refer :all]
            [binary-string-monad.core :refer :all]))

  (deftest binary-to-decimal-test
    (testing
      (is (= (binary-to-decimal "101") 5))
      (is (= (binary-to-decimal "1010111") 87))
      (is (= (binary-to-decimal "1010") 10))))

  (deftest first-monad-law-test
    (testing
      (is (= (bs-bind (bs-result 5) (comp inc dec)) 5))
      (is (= (bs-bind (bs-result 5) (comp inc inc)) 7))
      (is (= (bs-bind (bs-result 5) inc) 6))
      (is (= (bs-bind (bs-result 5) dec) 4))))
      
  (deftest second-monad-law-test
    (testing      
      (is (let [mv (bs-result 10)]
        (= (bs-bind mv bs-result) mv)))))

  (deftest third-monad-law-test        
    (testing
      (is (let [mv (bs-result 10)
                m-inc (lift-function-into-monad inc bs-result)]
            (= (bs-bind (bs-bind mv m-inc) dec) (bs-bind mv (fn [x] (bs-bind (m-inc x) dec))))))))  
  
  (deftest binary-add-test
    (testing
      (is (= (binary-to-decimal (add "1111" "111")) 22))           
      (is (= (binary-to-decimal (add "11" "101")) 8))))

  (deftest binary-multiply-test
    (testing
      (is (= (binary-to-decimal (multiply "1001" "1001")) 81))
      (is (= (binary-to-decimal (multiply "10" "100")) 8))
      (is (= (binary-to-decimal (multiply "11" "101")) 15))))

  (deftest binary-power-test
    (testing
      (is (= (binary-to-decimal (power "10" "11")) 8))           
      (is (= (power "100" "11") (multiply (multiply "100" "100") "100")))           
      (is (= (binary-to-decimal (power "11" "100")) 81))))
