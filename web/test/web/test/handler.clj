(ns web.test.handler
  (:use clojure.test
        ring.mock.request
        web.handler))

(deftest test-app
  (testing "main route"
    (let [response (app (request :get "/"))]
      (is (= (:status response) 200))))

  (testing "not-found route"
    (let [response (app (request :get "/very/invalid/path"))]
      (is (= (:status response) 404)))))
