(ns web.view
  (:use web.generic)
  (:require
    [net.cgrand.enlive-html :refer :all]))

(defsnippet logged-in
  {:parser xml-parser} "templates/index.html"
  [:#Auth]
  [])

(deftemplate view-index
  {:parser xml-parser} "templates/index.html"
  [auth_uri account]
  [:#GoogleOauth2Login] (set-attr :href auth_uri)
  [:div#LoggedIn] #(let [tr (if account
                              (content (:email account))
                              (constantly nil))]
                      (tr %)))

(deftemplate view-error
  {:parser xml-parser} "templates/error.html"
  [msg]
  [:#ErrorMessage] (content msg))
