(ns web.view
  (:use web.generic)
  (:require
    [net.cgrand.enlive-html :as html]))

(html/deftemplate view-index
  {:parser html/xml-parser} "templates/index.html"
  [auth_uri]
  [:#GoogleOauth2Login] (html/set-attr :href auth_uri))

(html/deftemplate view-error
  {:parser html/xml-parser} "templates/error.html"
  [msg]
  [:#ErrorMessage] (html/content msg))
