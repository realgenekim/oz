(ns ^:figwheel-always oz.core
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [cljs.core.async :as async  :refer (<! >! put! chan)]
            [taoensso.encore :as encore :refer-macros (have have?)]
            [taoensso.timbre :as timbre :refer-macros (tracef debugf infof warnf errorf)]
            [taoensso.sente :as sente :refer (cb-success?)]
            [taoensso.sente.packers.transit :as sente-transit]
            [cljsjs.vega]
            [cljsjs.vega-lite]
            [cljsjs.vega-embed]
            [cljsjs.vega-tooltip])
  (:require-macros
   [cljs.core.async.macros :as asyncm :refer (go go-loop)]))

(timbre/set-level! :info)
(enable-console-print!)

(defn- ^:no-doc log [a-thing]
  (.log js/console a-thing))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))


(defn ^:no-doc render-vega-lite
  ([spec elem]
   (log "render-vega-lite begin")
   (when spec
     (let [
           jsspec (clj->js spec)
           barespec (assoc-in spec [:data :values] [])
           opts {:renderer "canvas"
                 :mode "vega-lite"}
           vega-spec (. js/vl (compile (clj->js barespec)))]


       (log "Vega-lite translates to:")
       (log vega-spec)
       (log (str "barespec: " (pr-str barespec)))
       (log "calling js/vegaEmbed...")
       (-> (js/vegaEmbed elem (clj->js barespec) (clj->js opts))
           (.then (fn [res]
                    ; as per Christopher suggestion: now call view/change with full data
                    ; https://observablehq.com/@ijlyttle/using-changesets-with-vega-lite
                    ;
                    (log "vegaEmbed completed...")
                    (log res)
                    ; res.view.change('table', changeSet).run();
                    ;(log (.. js/vegaEmbed -vega -changeset))
                    ;(log "creating changeset")
                    ;(let [changeset ((.. js/vegaEmbed -vega -changeset))]
                    ;  (. changeset insert (clj->js (:values (:data spec))))
                    ;  (log "changeset: ")
                    ;  (log changeset)
                    ;  ((.. res -view -change) "table" changeset))

                    (log (.. res -view -insert))
                    (let [newdata (:values (:data spec))
                          ;_       (log newdata)
                          ins     (.insert (.. res -view) "table"
                                    (clj->js newdata))]

                      (. ins runAsync))

                    ;((.. res -view -change) "table" (clj->js spec))
                    ;(. js/view)
                    (log res)
                    (. js/vegaTooltip (vegaLite (.-view res) spec))))
           (.catch (fn [err]
                     (log err))))
       (log "render-vega-lite end")))))

(defn render-vega [spec elem]
  (when spec
    (let [spec (clj->js spec)
          opts {:renderer "canvas"
                :mode "vega"}]
      (-> (js/vegaEmbed elem spec (clj->js opts))
          (.then (fn [res]
                   (log res)
                   (. js/vegaTooltip (vega (.-view res) spec))))
          (.catch (fn [err]
                    (log err)))))))

(defn vega-lite
  "Reagent component that renders vega-lite."
  [spec]
  (log "reagent vega-lite")
  (r/create-class
   {:display-name "vega-lite"
    :component-did-mount (fn [this]
                           (render-vega-lite spec (r/dom-node this)))
    :component-will-update (fn [this [_ new-spec]]
                             (render-vega-lite new-spec (r/dom-node this)))
    :reagent-render (fn [spec]
                      [:div#vis])}))


(defn vega
  "Reagent component that renders vega"
  [spec]
  (r/create-class
   {:display-name "vega"
    :component-did-mount (fn [this]
                           (render-vega spec (r/dom-node this)))
    :component-will-update (fn [this [_ new-spec]]
                             (render-vega new-spec (r/dom-node this)))
    :reagent-render (fn [spec]
                      [:div#vis])}))


(defn ^:no-doc view-spec
  ;; should handle sharing data with nodes that need it?
  [spec]
  ;; prewalk spec, rendering special hiccup tags like :vega and :vega-lite, and potentially other composites,
  ;; rendering using the components above. Leave regular hiccup unchanged).
  ;; TODO finish writing; already hooked in below so will break now
  (log "starting view-spec...")
  (let [retval (clojure.walk/prewalk
                 (fn [x] (if (and (coll? x) (#{:vega :vega-lite} (first x)))
                           [(case (first x) :vega vega :vega-lite vega-lite)
                            (reduce merge (rest x))]
                           x))
                 spec)]
    (log "ending view-spec...")
    retval))


