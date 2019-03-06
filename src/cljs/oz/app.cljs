(ns ^:no-doc ^:figwheel-always oz.app
  (:require [reagent.core :as r]
            [cljs.core.async :as async  :refer (<! >! put! chan)]
            [taoensso.encore :as encore :refer-macros (have have?)]
            [taoensso.timbre :as timbre :refer-macros (tracef debugf infof warnf errorf)]
            [clojure.string :as str]
            [taoensso.sente :as sente :refer (cb-success?)]
            [taoensso.sente.packers.transit :as sente-transit]
            [oz.core :as core])
  (:require-macros
   [cljs.core.async.macros :as asyncm :refer (go go-loop)]))

;(timbre/set-level! :info)
(timbre/set-level! :debug)
(enable-console-print!)

(defn- log [a-thing]
  (.log js/console a-thing))

(defn truncate-str
  [s n]
  (apply str (take n s)))

(defonce app-state (r/atom {:text "YIYI! Pay no attention to the man behind the curtain!"
                            :view-spec nil}))

(let [packer (sente-transit/get-transit-packer)
      {:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket-client! "/chsk"
                                         {:type :auto
                                          :packer packer})]
  (def chsk chsk)
  (def ch-chsk ch-recv)
  (def chsk-send! send-fn)
  (def chsk-state state))

(defmulti -event-msg-handler :id)

(defn event-msg-handler [{:as ev-msg :keys [id ?data event]}]
  (debugf "Event: %s" event) []
  (-event-msg-handler ev-msg))

(defmethod -event-msg-handler :default
  [{:as ev-msg :keys [event]}]
  (debugf "Unhandled event: %s" event)
  (log (str "Unhandled event: " event)))

(defmethod -event-msg-handler :chsk/state
  [{:as ev-msg :keys [?data]}]
  (log "chsk/state")
  (let [[old-state-map new-state-map] (have vector? ?data)]
    (if (:first-open? new-state-map)
      (debugf "Channel socket successfully established!: %s" ?data)
      (debugf "Channel socket state change: %s" ?data))))

(defmethod -event-msg-handler :chsk/handshake
  [{:as ev-msg :keys [?data]}]
  (log "handshake")
  (let [[?uid ?csrf-token ?handshake-data] ?data]
    (debugf "Handshake: %s" ?data)))



;; This is the main event handler; If we want to do cool things with other kinds of data going back and forth,
;; this is where we'll inject it.
(defmethod -event-msg-handler :chsk/recv
  [{:as ev-msg :keys [?data]}]
  (log "chsk/recv: where action happens")
  (let [[id msg] ?data]
    (log (str "id: " id))
    (log (str "msg: " (truncate-str msg 240)))
    (case id
      :oz.core/view-spec (swap! app-state assoc :view-spec msg)
      (do
        (log "Push event from server: ")
        (log (truncate-str ?data 80))
        (debugf "Push event from server: %s" ?data)))))


(def router_ (atom nil))

(defn stop-router! []
  (when-let [stop-f @router_] (stop-f)))

(defn start-router! []
  (stop-router!)
  (reset! router_ (sente/start-client-chsk-router! ch-chsk event-msg-handler)))

(defn start! []
  (start-router!)
  (log "GK starting!"))



(defn application [app-state]
  (log "GK app")
  (if-let [spec (:view-spec @app-state)]
    [core/view-spec spec]
    [:div
      [:h1 "XXX Waiting for first spec to load..."]
      [:p "This may take a second the first time if you call a plot function, unless you first call " [:code '(oz/start-plot-server!)] "."]]))

(r/render-component [application app-state]
                    (. js/document (getElementById "app")))

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)


(start!)
