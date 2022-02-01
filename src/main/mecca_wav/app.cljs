(ns mecca-wav.app
  (:require 
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [goog.object :as o]
   [goog.crypt :as crypt]))

(defonce ^:dynamic *context* (js/AudioContext.))

(defn audio-buffer [input context]
  (let [sample-rate 44100
        frame-count (count input)
        buffer (.createBuffer context 1 frame-count sample-rate)
        data (.getChannelData buffer 0)]
    (doseq [i (range frame-count)]
      (aset data i (nth input i)))
    buffer))

(defn buffer-source [buffer]
  (let [source (.createBufferSource *context*)
        gain (.createGain *context*)]
    (.resume *context*)
    (set! (.-buffer source) buffer)
    (.setValueAtTime (.-gain gain) 0.5 (.-currentTime *context*))
    (.connect source gain)
    (.connect gain (.-destination *context*))
    (.start source (.-currentTime *context*))
    source))

(-> (js/Math.random) (* 2.0) (- 1.0))

(defn square-root
  [x]
  (.sqrt js/Math x))

(defonce file-atom (r/atom ""))

(defn file-upload []
  [:div
   [:h1 "File upload"]
   [:input#input
    {:type      "file"
     :on-change
     (fn [e]
       (let [dom    (o/get e "target")
             file   (o/getValueByKeys dom #js ["files" 0])
             reader (js/FileReader.)]
         (.readAsArrayBuffer reader file)
         (set! (.-onload reader)
               #(reset! file-atom (-> % .-target .-result
                                      (js/Uint8Array.)
                                      crypt/byteArrayToHex
                                      .toUpperCase
                                      )))))}]])

(defn offsets
  ([file n] (offsets file n (inc n)))
  ([file from to]
   (map #(apply str %)
        (partition 2 (take (- (* 2 to) (* 2 from))
                           (drop (* 2 from) file))))))

(defn ascii [bytes]
  (apply str (map #(js/String.fromCharCode (str "0x" %)) bytes)))

(defn decimal [bytes]
  (js/parseInt (str "0x" (apply str (reverse bytes)))))

(defn header [file]
  {:ckID            (ascii (offsets file 0 4))
   :cksize          (decimal (offsets file 4 8))
   :WAVEID          (ascii (offsets file 8 12))
   :fmtID           (ascii (offsets file 12 16))
   :fmtsize         (decimal (offsets file 16 20))
   :wFormatTag      (decimal (offsets file 20 22))
   :nChannels       (decimal (offsets file 22 24))
   :nSamplesPerSec  (decimal (offsets file 24 28))
   :nAvgBytesPerSec (decimal (offsets file 28 32))
   :nBlockAlign     (decimal (offsets file 32 34))
   :wBitsPerSample  (decimal (offsets file 34 36))
   :dataID          (ascii (offsets file 36 40))
   :datasize        (decimal (offsets file 40 44))})

(defn svg-bar [w h x y color]
   [:rect
    {:width        w
     :height       h
     :fill         color
     :x            x
     :y            y
     :stroke       "#00d0ff"
     :stroke-width 0.05}])

(let [items '(-1 -1 -1 1 1 1)
      bar-width (/ 500 (count items))]
  (for [bar (range (count items))]
    (svg-bar bar-width
             (+ (nth items bar) 1)
             (* bar bar-width)
             (- 50 (/ (nth items bar) 2.0))
             "magenta")))

(defn bars [items]
  (let [bar-width (/ 100 (count items))]
    (into [:g]
          (for [bar (range (count items))]
            (svg-bar bar-width
                     1
                     (* bar bar-width)
                     (- 20 (* 10 (nth items bar)))
                      "magenta")))))

(bars '(-1 -1 -1 1 1 1))

(defn make-path-data [x y]
  (str "L" x " " y))

(defn button [label onclick]
  [:button
   {:on-click onclick}
   label])

(defn scale2 [values]
  (let [maximum (apply max values)
        minimum (apply min values)
        spread (- maximum minimum)]
    (map #(- (* 2 (/ (- % minimum) spread)) 1)
    values)))

(subs  "ehtdsrfo" 1)

(defn app []
  [:div#app
   [:h1 "mecca-wav"]
   [:h2 "PCM data browser"]
   [file-upload]
   [:div [button "Play"
        (fn [] (buffer-source (audio-buffer (map decimal (map #(apply str %)
                                                              (partition 4 @file-atom)))
                                            *context*)))]]
   [:svg {:width    "100%"
          :view-box (str "0 0 100 50")}
    [:path {:d (let [data (let [data  (scale2 (drop 44 (map decimal (map #(apply str %)
                                                                              (partition 4 @file-atom)))))
                                     parts (.floor js/Math (/ (count data) 500))]
                                 (map first (partition parts data)))]
                      (str "M" (subs (apply str (for [x (range (count data))]
                                                  (make-path-data x (- 20 (* 10 (nth data x))))))
                                     1)))
            :stroke "black"
            :stroke-width 0.1
            :fill "none"}]]
   [:h3 "Header:"]
   [:textarea
    {:rows      6
     :cols      50
     :value     (str (header @file-atom))
     :read-only true}]
   [:h3 "Data:"]
   [:textarea
    {:rows      30
     :cols      110
     :value     (str (map decimal (map #(apply str %)
                                       (partition 4 @file-atom))))
     :read-only true}]])

(defn render []
  (rdom/render [app]
            (.getElementById js/document "root")))

(defn ^:dev/after-load start []
  (render)
  (js/console.log "start"))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
