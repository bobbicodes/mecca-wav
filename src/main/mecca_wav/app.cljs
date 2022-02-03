(ns mecca-wav.app
  (:require 
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [goog.object :as o]
   [goog.crypt :as crypt]))

(defonce ^:dynamic *context* (js/AudioContext.))
(defonce file-atom (r/atom ""))

(defn scale
  "Scales a sequence of values to output between t-min and t-max."
  [values t-min t-max]
  (let [maximum (apply max values)
        minimum (apply min values)
        spread (- maximum minimum)]
    (reverse (map #(+ (* (/ (- % minimum)
                            spread)
                         (- t-max t-min))
                      t-min)
                  values))))

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
    (.setValueAtTime (.-gain gain) 0.1 (.-currentTime *context*))
    (.connect source gain)
    (.connect gain (.-destination *context*))
    (.start source (.-currentTime *context*))
    source))

(defn file-upload []
  [:input#input
   {:type      "file"
    :on-change (fn [e]
                 (let [dom    (o/get e "target")
                       file   (o/getValueByKeys dom #js ["files" 0])
                       reader (js/FileReader.)]
                   (.readAsArrayBuffer reader file)
                   (set! (.-onload reader)
                         #(reset! file-atom (-> % .-target .-result
                                                (js/Uint8Array.)
                                                crypt/byteArrayToHex
                                                ;.toUpperCase
                                                )))))}])

(defn i16
  "Converts an integer (maximum 32-bits) to a signed 16-bit integer."
  [max-b32]
  (let [data-view (js/DataView. (js/ArrayBuffer. 2))]
    (.setUint16 data-view
                0
                max-b32)
    (.getInt16 data-view
               0)))

(defn s2c 
  "Reads a pair of bytes as signed little endian 2's complement."
  [bytes]
  (let [dec (js/parseInt (str "0x" (apply str (reverse bytes))))]
    (i16 dec)))

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

(defn button [label onclick]
  [:button
   {:on-click onclick}
   label])

(defn make-path [points]
  (str "M" (apply str (interpose " " (for [x (range (count points))]
                                       (str x " " (nth points x)))))))

(defn play! [file]
  (let [bytes (map s2c (partition 2 (map #(apply str %) (partition 2 (drop 44 file)))))]
    (buffer-source (audio-buffer (clj->js (scale (reverse bytes) -1 1)) *context*))))

(defn graph [file]
  (let [data (reverse (map s2c (partition 2 (map #(apply str %) (partition 2 (drop 44 file))))))
        parts (.floor js/Math (/ (count data) 500))]
    [:svg {:width    "100%"
           :view-box (str "0 0 500 150")}
     [:path {:d (make-path (scale (map first (partition parts data)) 0 150))
             :stroke       "black"
             :stroke-width 0.5
             :fill         "none"}]]))

(defn app []
  [:div#app
   [file-upload]
   [:div [button "Play"
        (fn [] (play! @file-atom))]]
   [:p]
   [graph @file-atom]
   [:h3 "Header (bytes 0-44):"]
   [:textarea
    {:rows      6
     :cols      50
     :value     (str (header @file-atom))
     :read-only true}]
   [:h3 "Data (hex):"]
   [:textarea
    {:rows      15
     :cols      110
     :value     (str (map #(apply str %)(partition 2 @file-atom)))
     :read-only true}]
   [:h3 "Data (decimal):"]
[:textarea
 {:rows      15
  :cols      110
  :value     (str (map s2c (partition 2 (map #(apply str %) (partition 2 @file-atom)))))
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
