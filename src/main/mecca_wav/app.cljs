(ns mecca-wav.app
  (:require 
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [goog.object :as o]
   [goog.crypt :as crypt]))

(defonce ^:dynamic *context* (js/AudioContext.))

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

(defn decimal [bytes]
  (js/parseInt (str "0x" (apply str (reverse bytes)))))

(defn audio-buffer [input context]
  (let [sample-rate 44100
        frame-count (count input)
        buffer (.createBuffer context 1 frame-count sample-rate)
        data (.getChannelData buffer 0)]
    (doseq [i (range 10000)]
      (aset data i (nth input i)))
    buffer))


(defonce file-atom (r/atom ""))

(comment
  (nth (scale (map decimal (map #(apply str %)
                                (partition 4 @file-atom)))
              -1 1)
       44000)

(repeatedly 10 #(-> (js/Math.random) (* 2.0) (- 1.0)) )

(let [duration 1.0
      context *context*
      sample-rate 44100
      frame-count (* sample-rate duration)
      buffer (.createBuffer context 1 frame-count sample-rate)
      data (.getChannelData buffer 0)
      wav (scale (map decimal (map #(apply str %)
                                   (partition 4 @file-atom)))
                 -1 1)]
 (doseq [i (range 10000)]
      (aset data i (nth wav i)))
(buffer-source buffer)
  )
  

  (let [context *context*
        input (scale (map decimal (map #(apply str %)
                                       (partition 4 @file-atom)))
                     -1 1)
        sample-rate 44100
        frame-count (count input)
        buffer (.createBuffer context 1 frame-count sample-rate)
        data (.getChannelData buffer 0)]
    (doseq [i (range frame-count)]
      (aset data i (nth input i)))
   (buffer-source buffer))
  )



#_(defn audio-buffer [context duration]
  (let [sample-rate 44100
        frame-count (* sample-rate duration)
        buffer (.createBuffer context 1 frame-count sample-rate)
        data (.getChannelData buffer 0)]
    (doseq [i (range frame-count)]
      (aset data i (-> (js/Math.random) (* 2.0) (- 1.0))))
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

(-> (js/Math.random) (* 2.0) (- 1.0))

(defn square-root
  [x]
  (.sqrt js/Math x))


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
                                                .toUpperCase
                                                )))))}])

(defn offsets
  ([file n] (offsets file n (inc n)))
  ([file from to]
   (map #(apply str %)
        (partition 2 (take (- (* 2 to) (* 2 from))
                           (drop (* 2 from) file))))))

(defn ascii [bytes]
  (apply str (map #(js/String.fromCharCode (str "0x" %)) bytes)))


(js/parseInt (str "0x" (apply str (reverse "0100"))))
(decimal "0100")

(decimal (offsets @file-atom 24 28))

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
     :stroke-width 0.5}])

(defn bars [items]
  (let [bar-width (/ 500 (count items))]
    (into [:g]
          (for [bar (range (count items))]
            (svg-bar bar-width
                     2
                     (* bar bar-width)
                     (- 50 (* 2 (nth items bar)))
                      "magenta")))))

(bars '(-1 -1 -1 1 1 1))

(defn make-path-data [x y]
  (str "L" x " " y))

(defn button [label onclick]
  [:button
   {:on-click onclick}
   label])



(defn make-path [points]
  (str "M" (apply str (interpose " " (for [x (range (count points))]
                                       (str x " " (nth points x)))))))

(comment
  
  )



(defn app []
  [:div#app
   [file-upload]
   [:div [button "Play"
        (fn [] (buffer-source (audio-buffer 
                               (scale (map decimal (map #(apply str %)
                                                                     (partition 4 @file-atom)))
                                                   -1 1)
                                            *context*
                               )))]]
   [:p]
   [:svg {:width    "100%"
          :view-box (str "0 0 500 150")}
    [:path {:d            (let [data (let [data  (scale (take 10000 (drop 44 (map decimal (map #(apply str %)
                                                                                             (partition 4 @file-atom)))))
                                                        -1 1)
                                           parts (.floor js/Math (/ (count data) 500))]
                                       (map first (partition parts data)))]
                            (make-path (scale data 0 150) ))
            :stroke       "black"
            :stroke-width 0.5
            :fill         "none"}]]
   [:h3 "Header:"]
   [:textarea
    {:rows      6
     :cols      50
     :value     (str (header @file-atom))
     :read-only true}]
   [:h3 "Data (hex):"]
   [:textarea
    {:rows      15
     :cols      110
     :value     (str (map #(apply str %)(partition 4 @file-atom)))
     :read-only true}]
   [:h3 "Data (decimal):"]
[:textarea
 {:rows      15
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
