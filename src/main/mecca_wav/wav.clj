(ns porkostomus.wav
  (:require [clojure.java.io :as io])
  (:import [java.nio ByteBuffer]
           [java.util.concurrent LinkedBlockingQueue]
           [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat
            AudioFormat$Encoding
            AudioInputStream
            AudioSystem]))

(defn- to-double-arrays
  "Return a seq of arrays of doubles that decode the values in buf."
  [^bytes buf ^long bytes-read ^long bytes-per-sample ^long chans]
  (let [samples-read (/ bytes-read bytes-per-sample chans)
        bb           (ByteBuffer/allocate bytes-read)
        arrs         (repeatedly chans #(double-array samples-read))]
    (.put bb buf 0 bytes-read)
    (.position bb 0)
    (dotimes [n samples-read]
      (doseq [arr arrs]
        ;; TODO: We're hardcoded to .getShort here, but the
        ;; bytes-per-sample is a parameter. Should probably have
        ;; something that knows how to read from a ByteBuffer given a
        ;; number of bits.
        (aset arr n (/ (double (.getShort bb)) 32768.0))))
    arrs))

(defn- sample-chunks
  "Return a seq of chunks from an AudioInputStream."
  [^AudioInputStream ais ^long chans ^long bytes-per-sample ^long chunk-size]
  (let [buf (byte-array (* chunk-size chans bytes-per-sample))
        bytes-read (.read ais buf)]
    (when (pos? bytes-read)
      (lazy-seq
       (cons (to-double-arrays buf (long bytes-read) bytes-per-sample chans)
             (sample-chunks ais chans bytes-per-sample chunk-size))))))

(comment
   (let [file                 (io/file "sine.wav")
         base-file-format     (-> file  AudioSystem/getAudioFileFormat .getFormat)
         base-file-properties (.properties base-file-format)
         chans                (.getChannels base-file-format)
         file-sample-rate     (.getSampleRate base-file-format)
         file-encoding        (.getEncoding base-file-format)
         bits-per-sample  16
         bytes-per-sample (-> bits-per-sample (/ 8) long)]
  (sample-chunks (AudioSystem/getAudioInputStream file)
                   chans bytes-per-sample 10000))
  )

(defn wav-bytes [file]
  (with-open [in  (io/input-stream file)
              out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn offsets [file from to]
  (take (- to from) (drop from (wav-bytes file))))

(defn ascii [bytes]
  (apply str (map char bytes)))

(defn decimal [bytes]
  (Integer/decode (str "0x" (apply str (map #(format "%02x" %) (reverse bytes))))))

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

(defn write-wav [file bytes]
  (with-open [out (io/output-stream (io/file file))]
    (.write out bytes)))

(comment
  (header "resources/test.wav")
  (header "resources/saw.wav")
  (header "resources/nes-tri.wav")
  )
