(ns crisis-graph.core
  (:use [clojure.walk :as walk]
        [dali.core]
        [dali.style]
        [dali.math]
        [dali.backend]
        [dali.backend.java-2d])
  (:require [dali.dev :as dev])
  (:import [java.awt.image BufferedImage]
           [javax.swing SwingUtilities UIManager JFrame ImageIcon]
           [javax.imageio ImageIO]
           [java.io File]))

(defn do-swing*
  "Runs thunk in the Swing event thread according to schedule:
    - :later => schedule the execution and return immediately
    - :now   => wait until the execution completes."
  [schedule thunk]
  (cond
   (= schedule :later) (SwingUtilities/invokeLater thunk)
   (= schedule :now) (if (SwingUtilities/isEventDispatchThread)
                       (thunk)
                       (SwingUtilities/invokeAndWait thunk)))
  nil)
(defmacro do-swing
  "Executes body in the Swing event thread asynchronously. Returns
  immediately after scheduling the execution."
  [& body]
  `(do-swing* :later (fn do-swing-fn [] ~@body)))

(def error-icon (get-laf-property "OptionPane.errorIcon"))

(defn- error-image [msg]
  (let [i (BufferedImage. 600 300 BufferedImage/TYPE_INT_RGB)
        g (.getGraphics i)
        icon-width (.getIconWidth error-icon)]
    (.paintIcon error-icon nil g 0 0)
    (.drawString g msg (+ 10 icon-width) 20)
    i))

(defn watch-image2
  "Shows the passed java.awt.Image in a frame, and re-paints at 15
  FPS (or the specified FPS). You can also pass a reference to an
  Image, which will be dereferenced at every frame, or an
  image-returning function, which will be called at every frame.  The
  function returns a future which can be cancelled to stop the
  re-painting. Of course the re-painting stops automatically when the
  frame is closed."
  ([image] (watch-image2 image 15))
  ([image fps]
     (let [current-frame (atom 0)
           get-image
           (fn get-image-fn
             [] (cond (instance? clojure.lang.IDeref image) @image
                      (fn? image)
                      (try (do
                             (swap! current-frame inc)
                             (image @current-frame))
                           (catch Exception e
                             (do
                               (.printStackTrace e)
                               (error-image (str (.getName (class e))
                                                 ", check your console")))))
                      :otherwise image))
           cached-image (ref nil)
           panel (proxy [javax.swing.JPanel] []
                   (paintComponent [g]
                                   (dosync (ref-set cached-image (get-image)))
                                   (if @cached-image
                                         (.drawImage g @cached-image 0 0 this)))
                   (getPreferredSize[] (if @cached-image
                                         (java.awt.Dimension.
                                          (.getWidth @cached-image)
                                          (.getHeight @cached-image))
                                         (java.awt.Dimension. 100 100))))
           updater (future
                    (while true
                      (Thread/sleep (/ 1000 fps))
                      (do-swing (.repaint panel))))]
       (doto (JFrame.)
         (.add panel)
         (.pack)
         (.setVisible true)
         (.addWindowListener
          (reify
            java.awt.event.WindowListener
            (windowClosing [listener event]
              (future-cancel updater))
            (windowIconified [_ _])
            (windowClosed [_ _])
            (windowDeiconified [_ _])
            (windowActivated [_ _])
            (windowOpened [_ _])
            (windowDeactivated [_ _]))))
       updater)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn interpolated-line [start end d]
  (line {:stroke {:width 2}} start (interpolate start end d)))

(defn segment-fn
  ([{:keys [start end keep-around? content]}]
     (fn segment-gen [bk frame]
       (if (and (>= frame start) (<= frame end))
         (content bk (- frame start))
         (if (and keep-around? (> frame end))
           (content bk (- end start)))))))

(defn segment
  ([start duration content]
     (segment start duration false content))
  ([start duration keep-around? content]
     {:type :segment
      :start start
      :end (+ start duration)
      :keep-around? keep-around?
      :content content}))

(defn pause [duration]
  (segment 0 duration (fn [bk frame])))

(defn offset-segment [{:keys [bk start end keep-around? content]} offset]
  (segment (+ offset start)
           (- end start)
           keep-around?
           content))

(defn sequence-fn [{:keys [start end content]}]
  (fn sequence-gen [bk frame]
    (doseq [content-fn content] (content-fn bk frame))))

(defn sequence [& segments]
  (let [content
        (reduce (fn [c segment]
                  (conj c (offset-segment segment (inc (:end (last c))))))
                [(first segments)] (rest segments))]
    {:type :sequence
     :start (:start (first content))
     :end (:end (last content))
     :content content}))

(def tracks-fn sequence-fn)

(defn tracks [& tracks]
  {:type :tracks
   :start (apply min (map :start tracks))
   :end (apply max (map :end tracks))
   :content tracks})

(defn forever-fn [{:keys [start end content]}]
  (fn forever-gen [bk frame]
    (content bk (mod frame (- end start)))))

(defn forever [content]
  {:type :forever
   :start (:start content)
   :end (:end content)
   :content content})

(defn animation-fn [anim]
  (walk/postwalk
   (fn [x]
     (if (map? x)       
       (condp = (:type x)
         :forever (forever-fn x)
         :sequence (sequence-fn x)
         :tracks (tracks-fn x)
         :segment (segment-fn x)
         :else x)
       x))
   anim))


(def ease-linear identity)

(defn line-rectangle-intersect? [line rectangle]
  (not (empty? (line-rectangle-intersection line rectangle))))

;;patch into dali.core
(in-ns 'dali.core)
(defn line-intersection
  "The point at which the passed lines intersect. nil if they don't."
  [{{[line1-x1 line1-y1] :start [line1-x2 line1-y2] :end} :geometry, :as line1}
   {{[line2-x1 line2-y1] :start [line2-x2 line2-y2] :end} :geometry, :as line2}]
  (let [[x y :as point] (line-projection-intersection line1 line2)]
    (when (and (within y [line1-y1 line1-y2] 2)
               (within y [line2-y1 line2-y2] 2)
               (within x [line1-x1 line1-x2] 2)
               (within x [line2-x1 line2-x2] 2))
      point)))
(in-ns 'crisis-graph.core)


;;;;;;;;;;;;;;;

(def img (atom (buffered-image [480 270])))
(def bk (image-backend @img))
(.setRenderingHint (.graphics bk)
                   java.awt.RenderingHints/KEY_ANTIALIASING
                   java.awt.RenderingHints/VALUE_ANTIALIAS_ON)
(def fps 23.975)
(def data [[20 10]
           [21 5]
           [22 12]
           [23 15]
           [24 1]
           [25 2]
           [26 5]])

(defn line-anim-fn [duration start end]
  (fn [bk f]
   (render bk (interpolated-line start end (/ f duration)))))

(defn polyline-anim [segment-duration points]
  (apply
   sequence
   (map
    (fn [[start end]]
      (segment
       0 segment-duration :k
       (line-anim-fn segment-duration start end)))
    (partition 2 1 points))))

(defn dot-grow-fn [position radius duration]
  (fn [bk f]
    (fill bk (circle position (min radius (* (/ f duration) radius))))))

(defn dots-track [radius segment-duration duration points]
  (apply tracks
         (map #(segment (* %1 segment-duration) duration :keep
                        (dot-grow-fn %2 radius duration)) (iterate inc 0) points)))

#_(-> (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment)
      (.getAvailableFontFamilyNames)
      (seq)
      (pprint))

(defn text-fade-in-fn [point other-points label duration]
  (fn [bk f]
    (let [padding 7
          style {:fill (color 255 255 255 (int (* (/ f duration) 255)))
                 :font {:family "Futura"
                        :size 14}}
          lines (map #(line point %) other-points)
          test-label (text style point label)
          b (text-bounds bk test-label)
          [w h] (get-in b [:geometry :dimensions])

          labels
          (map (fn make-label [trans] (text style (translate point trans) label))
               [[(- (/ w 2)) (- (* 1.5 padding))]
                [(- (/ w 2)) (+ (* 0.7 padding) h)]
                [padding (- padding)]
                [(- (- w) padding) (- padding)]
                [padding (+ padding h)]
                [(- (- w) padding) (+ padding h)]])

          boundz (map #(text-bounds bk %) labels)
          boundz-map (zipmap boundz labels)

          good-label
          (get boundz-map
               (second
                (first
                 (filter #(not (first %))
                         (map (fn [b]
                                [(some #(line-rectangle-intersect? % b) lines) b])
                              boundz)))))]
      (if good-label
        (render bk good-label)
        (render bk (first labels)))
      ;(draw bk b)
      ;(doseq [b boundz] (draw bk b))
      )))

(defn partition-points [coll]
  (concat [[(second coll)]]
          (map #(vector (first %) (last %)) (partition 3 1 coll))
          [[(last (butlast coll))]]))

(defn text-track [segment-duration duration points labels]
  (apply tracks
         (map #(segment (* %1 segment-duration) duration :keep
                        (text-fade-in-fn %2 %3 %4 duration))
              (iterate inc 0)
              points
              (partition-points points)
              labels)))

(defn point-graph-animation []
  (let [segment-time 1
        segment-frames (* segment-time fps)
        dot-radius 4
        points [[50 150] [100 170] [150 140] [200 180] [250 220] [300 175] [350 170] [400 210]]
        labels (rest (map (comp str second) points))]
    (forever
     (tracks
      (polyline-anim segment-frames points)
      (dots-track 4 segment-frames fps (drop 1 points))
      (text-track segment-frames fps (drop 1 points) labels)))))

(defn make-frame [f]
  (let [[iw ih] [(.getWidth @img) (.getHeight @img)]
        anim (animation-fn (point-graph-animation))]
    (doto bk
      (set-paint (color 0 0 0))
      (fill (rectangle [0 0] [iw ih]))
      (set-paint (color 255 255 255))
      (render (text {:fill (color 255 255 255 128)}
                    [50 25] "Testing the crisis animations")))
    (anim bk f)
    @img))

#_(time
   (render-to-files "/Users/sideris/devel/crisis-graph/data/" "frame"
                    [1920 1080]
                    [:scale 4]
                    (point-graph-animation)))

(defn render-to-files [path prefix dimensions transform anim]
  (let [start (:start anim)
        end (int (inc (:end anim)))
        fun (animation-fn anim)]
    (doseq [f (range start end)]
      (let [img (buffered-image dimensions :int-argb)
            bk (image-backend img)
            format-str (str "%0" (count (str end)) "d")
            filename (str path prefix (format format-str (int f)) ".png")]
        (.setRenderingHint (.graphics bk)
                           java.awt.RenderingHints/KEY_ANTIALIASING
                           java.awt.RenderingHints/VALUE_ANTIALIAS_ON)
        (with-transform
            bk
            transform
            (fun bk f))
        (ImageIO/write img "png" (File. filename))))))

#_(watch-image2 (fn [f] (make-frame f)) 23.975)


;; http://www3.weforum.org/docs/WEF_GlobalCompetitivenessReport_2012-13.pdf
;; selides 13 kai 384
