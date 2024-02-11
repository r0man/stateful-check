(ns stateful-check.java-queue-test
  (:refer-clojure :exclude [peek pop count])
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer :all])
  (:import [java.util.concurrent ArrayBlockingQueue]))

(defprotocol Queue
  (push [this val])
  (peek [this])
  (pop [this])
  (count [this]))

(deftype ArrayQueue [buffer ^:volatile-mutable read-i ^:volatile-mutable write-i]
  Queue
  (push [this val]
    (aset buffer write-i val)
    (set! write-i (mod (inc write-i) (alength buffer)))
    this)
  (peek [this]
    (aget buffer read-i))
  (pop [this]
    (let [val (aget buffer read-i)]
      (set! read-i (mod (inc read-i) (alength buffer)))
      val))
  (count [this]
    (mod (- write-i read-i) (alength buffer))))

(def array (atom clojure.lang.PersistentQueue/EMPTY))

(deftype SharedArrayQueue [length]
  Queue
  (push [this val]
    (let [add (fn [q v]
                (let [result (conj q v)]
                  (if (> (clojure.core/count result) length)
                    (clojure.core/pop result)
                    result)))]
      (swap! array add val))
    this)
  (peek [this]
    (clojure.core/peek @array))
  (pop [this]
    (let [val (clojure.core/peek @array)]
      (swap! array clojure.core/pop)
      val))
  (count [this]
    (clojure.core/count @array)))

(defn new-shared-queue [n]
  (reset! array clojure.lang.PersistentQueue/EMPTY)
  (SharedArrayQueue. n))

(defn new-array-queue [n]
  (ArrayQueue. (int-array (inc n)) 0 0))

;;
;; Generative testing commands
;;

(def new-shared-queue-command
  {:args (fn [_] [gen/nat])
   :precondition (fn [_ [size]] (pos? size))
   :command #'new-shared-queue
   :next-state (fn [state [size] queue]
                 (assoc state queue
                        {:elements []
                         :size size}))})

(def new-array-queue-command
  {:args (fn [_] [gen/nat])
   :precondition (fn [_ [size]] (pos? size))
   :command #'new-array-queue
   :next-state (fn [state [size] queue]
                 (assoc state queue
                        {:elements []
                         :size size}))})

(def push-queue-command
  {:requires (complement nil?)
   :args (fn [state]
           [(gen/elements (keys state))
            gen/nat])
   :precondition (fn [state [queue _]]
                   (let [{:keys [elements size]} (get state queue)]
                     (< (clojure.core/count elements) size)))
   :command #'push
   :next-state (fn [state [queue val] _]
                 (update-in state [queue :elements] conj val))})

(def peek-queue-command
  {:requires (complement nil?)
   :args (fn [state]
           [(gen/elements (keys state))])
   :precondition (fn [state [queue]]
                   (seq (get-in state [queue :elements])))
   :command #'peek
   :postcondition (fn [state _ [queue] val]
                    (= val (first (get-in state [queue :elements]))))})

(def pop-queue-command
  {:requires (complement nil?)
   :args (fn [state]
           [(gen/elements (keys state))])
   :precondition (fn [state [queue]]
                   (seq (get-in state [queue :elements])))
   :command #'pop
   :next-state (fn [state [queue] _]
                 (update-in state [queue :elements] (comp vec next)))
   :postcondition (fn [state _ [queue] val]
                    (= val (first (get-in state [queue :elements]))))})

(def count-queue-command
  {:requires (complement nil?)
   :args (fn [state]
           [(gen/elements (keys state))])
   :command #'count
   :postcondition (fn [state _ [queue] val]
                    (= val (clojure.core/count (get-in state [queue :elements]))))})

;;
;; Generative testing specification
;;

(def shared-queue-specification
  {:commands {:new #'new-shared-queue-command
              :push #'push-queue-command
              :peek #'peek-queue-command
              :pop #'pop-queue-command
              :count #'count-queue-command}
   :setup #(reset! array clojure.lang.PersistentQueue/EMPTY)
   :generate-command (fn [state]
                       (if (nil? state)
                         (gen/return :new)
                         (gen/frequency [[1 (gen/return :new)]
                                         [5 (gen/return :push)]
                                         [5 (gen/return :peek)]
                                         [5 (gen/return :pop)]
                                         [5 (gen/return :count)]])))})

(def array-queue-specification
  {:commands {:new #'new-array-queue-command
              :push #'push-queue-command
              :peek #'peek-queue-command
              :pop #'pop-queue-command
              :count #'count-queue-command}
   :generate-command (fn [state]
                       (if (nil? state)
                         (gen/return :new)
                         (gen/frequency [[1 (gen/return :new)]
                                         [5 (gen/return :push)]
                                         [5 (gen/return :peek)]
                                         [5 (gen/return :pop)]
                                         [5 (gen/return :count)]])))})

(deftest shared-queue-test
  ;; Ensure this test always fails using a fixed seed. The pass
  ;; when :seed is set to 0.
  (is (not (specification-correct? shared-queue-specification {:run {:seed 1}}))))

(deftest array-queue-test
  (is (specification-correct? array-queue-specification)))
