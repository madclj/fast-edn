(ns fast-edn.madison-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [clojure.test.check :refer [quick-check]]
   [clojure.test.check.generators :as gen]
   [com.gfredericks.test.chuck.generators :as gen']
   [com.gfredericks.test.chuck.properties :as prop']
   [clojure.edn :as cedn]
   [clojure.string :as str]
   [fast-edn.core :as edn]
   [clojure.walk :as walk]))

(defonce NAN (random-uuid))

(defn massage [x]
  (walk/postwalk (fn [v]
                   (if (and (number? v) (Double/isNaN v))
                     NAN
                     v))
                 x))

(deftest parity-test
  (let [p (prop'/for-all [x gen/any]
                         (let [s (pr-str x)
                               left (massage (cedn/read-string s))
                               right (massage (edn/read-string s))]
                           (= left right)))]
    ; :seed 1744248800430
    (quick-check 10000 p))
  )

(comment
  (pr-str #inst "2025-04-10T01:42:36.974-00:00")
  (edn/read-string (pr-str '{0 [[-]]}))
  (cedn/read-string (pr-str '{0 [[-]]}))
  (edn/read-string "{0 [[-]]}")
  (edn/read-string "{0 -}")
  (edn/read-string "{- -}")
  (edn/read-string "-")
  (edn/read-string "[-]")
  (edn/read-string (pr-str [##NaN]))
  (edn/read-string "(-)")
  (clojure.edn/read-string "{0 [[-]]}")
  ;=> {0 [[-]]}

  (cedn/read-string "#{{0 +}}")
  (edn/read-string "#{{0 +}}")
  )

(defn poison [x n replace-with]
  (let [s (pr-str x)
        n (mod n (count s))]
    (if (or ;(str/includes? s "/") ;; <- HUGE hammer
            ;(str/includes? s "@")
            ;(str/includes? s "`")
            (str/includes? s "^")
            )
      s
      (case replace-with
        :delete (if (or (and (< (inc n) (count s))
                             (or
                               ;; don't trigger [075] bug
                               (= \. (nth s (inc n)))
                               ;; don't trigger ://a bug
                               (= \\ (nth s (inc n)))
                               ;; don't trigger [1#{}] bug
                               (= \# (nth s (inc n)))
                               ))
                        (and (<= 0 (dec n))
                             (or
                               ;; don't trigger :A/0
                               (= \/ (nth s (dec n)))
                               ;; don't trigger #:00 {}}
                               (= \: (nth s (dec n)))
                               ;; don't trigger 075
                               (= \0 (nth s (dec n)))
                               ))
                        (or
                          ;; don't trigger -/10
                          (<= (int \0) (int (nth s n)) (int \9))
                          ;; don't trigger 075
                          (= \. (nth s n))
                          ))
                  s
                  (str (subs s 0 n)
                       (subs s (inc n) (count s))))
        (:space :comma :newline) (if (or ;; don't trigger #:,A{} bug
                                         (str/includes? s "#:")
                                         ;; don't trigger 1,00, or octal related bugs
                                         (= \. (nth s n))
                                         (and (< (inc n) (count s))
                                              ;; don't trigger 1,00, or octal related bugs
                                              (= \0 (nth s (inc n)))))
                                   s
                                   (str (subs s 0 n)
                                        (case replace-with
                                          :space " "
                                          :comma ","
                                          :newline "\n")
                                        (subs s (inc n) (count s))))))))

(comment
  (poison 'asdf 0 :delete)
  (poison 'asdf 1 :delete)
  (poison 'asdf 100 :delete)
  (poison 'asdf 101 :delete)
  )

(defn BLOWN-UP [])
(defn BAD-ERROR [])

(deftest parity2-test
  (let [p (prop'/for-all [x
                          (gen/one-of
                            [#_gen/symbol-ns
                             #_gen/keyword-ns
                             #_gen/int
                             #_gen/char-ascii
                             (gen/fmap
                                 (fn [v]
                                   (-> v
                                       (str/replace "^" "a")
                                       (str/replace ";" "a")
                                       (str/replace "#" "a")
                                       (str/replace "\"" "a")
                                       ))
                                 gen/string-ascii)
                             #_(gen/fmap
                                 ;;remove known problematic values
                                 (fn [v]
                                   (walk/postwalk (fn [v]
                                                    (when-not (or (and (number? v) (not (< -1000000 v 1000000)))
                                                                  #_(char? v)
                                                                  #_(set? v)
                                                                  #_(uuid? v)
                                                                  #_(and (ident? v)
                                                                         (some #(when %
                                                                                  (or (str/includes? % "\\")
                                                                                      (str/includes? % ":")))
                                                                               ((juxt name namespace) v))))
                                                      v))
                                                  v))
                                 gen/any-printable)])
                          n (gen/return 0) #_gen/nat
                          replace-with (gen/elements [:delete :comma :space])
                          s (gen/return (let [s (poison x n replace-with)]
                                          (if (seq s)
                                            (subs s 0 (dec (count s)))
                                            s)))
                          fast-edn (gen/return
                                     (try
                                       (massage (fast-edn.core/read-string s))
                                       (catch ArrayIndexOutOfBoundsException _ BAD-ERROR)
                                       (catch Exception _ BLOWN-UP)))
                          clojure-edn (gen/return
                                        (try
                                          (massage (clojure.edn/read-string s))
                                          (catch Exception _ BLOWN-UP)))]
                         (or (= BLOWN-UP clojure-edn)
                             (= clojure-edn fast-edn)))]
    ; :seed 1744248800430
    (-> (quick-check 100000000 p
                     ;:seed 1744249980402
                     )
        :shrunk
        :smallest
        first
        ))
  )

(deftest parse-substring-parity
  (let [p (prop'/for-all [s (gen/fmap
                              (fn [v]
                                (let [s (-> v
                                            (str/replace "^" "")
                                            (str/replace ";" "")
                                            (str/replace "#" " #")
                                            (str/replace "\"" " \"")
                                            (str/replace "//" "")
                                            (str/replace ":/" "")
                                            (str/replace "r0" "r1")
                                            (str/replace "R0" "R1")
                                            )]
                                  (str "[" s " " s "]")))
                              (gen/fmap clojure.string/join (gen/vector gen/char-ascii 3 7)))
                          fast-edn (gen/return
                                     (try
                                       (fast-edn.core/read-string s)
                                       (catch Exception _ BLOWN-UP)))
                          clojure-edn (gen/return
                                        (try
                                          (clojure.edn/read-string s)
                                          (catch Exception _ BLOWN-UP)))]
                         (do
                           (assert (not (Thread/interrupted)))
                           (or (not (= BLOWN-UP fast-edn))
                               (= BLOWN-UP clojure-edn))))]
    (-> (quick-check 100000000 p)
        :shrunk
        :smallest
        first
        ))
  )

(comment
  (cedn/read-string (poison [[] #{{[] :A0*6} #{[]}}] 19))
  "[[] #{{[] :A0*6} #{]}}]"

  ; THIS
  "[[] #{{[] :A0*6} #{]}}]"
  (edn/read-string (poison [[] #{{[] :A0*6} #{[]}}] 19))
  ;[[]
  ; #{{[] :A0*6}
  ;   #{#object[fast_edn.EdnParser$UnexpectedCharacter 0x5cc15140 "fast_edn.EdnParser$UnexpectedCharacter@5cc15140"]}}]

  
  (edn/read-string "[[] #{{[] :A0*6} #{]}}]")

  (edn/read-string "#{]}")
  (edn/read-string "#{)}")
  (edn/read-string "#{)}")

  (edn/read-string "[#{]]")

  (poison '[()] 1)
  (edn/read-string "(1])") ;; done
  ;; TODO write a property that tests 
  ;; Joel Martin (instacheck) conaka
  ;; idea: generate ..see #clojure-madison April 16th
  (edn/read-string "(1')")
  (edn/read-string "(1:)")
  (edn/read-string "(1/)") ;;FIXME low severity, bad error message but suspicious...
  (edn/read-string "1/") ;;FIXME  same ^^
  (clojure.edn/read-string "(1/)")
  (clojure.edn/read-string "1/")
  (edn/read-string "10r1/10")
  (clojure.edn/read-string "10r1/10")
  (clojure.edn/read-string "(1])") ;; TODO <-- use this msg
  (edn/read-string "[)]") ;; done
  (edn/read-string "#{)}") ;; done
  (edn/read-string "{)}") ;; TODO
  (edn/read-string "{) )}") ;; done
  (edn/read-string "{) )}") ;; done
  (edn/read-string "{) 1}") ;; done
  (edn/read-string "{) 1 ) 2}") ;;done
  (edn/read-string "{) 2 ) 1}") ;; done
  (edn/read-string "{1 ) ) 2 ] 3 ] 4}") ;;done
  (edn/read-string "{1 )}") ;;done
  (edn/read-string "{2 ) 1 )}") ;done

  ;; TODO symbols starting with numbers should be invalid
  ;; keywords starting with number are a documented diff, but same argument not
  ;; as compelling for symbols.
  (edn/read-string "A000/A0")
  (edn/read-string "A000/0")
  (clojure.edn/read-string "A000/A0")
  (clojure.edn/read-string "A000/0")
  (poison '#{A000/A0} 7)
  (clojure.edn/read-string "#{A000/0}")
  (edn/read-string "#{A000/0}")
  (do :1a)
  (clojure.edn/read-string ":1a")

  ;TODO https://github.com/tonsky/fast-edn/issues/7
  (clojure.edn/read-string "[0#uuid \"9bd519ea-1e9e-4ebb-a80d-6f1955d0c5c3\"]")
  (edn/read-string "[0#uuid \"9bd519ea-1e9e-4ebb-a80d-6f1955d0c5c3\"]")


  (poison [0
           #uuid "9bd519ea-1e9e-4ebb-a80d-6f1955d0c5c3"
           :A000
           true
           \ 
           -100
           :A0000000000000/A]
          87)
  ;; TODO https://github.com/tonsky/fast-edn/issues/8
  (fast-edn.core/read-string
    "\\ormfeed")
  (fast-edn.core/read-string "#{\\space-1}")
  (clojure.edn/read-string "#{\\space-1}")
  (fast-edn.core/read-string "\\space-1")
  (clojure.edn/read-string "\\space-1")

  ;; https://github.com/tonsky/fast-edn/issues/9
  (fast-edn.core/read-string "A0:")
  (fast-edn.core/read-string "A0::a")
  (fast-edn.core/read-string "::A0:a")
  (fast-edn.core/read-string ":::::A0:a")
  (fast-edn.core/read-string "A:::")
  (fast-edn.core/read-string "::::A:::")

  (clojure.edn/read-string "A0:")
  (clojure.edn/read-string ":A0:")
  (clojure.edn/read-string ":A0::a")
  (clojure.edn/read-string "::A0:a")

  (clojure.edn/read-string ":A0:a")
  (read-string "A0:")
  (read-string ":A0:")

  ;; https://github.com/tonsky/fast-edn/issues/10
  (fast-edn.core/read-string "#{##Inf-1}")
  (clojure.edn/read-string "#{##Inf-1}")

  ;; https://github.com/tonsky/fast-edn/issues/11
  (fast-edn.core/read-string "[075]")
  (clojure.edn/read-string "[075]")

  (fast-edn.core/read-string "#{\\o-1/2}")
  (clojure.edn/read-string "#{\\o-1/2}")

  ;; https://github.com/tonsky/fast-edn/issues/12
  (fast-edn.core/read-string "")
  (clojure.edn/read-string "")
  (fast-edn.core/read-string (str (char 28)))
  (clojure.edn/read-string (str (char 28)))
(fast-edn.core/read-string (str "[" (char 11) "]"))
(clojure.edn/read-string (str "[" (char 11) "]"))

;; https://github.com/tonsky/fast-edn/issues/13
(fast-edn.core/read-string "#{0-1}")
(clojure.edn/read-string "#{0-1}")
(fast-edn.core/read-string "0-1")
(clojure.edn/read-string "0-1")
(fast-edn.core/read-string "0+1")
(clojure.edn/read-string "0+1")

;; https://github.com/tonsky/fast-edn/issues/14
(fast-edn.core/read-string "#{100000000000000000000}")
(clojure.edn/read-string "#{100000000000000000000}")
(fast-edn.core/read-string "#{13/10 100000000000000000000}")
(clojure.edn/read-string "#{13/10 100000000000000000000}")
(fast-edn.core/read-string "[{-1000000000000000000000000000000000000000000000000000000000000000000000000000 0}]")
(clojure.edn/read-string "[{-1000000000000000000000000000000000000000000000000000000000000000000000000000 0}]")
(fast-edn.core/read-string "[0 -1000000000000000000000000000000000000000000000000000000000000000000000000000 0]")
(clojure.edn/read-string "[0 -1000000000000000000000000000000000000000000000000000000000000000000000000000 0]")


;https://github.com/tonsky/fast-edn/issues/15
(fast-edn.core/read-string "#:0{:A nil}")
(clojure.edn/read-string "#:0{:A nil}")

;https://github.com/tonsky/fast-edn/issues/16
(fast-edn.core/read-string "[{#{`} #{}}]")
(clojure.edn/read-string "[{#{`} #{}}]")
(fast-edn.core/read-string "`")
(clojure.edn/read-string "`")
(fast-edn.core/read-string "~")
(clojure.edn/read-string "~")
(fast-edn.core/read-string "@")
(clojure.edn/read-string "@")
(fast-edn.core/read-string "~@")
(clojure.edn/read-string "~@")
(fast-edn.core/read-string "~@a")
(clojure.edn/read-string "~@")

;https://github.com/tonsky/fast-edn/issues/17
(fast-edn.core/read-string "://a")
(clojure.edn/read-string "://a")

;https://github.com/tonsky/fast-edn/issues/18
(fast-edn.core/read-string "#:,A{}")
(clojure.edn/read-string "#:,A{}")

;;https://github.com/tonsky/fast-edn/issues/19
(fast-edn.core/read-string "$;")
(clojure.edn/read-string "$;")

;; https://github.com/tonsky/fast-edn/issues/20
(fast-edn.core/read-string "$^")
(clojure.edn/read-string "$^")

;; https://github.com/tonsky/fast-edn/issues/21
(fast-edn.core/read-string "1000000000000000000000000,")
(clojure.edn/read-string "1000000000000000000000000,")

;; https://github.com/tonsky/fast-edn/issues/22
(fast-edn.core/read-string "010\"")

;;https://github.com/tonsky/fast-edn/issues/23
(fast-edn.core/read-string "\\ ^")
(clojure.edn/read-string "\\ ^")

;; https://github.com/tonsky/fast-edn/issues/24
(fast-edn.core/read-string ":/!/!")
(clojure.edn/read-string ":/!/!")

;; https://github.com/tonsky/fast-edn/issues/25
(fast-edn.core/read-string "10R08")
(clojure.edn/read-string "10R08")

;; https://github.com/tonsky/fast-edn/issues/26
(fast-edn.core/read-string "25RN")
(clojure.edn/read-string "25RN")

;;https://github.com/tonsky/fast-edn/issues/27
(fast-edn.core/read-string "08/1")
(clojure.edn/read-string "08/1")
  )
