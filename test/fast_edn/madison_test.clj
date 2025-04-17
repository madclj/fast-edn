(ns fast-edn.madison-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [clojure.test.check :refer [quick-check]]
   [clojure.test.check.generators :as gen]
   [com.gfredericks.test.chuck.generators :as gen']
   [com.gfredericks.test.chuck.properties :as prop']
   [clojure.edn :as cedn]
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

(defn poison [x n]
  (let [s (pr-str x)
        n (mod n (count s))]
    (str (subs s 0 n)
         (subs s (inc n) (count s)))))

(comment
  (poison 'asdf 0)
  (poison 'asdf 1)
  (poison 'asdf 100)
  (poison 'asdf 101)
  )

(defonce BLOWN-UP (Object.))

(deftest parity2-test
  (let [p (prop'/for-all [x gen/any
                          n gen/nat]
                         (let [s (poison x n)
                               left (try
                                      (massage (cedn/read-string s))
                                      (catch Exception _ BLOWN-UP))
                               right (try
                                       (massage (edn/read-string s))
                                       (catch Exception _ BLOWN-UP))]
                           (= left right)))]
    ; :seed 1744248800430
    (quick-check 100 p
                 ;:seed 1744249980402
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
  (edn/read-string "[)]") ;; done
  (edn/read-string "{)}") ;; done
  (edn/read-string "{) )}") ;; done
  (edn/read-string "{) 1}") ;; done
  (edn/read-string "{) 1 ) 2}") ;;done
  (edn/read-string "{) 2 ) 1}") ;; done
  (edn/read-string "{1 ) ) 2 ] 3 } 4}") ;;done
  (edn/read-string "{1 ) ) 2 ] 3 ] 4}") ;;done
  (edn/read-string "{1 )}") ;;done
  (edn/read-string "{2 ) 1 )}") ;done
  )
