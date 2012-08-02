(ns sql.compiler
  (:use [clojure.string :only [join upper-case] :as string]
        [clojure.core.incubator :only [-?> -?>>]])
  (:import (clojure.lang Keyword Seqable)))

(def ^:dynamic *dialect* :sql)
(def emit-hierarchy (atom (make-hierarchy)))

(defmulti emit (fn [this] [*dialect* (type this)])
  :hierarchy emit-hierarchy)

(defmethod emit [:sql nil]     [_]    nil)
(defmethod emit [:sql Object]  [this] (str this))
(defmethod emit [:sql String]  [this] this)
(defmethod emit [:sql Keyword] [this] (-> this name upper-case))

(defmethod emit [:sql Seqable]
  [coll]
  (str "(" (join ", " (map emit coll)) ")"))


(defmacro defop
  ([name fields body]
     `(defop ~name ~fields [{:keys ~fields}] ~body))
  ([name fields params body]
     `(defop ~name :sql ~fields ~params ~body))
  ([name dialect fields params body]
     `(do
        (defrecord ~name ~fields)
        (defmethod emit [~dialect ~name] ~params ~body))))

(defmacro op [name & kvs]
  (let [factory (symbol (str name "/create"))
        m (apply hash-map kvs)]
    `(~factory ~m)))

(defmacro definfix [name op]
  `(defop ~name [~'x ~'y]
     (str (emit ~'x) ~(str " " op " ") (emit ~'y))))

(defmacro definfixes [& names+ops]
  (->> (for [[name op] (partition 2 names+ops)]
         `(definfix ~name ~op))
       (cons `do)))

;; TODO: parens
(definfixes
  Plus "+"     Minus "-"
  Multiply "*" Divide "/"

  Gt ">"    Gte ">="
  Lt "<"    Lte "<="

  Or "OR" And "AND" In "IN" Like "LIKE")

(defn emit-equality [sign null-check x y]
  (let [[x y] (map emit [x y])]
    (if (and x y)
      (join " " [x sign y])
      (join " " [(or x y "NULL") null-check "NULL"]))))

(defop Equal [x y]
  (emit-equality "=" "IS" x y))

(defop NotEqual [x y]
  (emit-equality "<>" "IS NOT" x y))

(defop As [column alias]
  (str (emit column) " AS " alias))

(defop Modify [modifiers]
  (-?>> modifiers
        (map emit)
        (join " ")))

;; A list of rows that can be used like a table. The column list of
;; the resulting table is C1, C2, and so on.
;; Example: SELECT * FROM (VALUES(1, 'Hello'), (2, 'World')) AS V;
(defop Values [keys+vals]
  (-?>> (seq keys+vals)
        (map emit)
        (join ", ")
        (str "VALUES ")))

;; Joins a table. The join expression is not supported for cross and
;; natural joins. A natural join is an inner join, where the condition
;; is automatically on the columns with the same name.
;; Example: TEST AS T LEFT JOIN TEST AS T1 ON T.ID = T1.ID
(defop TableExpression [table join]
  (->> [table join]
       (map emit)
       (remove nil?)
       (string/join " ")))

(defop Join [type table on]
  (let [type (if (#{:left :right :full :inner :cross :natural} type)
               (-> type name upper-case)
               type)
        on (-?>> on emit (str "ON "))]
    (->> [type "JOIN" (emit table) on]
         (remove nil?)
         (join " "))))

(defn add-parens [s]
  (str "(" s ")"))

(defn modify-if [cond f x]
  (if cond (f x) x))

;; Selects data from a table or multiple tables. GROUP BY groups the
;; result by the given expression(s). HAVING filter rows after
;; grouping. ORDER BY sorts the result by the given column(s) or
;; expression(s). UNION combines the result of this query with the
;; results of another query.  LIMIT limits the number of rows returned
;; by the query (no limit if null or smaller than zero). OFFSET
;; specified how many rows to skip.
(defop Select
  [modifiers project from where group-by having combinations order-by limit offset
   top-level]
  (->> (for [[keyword expr] {"FROM" from
                             "WHERE" where
                             "GROUP BY" group-by
                             "HAVING" having
                             ;; TODO: union etc
                             "ORDER BY" order-by
                             "LIMIT" limit
                             "OFFSET" offset}]
         (-?>> expr emit (str keyword " ")))
       (concat (map emit ["SELECT" modifiers project]))
       (remove nil?)
       (join " ")
       (modify-if (not top-level) add-parens)))

(defop MultipleValues [values]
  (join ", " (map emit values)))

;; Sorts the result by the given column number, or by an expression. If
;; the expression is a single parameter, then the value is interpreted as
;; a column number. Negative column numbers reverse the sort order.
;; Example: NAME DESC NULLS LAST
(defop Order [expr order nulls]
  (->> [(emit expr)
        (-?> order name upper-case)
        (-?>> nulls name upper-case (str "NULLS "))]
       (remove nil?)
       (join " ")))

(defop Aggregate [function field]
  (str (emit function) "(" (emit field) ")"))
