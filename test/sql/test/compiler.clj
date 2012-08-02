(ns sql.test.compiler
  (:use [sql.compiler :as c] :reload)
  (:use midje.sweet)
  (:import (sql.compiler Select Equal NotEqual Values As TableExpression Join MultipleValues
                         Modify Order Plus Aggregate In And)))

(facts "creating ops"
  (Equal. 1 2) => #sql.compiler.Equal{:x 1, :y 2}
  (op Equal :x 1 :y 2) => #sql.compiler.Equal{:x 1, :y 2})


(tabular "emit basic objects"
  (fact (emit in) => out)
  in out

  nil      nil
  42       "42"
  "foo"    "foo"
  :sum     "SUM"
  [1 2 3]  "(1, 2, 3)")


(tabular "emit infix ops"
  (fact (emit (Plus. x y)) => out)
  x    y    out
  "a"  "b"  "a + b"
  1    2    "1 + 2")


(tabular "emit Equal and NotEqual"
  (fact (emit in) arrow out)
  in arrow out

  (Equal. 1 2) => "1 = 2"
  (NotEqual. (Plus. 1 2) 3) => "1 + 2 <> 3"
  (Equal. "a" nil) => "a IS NULL"
  (NotEqual. nil "a") => "a IS NOT NULL"
  (Equal. nil nil) => "NULL IS NULL")


(tabular "emit Values"
  (fact (emit in) arrow out)
  in arrow out

  (Values. {1 "hello" 2 "world"}) => "VALUES (1, hello), (2, world)"
  (Values. {1 "hello"}) => "VALUES (1, hello)"
  (Values. {}) => nil)


(tabular "emit TableExpression"
  (fact (emit in) => out)
  out in

  "users"
  (op TableExpression :table "users" :join nil)

  "users, orders AS o"
  (TableExpression. (MultipleValues. ["users" (As. "orders" "o")]) nil)

  "(SELECT * FROM orders)"
  (TableExpression. (op Select
                        :project "*"
                        :from (TableExpression. "orders" nil))
                    nil)

  "(SELECT * FROM orders) AS o"
  (TableExpression. (As. (op Select :project "*" :from "orders")
                         "o")
                    nil)

  "Person JOIN City ON Person.CityId = City.Id"
  (TableExpression. "Person" (op Join
                                 :table "City"
                                 :on (Equal. "Person.CityId" "City.Id"))))


(tabular "emit Join"
  (fact (emit in) => out)
  out in

  "CROSS JOIN users"
  (Join. :cross "users" nil)

  "JOIN users ON t1.id = t2.id"
  (Join. nil "users" (Equal. "t1.id" "t2.id"))

  "LEFT JOIN users ON id"
  (Join. :left "users" "id"))


(tabular "emit Order"
  (fact (emit in) => out)
  out in

  "id"
  (Order. "id" nil nil)

  "date DESC"
  (Order. "date" :desc nil)

  "1 ASC"
  (Order. 1 :asc nil)

  "NAME DESC NULLS LAST"
  (op Order, :expr "NAME", :order :desc, :nulls :last))


(tabular "emit Aggregate"
  (fact (emit in) => out)
  out in

  "SUM(price)"
  (op Aggregate, :function :sum, :field "price")

  "COUNT(*)"
  (Aggregate. :count "*"))


;;; ----------------------------------------------------------------------------

(tabular "Basic"
  (fact (emit in) => out)
  out in

  "SELECT * FROM users"
  (op Select
      :project "*"
      :from "users"
      :top-level true)

  "(SELECT id, name FROM users)"
  (op Select
      :project (MultipleValues. ["id" "name"])
      :from "users")

  "(SELECT id, name FROM users WHERE users.id = 1)"
  (op Select
      :project (MultipleValues. ["id" "name"])
      :from "users"
      :where (Equal. "users.id" 1))

  "(SELECT name FROM users WHERE users.id = 1 LIMIT 10)"
  (op Select
      :project (MultipleValues. ["name"])
      :from "users"
      :where (Equal. "users.id" 1)
      :limit 10))


(tabular "Aggregates"
  (fact (emit in) => out)
  out in

  "SELECT AVG(users.wage) FROM users"
  (op Select
      :project (Aggregate. :avg "users.wage")
      :from "users"
      :top-level true)

  "SELECT AVG(users.wage) AS avg FROM users"
  (op Select
      :project (op As
                   :column (Aggregate. :avg "users.wage")
                   :alias "avg")
      :from "users"
      :top-level true)

  "SELECT COUNT(*), AVG(users.wage) FROM users WHERE users.admin = true"
  (op Select
      :project (MultipleValues. [(Aggregate. :count "*")
                                 (Aggregate. :avg "users.wage")])
      :from "users"
      :where (Equal. "users.admin" true)
      :top-level true))


(tabular "Modifiers"
  (fact (emit in) => out)
  out in

  "(SELECT DISTINCT users.* FROM users)"
  (op Select
      :modifiers "DISTINCT"
      :project "users.*"
      :from "users")

  "(SELECT HIGH_PRIORITY DISTINCT users.* FROM users)"
  (op Select
      :modifiers (Modify. ["HIGH_PRIORITY" "DISTINCT"])
      :project "users.*"
      :from "users"))

;; Where clauses
"SELECT users.id FROM users WHERE (users.id = 5)"
"SELECT users.* FROM users WHERE (users.id IS NULL)"
"SELECT users.* FROM users WHERE (users.id IS NOT NULL)"
"SELECT users.id FROM users WHERE ((users.id = 5) OR (users.id >= 10))"
"SELECT users.id FROM users WHERE ((users.id = 5) AND (users.id >= 10))"
"SELECT users.id FROM users WHERE ((users.id = 5) AND ((users.id >= 10) OR (users.id <= 20)))"
"SELECT users.id FROM users WHERE ((users.id != 5) AND ((users.id > 10) OR (users.id < 20)))"
"SELECT users.* FROM users WHERE (lower(name) = bob)"

;; Alias + join
"SELECT salary.wage AS something FROM users JOIN salary ON (users.id = salary.id)"

;; Joins
"SELECT users.id,salary.wage FROM users JOIN salary USING(id)"
"SELECT users.id,salary.wage FROM users JOIN salary ON (users.id = salary.id)"
"SELECT u.*,s.*,w.* FROM users u JOIN wages w ON (w.id = u.id) JOIN salary s ON (w.id = s.id)"
"SELECT * FROM t3 JOIN t1 USING(id) JOIN t2 USING(id)"
"SELECT users.*,wages.*,commits.* FROM users JOIN wages USING(wid) JOIN commits USING(cid)"

(tabular "Combinations"
  (fact (emit in) => out)
  out in
  ;; TODO: 
  )


(tabular "Sort"
  (fact (emit in) => out)
  out in

  "SELECT t1.* FROM t1 ORDER BY t1.id ASC LIMIT 5"
  (op Select
      :project "t1.*"
      :from "t1"
      :order-by (op Order :expr "t1.id" :order :asc)
      :limit 5
      :top-level true)

  "SELECT * FROM (SELECT t1.* FROM t1 ORDER BY t1.id ASC LIMIT 5) ORDER BY wage ASC"
  (op Select
      :project "*"
      :from (op Select
                :project "t1.*"
                :from "t1"
                :order-by (Order. "t1.id" :asc nil)
                :limit 5)
      :order-by (Order. "wage" :asc nil)
      :top-level true))


(tabular "Nested"
  (fact (emit in) => out)
  out in

  (str "SELECT maker, AVG(hd) FROM product "
       "JOIN pc ON product.model = pc.model WHERE maker IN ("
       "SELECT DISTINCT maker FROM product "
       "WHERE type = 'pc' AND maker IN ("
       "SELECT maker FROM product "
       "WHERE type = 'printer')) "
       "GROUP BY maker")

  (op Select
      :project (MultipleValues. ["maker" (Aggregate. :avg "hd")])
      :from (TableExpression. "product"
                              (op Join :table "pc" :on (Equal. "product.model"
                                                               "pc.model")))
      :where (In. "maker"
                  (op Select
                      :modifiers (Modify. [:distinct])
                      :project "maker"
                      :from "product"
                      :where (And. (Equal. "type" "'pc'")
                                   (In. "maker"
                                        (op Select
                                            :project "maker"
                                            :from "product"
                                            :where (Equal. "type" "'printer'"))))))
      :group-by "maker"
      :top-level true))
