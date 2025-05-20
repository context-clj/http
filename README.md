# context: http server

## params

## Middleware

You can pass middleware as an argument to `register-endpoint`.

```clojure
(defn handler [ctx req]
  {:status 200
   :body "Success"})

(defn auth-middleware [f]
  (fn [ctx req]
    (if (get-in req [:headers "authorization"])
      (f ctx req)
      {:status 401
       :body "Please log in"})))

(defn another-middleware [f]
  (fn [ctx req]
    (f ctx
       (assoc-in req [:headers "x-my-header"] "foo"))))

(http/register-endpoint
 context
 {:method :post :path "/" :fn handler :middleware [auth-middleware another-middleware]})
```
