# context: http server

## params



## Middleware

There are few types of middleware:

* context middleware - add something to context, like user or entity, which passed to handlers
* request transformers - transform request
* interceptors - when interceptor returns  - it breaks the chain
* response transformers - transform final response

Middlewares can be added to specific path branch, for example '/admin'







