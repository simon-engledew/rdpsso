Create a certificate and private key using openssl:

https://devcenter.heroku.com/articles/ssl-certificate-self

```
cert/server.crt
cert/server.key
```

Copy ```rdpsso.config.example``` to ```rdpsso.config``` and insert the login credentials

Build and run:

```
rebar get-deps
rebar compile
escript run.es
```

Connect with remote desktop to localhost:3000
