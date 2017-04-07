# chat server
A erlang command line chat server. 

## Server
Start on local computer to test
```
chat_server_ui:start().
```
Start on distributed server:
```
erl -name [server name]@[IP address] -noshell -s chat_server_ui start &
```

## Client
Start:
```
chat:login(username).
```
Chat:
chat:send(to_username, 'message').

Start on distributed server:
```
erl -name username@127.0.0.1 -s chat login username
```
