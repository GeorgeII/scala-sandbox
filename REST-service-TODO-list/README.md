### REST GET, POST, and DELETE method endpoints implementing a simple ToDo list.

### CURL commands.

To add tasks to the list:

```bash
curl -XPOST -H "Content-Type: application/json" -d '{"name":"sleep", "priority": 3}' localhost:8080/api/tasks

curl -XPOST -H "Content-Type: application/json" -d '{"name":"drink", "priority": 1}' localhost:8080/api/tasks

curl -XPOST -H "Content-Type: application/json" -d '{"name":"eat", "priority": 2}' localhost:8080/api/tasks
```

To see the list sorted by priority, enter:

```bash
curl localhost:8080/api/tasks
```

To remove the 'eat' task from the list:

```bash
curl -X DELETE localhost:8080/api/tasks/eat
```

### HTTPIE commands.
HTTPIE is a handy terminal utility with a prettified and colorized output.

The equivalent of the CURL commands above will be:

```bash
http POST localhost:8080/api/tasks name=sleep priority:=3
http POST localhost:8080/api/tasks name=drink priority:=1
http POST localhost:8080/api/tasks name=eat priority:=2

http localhost:8080/api/tasks

http DELETE localhost:8080/api/tasks/eat
```

### GET request example using HTTPIE:

<img src="https://raw.githubusercontent.com/GeorgeII/scala-sandbox/restService/REST-service-TODO-list/pictures/1.png?raw=true" />

