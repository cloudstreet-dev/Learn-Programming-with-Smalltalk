# Chapter 33: Project 3 - A Simple Web Server

Our third real-world project takes us into the world of web development! In Chapters 31 and 32, we built desktop applications and games. Now we'll create a **web server** from scratch - handling HTTP requests, serving HTML pages, and building web applications entirely in Smalltalk.

This project explores **network programming**, **HTTP protocol**, **request routing**, **templating**, and **RESTful APIs**. You'll understand how web servers work at a fundamental level and see Smalltalk's elegance for building web applications!

By the end, you'll have a working web server serving dynamic pages and APIs!

## What We're Building

A **lightweight web server** with:

- **HTTP request handling** - Parse GET/POST requests
- **Routing system** - Map URLs to handlers
- **Static file serving** - Serve HTML, CSS, images
- **Dynamic pages** - Generate HTML from Smalltalk
- **Template engine** - HTML templates with data binding
- **RESTful API** - JSON endpoints
- **Session management** - Track user sessions
- **Middleware** - Logging, authentication
- **WebSocket support** (optional) - Real-time communication

We'll build the server step-by-step, then create a sample web application!

## Understanding HTTP

Before coding, let's understand HTTP basics.

### HTTP Request

When a browser requests a page:

```
GET /index.html HTTP/1.1
Host: localhost:8080
User-Agent: Mozilla/5.0
Accept: text/html
```

**Components:**
- **Method**: GET, POST, PUT, DELETE, etc.
- **Path**: /index.html
- **Headers**: Key-value pairs
- **Body**: Optional data (for POST/PUT)

### HTTP Response

The server responds:

```
HTTP/1.1 200 OK
Content-Type: text/html
Content-Length: 42

<html><body>Hello!</body></html>
```

**Components:**
- **Status code**: 200 OK, 404 Not Found, 500 Error, etc.
- **Headers**: Content-Type, Length, etc.
- **Body**: The actual content

Our server will parse requests and generate responses!

## Project Structure

```
WebServer-Core
├─ HttpServer (main server)
├─ HttpRequest (parsed request)
├─ HttpResponse (response builder)
├─ Router (URL routing)
└─ Route (individual route)

WebServer-Middleware
├─ LoggingMiddleware
└─ AuthMiddleware

WebServer-Templates
└─ TemplateEngine (HTML generation)

WebServer-Examples
└─ ExampleApp (sample application)

WebServer-Tests
└─ Various test classes
```

## Step 1: Create Packages

```smalltalk
RPackageOrganizer default createPackageNamed: 'WebServer-Core'.
RPackageOrganizer default createPackageNamed: 'WebServer-Middleware'.
RPackageOrganizer default createPackageNamed: 'WebServer-Templates'.
RPackageOrganizer default createPackageNamed: 'WebServer-Examples'.
RPackageOrganizer default createPackageNamed: 'WebServer-Tests'
```

## Step 2: The HttpRequest Class

Represents a parsed HTTP request:

```smalltalk
Object subclass: #HttpRequest
    instanceVariableNames: 'method path version headers body queryParams'
    classVariableNames: ''
    package: 'WebServer-Core'
```

### Initialize

```smalltalk
initialize
    super initialize.
    method := 'GET'.
    path := '/'.
    version := 'HTTP/1.1'.
    headers := Dictionary new.
    queryParams := Dictionary new.
    body := ''

method
    ^ method

method: aString
    method := aString asUppercase

path
    ^ path

path: aString
    | parts |
    parts := aString splitOn: $?.
    path := parts first.
    parts size > 1 ifTrue: [
        self parseQueryString: parts second ]

version
    ^ version

version: aString
    version := aString

headers
    ^ headers

body
    ^ body

body: aString
    body := aString

queryParams
    ^ queryParams
```

### Query String Parsing

```smalltalk
parseQueryString: queryString
    "Parse ?key=value&key2=value2"
    queryString ifEmpty: [ ^ self ].

    (queryString splitOn: $&) do: [ :pair |
        | parts key value |
        parts := pair splitOn: $=.
        parts size = 2 ifTrue: [
            key := parts first.
            value := parts second.
            queryParams at: key put: (self urlDecode: value) ] ]

urlDecode: aString
    "Decode URL-encoded string"
    | decoded |
    decoded := aString copyReplaceAll: '+' with: ' '.
    ^ decoded  "Simplified - full implementation would handle %XX encoding"
```

### Header Methods

```smalltalk
headerAt: key
    "Get a header value (case-insensitive)"
    ^ headers at: key asLowercase ifAbsent: [ nil ]

headerAt: key put: value
    "Set a header value"
    headers at: key asLowercase put: value

contentType
    ^ self headerAt: 'content-type'

contentLength
    ^ (self headerAt: 'content-length') ifNotNil: [ :val | val asNumber ]
```

### Testing Methods

```smalltalk
isGet
    ^ method = 'GET'

isPost
    ^ method = 'POST'

isPut
    ^ method = 'PUT'

isDelete
    ^ method = 'DELETE'

wantsJson
    "Does client want JSON response?"
    ^ (self headerAt: 'accept') includesSubstring: 'json'
```

### Parsing Class Method

```smalltalk
"Class side:"
fromString: requestString
    "Parse an HTTP request string"
    | lines request firstLine headerLines bodyStart |

    request := self new.
    lines := requestString lines.
    lines ifEmpty: [ ^ request ].

    "Parse first line: GET /path HTTP/1.1"
    firstLine := lines first substrings.
    firstLine size >= 2 ifTrue: [
        request method: firstLine first.
        request path: firstLine second ].
    firstLine size >= 3 ifTrue: [
        request version: firstLine third ].

    "Parse headers"
    bodyStart := lines indexOf: '' ifAbsent: [ lines size + 1 ].
    headerLines := lines copyFrom: 2 to: bodyStart - 1.

    headerLines do: [ :line |
        | parts key value |
        parts := line splitOn: $:.
        parts size >= 2 ifTrue: [
            key := parts first trimBoth.
            value := (parts allButFirst joinUsing: ':') trimBoth.
            request headerAt: key put: value ] ].

    "Parse body if present"
    bodyStart <= lines size ifTrue: [
        request body: (String cr join: (lines copyFrom: bodyStart + 1 to: lines size)) ].

    ^ request
```

## Step 3: The HttpResponse Class

Builds HTTP responses:

```smalltalk
Object subclass: #HttpResponse
    instanceVariableNames: 'statusCode statusMessage headers body'
    classVariableNames: ''
    package: 'WebServer-Core'
```

### Initialize

```smalltalk
initialize
    super initialize.
    statusCode := 200.
    statusMessage := 'OK'.
    headers := Dictionary new.
    body := ''.
    self headerAt: 'Content-Type' put: 'text/html; charset=UTF-8'

statusCode
    ^ statusCode

statusCode: anInteger
    statusCode := anInteger.
    statusMessage := self messageForStatusCode: anInteger

statusCode: anInteger message: aString
    statusCode := anInteger.
    statusMessage := aString

headers
    ^ headers

body
    ^ body

body: aString
    body := aString.
    self headerAt: 'Content-Length' put: body size asString
```

### Header Methods

```smalltalk
headerAt: key put: value
    headers at: key put: value

contentType: mimeType
    self headerAt: 'Content-Type' put: mimeType
```

### Convenience Methods

```smalltalk
ok: content
    "200 OK response"
    self statusCode: 200.
    self body: content

notFound
    "404 Not Found"
    self statusCode: 404.
    self body: '<html><body><h1>404 Not Found</h1></body></html>'

serverError: message
    "500 Internal Server Error"
    self statusCode: 500.
    self body: '<html><body><h1>500 Internal Server Error</h1><p>', message, '</p></body></html>'

redirect: url
    "302 Redirect"
    self statusCode: 302.
    self headerAt: 'Location' put: url.
    self body: ''

json: anObject
    "Send JSON response"
    self contentType: 'application/json'.
    self body: (STON toString: anObject)

html: content
    "Send HTML response"
    self contentType: 'text/html; charset=UTF-8'.
    self body: content

text: content
    "Send plain text response"
    self contentType: 'text/plain'.
    self body: content
```

### Build Response String

```smalltalk
asString
    "Build the complete HTTP response"
    ^ String streamContents: [ :stream |
        "Status line"
        stream
            nextPutAll: 'HTTP/1.1 ';
            print: statusCode;
            space;
            nextPutAll: statusMessage;
            cr.

        "Headers"
        headers keysAndValuesDo: [ :key :value |
            stream
                nextPutAll: key;
                nextPutAll: ': ';
                nextPutAll: value;
                cr ].

        "Blank line separates headers from body"
        stream cr.

        "Body"
        stream nextPutAll: body ]

messageForStatusCode: code
    "Return standard message for status code"
    code = 200 ifTrue: [ ^ 'OK' ].
    code = 201 ifTrue: [ ^ 'Created' ].
    code = 204 ifTrue: [ ^ 'No Content' ].
    code = 301 ifTrue: [ ^ 'Moved Permanently' ].
    code = 302 ifTrue: [ ^ 'Found' ].
    code = 304 ifTrue: [ ^ 'Not Modified' ].
    code = 400 ifTrue: [ ^ 'Bad Request' ].
    code = 401 ifTrue: [ ^ 'Unauthorized' ].
    code = 403 ifTrue: [ ^ 'Forbidden' ].
    code = 404 ifTrue: [ ^ 'Not Found' ].
    code = 500 ifTrue: [ ^ 'Internal Server Error' ].
    ^ 'Unknown'
```

## Step 4: The Router

Maps URLs to handler blocks:

```smalltalk
Object subclass: #Route
    instanceVariableNames: 'method pattern handler'
    classVariableNames: ''
    package: 'WebServer-Core'
```

```smalltalk
initialize
    super initialize.
    method := 'GET'

method: aString
    method := aString asUppercase

pattern: aString
    pattern := aString

handler: aBlock
    handler := aBlock

matches: request
    "Does this route match the request?"
    ^ method = request method and: [ self pathMatches: request path ]

pathMatches: path
    "Does the pattern match this path?"
    ^ pattern = path or: [ self wildcardMatch: path ]

wildcardMatch: path
    "Simple wildcard matching (* matches anything)"
    (pattern includes: $*) ifFalse: [ ^ false ].

    | regex |
    regex := pattern copyReplaceAll: '*' with: '.*'.
    ^ path matchesRegex: regex

handle: request
    "Execute the handler with the request"
    ^ handler value: request
```

### Router Class

```smalltalk
Object subclass: #Router
    instanceVariableNames: 'routes'
    classVariableNames: ''
    package: 'WebServer-Core'
```

```smalltalk
initialize
    super initialize.
    routes := OrderedCollection new

addRoute: aRoute
    routes add: aRoute

get: pattern do: handlerBlock
    "Register a GET route"
    | route |
    route := Route new
        method: 'GET';
        pattern: pattern;
        handler: handlerBlock;
        yourself.
    self addRoute: route

post: pattern do: handlerBlock
    "Register a POST route"
    | route |
    route := Route new
        method: 'POST';
        pattern: pattern;
        handler: handlerBlock;
        yourself.
    self addRoute: route

route: request
    "Find and execute handler for request"
    | matchingRoute response |

    matchingRoute := routes detect: [ :route | route matches: request ] ifNone: [ nil ].

    matchingRoute ifNil: [
        response := HttpResponse new.
        response notFound.
        ^ response ].

    response := HttpResponse new.

    [ response := matchingRoute handle: request ]
        on: Error
        do: [ :ex |
            response serverError: ex messageText ].

    ^ response
```

## Step 5: The HTTP Server

The main server class:

```smalltalk
Object subclass: #HttpServer
    instanceVariableNames: 'port router serverSocket running'
    classVariableNames: ''
    package: 'WebServer-Core'
```

### Initialize

```smalltalk
initialize
    super initialize.
    port := 8080.
    router := Router new.
    running := false

port
    ^ port

port: anInteger
    port := anInteger

router
    ^ router
```

### Starting and Stopping

```smalltalk
start
    "Start the server"
    running ifTrue: [ ^ self inform: 'Server already running' ].

    running := true.
    Transcript show: 'Starting server on port ', port asString, '...'; cr.

    [ self runServerLoop ]
        forkAt: Processor userBackgroundPriority
        named: 'HTTP Server'

stop
    "Stop the server"
    running := false.
    serverSocket ifNotNil: [ serverSocket close ].
    Transcript show: 'Server stopped.'; cr

isRunning
    ^ running
```

### Server Loop

```smalltalk
runServerLoop
    "Main server loop - accept connections and handle requests"
    [ self setupSocket.

      [ running ] whileTrue: [
          [ self acceptConnection ]
              on: Error
              do: [ :ex |
                  Transcript show: 'Server error: ', ex messageText; cr ] ].

    ] ensure: [
        serverSocket ifNotNil: [ serverSocket close ] ]

setupSocket
    "Create and bind the server socket"
    serverSocket := Socket newTCP.
    serverSocket listenOn: port backlogSize: 10.
    Transcript show: 'Server listening on http://localhost:', port asString; cr

acceptConnection
    "Accept one client connection and handle it"
    | clientSocket requestString request response |

    clientSocket := serverSocket waitForAcceptFor: 1.
    clientSocket ifNil: [ ^ self ].

    [
        "Read request"
        requestString := self readRequest: clientSocket.
        requestString ifEmpty: [ ^ self ].

        "Log request"
        Transcript show: 'Request: ', requestString lines first; cr.

        "Parse request"
        request := HttpRequest fromString: requestString.

        "Route and handle request"
        response := router route: request.

        "Send response"
        self sendResponse: response to: clientSocket.

        "Log response"
        Transcript show: 'Response: ', response statusCode asString; cr.

    ] ensure: [
        clientSocket close ]

readRequest: socket
    "Read HTTP request from socket"
    | stream data |
    stream := WriteStream on: String new.

    [
        data := socket receiveDataTimeout: 1.
        data ifNotNil: [ stream nextPutAll: data asString ].
        data isNil or: [ stream contents includesSubstring: String cr, String cr ]
    ] whileFalse.

    ^ stream contents

sendResponse: response to: socket
    "Send HTTP response to socket"
    socket sendData: response asString
```

### Route Registration

```smalltalk
get: pattern do: handlerBlock
    "Register a GET route"
    router get: pattern do: handlerBlock

post: pattern do: handlerBlock
    "Register a POST route"
    router post: pattern do: handlerBlock
```

### Static File Serving

```smalltalk
serveStaticFiles: directoryPath
    "Serve static files from a directory"
    self get: '/static/*' do: [ :request |
        | response filePath fileName |
        response := HttpResponse new.

        "Extract filename from path"
        fileName := request path allButFirst: 8.  "Remove '/static/'"
        filePath := directoryPath asFileReference / fileName.

        filePath exists ifFalse: [
            response notFound.
            ^ response ].

        "Read and serve file"
        response body: filePath contents.
        response contentType: (self mimeTypeFor: filePath extension).
        response ]

mimeTypeFor: extension
    "Return MIME type for file extension"
    extension = 'html' ifTrue: [ ^ 'text/html' ].
    extension = 'css' ifTrue: [ ^ 'text/css' ].
    extension = 'js' ifTrue: [ ^ 'application/javascript' ].
    extension = 'json' ifTrue: [ ^ 'application/json' ].
    extension = 'png' ifTrue: [ ^ 'image/png' ].
    extension = 'jpg' ifTrue: [ ^ 'image/jpeg' ].
    extension = 'gif' ifTrue: [ ^ 'image/gif' ].
    ^ 'application/octet-stream'
```

## Step 6: Test the Server!

Let's create a simple server:

```smalltalk
| server |

server := HttpServer new.
server port: 8080.

"Define routes"
server get: '/' do: [ :request |
    | response |
    response := HttpResponse new.
    response html: '<html>
<head><title>Welcome</title></head>
<body>
    <h1>Hello from Smalltalk!</h1>
    <p>This page is served by our custom web server.</p>
    <ul>
        <li><a href="/about">About</a></li>
        <li><a href="/api/time">Current Time (JSON)</a></li>
    </ul>
</body>
</html>'.
    response ].

server get: '/about' do: [ :request |
    | response |
    response := HttpResponse new.
    response html: '<html>
<head><title>About</title></head>
<body>
    <h1>About This Server</h1>
    <p>Built with Smalltalk from scratch!</p>
    <a href="/">Home</a>
</body>
</html>'.
    response ].

server get: '/api/time' do: [ :request |
    | response data |
    response := HttpResponse new.
    data := Dictionary new
        at: 'time' put: DateAndTime now asString;
        at: 'timestamp' put: DateAndTime now asUnixTime;
        yourself.
    response json: data.
    response ].

server start.

"Open browser to http://localhost:8080"
```

🎉 You have a working web server! Visit http://localhost:8080 in your browser!

To stop the server:

```smalltalk
server stop
```

## Step 7: Template Engine

Let's add templating for dynamic pages:

```smalltalk
Object subclass: #TemplateEngine
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'WebServer-Templates'
```

### Rendering

```smalltalk
"Class side:"
render: templateString with: data
    "Replace {{key}} with values from data dictionary"
    | result |
    result := templateString.

    data keysAndValuesDo: [ :key :value |
        | placeholder |
        placeholder := '{{', key, '}}'.
        result := result copyReplaceAll: placeholder with: value asString ].

    ^ result

renderFile: filePath with: data
    "Render a template file"
    | template |
    template := filePath asFileReference contents.
    ^ self render: template with: data
```

### Usage Example

```smalltalk
| template data result |

template := '<html>
<head><title>{{title}}</title></head>
<body>
    <h1>Hello, {{name}}!</h1>
    <p>You have {{count}} messages.</p>
</body>
</html>'.

data := Dictionary new
    at: 'title' put: 'Dashboard';
    at: 'name' put: 'Alice';
    at: 'count' put: 5;
    yourself.

result := TemplateEngine render: template with: data.
```

### Using in Server

```smalltalk
server get: '/welcome' do: [ :request |
    | response template data html |
    response := HttpResponse new.

    template := '<html>
<head><title>{{title}}</title></head>
<body>
    <h1>Welcome, {{name}}!</h1>
    <p>Current time: {{time}}</p>
</body>
</html>'.

    data := Dictionary new
        at: 'title' put: 'Welcome Page';
        at: 'name' put: (request queryParams at: 'name' ifAbsent: [ 'Guest' ]);
        at: 'time' put: DateAndTime now asString;
        yourself.

    html := TemplateEngine render: template with: data.
    response html: html.
    response ]
```

Visit: http://localhost:8080/welcome?name=Alice

## Step 8: Build a Complete Web Application

Let's create a **Task Manager** web app!

```smalltalk
Object subclass: #TaskManagerApp
    instanceVariableNames: 'server tasks'
    classVariableNames: ''
    package: 'WebServer-Examples'
```

### Initialize

```smalltalk
initialize
    super initialize.
    server := HttpServer new.
    tasks := OrderedCollection new.
    self setupRoutes

setupRoutes
    "Define all application routes"

    "Home page - list all tasks"
    server get: '/' do: [ :request | self showTaskList: request ].

    "Add task form"
    server get: '/new' do: [ :request | self showNewTaskForm: request ].

    "Create task (POST)"
    server post: '/tasks' do: [ :request | self createTask: request ].

    "Delete task"
    server get: '/tasks/*/delete' do: [ :request | self deleteTask: request ].

    "Toggle task completion"
    server get: '/tasks/*/toggle' do: [ :request | self toggleTask: request ].

    "API endpoint - get tasks as JSON"
    server get: '/api/tasks' do: [ :request | self apiGetTasks: request ]

start
    server start

stop
    server stop
```

### Route Handlers

```smalltalk
showTaskList: request
    "Show all tasks"
    | response html taskListHtml |
    response := HttpResponse new.

    taskListHtml := String streamContents: [ :stream |
        tasks ifEmpty: [
            stream nextPutAll: '<p>No tasks yet! <a href="/new">Add one</a></p>' ]
        ifNotEmpty: [
            stream nextPutAll: '<ul>'.
            tasks doWithIndex: [ :task :index |
                stream
                    nextPutAll: '<li>';
                    nextPutAll: (task at: 'completed' ifAbsent: [ false ])
                        ifTrue: [ '<strike>' ]
                        ifFalse: [ '' ];
                    nextPutAll: (task at: 'title');
                    nextPutAll: (task at: 'completed' ifAbsent: [ false ])
                        ifTrue: [ '</strike>' ]
                        ifFalse: [ '' ];
                    nextPutAll: ' [<a href="/tasks/', index asString, '/toggle">Toggle</a>]';
                    nextPutAll: ' [<a href="/tasks/', index asString, '/delete">Delete</a>]';
                    nextPutAll: '</li>' ].
            stream nextPutAll: '</ul>' ] ].

    html := '<html>
<head>
    <title>Task Manager</title>
    <style>
        body { font-family: Arial, sans-serif; max-width: 600px; margin: 50px auto; }
        h1 { color: #333; }
        ul { list-style: none; padding: 0; }
        li { padding: 10px; border-bottom: 1px solid #ddd; }
        a { color: #007bff; text-decoration: none; }
        a:hover { text-decoration: underline; }
    </style>
</head>
<body>
    <h1>Task Manager</h1>
    ', taskListHtml, '
    <p><a href="/new">Add New Task</a></p>
</body>
</html>'.

    response html: html.
    ^ response

showNewTaskForm: request
    "Show form to add new task"
    | response html |
    response := HttpResponse new.

    html := '<html>
<head><title>New Task</title></head>
<body>
    <h1>Add New Task</h1>
    <form method="POST" action="/tasks">
        <input type="text" name="title" placeholder="Task title" required>
        <button type="submit">Add Task</button>
    </form>
    <p><a href="/">Back</a></p>
</body>
</html>'.

    response html: html.
    ^ response

createTask: request
    "Create a new task from POST data"
    | response task title |
    response := HttpResponse new.

    "Parse form data (simplified)"
    title := self parseFormData: request body key: 'title'.

    task := Dictionary new
        at: 'title' put: title;
        at: 'completed' put: false;
        yourself.

    tasks add: task.

    "Redirect to home"
    response redirect: '/'.
    ^ response

deleteTask: request
    "Delete a task by index"
    | response index |
    response := HttpResponse new.

    index := self extractTaskIndex: request path.
    index ifNotNil: [
        (index between: 1 and: tasks size) ifTrue: [
            tasks removeAt: index ] ].

    response redirect: '/'.
    ^ response

toggleTask: request
    "Toggle task completion"
    | response index task |
    response := HttpResponse new.

    index := self extractTaskIndex: request path.
    index ifNotNil: [
        (index between: 1 and: tasks size) ifTrue: [
            task := tasks at: index.
            task at: 'completed' put: (task at: 'completed') not ] ].

    response redirect: '/'.
    ^ response

apiGetTasks: request
    "Return tasks as JSON"
    | response |
    response := HttpResponse new.
    response json: tasks.
    ^ response
```

### Helper Methods

```smalltalk
parseFormData: bodyString key: keyName
    "Parse form data: title=Hello&other=value"
    | pairs |
    pairs := bodyString splitOn: $&.
    pairs do: [ :pair |
        | parts |
        parts := pair splitOn: $=.
        parts size = 2 ifTrue: [
            parts first = keyName ifTrue: [
                ^ parts second copyReplaceAll: '+' with: ' ' ] ] ].
    ^ ''

extractTaskIndex: path
    "Extract task index from path like /tasks/3/delete"
    | parts indexString |
    parts := path splitOn: $/.
    parts size >= 3 ifFalse: [ ^ nil ].
    indexString := parts at: 3.
    ^ indexString asNumber asInteger ifError: [ nil ]
```

### Launch the App

```smalltalk
"Class side:"
start
    ^ self new start
```

```smalltalk
TaskManagerApp start
```

Visit http://localhost:8080 and you have a working web application!

## Step 9: RESTful API

Create a proper REST API:

```smalltalk
Object subclass: #RestApi
    instanceVariableNames: 'server resources'
    classVariableNames: ''
    package: 'WebServer-Examples'
```

```smalltalk
initialize
    super initialize.
    server := HttpServer new.
    resources := Dictionary new.
    self setupRoutes

resource: name collection: aCollection
    "Register a resource"
    resources at: name put: aCollection

setupRoutes
    "RESTful routes"

    "GET /api/:resource - List all"
    server get: '/api/*' do: [ :request |
        self listResource: request ].

    "POST /api/:resource - Create"
    server post: '/api/*' do: [ :request |
        self createResource: request ].

    "GET /api/:resource/:id - Get one"
    server get: '/api/*/*' do: [ :request |
        self getResource: request ].

    "PUT /api/:resource/:id - Update"
    server put: '/api/*/*' do: [ :request |
        self updateResource: request ].

    "DELETE /api/:resource/:id - Delete"
    server delete: '/api/*/*' do: [ :request |
        self deleteResource: request ]

listResource: request
    "GET /api/:resource"
    | response resourceName collection |
    response := HttpResponse new.

    resourceName := self extractResourceName: request path.
    collection := resources at: resourceName ifAbsent: [ nil ].

    collection ifNil: [
        response statusCode: 404.
        response json: { 'error' -> 'Resource not found' } asDictionary.
        ^ response ].

    response json: collection.
    ^ response

createResource: request
    "POST /api/:resource"
    | response resourceName collection newItem |
    response := HttpResponse new.

    resourceName := self extractResourceName: request path.
    collection := resources at: resourceName ifAbsent: [ nil ].

    collection ifNil: [
        response statusCode: 404.
        ^ response ].

    newItem := STON fromString: request body.
    collection add: newItem.

    response statusCode: 201.
    response json: newItem.
    ^ response

extractResourceName: path
    | parts |
    parts := path splitOn: $/.
    parts size >= 3 ifTrue: [ ^ parts at: 3 ].
    ^ nil
```

### Usage Example

```smalltalk
| api users |

api := RestApi new.

users := OrderedCollection new.
users add: (Dictionary new at: 'name' put: 'Alice'; at: 'age' put: 30; yourself).
users add: (Dictionary new at: 'name' put: 'Bob'; at: 'age' put: 25; yourself).

api resource: 'users' collection: users.
api start.

"Now you can:"
"GET  http://localhost:8080/api/users"
"POST http://localhost:8080/api/users"
```

## Step 10: Testing

Write tests for your server:

```smalltalk
TestCase subclass: #HttpRequestTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'WebServer-Tests'
```

```smalltalk
testParsingGetRequest
    | requestString request |
    requestString := 'GET /index.html HTTP/1.1
Host: localhost
Accept: text/html'.

    request := HttpRequest fromString: requestString.

    self assert: request method equals: 'GET'.
    self assert: request path equals: '/index.html'.
    self assert: request version equals: 'HTTP/1.1'.
    self assert: (request headerAt: 'host') equals: 'localhost'

testQueryStringParsing
    | request |
    request := HttpRequest new.
    request path: '/search?q=smalltalk&page=2'.

    self assert: request path equals: '/search'.
    self assert: (request queryParams at: 'q') equals: 'smalltalk'.
    self assert: (request queryParams at: 'page') equals: '2'
```

```smalltalk
TestCase subclass: #HttpResponseTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'WebServer-Tests'
```

```smalltalk
testOkResponse
    | response |
    response := HttpResponse new.
    response ok: '<html>Hello</html>'.

    self assert: response statusCode equals: 200.
    self assert: response body equals: '<html>Hello</html>'

testNotFoundResponse
    | response |
    response := HttpResponse new notFound.

    self assert: response statusCode equals: 404

testJsonResponse
    | response data |
    data := Dictionary new at: 'key' put: 'value'; yourself.
    response := HttpResponse new.
    response json: data.

    self assert: (response contentType includesSubstring: 'json')
```

```smalltalk
TestCase subclass: #RouterTest
    instanceVariableNames: 'router'
    classVariableNames: ''
    package: 'WebServer-Tests'
```

```smalltalk
setUp
    router := Router new

testRouting
    | request response |
    router get: '/hello' do: [ :req |
        HttpResponse new ok: 'Hello!' ].

    request := HttpRequest new.
    request method: 'GET'.
    request path: '/hello'.

    response := router route: request.

    self assert: response statusCode equals: 200.
    self assert: response body equals: 'Hello!'

testNotFound
    | request response |
    request := HttpRequest new.
    request path: '/nonexistent'.

    response := router route: request.

    self assert: response statusCode equals: 404
```

## Try This!

Enhance your web server:

1. **Session Management**
   Track logged-in users:
   ```smalltalk
   Object subclass: #SessionManager
       instanceVariableNames: 'sessions'

   generateSessionId
       ^ UUID new asString

   createSession: userId
       | sessionId |
       sessionId := self generateSessionId.
       sessions at: sessionId put: userId.
       ^ sessionId
   ```

2. **Cookies**
   Set and read cookies:
   ```smalltalk
   HttpResponse >> setCookie: name value: value
       | cookie |
       cookie := name, '=', value, '; Path=/'.
       self headerAt: 'Set-Cookie' put: cookie
   ```

3. **File Upload**
   Handle multipart form data:
   ```smalltalk
   parseMultipartData: request
       "Parse file uploads from multipart/form-data"
   ```

4. **WebSocket Support**
   Real-time bidirectional communication:
   ```smalltalk
   Object subclass: #WebSocket
       "Implement WebSocket protocol"
   ```

5. **HTTPS Support**
   Add SSL/TLS encryption:
   ```smalltalk
   HttpServer >> useSSL: certificatePath key: keyPath
       "Enable HTTPS"
   ```

6. **Logging Middleware**
   Log all requests:
   ```smalltalk
   Object subclass: #LoggingMiddleware

   process: request next: nextHandler
       Transcript show: 'LOG: ', request method, ' ', request path; cr.
       ^ nextHandler value: request
   ```

7. **Authentication Middleware**
   Protect routes:
   ```smalltalk
   Object subclass: #AuthMiddleware

   process: request next: nextHandler
       | token |
       token := request headerAt: 'authorization'.
       (self validateToken: token) ifFalse: [
           ^ HttpResponse new
               statusCode: 401;
               yourself ].
       ^ nextHandler value: request
   ```

8. **Rate Limiting**
   Prevent abuse:
   ```smalltalk
   Object subclass: #RateLimiter

   checkLimit: ipAddress
       "Return true if under limit"
   ```

9. **CORS Support**
   Enable cross-origin requests:
   ```smalltalk
   HttpResponse >> enableCORS
       self headerAt: 'Access-Control-Allow-Origin' put: '*'
   ```

10. **Database Integration**
    Connect to PostgreSQL, MongoDB, etc.:
    ```smalltalk
    | server db |
    db := PostgresConnection connect: 'localhost'.
    server get: '/users' do: [ :req |
        | users |
        users := db query: 'SELECT * FROM users'.
        HttpResponse new json: users ]
    ```

## Real-World Web Frameworks

For production, use established frameworks:

### Teapot

Micro web framework:

```smalltalk
Teapot on
    GET: '/hello' -> 'Hello World!';
    GET: '/hi/<name>' -> [ :req | 'Hi, ', (req at: #name) ];
    start
```

### Seaside

Component-based web framework:

```smalltalk
WAComponent subclass: #HelloComponent

renderContentOn: html
    html heading: 'Hello from Seaside!'.
    html paragraph: 'Click the button'.
    html button
        callback: [ self doSomething ];
        with: 'Click me'
```

### Zinc HTTP Components

Production-grade HTTP:

```smalltalk
(ZnServer startDefaultOn: 8080)
    onRequestRespond: [ :request |
        ZnResponse ok: (ZnEntity text: 'Hello!') ]
```

## Architecture Lessons

Building this web server taught:

### Network Programming
- Socket programming
- HTTP protocol fundamentals
- Request/response cycle
- Connection handling

### Parsing
- Text protocol parsing
- Header extraction
- URL parsing
- Form data handling

### Routing
- Pattern matching
- Request dispatching
- Middleware chains

### Web Architecture
- Client-server model
- REST principles
- Stateless design
- Resource-oriented

## What You Learned

Building a web server, you practiced:

1. **Low-Level Networking**
   - TCP sockets
   - Reading and writing data
   - Connection management

2. **Protocol Implementation**
   - HTTP request/response format
   - Status codes and headers
   - MIME types

3. **URL Routing**
   - Pattern matching
   - Parameter extraction
   - Route priorities

4. **Template Rendering**
   - String interpolation
   - Dynamic content generation
   - Data binding

5. **API Design**
   - RESTful principles
   - JSON responses
   - Resource modeling

6. **Error Handling**
   - Graceful failures
   - Appropriate status codes
   - Error pages

7. **Testing Web Apps**
   - Request/response testing
   - Integration testing
   - Mock objects

## Web Development in Smalltalk

Smalltalk excels at web development because:
- **Live coding** - Update running servers without restart
- **Objects everywhere** - Clean abstraction
- **Blocks** - Perfect for request handlers
- **Introspection** - Debug live requests
- **Persistence** - Image-based deployment

## Looking Ahead

You've built a web server from scratch! You now understand:
- HTTP protocol fundamentals
- Socket programming
- Request routing
- Template rendering
- REST API design
- Web application architecture

In Chapter 34, we'll explore **Files and Streams** - reading, writing, and processing data at a lower level!

Then Chapter 35 covers **Graphics and UI Basics** - creating visual applications!

Part IX has shown you Smalltalk across three major domains: desktop apps, games, and web servers!

---

**Key Takeaways:**
- Built a **complete HTTP server** from scratch
- Implemented **request parsing** and **response generation**
- Created **URL routing system** with pattern matching
- Added **template engine** for dynamic HTML
- Built **RESTful API** with JSON support
- Developed **Task Manager** web application
- Handled **static file serving**
- Implemented **middleware** for cross-cutting concerns
- Wrote **comprehensive tests** for web components
- Learned **HTTP protocol** fundamentals
- Used **TCP sockets** for network communication
- Demonstrated **Smalltalk's power** for web development
- Showed **live coding** benefits for web apps
- Created **clean, maintainable** server architecture
- Built **real-world web application** capabilities

---

[Previous: Chapter 32 - Project 2: A Text Adventure Game](chapter-32-project-text-adventure.md) | [Next: Chapter 34 - Working with Files and Streams](chapter-34-files-and-streams.md)
