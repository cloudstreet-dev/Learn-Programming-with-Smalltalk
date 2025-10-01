# Chapter 24: Error Handling - When Things Go Wrong

Programs encounter errors: files don't exist, networks fail, users provide invalid input, calculations overflow. How you handle these situations determines whether your software is robust or brittle.

Smalltalk has a sophisticated exception handling system based on objects. Instead of error codes or special return values, Smalltalk uses **Exception objects** - first-class objects you can signal, catch, resume, and handle.

In this chapter, you'll learn to handle errors gracefully, signal your own exceptions, and write robust code that recovers from failures.

## The Basic Idea

### Traditional Error Handling:

```c
int result = doSomething();
if (result == ERROR_CODE) {
    // Handle error
}
```

Errors are values (integers, nulls, special returns).

### Smalltalk Exception Handling:

```smalltalk
[ self doSomething ]
    on: Error
    do: [ :exception | self handleError: exception ]
```

Errors are objects that are **signaled** and **caught** by handlers.

## Signaling Exceptions

To raise an error, you **signal** an exception:

### Simple Error

```smalltalk
self error: 'Something went wrong!'
```

This signals a generic `Error` exception with a message. The Debugger opens (if no handler catches it).

### Specific Exceptions

```smalltalk
ZeroDivide signal.
FileDoesNotExistException signal.
InvalidArgumentError signal: 'Age must be positive'
```

Each type of error has its own exception class.

### Creating Custom Exceptions

```smalltalk
Error subclass: #InsufficientFundsError
    instanceVariableNames: 'requestedAmount availableAmount'
    classVariableNames: ''
    package: 'MyApp'
```

```smalltalk
"Instance side:"
requestedAmount: reqAmount availableAmount: availAmount
    requestedAmount := reqAmount.
    availableAmount := availAmount.
    ^ self
```

```smalltalk
"Signaling:"
(InsufficientFundsError new
    requestedAmount: 100;
    availableAmount: 50) signal
```

Now you have a custom exception with specific data!

## Catching Exceptions

Use `on:do:` to catch exceptions:

```smalltalk
[ "code that might fail" ]
    on: ExceptionClass
    do: [ :exception | "handle the exception" ]
```

### Example: Catching ZeroDivide

```smalltalk
| result |
result := [
    10 / 0 ]
    on: ZeroDivide
    do: [ :ex |
        Transcript show: 'Cannot divide by zero! Returning nil.'; cr.
        nil ].

result  "nil"
```

The exception is caught, and `nil` is returned.

### Example: Catching File Errors

```smalltalk
| contents |
contents := [
    '/nonexistent/file.txt' asFileReference contents ]
    on: FileDoesNotExistException
    do: [ :ex |
        Transcript show: 'File not found!'; cr.
        'Default content' ].

contents  "'Default content'"
```

## The Exception Object

When you catch an exception, the block receives the exception object:

```smalltalk
[ ... ]
    on: Error
    do: [ :exception |
        exception inspect.
        exception messageText.  "The error message"
        exception signalerContext.  "Where it was signaled"
        exception class  "The exception class"
    ]
```

You can query the exception for details!

### Useful Exception Methods

- **`messageText`** - The error message string
- **`signalerContext`** - The stack context where it was signaled
- **`retry`** - Retry the protected block
- **`resume`** - Resume execution after the signal
- **`resume:`** - Resume with a specific value
- **`pass`** - Let the exception propagate to outer handlers
- **`return`** - Return from the protected block
- **`return:`** - Return a specific value from the protected block

## Exception Hierarchy

Exceptions form a hierarchy:

```
Exception
├─ Error
│  ├─ ZeroDivide
│  ├─ MessageNotUnderstood
│  ├─ SubscriptOutOfBounds
│  ├─ FileException
│  │  ├─ FileDoesNotExistException
│  │  └─ CannotDeleteFileException
│  └─ ... (many more)
├─ Notification
├─ Warning
└─ UnhandledError
```

Catch broad categories or specific exceptions:

```smalltalk
[ ... ]
    on: Error  "Catches all errors"
    do: [ :ex | ... ]
```

```smalltalk
[ ... ]
    on: FileException  "Catches all file-related errors"
    do: [ :ex | ... ]
```

```smalltalk
[ ... ]
    on: FileDoesNotExistException  "Catches only this specific error"
    do: [ :ex | ... ]
```

## Multiple Exception Handlers

Catch different exceptions differently:

```smalltalk
[
    self processFile: filename ]
    on: FileDoesNotExistException
    do: [ :ex | Transcript show: 'File not found'; cr ].
    on: ZeroDivide
    do: [ :ex | Transcript show: 'Math error'; cr ].
    on: Error
    do: [ :ex | Transcript show: 'Unknown error'; cr ]
```

Or use a single handler with conditions:

```smalltalk
[
    self processFile: filename ]
    on: Error
    do: [ :ex |
        ex isKindOf: FileDoesNotExistException
            ifTrue: [ Transcript show: 'File not found'; cr ].
        ex isKindOf: ZeroDivide
            ifTrue: [ Transcript show: 'Math error'; cr ].
        ex pass  "Re-signal if not handled"
    ]
```

## Retry and Resume

Exceptions aren't just for aborting - you can retry or resume!

### Retry

```smalltalk
| attempts result |
attempts := 0.
result := [
    attempts := attempts + 1.
    attempts < 3 ifTrue: [ self error: 'Not yet!' ].
    'Success!' ]
    on: Error
    do: [ :ex |
        Transcript show: 'Attempt ', attempts printString, ' failed. Retrying...'; cr.
        ex retry ].

result  "'Success!'"
```

The protected block is re-executed from the beginning!

### Resume

```smalltalk
| result |
result := [
    | value |
    value := self askUserForNumber.
    value <= 0 ifTrue: [
        (Error new messageText: 'Must be positive') signal ].
    value ]
    on: Error
    do: [ :ex |
        Transcript show: 'Invalid input. Using default.'; cr.
        ex resume: 42 ].

result  "42 (or the user's valid input)"
```

`resume:` provides a value and continues execution!

## Ensure and IfCurtailed

Sometimes you need cleanup code that runs whether an exception occurs or not:

### ensure:

```smalltalk
| file |
file := '/tmp/data.txt' asFileReference.
[
    file openForWrite.
    file nextPutAll: 'Some data'.
    "... more operations ..."
]
    ensure: [ file close ].
```

The `ensure:` block **always** runs, even if an exception occurs, or if you return early.

This guarantees cleanup!

### ifCurtailed:

```smalltalk
[
    self longRunningOperation ]
    ifCurtailed: [
        Transcript show: 'Operation was interrupted!'; cr ]
```

The `ifCurtailed:` block runs only if execution is **interrupted** (by an exception or early return), not if it completes normally.

## Practical Examples

### Example 1: Safe File Reading

```smalltalk
readFile: filename
    "Read a file safely, returning nil if it doesn't exist"
    ^ [
        filename asFileReference contents ]
        on: FileDoesNotExistException
        do: [ :ex |
            Transcript show: 'File ', filename, ' not found.'; cr.
            nil ]
```

### Example 2: Retry Network Request

```smalltalk
fetchWithRetry: url maxAttempts: maxAttempts
    "Fetch a URL, retrying on failure"
    | attempts |
    attempts := 0.
    ^ [
        attempts := attempts + 1.
        self fetch: url ]
        on: NetworkError
        do: [ :ex |
            attempts < maxAttempts
                ifTrue: [
                    Transcript show: 'Attempt ', attempts printString, ' failed. Retrying...'; cr.
                    (Delay forSeconds: 2) wait.
                    ex retry ]
                ifFalse: [
                    Transcript show: 'All attempts failed.'; cr.
                    ex pass ] ]
```

### Example 3: Resource Management

```smalltalk
withDatabaseConnection: aBlock
    "Execute aBlock with a database connection, ensuring cleanup"
    | connection |
    connection := self openConnection.
    [ aBlock value: connection ]
        ensure: [ connection close ]
```

Usage:

```smalltalk
self withDatabaseConnection: [ :db |
    db query: 'SELECT * FROM users' ]
```

The connection is guaranteed to close, even if an error occurs!

### Example 4: User Input Validation

```smalltalk
getPositiveNumber
    "Ask user for a positive number, retrying on invalid input"
    ^ [
        | input |
        input := self askUser: 'Enter a positive number:'.
        input asNumber <= 0 ifTrue: [
            (InvalidInputError new messageText: 'Must be positive') signal ].
        input asNumber ]
        on: InvalidInputError
        do: [ :ex |
            Transcript show: ex messageText; cr.
            ex retry ]
```

### Example 5: Graceful Degradation

```smalltalk
getUserProfile: userId
    "Get user profile, falling back to default if unavailable"
    ^ [
        self fetchUserProfileFromAPI: userId ]
        on: NetworkError
        do: [ :ex |
            Transcript show: 'Network error. Using cached profile.'; cr.
            self getCachedProfile: userId ]
        on: Error
        do: [ :ex |
            Transcript show: 'Unknown error. Using default profile.'; cr.
            self defaultProfile ]
```

## Notifications and Warnings

Not all exceptions are errors. Some are informational:

### Notification

```smalltalk
Notification signal: 'Processing item 100 of 1000'
```

Notifications can be handled or ignored:

```smalltalk
[
    1 to: 1000 do: [ :i |
        i \\ 100 = 0 ifTrue: [
            Notification signal: 'Processing item ', i printString ].
        self processItem: i ] ]
    on: Notification
    do: [ :ex |
        Transcript show: ex messageText; cr.
        ex resume ]
```

### Warning

```smalltalk
Warning signal: 'This method is deprecated. Use newMethod instead.'
```

Warnings are like notifications but more serious. They can be handled to suppress or log.

## Pass and Outer Handlers

Use `pass` to let an exception propagate to outer handlers:

```smalltalk
[
    [
        self doSomething ]
        on: Error
        do: [ :ex |
            ex isKindOf: ZeroDivide
                ifTrue: [ Transcript show: 'Caught ZeroDivide'; cr ]
                ifFalse: [ ex pass ]  "Let outer handlers deal with it"
        ] ]
    on: Error
    do: [ :ex |
        Transcript show: 'Caught by outer handler: ', ex messageText; cr ]
```

Inner handler catches `ZeroDivide`, passes everything else to the outer handler.

## Custom Exception Behavior

You can customize how exceptions behave by overriding methods:

```smalltalk
Error subclass: #CustomError
    instanceVariableNames: 'details'
    ...

details
    ^ details

details: aString
    details := aString

messageText
    ^ 'CustomError: ', details
```

Now:

```smalltalk
(CustomError new details: 'Something specific') signal
```

The error message includes your custom details!

## Handling Exceptions in Classes

Encapsulate error handling in classes:

```smalltalk
Object subclass: #BankAccount
    instanceVariableNames: 'balance'
    ...

withdraw: amount
    balance < amount ifTrue: [
        (InsufficientFundsError new
            requestedAmount: amount;
            availableAmount: balance) signal ].
    balance := balance - amount.
    ^ amount
```

Usage:

```smalltalk
[
    account withdraw: 100 ]
    on: InsufficientFundsError
    do: [ :ex |
        Transcript show: 'Cannot withdraw ', ex requestedAmount printString,
            '. Only ', ex availableAmount printString, ' available.'; cr ]
```

The caller handles the exception with full context!

## Exception Best Practices

### 1. Be Specific

**Bad:**
```smalltalk
self error: 'Error'
```

**Good:**
```smalltalk
(InvalidArgumentError new messageText: 'Age must be positive') signal
```

Specific exceptions are easier to handle!

### 2. Provide Context

**Bad:**
```smalltalk
Error signal
```

**Good:**
```smalltalk
Error signal: 'Failed to process order #', orderId printString
```

Include relevant information in the error message!

### 3. Don't Swallow Exceptions

**Bad:**
```smalltalk
[ self doSomething ]
    on: Error
    do: [ :ex | "Do nothing" ]
```

Silent failures are dangerous!

**Good:**
```smalltalk
[ self doSomething ]
    on: Error
    do: [ :ex |
        Transcript show: 'Error: ', ex messageText; cr.
        self logError: ex ]
```

At least log it!

### 4. Clean Up Resources

Always use `ensure:` for cleanup:

```smalltalk
[
    resource := self openResource.
    self useResource: resource ]
    ensure: [ resource ifNotNil: [ resource close ] ]
```

### 5. Fail Fast

Don't let errors propagate silently. Signal early:

```smalltalk
initialize
    super initialize.
    self validateConfiguration ifFalse: [
        (ConfigurationError new) signal ]
```

### 6. Document Exceptions

In method comments, document what exceptions might be signaled:

```smalltalk
divide: numerator by: denominator
    "Divide numerator by denominator.
    Signals ZeroDivide if denominator is zero."
    denominator = 0 ifTrue: [ ZeroDivide signal ].
    ^ numerator / denominator
```

## Debugging Exceptions

When an exception is unhandled, the Debugger opens. You can:

1. **Examine the exception**: Inspect the exception object
2. **View the stack**: See where it was signaled
3. **Fix the code**: Edit methods in the Debugger
4. **Retry or resume**: Continue execution with the fix

See Chapter 21 (The Debugger) for details!

## Exception Handling Patterns

### The Transaction Pattern

```smalltalk
executeTransaction: aBlock
    "Execute aBlock as a transaction, rolling back on error"
    self beginTransaction.
    [ aBlock value.
      self commitTransaction ]
        on: Error
        do: [ :ex |
            self rollbackTransaction.
            ex pass ]
```

### The Fallback Pattern

```smalltalk
[ self primaryMethod ]
    on: Error
    do: [ :ex | self fallbackMethod ]
```

### The Retry with Backoff Pattern

```smalltalk
retryWithBackoff: aBlock maxAttempts: max
    | attempts delay |
    attempts := 0.
    delay := 1.
    [ attempts := attempts + 1.
      aBlock value ]
        on: Error
        do: [ :ex |
            attempts < max
                ifTrue: [
                    (Delay forSeconds: delay) wait.
                    delay := delay * 2.  "Exponential backoff"
                    ex retry ]
                ifFalse: [ ex pass ] ]
```

### The Circuit Breaker Pattern

```smalltalk
Object subclass: #CircuitBreaker
    instanceVariableNames: 'failureCount threshold isOpen'
    ...

execute: aBlock
    isOpen ifTrue: [ CircuitOpenError signal ].
    [
        | result |
        result := aBlock value.
        self recordSuccess.
        ^ result ]
        on: Error
        do: [ :ex |
            self recordFailure.
            failureCount >= threshold ifTrue: [ self open ].
            ex pass ]
```

## Try This!

Practice exception handling:

1. **Basic exception handling:**
   ```smalltalk
   [
       10 / 0 ]
       on: ZeroDivide
       do: [ :ex |
           Transcript show: 'Caught division by zero!'; cr.
           0 ]
   ```

2. **Custom exception:**
   ```smalltalk
   Error subclass: #MyCustomError
       instanceVariableNames: 'customData'
       ...

   customData: aString
       customData := aString

   (MyCustomError new customData: 'Test') signal
   ```

   Catch it:
   ```smalltalk
   [
       (MyCustomError new customData: 'Test') signal ]
       on: MyCustomError
       do: [ :ex |
           Transcript show: 'Caught: ', ex customData; cr ]
   ```

3. **Retry logic:**
   ```smalltalk
   | attempts |
   attempts := 0.
   [
       attempts := attempts + 1.
       attempts < 3 ifTrue: [ Error signal: 'Not yet!' ].
       Transcript show: 'Success on attempt ', attempts printString; cr ]
       on: Error
       do: [ :ex | ex retry ]
   ```

4. **Ensure cleanup:**
   ```smalltalk
   [
       Transcript show: 'Starting...'; cr.
       Error signal.
       Transcript show: 'This never prints'; cr ]
       ensure: [ Transcript show: 'Cleanup always runs'; cr ]
   ```

5. **File handling:**
   ```smalltalk
   readFileSafely: filename
       ^ [
           filename asFileReference contents ]
           on: FileDoesNotExistException
           do: [ :ex | 'File not found' ]
   ```

   Test:
   ```smalltalk
   self readFileSafely: '/nonexistent/file.txt'
   ```

6. **Multiple handlers:**
   ```smalltalk
   [
       "Your code here"
       self mightFail ]
       on: ZeroDivide
       do: [ :ex | Transcript show: 'Math error'; cr ]
       on: FileException
       do: [ :ex | Transcript show: 'File error'; cr ]
       on: Error
       do: [ :ex | Transcript show: 'Other error'; cr ]
   ```

7. **Resume:**
   ```smalltalk
   | result |
   result := [
       | value |
       value := nil.  "Simulate invalid input"
       value ifNil: [ Error signal: 'Value is nil!' ].
       value ]
       on: Error
       do: [ :ex | ex resume: 42 ].
   result  "Returns 42"
   ```

## Common Mistakes

### Catching Too Broadly

**Bad:**
```smalltalk
[ ... ]
    on: Exception  "Catches everything, including notifications!"
    do: [ :ex | ... ]
```

**Good:**
```smalltalk
[ ... ]
    on: Error  "Catches errors, not notifications"
    do: [ :ex | ... ]
```

### Not Cleaning Up

**Bad:**
```smalltalk
file := self openFile.
[ self processFile: file ]
    on: Error
    do: [ :ex | ... ].
"File not closed if exception occurs!"
```

**Good:**
```smalltalk
file := self openFile.
[ self processFile: file ]
    ensure: [ file close ]
```

### Swallowing Exceptions Silently

**Bad:**
```smalltalk
[ self doSomething ]
    on: Error
    do: [ :ex | ]  "Silent failure!"
```

Log or handle it!

### Not Using Specific Exception Classes

**Bad:**
```smalltalk
self error: 'Some error'  "Generic error"
```

**Good:**
```smalltalk
MySpecificError signal: 'Detailed message'
```

## The Philosophy

Smalltalk's exception system embodies its philosophy:

### Everything is an Object

Exceptions are objects. You can inspect them, modify them, send messages to them.

### Uniform Message Sending

Exceptions are signaled and handled via message sends (`signal`, `on:do:`, `retry`, `resume`).

### Interactive Development

Unhandled exceptions open the Debugger where you can fix the problem and continue!

## Looking Ahead

You now understand exception handling in Smalltalk! You can:
- Signal exceptions with `signal` and `error:`
- Catch exceptions with `on:do:`
- Create custom exception classes
- Use `retry`, `resume`, `pass` for sophisticated handling
- Ensure cleanup with `ensure:`
- Design robust, error-tolerant systems

In Chapter 25, we'll explore **Testing Your Code** with SUnit - Smalltalk's testing framework. You'll learn to write automated tests that verify your code works correctly, including testing error handling!

Then Chapter 26 covers **Packages and Code Organization** - how to structure larger projects.

Part VII is equipping you with professional development skills!

---

**Key Takeaways:**
- **Exceptions** are objects that represent errors or notifications
- **Signal** exceptions with `signal` or `error:`
- **Catch** exceptions with `on:do:`
- Exception hierarchy: `Exception` → `Error`, `Notification`, `Warning`
- The exception object provides context: `messageText`, `signalerContext`
- **`retry`** re-executes the protected block
- **`resume`** continues execution after the signal
- **`resume:`** resumes with a specific value
- **`pass`** propagates to outer handlers
- **`ensure:`** guarantees cleanup code runs
- **`ifCurtailed:`** runs only if interrupted
- Create custom exceptions by subclassing `Error`
- Be specific with exception types
- Always clean up resources with `ensure:`
- Don't swallow exceptions silently
- Document what exceptions methods might signal
- Unhandled exceptions open the Debugger for interactive fixing

---

[Previous: Chapter 23 - Protocols and Polymorphism](chapter-23-protocols-and-polymorphism.md) | [Next: Chapter 25 - Testing Your Code](chapter-25-testing.md)
