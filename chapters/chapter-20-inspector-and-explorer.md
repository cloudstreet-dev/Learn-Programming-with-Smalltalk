# Chapter 20: The Inspector and Explorer

In Chapter 19, you learned to navigate code with the System Browser. Now you'll learn to explore **live objects** with the Inspector and Explorer - tools that let you peer inside running objects, examine their state, modify their values, and even execute code within their context.

These tools embody Smalltalk's philosophy: everything is an object, and you can inspect and interact with any object at any time. This makes debugging easier, learning faster, and understanding deeper.

## What is the Inspector?

The **Inspector** is a tool for examining the internal state of an object. When you inspect an object, you see:
- Its class
- Its instance variables and their values
- Methods you can call on it
- The ability to modify variables
- The ability to execute code as that object

Think of it like:
- **An X-ray** - See inside an object
- **A microscope** - Examine structure in detail
- **A laboratory** - Experiment with the object interactively

## Opening an Inspector

There are several ways to inspect an object:

### Method 1: From the Playground

```smalltalk
'hello' inspect
```

Select this code and "Do it" (`Ctrl+D` / `Cmd+D`). An Inspector opens showing the string 'hello'.

### Method 2: Inspect It

```smalltalk
'hello'
```

Select and press **Inspect it** (`Ctrl+I` / `Cmd+I`).

### Method 3: Right-Click Result

In the Playground:
```smalltalk
OrderedCollection new add: 1; add: 2; add: 3; yourself
```

Print it, then right-click the result and choose `Inspect`.

### Method 4: From Debugger

When debugging (Chapter 21), you can inspect any variable.

## The Inspector Interface

An Inspector has several parts:

### Left Pane: Instance Variables

Shows:
- `self` - The object itself
- `all inst vars` - View all variables at once
- Each instance variable by name

Click on an entry to see its value in the right pane.

### Top-Right Pane: Value Display

Shows the value of the selected variable:
- Simple values: numbers, strings, booleans
- Complex objects: clickable links to inspect further

### Bottom-Right Pane: Evaluation Context

A code editor where you can execute code **as the inspected object**. When you write code here, `self` refers to the inspected object!

## Basic Inspection Example

Let's inspect a simple object:

```smalltalk
'Hello, World!' inspect
```

In the Inspector:

### Left Pane Shows:
- `self` - The string itself
- `all inst vars` - Click to see all variables

String objects don't have visible instance variables (they're primitive), but you can still interact with them.

### Evaluation Pane:

Try executing code:

```smalltalk
self size
```

Execute with `Ctrl+P` / `Cmd+P` (Print it). Result: `13`

```smalltalk
self asUppercase
```

Result: `'HELLO, WORLD!'`

```smalltalk
self, ' How are you?'
```

Result: `'Hello, World! How are you?'`

You're sending messages to the object **as if you were inside it**!

## Inspecting Complex Objects

Let's inspect something more interesting:

```smalltalk
| dict |
dict := Dictionary new.
dict at: 'name' put: 'Alice'.
dict at: 'age' put: 30.
dict at: 'city' put: 'New York'.
dict inspect
```

In the Inspector:

### Left Pane:
- `self` - The dictionary
- `all inst vars` - Shows all variables
- `array` - The internal storage
- `tally` - Number of entries

Click on `array` to see the internal array structure!

### Evaluation Pane:

```smalltalk
self at: 'name'  "Returns 'Alice'"
```

```smalltalk
self keys  "Returns #('name' 'age' 'city')"
```

```smalltalk
self at: 'email' put: 'alice@example.com'.
self  "Now includes email!"
```

You modified the object from the Inspector!

## Inspecting Custom Objects

Create a custom class:

```smalltalk
Object subclass: #Person
    instanceVariableNames: 'firstName lastName age'
    classVariableNames: ''
    package: 'MyApp'
```

Add accessor methods and initialize:

```smalltalk
initialize
    super initialize.
    firstName := ''.
    lastName := ''.
    age := 0
```

Create and inspect:

```smalltalk
| person |
person := Person new.
person firstName: 'Alice'.
person lastName: 'Smith'.
person age: 30.
person inspect
```

In the Inspector:

### Left Pane:
- `self` - The Person object
- `all inst vars` - All variables
- `firstName` - 'Alice'
- `lastName` - 'Smith'
- `age` - 30

Click each to see values!

### Modifying Variables

Click on `age`. The value shows `30`.

In the evaluation pane, modify it:

```smalltalk
age := 35.
self
```

The age is now 35! You directly modified the instance variable.

**Note**: This bypasses setter methods. Use with caution!

### Calling Methods

```smalltalk
self firstName  "Returns 'Alice'"
```

```smalltalk
self class  "Returns Person"
```

```smalltalk
self class allInstVarNames  "Returns #('firstName' 'lastName' 'age')"
```

## The "All Instance Variables" View

Click `all inst vars` in the left pane.

The right pane shows a table with all variables and their values:

| Variable | Value |
|----------|-------|
| firstName | 'Alice' |
| lastName | 'Smith' |
| age | 35 |

This is a quick overview of the object's entire state.

## Drilling Down

Objects often contain other objects. You can drill down:

```smalltalk
| company |
company := Dictionary new.
company at: 'name' put: 'Acme Corp'.
company at: 'employees' put: (OrderedCollection new
    add: (Dictionary new at: 'name' put: 'Alice'; yourself);
    add: (Dictionary new at: 'name' put: 'Bob'; yourself);
    yourself).
company inspect
```

In the Inspector:

1. Click on `company` → see the dictionary
2. Click on the value for `'employees'` → opens Inspector on the OrderedCollection
3. Click on first employee → opens Inspector on that dictionary
4. See `'name' → 'Alice'`

You can **navigate through a graph of objects**, inspecting each one!

## Inspector Actions

Right-click in the Inspector for powerful actions:

### Browse Class

Right-click on `self` → `Browse class`

Opens System Browser on the object's class. Great for seeing the code behind the object!

### Explore

Right-click → `Explore`

Opens an Explorer (see below) instead of an Inspector.

### Copy

Copy values or code.

### Store

Save the object to a global variable for later use.

## What is the Explorer?

The **Explorer** is similar to the Inspector but shows a **tree view** of the object and all its nested objects.

Open an Explorer:

```smalltalk
| data |
data := OrderedCollection new
    add: 'Alice';
    add: 'Bob';
    add: (Dictionary new at: 'name' put: 'Charlie'; yourself);
    yourself.
data explore
```

Or use `Explore it` (`Ctrl+Shift+I` / `Cmd+Shift+I`) in the Playground.

### Explorer Interface

The Explorer shows:

- **Left pane**: Tree structure of the object
- **Right pane**: Code evaluation context

### Tree View

Expand nodes to see nested objects:

```
OrderedCollection
├─ [1] 'Alice'
├─ [2] 'Bob'
└─ [3] Dictionary
    └─ 'name' → 'Charlie'
```

Click on any node to see its value and execute code in that context.

### When to Use Explorer vs Inspector

- **Inspector**: For examining one object in detail
- **Explorer**: For navigating complex object graphs with many nested objects

## Practical Examples

### Example 1: Debugging Data Structures

You have a data structure that's not working:

```smalltalk
| users |
users := OrderedCollection new.
users add: (Dictionary new at: 'name' put: 'Alice'; at: 'age' put: 30; yourself).
users add: (Dictionary new at: 'name' put: 'Bob'; yourself).  "Oops, forgot age!"
users inspect
```

Inspect `users`:
1. Click on first user → see name and age
2. Click on second user → see name but **no age key**!
3. Found the problem!

Fix it:

```smalltalk
self at: 2 :: at: 'age' put: 25
```

Or in the evaluation pane:

```smalltalk
(self at: 2) at: 'age' put: 25.
self
```

Now the second user has an age!

### Example 2: Understanding Collections

Inspect different collections to understand their internal structure:

```smalltalk
Array with: 1 with: 2 with: 3 inspect
```

vs

```smalltalk
OrderedCollection new add: 1; add: 2; add: 3; yourself inspect
```

vs

```smalltalk
Set new add: 1; add: 2; add: 3; yourself inspect
```

Look at their internal variables. See how they're implemented differently!

### Example 3: Exploring Class Hierarchies

```smalltalk
OrderedCollection allSuperclasses explore
```

Expand the tree to see the entire hierarchy:

```
OrderedCollection
SequenceableCollection
Collection
Object
ProtoObject
```

### Example 4: Modifying Objects in Place

```smalltalk
| counter |
counter := Counter new.
counter increment.
counter increment.
counter inspect
```

In the Inspector, modify the count:

```smalltalk
count := 100.
self
```

The counter now has a count of 100, bypassing the increment method!

**Use case**: Testing, debugging, setting up scenarios.

## Inspector Tips and Tricks

### Tip 1: Inspect Intermediate Results

Don't just print results; inspect them:

```smalltalk
(1 to: 10) select: [ :n | n isPrime ] inspect
```

Examine the resulting collection closely.

### Tip 2: Modify and Test

Modify an object's state and test behavior:

```smalltalk
| account |
account := BankAccount new balance: 100.
account inspect.

"In Inspector, set balance to negative:"
balance := -50.
self canWithdraw: 100  "Test with negative balance!"
```

### Tip 3: Understand Instance Variables

Don't know what an instance variable does? Inspect an object and see the values!

### Tip 4: Learn by Exploring

Curious how String works? Inspect one!

```smalltalk
'hello' inspect
```

See the internal representation (might be primitive, but you can still send messages).

### Tip 5: Save Objects to Globals

In the Inspector evaluation pane:

```smalltalk
Smalltalk globals at: #MyTestObject put: self
```

Now you can access `MyTestObject` from anywhere!

## Advanced: Custom Inspector Views

You can customize how your objects appear in the Inspector by implementing:

```smalltalk
inspectorFields
    "Return a collection of fields to show in the Inspector"
    ^ #(firstName lastName age fullName)
```

```smalltalk
fullName
    ^ firstName , ' ' , lastName
```

Now inspecting a Person shows `fullName` as a computed field!

Pharo's Inspector is extensible - advanced users can create custom views for complex objects.

## Common Use Cases

### 1. Debugging

When something goes wrong, inspect objects to see their state:

```smalltalk
"Code that fails:"
result := self calculate.
result inspect  "What is result? Why did it fail?"
```

### 2. Learning

Inspect system objects to understand how they work:

```smalltalk
Date today inspect.
Time now inspect.
Point x: 10 y: 20 inspect
```

### 3. Testing

Set up complex test scenarios:

```smalltalk
| testObject |
testObject := MyClass new.
testObject inspect.
"Manually set variables to unusual values, then test methods"
```

### 4. Prototyping

Build objects interactively:

```smalltalk
| config |
config := Dictionary new inspect.
"Add keys and values in the Inspector"
"Test configuration without writing code"
```

## Inspecting Special Objects

### nil

```smalltalk
nil inspect
```

nil is an object (instance of UndefinedObject)!

### Booleans

```smalltalk
true inspect.
false inspect
```

Each is an instance of `True` and `False` classes.

### Classes

```smalltalk
String inspect
```

Classes are objects too! See their methods, superclass, subclasses, etc.

### Blocks

```smalltalk
[ :x | x * 2 ] inspect
```

Blocks are objects (instances of BlockClosure). See their compiled code, outer context, etc.

## Inspector vs System Browser

Don't confuse these tools:

### System Browser
- Browse and edit **code** (classes and methods)
- Static view of class definitions
- For writing and organizing code

### Inspector
- Examine **live objects** (instances)
- Dynamic view of runtime state
- For understanding and debugging

They complement each other!

## Keyboard Shortcuts

In the Inspector:

- **Ctrl+P / Cmd+P** - Print it (execute and show result)
- **Ctrl+D / Cmd+D** - Do it (execute without printing)
- **Ctrl+I / Cmd+I** - Inspect result
- **Ctrl+B** - Browse class
- **Tab** - Move between panes

## Multiple Inspectors

You can have many Inspectors open simultaneously:

- One for each object you're examining
- Compare states side-by-side
- Track changes over time

## Try This!

Practice with the Inspector:

1. **Inspect basic objects:**
   ```smalltalk
   42 inspect.
   'Hello' inspect.
   true inspect.
   nil inspect.
   #symbol inspect.
   $A inspect  "A character"
   ```

2. **Inspect collections:**
   ```smalltalk
   #(1 2 3) inspect.
   (1 to: 100) inspect.
   (Dictionary new at: 'a' put: 1; at: 'b' put: 2; yourself) inspect
   ```

3. **Create and inspect a custom object:**
   ```smalltalk
   Object subclass: #Book
       instanceVariableNames: 'title author pages'
       ...

   | book |
   book := Book new.
   book title: 'Smalltalk Best Practices'.
   book author: 'Expert'.
   book pages: 300.
   book inspect
   ```

   Examine each variable. Modify them. Call methods.

4. **Explore nested structures:**
   ```smalltalk
   | library |
   library := Dictionary new.
   library at: 'fiction' put: (OrderedCollection new
       add: 'Book A';
       add: 'Book B';
       yourself).
   library at: 'nonfiction' put: (OrderedCollection new
       add: 'Book C';
       yourself).
   library explore
   ```

   Navigate through the tree!

5. **Inspect class objects:**
   ```smalltalk
   String inspect.
   OrderedCollection inspect.
   Object inspect
   ```

   See their methods, hierarchy, etc.

6. **Modify an object:**
   ```smalltalk
   | point |
   point := Point x: 10 y: 20.
   point inspect.
   "In Inspector, change x to 100:"
   x := 100.
   self
   ```

7. **Inspect method results:**
   ```smalltalk
   String allSubclasses inspect.
   Collection allSubclasses explore
   ```

8. **Save an object globally:**
   ```smalltalk
   | myData |
   myData := OrderedCollection new add: 1; add: 2; yourself.
   myData inspect.
   "In Inspector:"
   Smalltalk globals at: #MyData put: self.
   "Now access from anywhere:"
   MyData
   ```

## Common Mistakes

### Confusing Inspector with Browser

The Inspector is for **objects** (runtime), not **code** (source).

### Modifying Variables Without Understanding

Directly setting instance variables can break object invariants. Be careful!

### Forgetting to Save Globals

If you set a global in the Inspector but don't save the Image, it's lost when you quit.

### Over-Reliance on Inspection

Don't replace proper debugging with "inspect everything". Use the Inspector as one tool among many.

## Safety and Limitations

### Safety

The Inspector is generally safe:
- Changes are local to that object
- You can't break the system (usually!)
- Worst case: restart the Image

### Limitations

- Can't inspect very low-level objects (VM-level primitives)
- Some objects have protected state
- Modifying objects can lead to inconsistent state

## The Philosophy

The Inspector embodies Smalltalk's core ideas:

### Transparency

You can see inside **everything**. No hidden state, no black boxes.

### Interactivity

You can **interact** with objects, not just observe them.

### Liveness

The Inspector shows **living objects** in a **running system**. You're not looking at a dump or snapshot; you're interacting with the real thing.

### Experimentation

The Inspector encourages **exploration and experimentation**. Try things! See what happens!

## Looking Ahead

You now understand the Inspector and Explorer - powerful tools for examining and interacting with live objects! You can:
- Inspect any object to see its state
- Drill down through nested objects
- Modify variables directly
- Execute code in the context of an object
- Explore object graphs with the Explorer

In Chapter 21, we'll meet the **Debugger** - perhaps Smalltalk's most famous tool. The Debugger lets you step through code, examine stack frames, modify methods while debugging, and even continue execution with fixes applied. It's revolutionary!

Then in Chapter 22, we'll explore the **Finder** and **Spotter** - tools for discovering code, finding methods, and navigating the system quickly.

Part VI is revealing the tools that make Smalltalk development uniquely powerful and productive!

---

**Key Takeaways:**
- The **Inspector** lets you examine live objects in detail
- Shows instance variables, their values, and the object's class
- The **evaluation pane** executes code as the inspected object
- Use `inspect` or **Inspect it** (`Ctrl+I` / `Cmd+I`) to open
- You can **modify instance variables** directly (use with care!)
- Click on instance variables to see their values
- Click on nested objects to inspect them (drill down)
- The **Explorer** shows object graphs as a tree
- Use Explorer for complex nested structures
- Inspector is for **runtime objects**, Browser is for **source code**
- Inspect system objects to understand how they work
- Great for debugging, learning, and prototyping
- Right-click for actions: Browse class, Explore, etc.
- Multiple Inspectors can be open simultaneously
- The Inspector embodies Smalltalk's philosophy of transparency and interactivity

---

[Previous: Chapter 19 - The System Browser](chapter-19-system-browser.md) | [Next: Chapter 21 - The Debugger - Your New Best Friend](chapter-21-debugger.md)
