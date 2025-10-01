# Chapter 11: Classes - The Blueprint

This is it - the moment where you stop being just a user of Smalltalk and become a creator! Until now, you've been working with objects that came with Smalltalk: numbers, strings, collections. Now you'll learn to create your own types of objects by defining **classes**.

A **class** is a blueprint or template for creating objects. It describes what objects of that type know (their data) and what they can do (their behavior).

Think of a class like a cookie cutter. The class is the cutter, and objects are the cookies. One cookie cutter can make many cookies, and one class can create many objects.

## Opening the System Browser

To create classes, we need a different tool than the Playground. We need the **System Browser** - Smalltalk's IDE for browsing and editing classes.

Open it by:
- **Right-clicking** the World → `Browser`
- **Or press** `Ctrl+O` then `B` (Windows/Linux) or `Cmd+O` then `B` (macOS)

The System Browser opens, showing several panes:

1. **Package pane** (far left) - Groups of related classes
2. **Class pane** (second from left) - Classes in the selected package
3. **Protocol pane** (third) - Groups of related methods
4. **Method pane** (fourth) - Individual methods
5. **Code pane** (bottom) - The actual code

Don't worry if this looks overwhelming at first. We'll take it step by step.

## Your First Class

Let's create a simple class to represent a point in 2D space. (Smalltalk already has a Point class, but we'll make our own simple version to learn.)

### Step 1: Create a Package

Packages organize related classes. Let's create one for our learning projects:

1. In the System Browser, right-click in the **Package pane** (far left, probably empty or showing some existing packages)
2. Choose `New package...`
3. Name it `MyFirstClasses`
4. Press Enter

You now have a package! It appears in the package list.

### Step 2: Define the Class

With `MyFirstClasses` selected, look at the code pane at the bottom. You'll see a class definition template that looks something like:

```smalltalk
Object subclass: #NameOfSubclass
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyFirstClasses'
```

This is Smalltalk's way of saying "create a new class that's a subclass of Object."

Let's modify this to create our Point2D class:

```smalltalk
Object subclass: #Point2D
    instanceVariableNames: 'x y'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

**What this means:**

- `Object subclass: #Point2D` - Create a new class called Point2D that inherits from Object
- `instanceVariableNames: 'x y'` - Each Point2D object will have two pieces of data: x and y coordinates
- `classVariableNames: ''` - No class variables for now (we'll cover these later)
- `package: 'MyFirstClasses'` - This class belongs to the MyFirstClasses package

Now, with this code in the code pane:
- **Right-click** and choose `Accept` (or press `Ctrl+S` or `Cmd+S`)

Pharo compiles your class! You'll see `Point2D` appear in the class list.

**Congratulations! You just created your first class!**

## Understanding What You Created

You've created a blueprint that says:

"A Point2D object has two pieces of data: an x coordinate and a y coordinate."

But so far, Point2D objects can't do much. They have data slots (x and y), but no behavior yet. We'll add methods in Chapter 12.

For now, let's explore what you have.

## Creating Instances

A class is a factory for creating objects. Let's create some Point2D objects (we call them **instances** of the Point2D class).

Open a Playground (`Ctrl+O` `W` or `Cmd+O` `W`) and try:

```smalltalk
Point2D new
```

Execute this with "Print it" (`Ctrl+P` or `Cmd+P`).

You'll see something like: `a Point2D`

You just created an instance of your Point2D class! The `new` message creates a new object from the class.

Create multiple instances:

```smalltalk
| point1 point2 point3 |
point1 := Point2D new.
point2 := Point2D new.
point3 := Point2D new.
point1 == point2  "false - they're different objects!"
```

Each `new` message creates a separate object. They're all Point2Ds, but they're distinct objects in memory.

## Inspecting Your Objects

Let's inspect a Point2D:

```smalltalk
Point2D new inspect
```

An Inspector opens! Look at the left pane - you'll see `x` and `y` listed, both containing `nil`.

Your Point2D object has those two instance variables, but they're not initialized yet. We'll learn how to set them in the next chapters.

## The Class Hierarchy

Every class in Smalltalk inherits from another class, forming a tree. At the root of everything is `Object`.

```
Object
  └─ Point2D (your class!)
```

Your Point2D class inherits from Object, which means it automatically gets all of Object's behavior (like `new`, `inspect`, `printString`, etc.).

### Viewing the Hierarchy

In the System Browser, with Point2D selected, look at the class pane. You might see a small icon or way to view the hierarchy. Or use the Playground:

```smalltalk
Point2D superclass  "Returns Object"
```

```smalltalk
Point2D allSuperclasses  "Returns an OrderedCollection(Object ProtoObject)"
```

ProtoObject is the absolute root - even Object inherits from it!

## Class-Side vs Instance-Side

This is an important concept: classes have two sides:

1. **Instance side** - Methods that objects (instances) understand
2. **Class side** - Methods that the class itself understands

In the System Browser, you'll see buttons or tabs to switch between them.

### Instance Side

These are methods that instances of the class understand. For example, when we add a method to calculate the distance from the origin, that will be an instance method.

```smalltalk
Point2D new distanceFromOrigin  "Instance method"
```

### Class Side

These are methods that the class itself understands. The `new` method is a class method:

```smalltalk
Point2D new  "Class method - 'new' is sent to the class"
```

For now, focus on instance methods. We'll work with class methods more in later chapters.

## Naming Conventions

Smalltalk has strong naming conventions:

### Class Names

- **Always start with an uppercase letter**: `Point2D`, `BankAccount`, `Customer`
- **Use camelCase**: `HTTPServer`, not `HTTPserver` or `hTTPServer`
- **Be descriptive**: `Person`, not `P`
- **Singular, not plural**: `Book`, not `Books` (the class represents one book, even if you'll create many)

### Instance Variable Names

- **Always start with a lowercase letter**: `x`, `y`, `accountBalance`, `firstName`
- **Use camelCase**: `accountBalance`, not `account_balance`
- **Be descriptive**: `accountBalance`, not `ab`

## What Can You Do With Just a Class?

Even without methods, your class already has some capability:

```smalltalk
Point2D new  "Create an instance"
```

```smalltalk
Point2D new inspect  "Inspect an instance"
```

```smalltalk
Point2D new printString  "Get a string representation"
```

```smalltalk
Point2D new class  "Returns Point2D"
```

```smalltalk
Point2D new isKindOf: Point2D  "true"
```

```smalltalk
Point2D new isKindOf: Object  "true (Point2D inherits from Object)"
```

All this behavior comes from inheriting from Object!

## Creating Another Class

Let's create another class to solidify the concept. This time, we'll make a `Rectangle` class.

In the System Browser, with the `MyFirstClasses` package selected, modify the class definition template:

```smalltalk
Object subclass: #Rectangle
    instanceVariableNames: 'width height'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

Accept it (right-click → Accept, or `Ctrl+S` / `Cmd+S`).

Now you have two classes in your package!

Try creating a Rectangle:

```smalltalk
Rectangle new inspect
```

You'll see it has `width` and `height` instance variables, both `nil`.

## A More Complex Example

Let's create a `BankAccount` class with more data:

```smalltalk
Object subclass: #BankAccount
    instanceVariableNames: 'accountNumber balance ownerName'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

This creates a class where each BankAccount object has:
- An account number
- A balance
- An owner name

```smalltalk
BankAccount new inspect
```

You'll see all three instance variables, all set to `nil` initially.

## Instance Variables Are Private

Instance variables are **private** - they can only be accessed by methods defined in the class. This is called **encapsulation**.

Try this in the Playground:

```smalltalk
| account |
account := BankAccount new.
account accountNumber  "This will cause an error!"
```

The error says something like "BankAccount does not understand #accountNumber".

Why? Because we haven't defined a method called `accountNumber` yet! Even though the BankAccount has an instance variable called `accountNumber`, you can't access it directly from outside.

In Chapter 12, we'll learn how to create **accessor methods** that let you read and write instance variables.

## Classes Are Objects Too!

Remember: in Smalltalk, everything is an object - even classes!

```smalltalk
Point2D class  "Returns 'Point2D class'"
```

```smalltalk
Point2D isKindOf: Class  "true"
```

You can send messages to classes:

```smalltalk
Point2D methodNames  "Returns a Set of method names"
```

```smalltalk
Point2D instVarNames  "Returns #('x' 'y')"
```

```smalltalk
Point2D superclass  "Returns Object"
```

Classes are first-class objects, just like numbers and strings!

## The Complete Picture

Let's review what happens when you define a class:

```smalltalk
Object subclass: #Point2D
    instanceVariableNames: 'x y'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

1. **You send a message** - `subclass:instanceVariableNames:classVariableNames:package:` is sent to Object
2. **Object creates a new class** - A class object named Point2D is created
3. **The class is configured** - It's told it has instance variables x and y
4. **The class is registered** - Smalltalk adds it to the MyFirstClasses package
5. **You can now create instances** - `Point2D new` creates objects with x and y slots

## Viewing Your Class Definition

In the System Browser, click on your class name (Point2D). The class definition appears in the code pane.

You can edit the class definition here. For example, to add a new instance variable:

```smalltalk
Object subclass: #Point2D
    instanceVariableNames: 'x y color'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

Accept this change (Ctrl+S or Cmd+S). Now Point2D objects have three instance variables!

**Warning**: If you already created Point2D objects before adding `color`, they'll be automatically updated. Smalltalk handles this for you - existing objects get the new instance variable (set to `nil`).

## Deleting a Class

To delete a class:
1. Right-click on the class name in the System Browser
2. Choose `Remove`
3. Confirm

The class and all its methods are deleted. (But any existing instances in memory remain - they're now instances of a deleted class!)

For now, keep your classes. We'll use them in the next chapter.

## Comments in Class Definitions

You can add a **class comment** to document what your class is for. In the System Browser:

1. Select your class (e.g., Point2D)
2. Click the `Comments` button or tab (usually near the code pane)
3. Type your comment:

```
Point2D represents a point in 2D space with x and y coordinates.

Example usage:
    point := Point2D new.
```

4. Accept (Ctrl+S or Cmd+S)

Now anyone browsing your class can read this documentation!

## Common Mistakes

### Forgetting the `#` Before Class Name

```smalltalk
Object subclass: Point2D  "Wrong!"
```

Should be:

```smalltalk
Object subclass: #Point2D  "Correct - note the #"
```

The `#` makes it a symbol, not a variable reference.

### Capitalizing Instance Variables

```smalltalk
instanceVariableNames: 'X Y'  "Wrong - should be lowercase"
```

Should be:

```smalltalk
instanceVariableNames: 'x y'  "Correct"
```

### Forgetting to Accept

After typing your class definition, you must **Accept** it (Ctrl+S or Cmd+S). If you just close the browser, your changes are lost!

### Using Spaces in Instance Variable Names

```smalltalk
instanceVariableNames: 'account number'  "Wrong - this creates TWO variables: 'account' and 'number'"
```

Should be:

```smalltalk
instanceVariableNames: 'accountNumber'  "Correct - one variable"
```

## Try This!

Practice creating classes:

1. **Create a Person class:**
   ```smalltalk
   Object subclass: #Person
       instanceVariableNames: 'firstName lastName age'
       classVariableNames: ''
       package: 'MyFirstClasses'
   ```

   Then create some people:
   ```smalltalk
   | person |
   person := Person new.
   person inspect
   ```

2. **Create a Book class:**
   ```smalltalk
   Object subclass: #Book
       instanceVariableNames: 'title author pages'
       classVariableNames: ''
       package: 'MyFirstClasses'
   ```

3. **Create a Circle class:**
   ```smalltalk
   Object subclass: #Circle
       instanceVariableNames: 'radius'
       classVariableNames: ''
       package: 'MyFirstClasses'
   ```

4. **Inspect different classes:**
   ```smalltalk
   Person new inspect.
   Book new inspect.
   Circle new inspect
   ```

5. **Check class relationships:**
   ```smalltalk
   Person superclass.
   Person allSuperclasses.
   Person subclasses
   ```

6. **See what a class knows about itself:**
   ```smalltalk
   Person instVarNames.
   Person methodNames
   ```

7. **Create multiple instances:**
   ```smalltalk
   | people |
   people := OrderedCollection new.
   10 timesRepeat: [ people add: Person new ].
   people inspect
   ```

## What's Next?

You've created classes - blueprints for objects! But your objects can't do much yet because they don't have any methods (behavior).

In Chapter 12, we'll learn how to add **methods** to your classes. Methods are where you define what your objects can do: calculate, respond to questions, perform actions, and more.

We'll learn:
- How to define methods
- Different types of methods (accessors, mutators, computed values)
- How to write good method names
- How methods access instance variables
- How methods can call other methods

Get ready - this is where your objects come alive!

## Looking Back

Think about how far you've come:

- **Chapter 1-3**: You learned about programming and Smalltalk basics
- **Chapter 4-7**: You understood objects, messages, types, and variables
- **Chapter 8-10**: You learned control flow with conditionals, blocks, and loops
- **Chapter 11**: You created your own classes!

You're not just using Smalltalk anymore - you're creating new types of objects. You're designing abstractions. You're thinking like a programmer!

Next, we'll make your objects actually *do* things by adding methods.

---

**Key Takeaways:**
- A **class** is a blueprint for creating objects
- Classes define **instance variables** (data) that objects will have
- Create classes using the System Browser
- Use `Object subclass: #ClassName instanceVariableNames: 'var1 var2' ...`
- Class names start with uppercase, instance variables with lowercase
- Create instances with `ClassName new`
- Instance variables are private - accessible only within the class
- Classes inherit from other classes, forming a hierarchy
- Every class ultimately inherits from Object
- Classes are objects too - you can send them messages
- Use packages to organize related classes
- Always Accept (Ctrl+S / Cmd+S) your changes!

---

[Previous: Chapter 10 - Loops and Iteration](chapter-10-loops-and-iteration.md) | [Next: Chapter 12 - Methods](chapter-12-methods.md)
