# Chapter 13: Instance Variables - Giving Objects Memory

You've been using instance variables in the last two chapters, but now it's time to really understand them. **Instance variables** are what give objects memory - the ability to remember information between method calls. They're fundamental to object-oriented programming.

In this chapter, we'll explore instance variables in depth: what they are, how they work, when to use them, and important concepts like encapsulation and object identity.

## What Are Instance Variables?

An **instance variable** is a named piece of data that belongs to an object. Each object (instance) of a class has its own copy of each instance variable.

Think of an object like a person, and instance variables like the person's attributes:
- Name
- Age
- Height
- Eye color

Each person has their own name, age, etc. Similarly, each object has its own instance variable values.

## Declaring Instance Variables

You declare instance variables when you define a class:

```smalltalk
Object subclass: #Person
    instanceVariableNames: 'firstName lastName age'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

This says: "Every Person object will have three instance variables: firstName, lastName, and age."

## Each Object Has Its Own Values

Let's see this in action:

```smalltalk
| person1 person2 |
person1 := Person new.
person1 firstName: 'Alice'.
person1 age: 30.

person2 := Person new.
person2 firstName: 'Bob'.
person2 age: 25.

person1 firstName  "Returns 'Alice'"
person2 firstName  "Returns 'Bob'"
```

Even though both are Person objects, they have different values for their instance variables. Each object has its own memory!

## Instance Variables Are Private

Instance variables can only be accessed from inside the object - from methods defined in the class. You cannot access them directly from outside:

```smalltalk
| person |
person := Person new.
person firstName  "Error if no firstName method exists!"
```

This is **encapsulation** - hiding the internal details of an object. To access instance variables from outside, you need accessor methods:

```smalltalk
firstName
    "Return the first name"
    ^ firstName
```

Now `person firstName` works because you're calling a method, not accessing the variable directly.

## Why Encapsulation Matters

Encapsulation has several benefits:

### 1. Flexibility

You can change how data is stored internally without affecting code that uses your class:

```smalltalk
"Originally:"
Object subclass: #Person
    instanceVariableNames: 'firstName lastName'
    ...
```

Later, you decide to store the full name as one string:

```smalltalk
"Later:"
Object subclass: #Person
    instanceVariableNames: 'fullName'
    ...
```

You can keep the same accessor methods, just change how they work:

```smalltalk
firstName
    "Extract first name from full name"
    ^ fullName substrings first
```

Code using `person firstName` still works - it doesn't know or care that the internal representation changed!

### 2. Validation

You can validate values when they're set:

```smalltalk
age: aNumber
    "Set the age, ensuring it's valid"
    aNumber < 0 ifTrue: [ self error: 'Age cannot be negative' ].
    aNumber > 150 ifTrue: [ self error: 'Age seems unrealistic' ].
    age := aNumber
```

If instance variables were public, anyone could set invalid values!

### 3. Side Effects

You can do additional work when a value changes:

```smalltalk
balance: newBalance
    "Set the balance and log the change"
    | oldBalance |
    oldBalance := balance.
    balance := newBalance.
    self logBalanceChange: oldBalance to: newBalance
```

### 4. Computed Values

Sometimes what looks like an instance variable is actually computed:

```smalltalk
fullName
    "Return the full name"
    ^ firstName , ' ' , lastName
```

Users of the class can't tell if `fullName` is stored or computed - that's encapsulation!

## Naming Instance Variables

Instance variable names should:
- **Start with lowercase**: `firstName`, not `FirstName`
- **Be descriptive**: `accountNumber`, not `an`
- **Use camelCase**: `dateOfBirth`, not `date_of_birth`
- **Avoid abbreviations**: `description`, not `desc`

## Initialization

Instance variables start as `nil`. Good practice is to initialize them:

```smalltalk
initialize
    "Initialize a new person"
    super initialize.
    firstName := ''.
    lastName := ''.
    age := 0
```

This ensures objects start in a known, valid state.

## Instance Variables vs Temporary Variables

Let's clarify the difference:

### Instance Variables
- Declared in the class definition
- Belong to the object
- Live as long as the object lives
- Accessible from any method in the class
- Different for each instance

### Temporary Variables
- Declared in a method with `| varName |`
- Exist only while the method runs
- Gone when the method finishes
- Only accessible within that method
- Not shared between objects

Example:

```smalltalk
calculateArea
    "Calculate the area of a rectangle"
    | area |  "Temporary variable"
    area := width * height.  "width and height are instance variables"
    ^ area
```

`area` is temporary - it disappears after the method finishes. But `width` and `height` are instance variables - they persist.

## Object Identity

Instance variables are key to object identity. Two objects are different if they have different identity, even if their values are the same:

```smalltalk
| person1 person2 |
person1 := Person new firstName: 'Alice'; age: 30; yourself.
person2 := Person new firstName: 'Alice'; age: 30; yourself.

person1 = person2  "Depends on how = is implemented"
person1 == person2  "false - they're different objects!"
```

The `==` operator tests if two variables refer to the same object. Even though person1 and person2 have identical values, they're different objects in memory, with their own instance variables.

## Shared vs Independent State

### Independent State

Most instance variables hold independent values:

```smalltalk
| account1 account2 |
account1 := BankAccount new balance: 1000.
account2 := BankAccount new balance: 2000.

account1 deposit: 500.
account1 balance  "1500"
account2 balance  "2000 - unchanged!"
```

Each object's balance is independent.

### Shared State (with Care)

Sometimes multiple objects reference the same object:

```smalltalk
| person1 person2 address |
address := Address new street: 'Main St'; city: 'Springfield'.
person1 := Person new address: address.
person2 := Person new address: address.

person1 address street: 'Oak Ave'.
person2 address street  "Returns 'Oak Ave' - they share the same address object!"
```

Both persons have an `address` instance variable, but they both point to the same Address object. Modifying it affects both!

To avoid this, make copies:

```smalltalk
person2 := Person new address: address copy.
```

## Lazy Initialization

Sometimes you don't want to create an object until it's needed:

```smalltalk
transactions
    "Return the list of transactions, creating it if necessary"
    transactions ifNil: [ transactions := OrderedCollection new ].
    ^ transactions
```

The first time `transactions` is called, it creates the collection. After that, it returns the existing one.

This is useful for expensive objects you might not always need.

## Default Values

Choose sensible defaults in `initialize`:

```smalltalk
initialize
    "Initialize a new bank account"
    super initialize.
    balance := 0.  "Start with zero balance"
    accountNumber := ''.
    transactions := OrderedCollection new.
    isActive := true  "New accounts are active"
```

## Mutability

### Mutable Objects

Most objects are **mutable** - their instance variables can change:

```smalltalk
| point |
point := Point2D new x: 10; y: 20.
point x: 30.  "Changed!"
point y: 40.  "Changed!"
```

### Immutable Objects

Some objects are **immutable** - once created, they never change. Numbers and strings are examples:

```smalltalk
5 + 3  "Doesn't change 5, returns a new object: 8"
'Hello' , ' World'  "Doesn't change 'Hello', returns new string"
```

You can make your own immutable classes by not providing setter methods:

```smalltalk
Object subclass: #ImmutablePoint
    instanceVariableNames: 'x y'
    ...

"Only provide getters, no setters:"
x
    ^ x

y
    ^ y

"Provide initialization:"
x: xValue y: yValue
    x := xValue.
    y := yValue.
    ^ self

"Create with a class method:"
"Class side:"
x: xValue y: yValue
    ^ self new x: xValue y: yValue
```

Now users must set values at creation:

```smalltalk
point := ImmutablePoint x: 10 y: 20.
point x: 30  "Error! No setter method!"
```

## Instance Variables and Inheritance

Instance variables are inherited. If a class defines instance variables, all its subclasses have those variables too:

```smalltalk
Object subclass: #Vehicle
    instanceVariableNames: 'make model year'
    ...

Vehicle subclass: #Car
    instanceVariableNames: 'numberOfDoors'
    ...
```

A Car object has ALL of these instance variables:
- make (from Vehicle)
- model (from Vehicle)
- year (from Vehicle)
- numberOfDoors (from Car)

We'll explore inheritance more in Chapter 15.

## Instance Variables Are Not Slots

In some object systems, you can access "slots" or "fields" by name at runtime. Smalltalk doesn't work that way. Instance variables are fixed at class definition time and accessed by name in methods.

You can't do:

```smalltalk
object getVariable: 'firstName'  "Doesn't work!"
```

You must use methods:

```smalltalk
object firstName  "Works if there's a firstName method"
```

## Getters and Setters: To Provide or Not?

Not every instance variable needs public accessors. Consider carefully:

### Always Private
- Implementation details users shouldn't know about
- Cached values
- Internal state

```smalltalk
Object subclass: #WebPage
    instanceVariableNames: 'url cachedContent lastFetchTime'
    ...
```

Users should call `content`, not access `cachedContent` directly.

### Read-Only
- Properties that shouldn't be changed after creation
- Computed values

```smalltalk
Object subclass: #Customer
    instanceVariableNames: 'customerId name email'
    ...
```

Provide `customerId` getter but no setter - IDs shouldn't change after creation.

### Read-Write
- Properties users need to get and set

```smalltalk
Object subclass: #Settings
    instanceVariableNames: 'theme fontSize language'
    ...
```

Provide both getters and setters for configuration values.

## A Complete Example

Let's build a well-designed class that uses instance variables properly:

```smalltalk
Object subclass: #TodoItem
    instanceVariableNames: 'description completed createdAt dueDate'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

Methods:

```smalltalk
initialize
    "Initialize a new todo item"
    super initialize.
    description := ''.
    completed := false.
    createdAt := DateAndTime now.
    dueDate := nil
```

```smalltalk
description
    "Return the description"
    ^ description
```

```smalltalk
description: aString
    "Set the description"
    description := aString
```

```smalltalk
isCompleted
    "Return whether this item is completed"
    ^ completed
```

```smalltalk
markCompleted
    "Mark this item as completed"
    completed := true
```

```smalltalk
markIncomplete
    "Mark this item as incomplete"
    completed := false
```

```smalltalk
dueDate
    "Return the due date"
    ^ dueDate
```

```smalltalk
dueDate: aDate
    "Set the due date"
    dueDate := aDate
```

```smalltalk
isOverdue
    "Return whether this item is overdue"
    dueDate ifNil: [ ^ false ].
    ^ dueDate < Date today and: [ completed not ]
```

```smalltalk
daysSinceCreation
    "Return the number of days since this item was created"
    ^ (DateAndTime now - createdAt) days
```

Notice:
- `completed` uses `isCompleted` (getter), `markCompleted` and `markIncomplete` (setters with meaningful names)
- `createdAt` has no public accessor - it's set once in `initialize`
- `isOverdue` is computed, not stored
- Validation and logic in methods, not exposed to users

Usage:

```smalltalk
| item |
item := TodoItem new.
item description: 'Write Chapter 13'.
item dueDate: Date today + 7.
item isOverdue  "false - not due yet"
item markCompleted.
item isCompleted  "true"
```

## Memory Considerations

Each instance variable adds to an object's memory footprint. For most cases, this is negligible. But if you're creating millions of objects:

```smalltalk
"Each Person object has 3 instance variables:"
Object subclass: #Person
    instanceVariableNames: 'firstName lastName age'
    ...

"Creating a million Persons uses more memory than:"
Object subclass: #SimplePerson
    instanceVariableNames: 'name'
    ...
```

In practice, worry about correctness and design first, performance later.

## Try This!

Practice with instance variables:

1. **Create a Book class:**
   ```smalltalk
   Object subclass: #Book
       instanceVariableNames: 'title author isbn pages currentPage'
       ...
   ```

   Add methods:
   - `initialize` - Set defaults
   - Getters and setters for each variable
   - `isFinished` - Is currentPage >= pages?
   - `percentComplete` - What percentage is read?
   - `turnPage` - Increment currentPage

2. **Create a Timer class:**
   ```smalltalk
   Object subclass: #Timer
       instanceVariableNames: 'startTime isRunning'
       ...
   ```

   Add methods:
   - `initialize`
   - `start` - Record startTime, set isRunning
   - `stop` - Clear isRunning
   - `elapsed` - Calculate time since start
   - `reset` - Clear everything

3. **Create a Temperature class that stores Celsius:**
   ```smalltalk
   Object subclass: #Temperature
       instanceVariableNames: 'celsius'
       ...
   ```

   But provide methods:
   - `celsius` and `celsius:`
   - `fahrenheit` and `fahrenheit:` (convert!)
   - `kelvin` and `kelvin:` (convert!)

   The user can set temperature in any unit, but it's stored as Celsius internally.

4. **Create a Counter with limits:**
   ```smalltalk
   Object subclass: #BoundedCounter
       instanceVariableNames: 'value minimum maximum'
       ...
   ```

   The value can't go below minimum or above maximum.

## Common Mistakes

### Accessing Instance Variables Before Initialization

```smalltalk
| account |
account := BankAccount new.
account balance  "Returns nil if not initialized!"
```

Always initialize instance variables in `initialize`.

### Confusing Instance Variables with Temporary Variables

```smalltalk
calculateArea
    | area |  "Temporary variable"
    area := width * height.
    ^ area

"Later in another method:"
calculatePerimeter
    ^ area * 2  "Error! 'area' doesn't exist here!"
```

Temporary variables only exist within their method. If you need to remember something, use an instance variable.

### Direct Access from Outside

```smalltalk
person firstName := 'Alice'  "Error! Cannot access directly!"
```

Use methods:

```smalltalk
person firstName: 'Alice'  "Correct!"
```

### Forgetting `self` in Methods

Inside methods, you don't need to specify the object - instance variables are directly accessible:

```smalltalk
calculateTotal
    ^ price * quantity  "Correct - price and quantity are instance variables"
```

Not:

```smalltalk
calculateTotal
    ^ self price * self quantity  "Unnecessary - these are instance variables, not methods!"
```

(Unless `price` and `quantity` ARE methods, not variables - then you need `self`.)

## Design Principles

### Tell, Don't Ask

Instead of asking an object for data and doing calculations, tell the object to do the calculation:

**Bad:**
```smalltalk
area := rectangle width * rectangle height.
```

**Good:**
```smalltalk
area := rectangle area.
```

The Rectangle should calculate its own area!

### Law of Demeter

Don't reach through objects:

**Bad:**
```smalltalk
person address city name
```

This violates encapsulation - you're reaching through person to address to city.

**Better:**
```smalltalk
person cityName  "Person has a method that asks its address"
```

Let each object handle its own responsibilities.

## Looking Ahead

You now deeply understand instance variables - how they give objects memory, how encapsulation works, and how to design classes with proper state management.

In Chapter 14, we'll explore `self` and `super` - two special variables that are crucial for understanding how methods really work and how objects interact.

Then in Chapter 15, we'll tackle **inheritance** - how classes can build on other classes, sharing both instance variables and methods.

You're building sophisticated, well-designed objects now. This is real object-oriented programming!

---

**Key Takeaways:**
- **Instance variables** give objects memory - state that persists between method calls
- Each object has its own copy of instance variables
- Instance variables are **private** - accessible only from within the class
- **Encapsulation** hides implementation details and allows flexibility
- Use **accessor methods** (getters) to read values
- Use **mutator methods** (setters) to change values
- Always initialize instance variables in `initialize`
- Instance variables vs temporary variables: persistence vs transient
- Not all instance variables need public accessors
- Design with **Tell, Don't Ask** and **Law of Demeter** principles
- Object identity is separate from object values
- Instance variables are inherited by subclasses

---

[Previous: Chapter 12 - Methods](chapter-12-methods.md) | [Next: Chapter 14 - self and super](chapter-14-self-and-super.md)
