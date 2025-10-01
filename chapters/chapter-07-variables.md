# Chapter 7: Variables - Naming Your Objects

We've been using variables in previous chapters without fully explaining them. Now it's time to understand what variables really are, how they work in Smalltalk, and the important concepts of scope and lifetime.

A **variable** is a name that refers to an object. Think of it as a label or tag you attach to an object so you can refer to it later.

## Why Do We Need Variables?

Imagine trying to have a conversation without using names:

"Tell the person who lives in the third house on Oak Street that the person who drives the red car wants to meet them at the place where we went last Tuesday."

Exhausting, right? Names make communication easier:

"Tell Alice that Bob wants to meet her at the coffee shop."

Variables serve the same purpose in programming - they give us convenient names for objects we want to work with.

## Anatomy of a Variable

A variable has two parts:

1. **Name**: The identifier you use in your code (like `x`, `count`, `userName`)
2. **Value**: The object the name refers to

The variable itself isn't the object - it's a reference *to* an object, like a string tied to a balloon. The balloon is the object; the string is the variable.

## Temporary Variables (Local Variables)

**Temporary variables** (also called local variables) are variables that exist only within a specific piece of code - like a Playground expression or a method. Once that code finishes executing, the variables disappear.

### Declaring Temporary Variables

You've seen this syntax before:

```smalltalk
| x |
x := 42.
x
```

The vertical bars `| |` create a "variable declaration zone." Inside those bars, you list the variables you want to use, separated by spaces:

```smalltalk
| firstName lastName age |
firstName := 'Alice'.
lastName := 'Wonderland'.
age := 30.
```

**Multiple variables** in one declaration:

```smalltalk
| x y z |
x := 10.
y := 20.
z := 30.
```

Or you can declare them individually (though this is less common):

```smalltalk
| x |
| y |
| z |
```

Most Smalltalkers use the first style.

### Variable Naming Rules

Variable names in Smalltalk:

- **Must start with a lowercase letter**: `count`, `userName`, `x`
- **Can contain letters, digits, and underscores**: `count1`, `user_name`, `myVariable`
- **Are case-sensitive**: `count` and `Count` are different variables
- **Cannot be reserved words**: You can't name a variable `true`, `false`, `nil`, `self`, or `super`

**Convention**: Use **camelCase** for multi-word variable names:
- `firstName` (not `first_name` or `FirstName`)
- `totalPrice`
- `isReady`

### Good Variable Names

Choose names that describe what the variable represents:

**Good:**
```smalltalk
| customerName orderTotal itemCount |
```

**Bad:**
```smalltalk
| x y z |
```

Exception: In very short pieces of code or mathematical contexts, single letters are fine:

```smalltalk
| x y |
x := 5.
y := 3.
x + y
```

But for longer code or when the meaning isn't obvious, use descriptive names.

## Assignment: Giving Variables Values

The **assignment operator** `:=` (colon-equals) assigns a value to a variable:

```smalltalk
| count |
count := 0.
```

This makes `count` refer to the object `0`.

You can reassign variables:

```smalltalk
| count |
count := 0.
count := 10.
count := 100.
count  "Returns 100"
```

Each assignment makes the variable refer to a different object.

### Assignment is Not Equality

Don't confuse `:=` (assignment) with `=` (equality test):

```smalltalk
| x |
x := 5.     "Assignment: make x refer to 5"
x = 5       "Comparison: does x equal 5? Returns true"
```

One common mistake:

```smalltalk
| x |
x = 5.  "Wrong! This is a comparison, not assignment"
```

This will cause an error because you're trying to compare an unassigned variable (which contains `nil`) with `5`.

## Scope: Where Variables Live

**Scope** determines where a variable can be used. Temporary variables have **local scope** - they only exist within the code block where they're declared.

### Playground Scope

When you write code in the Playground:

```smalltalk
| x |
x := 42.
Transcript show: x printString.
```

The variable `x` exists only for this one execution. If you try to use `x` in a different expression:

```smalltalk
x + 10
```

You'll get an error - `x` doesn't exist outside the original code block.

### Method Scope (Preview)

When we write methods (in Chapter 12), temporary variables declared in a method only exist while that method is running. When the method finishes, its variables disappear.

This is actually a good thing - it prevents variable names from conflicting across different pieces of code.

## Initial Values: `nil`

When you declare a variable but don't assign it a value, it contains `nil`:

```smalltalk
| x |
x isNil  "true"
```

Always initialize your variables before using them (unless `nil` is what you want):

```smalltalk
| count |
count := 0.  "Good - explicitly initialized"
```

## Multiple Assignment

You can use the result of an assignment:

```smalltalk
| x y |
x := y := 42.
```

This assigns `42` to `y`, then assigns the result (also `42`) to `x`. Both variables now refer to `42`.

However, this style can be confusing. It's usually clearer to write:

```smalltalk
| x y |
x := 42.
y := 42.
```

## Variables Hold References, Not Objects

This is subtle but important: variables don't *contain* objects; they **refer to** objects.

```smalltalk
| x y |
x := OrderedCollection with: 1 with: 2 with: 3.
y := x.
```

Now both `x` and `y` refer to the **same** collection object. If you modify the collection through `y`:

```smalltalk
y add: 4.
```

Then `x` will see the change too, because they're both referring to the same object:

```smalltalk
x  "Returns OrderedCollection(1 2 3 4)"
```

Think of it like two people (variables) holding strings attached to the same balloon (object). If the balloon changes, both people see the change.

### Copying vs Sharing

If you want `y` to have its own independent copy:

```smalltalk
| x y |
x := OrderedCollection with: 1 with: 2 with: 3.
y := x copy.
y add: 4.
x  "Still OrderedCollection(1 2 3)"
y  "OrderedCollection(1 2 3 4)"
```

Now they're separate objects.

## Block Arguments (Preview)

You've seen this pattern in collection iterations:

```smalltalk
#(1 2 3 4 5) do: [ :each |
    Transcript show: each printString; cr ]
```

The `:each` is a **block argument** - a special kind of variable that receives each element as the block executes. Block arguments are declared inside the block with a colon prefix:

```smalltalk
[ :argumentName | "code that uses argumentName" ]
```

We'll cover blocks in detail in Chapter 9, but you've been using block arguments already with collections!

More examples:

```smalltalk
#(1 2 3) collect: [ :num | num * 2 ]
```

Here, `:num` is the block argument that represents each number in turn.

```smalltalk
#(1 2 3 4 5) select: [ :n | n even ]
```

`:n` represents each element being tested.

Block arguments are automatically assigned - you don't use `:=` with them. The block receives the value when it's executed.

## Instance Variables (Preview)

Temporary variables disappear when code finishes executing. But what if you want variables that persist as long as an object exists?

That's what **instance variables** are for. They're variables that belong to a specific object and last as long as that object does.

For example, imagine a `BankAccount` object. It needs to remember its balance even after a deposit or withdrawal method finishes. The balance would be stored in an instance variable.

We'll explore instance variables thoroughly in Chapter 13 when we create our own classes. For now, just know they exist and serve a different purpose than temporary variables.

## Class Variables (Preview)

There's also something called **class variables** - variables shared by all instances of a class. We'll cover these in later chapters as well.

For now, focus on understanding temporary variables, as they're what you'll use most often when starting out.

## Constants: Variables That Don't Change

Smalltalk doesn't have a special "constant" syntax, but by convention, variables that shouldn't change are given names starting with uppercase letters:

```smalltalk
MaxRetries := 3.
DefaultTimeout := 30.
```

These look like class names, which is intentional - they're treated similarly. However, in typical Playground code, you'll mostly use lowercase variable names.

For truly constant values, you might just use the literal directly:

```smalltalk
| retryCount |
retryCount := 0.
[ retryCount < 3 ] whileTrue: [
    "retry logic here"
    retryCount := retryCount + 1 ]
```

## Common Patterns with Variables

### Accumulator Pattern

```smalltalk
| sum |
sum := 0.
#(1 2 3 4 5) do: [ :num |
    sum := sum + num ].
sum  "Returns 15"
```

This is the classic "accumulator" pattern - starting with an initial value and building it up.

### Swap Pattern

```smalltalk
| x y temp |
x := 10.
y := 20.
temp := x.
x := y.
y := temp.
"Now x is 20 and y is 10"
```

You need a temporary variable to swap two values without losing one.

### Counter Pattern

```smalltalk
| count |
count := 0.
#(1 2 3 4 5 6 7 8 9 10) do: [ :num |
    num even ifTrue: [ count := count + 1 ] ].
count  "Returns 5 (there are 5 even numbers)"
```

Counting things as you iterate through a collection.

### Flag Pattern

```smalltalk
| found |
found := false.
#(1 2 3 4 5) do: [ :num |
    num = 3 ifTrue: [ found := true ] ].
found  "Returns true"
```

Using a boolean variable to remember whether something happened. (Though for this specific case, `includes:` would be better!)

## Variable Lifetime

The **lifetime** of a variable is how long it exists:

- **Temporary variables**: Exist only during the execution of their code block
- **Instance variables**: Exist as long as the object exists
- **Class variables**: Exist as long as the class exists (essentially forever in a running image)

For now, you're working with temporary variables, which means they're created when your code starts and destroyed when it finishes.

```smalltalk
| x |
x := 42.
Transcript show: x printString.
"When this finishes, x disappears"
```

If you run this code again, a *new* `x` is created - it's not the same variable as before.

## Debugging with Variables

When something goes wrong, inspecting variables is your first tool:

```smalltalk
| result |
result := 10 / 0.  "This will cause an error"
```

When the debugger opens, you can see the value of `result` (which never got assigned because the error happened first).

More usefully:

```smalltalk
| numbers sum |
numbers := #(1 2 3 4 5).
sum := 0.
numbers inspect.  "Open an inspector to see what's in numbers"
sum inspect.      "Inspect sum too"
```

Inspecting variables helps you understand what's happening in your code.

## Unused Variables

If you declare a variable but never use it, Pharo will warn you. This is helpful - it catches typos and unused code:

```smalltalk
| x y |  "If you only use x, Pharo warns about y"
x := 42.
x
```

Just remove unused variables:

```smalltalk
| x |
x := 42.
x
```

## Variable Naming Conventions

Good Smalltalk style:

**Do:**
- Use descriptive names: `customerName`, `orderTotal`
- Use camelCase: `firstName`, `totalPrice`
- Start with lowercase: `count`, `userName`
- Keep them reasonably short: `totalAmount` not `theTotalAmountOfThisOrder`

**Don't:**
- Use single letters for important values: `n`, `x`, `y` (except in short, obvious contexts)
- Use underscores for normal variables: `customer_name` (use `customerName` instead)
- Use abbreviations: `custNm` (use `customerName`)
- Start with uppercase for local variables: `Count` (that's for classes and constants)

## Try This!

Practice working with variables:

1. **Basic assignment and arithmetic:**
   ```smalltalk
   | x y result |
   x := 10.
   y := 25.
   result := x + y.
   result
   ```

2. **Reassignment:**
   ```smalltalk
   | price tax total |
   price := 100.
   tax := price * 0.08.
   total := price + tax.
   total
   ```

3. **String concatenation:**
   ```smalltalk
   | firstName lastName fullName |
   firstName := 'Alice'.
   lastName := 'Wonderland'.
   fullName := firstName , ' ' , lastName.
   fullName
   ```

4. **Collection accumulation:**
   ```smalltalk
   | numbers product |
   numbers := #(2 3 4 5).
   product := 1.
   numbers do: [ :num |
       product := product * num ].
   product
   ```

5. **Reference vs copy:**
   ```smalltalk
   | original copy |
   original := OrderedCollection with: 'a' with: 'b' with: 'c'.
   copy := original.  "This is a reference, not a copy!"
   copy add: 'd'.
   original  "What do you see?"
   ```

   Now try with an actual copy:
   ```smalltalk
   | original copy |
   original := OrderedCollection with: 'a' with: 'b' with: 'c'.
   copy := original copy.  "Now it's a real copy"
   copy add: 'd'.
   original  "What do you see now?"
   ```

6. **Counting pattern:**
   ```smalltalk
   | words vowelCount |
   words := #('hello' 'world' 'from' 'smalltalk').
   vowelCount := 0.
   words do: [ :word |
       word do: [ :char |
           char isVowel ifTrue: [ vowelCount := vowelCount + 1 ] ] ].
   vowelCount
   ```

7. **Swap variables:**
   ```smalltalk
   | a b temp |
   a := 'first'.
   b := 'second'.
   Transcript show: 'Before: a=' , a , ', b=' , b; cr.
   temp := a.
   a := b.
   b := temp.
   Transcript show: 'After: a=' , a , ', b=' , b; cr.
   ```

8. **Finding maximum:**
   ```smalltalk
   | numbers max |
   numbers := #(23 45 12 67 34 89 15).
   max := numbers first.
   numbers do: [ :num |
       num > max ifTrue: [ max := num ] ].
   max
   ```

## Common Mistakes

### Forgetting to Declare

```smalltalk
x := 42.  "Error! x wasn't declared"
```

You must declare variables with `| varName |` first.

### Forgetting to Initialize

```smalltalk
| sum |
sum := sum + 10.  "Error! sum is nil, can't add to it"
```

Initialize before using:

```smalltalk
| sum |
sum := 0.
sum := sum + 10.  "Now it works"
```

### Using = Instead of :=

```smalltalk
| x |
x = 42.  "Wrong! This compares x to 42, doesn't assign"
x := 42.  "Correct - this assigns 42 to x"
```

### Expecting Variables to Persist

```smalltalk
| x |
x := 42.
```

Later (in a separate expression):

```smalltalk
x + 10  "Error! x doesn't exist anymore"
```

Temporary variables only live during one execution. To make them persist, you need instance variables (Chapter 13) or you need to keep them in the same expression.

## When to Use Variables

Variables are useful when:

1. **You need to refer to a value multiple times**
   ```smalltalk
   | userName |
   userName := 'Alice'.
   Transcript show: 'Hello, ' , userName; cr.
   Transcript show: 'Welcome back, ' , userName; cr.
   ```

2. **You need to transform a value step by step**
   ```smalltalk
   | text |
   text := 'hello world'.
   text := text capitalized.
   text := text reversed.
   text
   ```

3. **You need to accumulate results**
   ```smalltalk
   | sum |
   sum := 0.
   #(1 2 3 4 5) do: [ :n | sum := sum + n ].
   ```

4. **You need to make code clearer**
   ```smalltalk
   | taxRate subtotal tax total |
   taxRate := 0.08.
   subtotal := 100.
   tax := subtotal * taxRate.
   total := subtotal + tax.
   ```

   This is clearer than:
   ```smalltalk
   100 + (100 * 0.08)
   ```

Don't use variables when you don't need them:

```smalltalk
| x |
x := 42.
x  "Why use a variable for this?"
```

Just write:

```smalltalk
42
```

## Looking Ahead

Variables are fundamental - you'll use them in every program you write. Understanding:

- How to declare and assign variables
- That variables hold references, not objects
- The difference between assignment (`:=`) and comparison (`=`)
- When to use variables vs. direct values

...will serve you well as we move forward.

In the next three chapters, we'll learn about:

- **Chapter 8**: Conditionals - making decisions based on conditions
- **Chapter 9**: Blocks - packaging code into objects you can pass around
- **Chapter 10**: Loops and iteration - repeating actions

All of these will make heavy use of variables, so the understanding you've gained here is essential.

## Coming Up Next

Now that you understand variables, let's learn how to make decisions in your code. In Chapter 8, we'll explore **conditionals** - how to do different things based on different conditions.

In Smalltalk, conditionals aren't special syntax - they're messages sent to boolean objects! This is where the "everything is an object" philosophy really pays off.

Let's see how it works!

---

**Key Takeaways:**
- Variables are names that refer to objects
- Declare temporary variables with `| varName |`
- Assign values with `:=` (colon-equals)
- Use `=` for comparison, not assignment
- Variables have **scope** (where they can be used) and **lifetime** (how long they exist)
- Temporary variables only exist during their code block's execution
- Variables hold **references** to objects, not the objects themselves
- Use descriptive, camelCase names starting with lowercase
- Initialize variables before using them
- Good variable names make code readable and maintainable

---

[Previous: Chapter 6 - Collections](chapter-06-collections.md) | [Next: Chapter 8 - Conditionals](chapter-08-conditionals.md)
