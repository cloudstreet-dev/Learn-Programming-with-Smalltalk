# Chapter 21: The Debugger - Your New Best Friend

The Smalltalk Debugger is not like debuggers in other languages. It's not just a tool for finding bugs - it's a **live development environment** where you write code, fix errors, and continue execution without restarting your program.

Many developers say the Debugger is Smalltalk's "killer feature" - the tool that makes Smalltalk development fundamentally different and more productive than other environments.

In this chapter, you'll learn to use the Debugger not just for fixing bugs, but as a primary development tool. By the end, you'll wonder how you ever programmed without it!

## What Makes Smalltalk's Debugger Special?

In traditional languages:
```
1. Code throws an error
2. Program crashes or prints stack trace
3. You read the trace
4. You edit the source file
5. You recompile
6. You re-run the entire program
7. Hope it works this time
```

In Smalltalk:
```
1. Code throws an error
2. Debugger opens, showing the exact execution point
3. You see all variables and their values
4. You fix the method RIGHT THERE in the Debugger
5. You continue execution from where it stopped
6. The program keeps running with the fix applied
```

**No restart. No recompile. No losing state. Just fix and continue.**

## How the Debugger Opens

The Debugger opens when an error occurs:

### Automatic Opening

Try this in the Playground:

```smalltalk
1 / 0
```

Execute it (`Ctrl+D` / `Cmd+D`).

Boom! A Debugger window opens with the error: `ZeroDivide: attempt to divide by zero`

### Manual Opening

You can also explicitly open the Debugger:

```smalltalk
self halt
```

This sets a **breakpoint**. When execution reaches `halt`, the Debugger opens.

### Error Handling

When any unhandled error occurs, the Debugger opens automatically. This includes:
- `ZeroDivide`
- `MessageNotUnderstood`
- `SubscriptOutOfBounds`
- Custom errors you signal

## The Debugger Interface

The Debugger has several panes:

### 1. Error Message (Top)

Shows what went wrong:
```
ZeroDivide: attempt to divide by zero
```

### 2. Stack Trace (Top-Left)

Shows the call stack - the sequence of method calls that led to the error:

```
UndefinedObject >> DoIt
SmallInteger >> /
...
```

Each line is a method call. The topmost is where the error occurred.

### 3. Context Pane (Top-Right)

Shows local variables and their values for the selected stack frame:
- `self` - The receiver
- Method parameters
- Temporary variables

### 4. Code Pane (Bottom)

Shows the source code of the selected method, with the current execution point highlighted (often with a yellow arrow or marker).

## A Simple Debugging Session

Let's debug a real error:

### Step 1: Create Buggy Code

In the Playground:

```smalltalk
| numbers |
numbers := #(1 2 3 4 5).
numbers at: 10
```

Execute. Error! `SubscriptOutOfBounds: 10 is out of bounds`

The Debugger opens.

### Step 2: Examine the Stack

Look at the stack trace:

```
UndefinedObject >> DoIt
Array >> at:
Array >> at:ifAbsent:
...
```

Click on `Array >> at:`. The code pane shows the `at:` method implementation.

### Step 3: Examine Variables

In the Context pane, see:
- `self` - the array `#(1 2 3 4 5)`
- `index` - `10`

The problem is clear: trying to access index 10 in an array of size 5!

### Step 4: Fix

The fix is in your code (the Playground), not in Array. Close the Debugger and fix:

```smalltalk
| numbers |
numbers := #(1 2 3 4 5).
numbers at: 3  "Valid index!"
```

## Debugging Your Own Code

Let's debug code you wrote:

### Step 1: Create Buggy Code

Create a Calculator class:

```smalltalk
Object subclass: #Calculator
    instanceVariableNames: 'result'
    classVariableNames: ''
    package: 'MyApp'
```

Add a buggy method:

```smalltalk
initialize
    result := 0

divide: number
    result := result / number.
    ^ result
```

### Step 2: Trigger the Bug

```smalltalk
| calc |
calc := Calculator new.
calc divide: 5.  "Works: 0 / 5 = 0"
calc divide: 0.  "Error!"
```

Debugger opens: `ZeroDivide: attempt to divide by zero`

### Step 3: Examine

Stack shows:

```
Calculator >> divide:
SmallInteger >> /
...
```

Click on `Calculator >> divide:`. See the code:

```smalltalk
divide: number
    result := result / number.  ← Error here
    ^ result
```

Context pane shows:
- `self` - the Calculator
- `number` - 0
- `result` - 0

You're dividing `result` (0) by `number` (0). That's the bug!

### Step 4: Fix It

**Here's where it gets amazing:** Fix the method RIGHT IN THE DEBUGGER!

In the code pane, edit the method:

```smalltalk
divide: number
    number = 0 ifTrue: [ self error: 'Cannot divide by zero' ].
    result := result / number.
    ^ result
```

### Step 5: Accept (Save)

Press `Ctrl+S` / `Cmd+S` to accept the change.

The method is now fixed! It's compiled into the Image.

### Step 6: Continue or Restart

Now you have options:

- **Proceed** - Continue execution from where it stopped
- **Restart** - Re-run the current method with the new code
- **Step** - Execute one line at a time
- **Into** - Step into method calls

Click `Restart`. The method runs again with the new code!

Now it throws your custom error: `Cannot divide by zero`. Much better!

## Stepping Through Code

The Debugger lets you execute code line by line:

### Buttons/Commands:

- **Step Over** (or `Over` button) - Execute current line, don't enter method calls
- **Step Into** (or `Into` button) - Execute current line, enter method calls
- **Step Through** - Like Step Into, but more aggressive
- **Restart** - Re-run the current method
- **Proceed** - Continue normal execution
- **Return Value** - Return early from the method

### Example

```smalltalk
Object subclass: #Greeter
    instanceVariableNames: 'name'
    ...

greet
    | greeting |
    greeting := self buildGreeting.
    ^ greeting

buildGreeting
    ^ 'Hello, ' , name , '!'
```

Set a breakpoint:

```smalltalk
greet
    | greeting |
    self halt.  "Breakpoint!"
    greeting := self buildGreeting.
    ^ greeting
```

Execute:

```smalltalk
| greeter |
greeter := Greeter new name: 'Alice'.
greeter greet
```

Debugger opens at `self halt`.

#### Step Over:
1. Click `Step Over`
2. Executes `greeting := self buildGreeting`
3. You see `greeting` now has the value `'Hello, Alice!'`
4. Click `Step Over` again
5. Executes `^ greeting`
6. Method returns

#### Step Into:
1. Click `Step Into` instead
2. Enters `buildGreeting` method
3. You see the code: `^ 'Hello, ' , name , '!'`
4. Click `Step` again
5. Executes the concatenation
6. Method returns to `greet`

This lets you trace execution line by line!

## Inspecting Variables

While debugging, you can inspect any variable:

In the Context pane, right-click on a variable → `Inspect`.

An Inspector opens showing that variable's value and structure!

This combines the Inspector (Chapter 20) with the Debugger.

## Modifying Variables

You can change variable values while debugging:

### In the Debugger Code Pane:

```smalltalk
greeting := 'Goodbye, cruel world!'
```

Execute this line (`Ctrl+D` / `Cmd+D`).

Now `greeting` has a new value! Continue execution and it uses the new value.

## Fixing Code Without Restarting

This is the game-changer. A realistic scenario:

### Scenario: Long-Running Process

```smalltalk
1 to: 1000000 do: [ :i |
    self processItem: i.
    i = 500000 ifTrue: [ self error: 'Oops!' ] ]
```

This processes a million items. At item 500,000, it errors.

Traditional debugger: Start over. Process 500,000 items again. Slow!

Smalltalk Debugger:
1. Error occurs at 500,000
2. Debugger opens
3. You see the problem
4. Fix the method
5. Click `Restart` or `Proceed`
6. Processing continues from 500,000!

**No restart. No re-processing. Just fix and continue.**

## Writing Code in the Debugger

Some Smalltalkers write most code in the Debugger:

### The Workflow:

1. Write a skeleton method:
   ```smalltalk
   processOrder: order
       self notYetImplemented
   ```

2. Execute it:
   ```smalltalk
   processor processOrder: myOrder
   ```

3. Debugger opens: `notYetImplemented`

4. Implement the method in the Debugger:
   ```smalltalk
   processOrder: order
       | total |
       total := order calculateTotal.
       self validateTotal: total.
       self chargeCustomer: order customer amount: total.
       ^ order
   ```

5. Accept the method.

6. Now `validateTotal:` doesn't exist. Debugger opens again.

7. Implement `validateTotal:` in the Debugger.

8. Continue!

This is called **Debugging-Driven Development**. You write code incrementally, in the context where it runs, with live data.

## MessageNotUnderstood

A common error: calling a method that doesn't exist.

```smalltalk
'hello' yell
```

Error: `MessageNotUnderstood: ByteString>>yell`

The Debugger opens. In the Code pane, you see where the error occurred.

### Fix It:

1. The Debugger often has a `Create` button
2. Click it to create the missing method
3. Choose the class: `String`
4. Implement:
   ```smalltalk
   yell
       ^ self asUppercase , '!!!'
   ```
5. Accept
6. Click `Proceed` or `Restart`

The method is now defined and execution continues!

## Halt and Breakpoints

### Explicit Halt

```smalltalk
processData: data
    self halt.  "Breakpoint here!"
    result := data collect: [ :each | each * 2 ].
    ^ result
```

When execution reaches `self halt`, the Debugger opens. You can inspect variables, step through, etc.

### Conditional Halt

```smalltalk
processData: data
    data size > 1000 ifTrue: [ self halt ].  "Only halt on large data"
    result := data collect: [ :each | each * 2 ].
    ^ result
```

### Halt Once

```smalltalk
self haltOnce
```

Halts the first time it's reached, then disables itself. Great for loops!

## The Stack is Live

The stack in the Debugger is **live** - it's the actual execution stack, not a copy.

You can:
- Modify variables in any stack frame
- Restart any method
- Return from any method early

Example:

1. Method A calls Method B calls Method C
2. C errors
3. Debugger shows stack: A → B → C
4. Click on B (middle of stack)
5. Modify variables in B
6. Restart B
7. B re-executes with new values

This is incredibly powerful for testing what-if scenarios!

## Debugging Blocks

Blocks can be tricky to debug, but the Debugger handles them:

```smalltalk
numbers := #(1 2 3 4 5).
numbers do: [ :n |
    n = 3 ifTrue: [ self halt ].
    Transcript show: n printString; cr ]
```

The Debugger opens when `n = 3`. You can see:
- `n` - 3
- `numbers` - the array
- The block context

Step through the block's code!

## Debugging Unit Tests

When a unit test fails, the Debugger opens automatically (in test mode):

```smalltalk
testAddition
    | result |
    result := calculator add: 2 to: 2.
    self assert: result equals: 5  "Wrong! Should be 4"
```

The Debugger opens showing the assertion failure. You can:
- See why it failed (`result` is 4, not 5)
- Fix the test or the code
- Re-run the test

## Advanced: Proceed and Return

### Proceed

Click `Proceed` to continue normal execution after an error. Use this if you fixed the problem and want to keep going.

### Return Value

Want to skip a method and return a specific value?

In the Debugger code pane:

```smalltalk
^ 42
```

Execute (`Ctrl+D` / `Cmd+D`). The method returns 42 immediately!

Or right-click on a stack frame → `Return entered value` → Enter a value.

## Debugging Tips

### Tip 1: Don't Fear Errors

Errors are opportunities! They open the Debugger where you can fix things.

### Tip 2: Use `halt` Liberally

Drop `self halt` anywhere to pause execution and explore.

### Tip 3: Inspect Everything

Right-click on variables in the Context pane and inspect them.

### Tip 4: Restart Methods

Made a change? Click `Restart` to re-run the method with the new code.

### Tip 5: Write Code in the Debugger

Don't pre-write everything. Let errors guide you to what needs implementing.

### Tip 6: Read the Stack

The stack trace tells a story. Follow it from top to bottom to understand how you got here.

### Tip 7: Test Assumptions

Not sure what a variable is? Execute code in the Debugger:

```smalltalk
myVar class.
myVar inspect.
myVar printString
```

## Common Debugging Scenarios

### Scenario 1: Nil Reference

```smalltalk
person address city
```

Error: `MessageNotUnderstood: UndefinedObject>>city`

Meaning: `address` returned `nil`, and you tried to send `city` to nil.

Fix: Check for nil:

```smalltalk
person address ifNotNil: [ :addr | addr city ]
```

### Scenario 2: Wrong Variable Value

A variable has an unexpected value. Inspect it in the Debugger. Trace back through the stack to see where it was set incorrectly.

### Scenario 3: Infinite Loop

Code loops forever. Press `Ctrl+.` (or `Cmd+.`) to interrupt. The Debugger opens mid-loop. Examine variables to see why the loop won't terminate.

### Scenario 4: Performance Issue

Code is slow. Use `self halt` to pause at various points. Inspect variables to see if data structures are too large or algorithms inefficient.

## Keyboard Shortcuts

- **Ctrl+. / Cmd+.** - Interrupt execution (opens Debugger)
- **Over** - Step over
- **Into** - Step into
- **Through** - Step through
- **Restart** - Restart current method
- **Proceed** - Continue execution
- **Ctrl+S / Cmd+S** - Accept method changes
- **Ctrl+I / Cmd+I** - Inspect variable

## Debugging Mindset

### Traditional Debugging:
1. Error occurs
2. Panic!
3. Add print statements
4. Re-run
5. Guess at the problem
6. Try a fix
7. Re-run
8. Repeat

### Smalltalk Debugging:
1. Error occurs
2. Debugger opens
3. Examine state
4. Understand problem
5. Fix it right there
6. Continue
7. Done!

**Much faster. Much less stressful.**

## Example: Full Debugging Session

Let's debug a realistic problem:

### Code:

```smalltalk
Object subclass: #ShoppingCart
    instanceVariableNames: 'items'
    ...

initialize
    super initialize.
    items := OrderedCollection new

addItem: item
    items add: item

total
    | sum |
    sum := 0.
    items do: [ :item | sum := sum + item price ].
    ^ sum
```

```smalltalk
Object subclass: #Product
    instanceVariableNames: 'name price'
    ...

name: aName price: aPrice
    name := aName.
    price := aPrice.
    ^ self
```

### Usage:

```smalltalk
| cart |
cart := ShoppingCart new.
cart addItem: (Product new name: 'Book' price: 15).
cart addItem: (Product new name: 'Pen' price: 2).
cart addItem: (Product new name: 'Notebook').  "Oops, forgot price!"
cart total
```

Error! `MessageNotUnderstood: UndefinedObject>>#+`

### Debugging:

1. **Debugger opens** showing `UndefinedObject>>#+`

2. **Stack trace** shows: `ShoppingCart>>total` called `+` on `nil`

3. **Context pane** shows:
   - `sum` - 17
   - `item` - a Product (the Notebook)

4. **Inspect `item`**: Right-click → Inspect. See:
   - `name` - 'Notebook'
   - `price` - `nil` (Aha!)

5. **Root cause**: Product's price is nil because we didn't set it!

6. **Fix #1 - Defensive**: Edit `total` in the Debugger:
   ```smalltalk
   total
       | sum |
       sum := 0.
       items do: [ :item |
           item price ifNotNil: [ :p | sum := sum + p ] ].
       ^ sum
   ```

   Accept. Click `Restart`. Now it works, returning 17 (skipping the nil-price item).

7. **Fix #2 - Proper**: Also fix Product to have a default price:
   ```smalltalk
   initialize
       super initialize.
       name := ''.
       price := 0  "Default price"
   ```

Fixed! And you did it all without restarting the program!

## Try This!

Practice debugging:

1. **Trigger simple errors:**
   ```smalltalk
   1 / 0.
   'hello' at: 100.
   nil size
   ```

   Open the Debugger. Examine the stack and variables.

2. **Use `halt`:**
   ```smalltalk
   1 to: 10 do: [ :i |
       i = 5 ifTrue: [ self halt ].
       Transcript show: i printString; cr ]
   ```

   The Debugger opens at 5. Inspect `i`. Step through the loop.

3. **Fix a method in the Debugger:**
   ```smalltalk
   Object subclass: #BuggyClass
       instanceVariableNames: ''
       ...

   buggyMethod
       ^ 1 / 0  "Intentionally buggy!"
   ```

   Execute:
   ```smalltalk
   BuggyClass new buggyMethod
   ```

   Debugger opens. Fix the method to return 42. Accept. Restart. Success!

4. **Create a missing method:**
   ```smalltalk
   'hello' rot13
   ```

   Debugger opens: `MessageNotUnderstood`. Create the `rot13` method in the Debugger:
   ```smalltalk
   rot13
       "Simple rot13 cipher"
       ^ self collect: [ :char |
           (char isLetter)
               ifTrue: [ ... ]  "Implement ROT13 logic"
               ifFalse: [ char ] ]
   ```

5. **Debug nested calls:**
   ```smalltalk
   methodA
       ^ self methodB

   methodB
       ^ self methodC

   methodC
       self halt.
       ^ 42
   ```

   Execute `object methodA`. Debugger opens in `methodC`. Look at the stack: A → B → C. Click on each to see how you got here.

6. **Modify and continue:**
   ```smalltalk
   | count |
   count := 0.
   1 to: 10 do: [ :i |
       count := count + 1.
       i = 5 ifTrue: [ self halt ] ].
   count
   ```

   Debugger opens at 5. In the code pane, execute: `count := 100`. Click `Proceed`. Final count is 105!

## Common Mistakes

### Closing the Debugger Too Soon

Don't panic-close the Debugger! Examine what went wrong first.

### Not Using Restart

After fixing a method, use `Restart` to re-run it, not `Proceed`.

### Editing Without Accepting

Edit the code but forget to press `Ctrl+S` / `Cmd+S`. Your changes aren't saved!

### Fear of the Debugger

Embrace errors! They're learning opportunities.

## The Philosophy

The Debugger embodies Smalltalk's core philosophy:

### Live Programming

You're not editing dead text files. You're modifying a running system.

### Immediate Feedback

See results instantly. No compile-run cycle.

### Exploration

The Debugger encourages exploration. Poke around! Try things!

### Safety

Mistakes aren't fatal. The Debugger catches them and lets you fix them.

## Looking Ahead

You now understand the Debugger - one of Smalltalk's most powerful and distinctive tools! You can:
- Debug errors when they occur
- Step through code line by line
- Fix methods while debugging
- Continue execution without restarting
- Write code incrementally in the Debugger
- Inspect variables and modify them
- Create missing methods on the fly

In Chapter 22, we'll explore the **Finder** and **Spotter** - tools for quickly finding code, navigating the system, and discovering methods. These complete your tool toolkit!

Part VI has revealed why Smalltalk developers are so productive: the tools are designed around the living system, enabling a fluid, interactive workflow impossible in traditional environments.

---

**Key Takeaways:**
- The **Debugger** opens when errors occur
- Shows the execution stack, variables, and source code
- You can **fix methods directly in the Debugger**
- **Restart** re-runs a method with the new code
- **Proceed** continues normal execution
- **Step Over/Into** executes code line by line
- Use `self halt` to set breakpoints
- The stack is **live** - you can modify any frame
- `MessageNotUnderstood` errors let you create missing methods
- The Debugger combines code editing, execution, and inspection
- **Debugging-Driven Development**: Write code incrementally in the Debugger
- No need to restart the program after fixes
- Interrupt with `Ctrl+.` / `Cmd+.`
- Inspect variables by right-clicking in the Context pane
- The Debugger is a primary development tool, not just for bugs
- Embrace errors - they guide development!

---

[Previous: Chapter 20 - The Inspector and Explorer](chapter-20-inspector-and-explorer.md) | [Next: Chapter 22 - The Finder - Discovering Code](chapter-22-finder.md)
