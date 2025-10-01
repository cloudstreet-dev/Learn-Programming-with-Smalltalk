# Chapter 3: The Workspace - Your Programming Playground

In the last chapter, you got a glimpse of the Playground (also called the Workspace in some Smalltalks). Now it's time to really learn how to use this powerful tool. The Playground is where you'll spend much of your time experimenting, testing ideas, and learning how Smalltalk works.

Think of the Playground as a conversation with your computer - you type something, execute it, and see what comes back. It's immediate, forgiving, and endlessly useful.

## Opening the Playground (Reminder)

Before we dive in, let's make sure you have a Playground open:

- **Right-click the World** (gray background) → Choose `Playground`
- **Or use the keyboard shortcut**: `Ctrl+O` then `W` (Windows/Linux) or `Cmd+O` then `W` (macOS)

You should see a window with a white text area. This is your playground!

## The Three Essential Commands

The Playground has three commands that you'll use constantly. These are the heart of interactive programming in Smalltalk:

### 1. Do it - Execute Code

**Do it** runs your code but doesn't show you the result. It just executes it and moves on.

Let's try it. Type this in your Playground:

```smalltalk
Transcript show: 'Hello, World!'.
```

Now select that text (highlight it with your mouse) and either:
- **Right-click** → Choose `Do it`
- **Press** `Ctrl+D` (Windows/Linux) or `Cmd+D` (macOS)

### What Happened?

You probably saw... nothing obvious! That's because "Do it" executed the code but didn't show you the result in the Playground.

However, the code DID run. It sent the message to the Transcript. Let's open the Transcript to see:

1. Right-click the World → Choose `Transcript`
2. A new window opens - this is the Transcript, Smalltalk's output log
3. You should see "Hello, World!" displayed there!

The **Transcript** is like Smalltalk's console or output window. When you want to display something for debugging or logging, you send it to the Transcript.

Go back to your Playground and try "Do it" on this:

```smalltalk
Transcript show: 'Smalltalk is fun!'; cr.
```

The `cr` stands for "carriage return" - it adds a new line. Check your Transcript window - you should see both messages now!

**When to use "Do it":** When you want to execute code but don't need to see a return value. Common for things like printing to the Transcript, opening windows, or performing actions.

### 2. Print it - Execute and Show Result

**Print it** runs your code AND displays the result right in the Playground.

Type this:

```smalltalk
2 + 2
```

Select it and either:
- **Right-click** → Choose `Print it`
- **Press** `Ctrl+P` (Windows/Linux) or `Cmd+P` (macOS)

You should see: `2 + 2 . 4`

The `. 4` is Smalltalk showing you the result. The period and space before the result just make it easier to read.

Let's try a few more. Type each of these, select them, and use "Print it":

```smalltalk
10 * 5
```
Result: `. 50`

```smalltalk
'Hello' , ' ' , 'World'
```
Result: `. 'Hello World'`

The comma `,` concatenates (joins) strings together.

```smalltalk
#(1 2 3 4 5) size
```
Result: `. 5`

This creates an array with five numbers and asks for its size.

**When to use "Print it":** Most of the time! When you want to see what an expression evaluates to, "Print it" is your friend.

### 3. Inspect it - Execute and Explore Result

**Inspect it** runs your code and opens an **Inspector** window on the result. This lets you explore the object in detail.

Type this:

```smalltalk
Date today
```

Select it and either:
- **Right-click** → Choose `Inspect it`
- **Press** `Ctrl+I` (Windows/Linux) or `Cmd+I` (macOS)

### The Inspector Window

An Inspector window opens! This is a powerful tool for exploring objects. You should see:

- **Top section**: Shows "a Date" and the date value
- **Left panel**: Lists the object's variables (things like `start`, `julianDayNumber`, etc.)
- **Right panel**: Shows the value of whatever you select in the left panel
- **Bottom section**: Another playground where you can send messages to this object

The Inspector lets you look *inside* an object. Click on different variables in the left panel to see what they contain.

In the bottom section of the Inspector (which is like a mini-playground), type:

```smalltalk
self dayOfWeek
```

Select it and "Print it" (`Ctrl+P` or `Cmd+P`). It will show you what day of the week today is!

In an Inspector, `self` refers to the object you're inspecting - in this case, today's date.

**When to use "Inspect it":** When you want to explore an object, see what's inside it, or experiment with sending it messages. This is incredibly useful for learning and debugging.

## Understanding Expressions

Everything you type in the Playground is an **expression** - something that evaluates to a value (an object).

### Simple Expressions

The simplest expressions are literals - direct representations of objects:

```smalltalk
42
```
This is the number 42.

```smalltalk
'Hello'
```
This is a string.

```smalltalk
true
```
This is the boolean value true.

```smalltalk
#mySymbol
```
This is a symbol (we'll cover symbols later).

Try "Print it" on each of these. They each evaluate to themselves.

### Message Expressions

More interesting expressions involve sending messages to objects:

```smalltalk
42 + 8
```
Send the `+` message to `42` with argument `8`.

```smalltalk
'hello' capitalized
```
Send the `capitalized` message to `'hello'`.

```smalltalk
Date today
```
Send the `today` message to the Date class.

### Chaining Messages

You can chain messages together:

```smalltalk
'hello world' capitalized reversed
```

Try "Print it" on this. What happens?

First, `'hello world'` receives the `capitalized` message, producing `'Hello world'`.
Then that result receives the `reversed` message, producing `'dlrow olleH'`.

Messages are evaluated left to right (with some precedence rules we'll cover soon).

### Multiple Statements

You can write multiple statements separated by periods:

```smalltalk
Transcript show: 'First line'; cr.
Transcript show: 'Second line'; cr.
Transcript show: 'Third line'; cr.
```

Select all three lines and "Do it". Check your Transcript - you should see all three lines!

The period `.` separates statements. Each statement is executed in order.

## Comments - Notes to Yourself

You can (and should!) add comments to your code. Comments are notes for humans; Smalltalk ignores them.

Comments in Smalltalk use double quotes:

```smalltalk
"This is a comment"
2 + 2  "This adds two and two"
```

Try "Print it" on this:

```smalltalk
"Calculate the area of a rectangle"
10 * 5  "width times height"
```

The result is just `50` - the comments are ignored.

Comments are incredibly useful for explaining what your code does, especially when you come back to it later. Get in the habit of commenting!

## Variables in the Playground

Sometimes you want to store a value to use it later. That's what variables are for.

### Temporary Variables

In the Playground, you can declare **temporary variables** using vertical bars `|`:

```smalltalk
| x |
x := 10.
x + 5
```

Select all three lines and "Print it". The result should be `15`.

Let's break this down:

- `| x |` declares a variable named `x`
- `x := 10` assigns the value `10` to `x` (the `:=` means "assign")
- `x + 5` adds 5 to x

The vertical bars `||` create a "variable declaration zone". You can declare multiple variables:

```smalltalk
| width height area |
width := 10.
height := 5.
area := width * height.
area
```

Try "Print it" on this entire block. The result is `50`.

**Important**: The last expression in a block is what gets returned. In this case, `area` is the last line, so its value (50) is what "Print it" displays.

### The Assignment Operator `:=`

The `:=` operator assigns a value to a variable. It's two characters: colon followed by equals.

```smalltalk
| message |
message := 'Hello, Smalltalk!'.
message
```

This creates a variable `message`, assigns a string to it, and then returns that string.

You can change a variable's value:

```smalltalk
| counter |
counter := 0.
counter := counter + 1.
counter := counter + 1.
counter
```

Try "Print it". The result is `2`.

Each time we do `counter := counter + 1`, we're:
1. Getting the current value of counter
2. Adding 1 to it
3. Storing the result back in counter

### Variables Persist Within an Execution

When you "Do it", "Print it", or "Inspect it" on a block of code, variables exist for that execution only. Once it's done, they're gone.

Try this:

```smalltalk
| x |
x := 42.
x
```

Print it - you get `42`. Good!

Now, on a new line, try to print `x`:

```smalltalk
x
```

If you "Print it" on just this line, you'll get an error! The variable `x` doesn't exist anymore - it was only alive during the previous execution.

To use the same variable across multiple statements, put them all together:

```smalltalk
| x |
x := 42.
Transcript show: x printString; cr.
x + 10
```

This works because it's all one execution.

## The Cascade Operator `;`

When you want to send multiple messages to the same object, you can use the **cascade operator** `;` instead of repeating the object:

Without cascade:

```smalltalk
Transcript show: 'Hello'.
Transcript show: ' '.
Transcript show: 'World'.
Transcript cr.
```

With cascade:

```smalltalk
Transcript
    show: 'Hello';
    show: ' ';
    show: 'World';
    cr.
```

Try "Do it" on the cascade version (make sure your Transcript is open). Both versions do the same thing, but the cascade version is cleaner - you write `Transcript` once, then send it multiple messages separated by semicolons.

The semicolon means "send the next message to the same object I just sent a message to."

Here's another example:

```smalltalk
| array |
array := OrderedCollection new.
array
    add: 10;
    add: 20;
    add: 30;
    yourself.
array
```

Try "Print it" on this whole block. You create an OrderedCollection (a growable list) and add three numbers to it.

The `yourself` at the end is a common idiom - it returns the object itself. This ensures the whole cascade expression returns the collection, not the result of the last `add:` message.

## Return Values vs Side Effects

It's important to understand the difference between **return values** and **side effects**.

### Return Value

Every message send returns something. That's the return value.

```smalltalk
2 + 3
```

The return value is `5`.

```smalltalk
'hello' size
```

The return value is `5` (the number of characters).

### Side Effect

Some messages also cause side effects - they do something besides just returning a value.

```smalltalk
Transcript show: 'Hello'.
```

This has a **side effect**: it displays "Hello" in the Transcript window. The return value? Actually, it returns the Transcript object itself, but we usually don't care about that - we're sending this message for the side effect.

When you use "Print it", you see the return value. That's why `Transcript show: 'Hello'` doesn't show "Hello" in the Playground - instead, it shows the Transcript object. To see "Hello", you need to look at the Transcript window (the side effect) or use "Do it" instead of "Print it".

## Parentheses for Grouping

Sometimes you need to control the order of operations. Use parentheses:

```smalltalk
2 + 3 * 4
```

Try "Print it". What result do you get?

You get `20`. That's because in Smalltalk, messages are evaluated strictly left to right (we'll learn about precedence properly in Chapter 4). So it's (2 + 3) * 4 = 5 * 4 = 20.

If you wanted 2 + (3 * 4), you'd write:

```smalltalk
2 + (3 * 4)
```

Now you get `14`.

Parentheses force Smalltalk to evaluate the expression inside them first.

## Experimenting Safely

Here's the beautiful thing about the Playground: **you can't break anything**.

Well, technically you *could* do something disruptive if you really tried, but for normal experimentation, the Playground is completely safe. If you write something that doesn't work, you'll get an error dialog. Just close it and try again.

Let's intentionally cause an error:

```smalltalk
10 / 0
```

Try "Print it" on this. Division by zero! A debugger window opens showing the error.

Don't panic! This is normal. The debugger is actually a powerful tool (we'll explore it in Chapter 21), but for now, just close the debugger window. Your Playground is fine. Nothing is broken.

This forgiveness is one of Smalltalk's greatest teaching features. You can try things, see what happens, and learn from the results without fear.

## Common Playground Patterns

Here are some patterns you'll use frequently:

### Quick Calculations

```smalltalk
"How many seconds in a day?"
24 * 60 * 60
```

### String Manipulation

```smalltalk
| name |
name := 'smalltalk'.
name capitalized
```

### Exploring Objects

```smalltalk
Date today inspect  "Opens an inspector"
```

The `inspect` message is like using "Inspect it" - it opens an inspector on the object.

### Testing Methods

```smalltalk
'programming' includesSubstring: 'gram'
```

Returns `true`.

### Working with Collections

```smalltalk
| numbers |
numbers := #(1 2 3 4 5).
numbers sum
```

Returns `15`.

### Concatenating

```smalltalk
'Smalltalk' , ' ' , 'is' , ' ' , 'awesome!'
```

Returns `'Smalltalk is awesome!'`.

## The Spotter - Quick Search

While we're talking about essential tools, let me introduce you to the **Spotter**. It's not part of the Playground, but it's so useful you should know about it now.

Press `Shift+Enter` anywhere in Pharo (on Windows/Linux) or `Shift+Return` (on macOS).

A search box appears! This is the **Spotter** - Pharo's quick search tool. You can:

- Type class names to open them
- Search for methods
- Find packages
- Look for menu items
- And much more

Try typing "Array" in the Spotter and press Enter. It opens a browser showing the Array class!

Press `Escape` to close the Spotter. Press `Shift+Enter` anytime you need to find something quickly.

## Saving Your Playground Content

The Playground's content is part of the image. If you save your image (`Ctrl+S` or `Cmd+S`, or via the World Menu), your Playground content is saved too.

But what if you want to save specific code snippets separately?

### The Publish Feature

At the bottom of the Playground, you might see a "Publish" button or icon. Clicking it lets you save your current Playground content as a named snippet that you can retrieve later.

Try it:
1. Write something in your Playground
2. Click "Publish" (or look in the Playground menu)
3. Give it a name
4. Later, you can open a new Playground and access saved snippets

(Note: The exact publish feature varies between Pharo versions, but the concept is there.)

### Multiple Playgrounds

You can have multiple Playground windows open at once! Each is independent.

Open a few Playgrounds (`Ctrl+O` `W` several times). Use them for different experiments. You might have:
- One for math experiments
- One for string manipulations
- One for testing date/time functions

This helps keep your work organized.

## Keyboard Shortcuts Summary

Here are the essential shortcuts:

| Action | Windows/Linux | macOS |
|--------|---------------|-------|
| Do it | `Ctrl+D` | `Cmd+D` |
| Print it | `Ctrl+P` | `Cmd+P` |
| Inspect it | `Ctrl+I` | `Cmd+I` |
| Save image | `Ctrl+S` | `Cmd+S` |
| Open Playground | `Ctrl+O` then `W` | `Cmd+O` then `W` |
| Spotter | `Shift+Enter` | `Shift+Return` |

You don't need to memorize these right now - they'll become natural with practice.

## Common Beginner Mistakes

### Forgetting to Select Code

Remember: you need to **select** (highlight) the code you want to execute before using "Do it", "Print it", or "Inspect it".

A common mistake is clicking in the code but not selecting it, then wondering why nothing happens when you press `Ctrl+P`.

**Tip**: To execute all the code in your Playground, press `Ctrl+A` (or `Cmd+A`) to select all, then use your command.

### Mixing Up `:=` and `=`

Assignment uses `:=` (colon-equals), not just `=`.

```smalltalk
x := 10  "Correct - assignment"
x = 10   "This is a comparison, not assignment!"
```

The single `=` is actually a comparison operator (returns true or false). We'll cover this in Chapter 8.

### Forgetting the Variable Declaration

If you try to use a variable without declaring it, you'll get an error:

```smalltalk
x := 10.  "Error! x hasn't been declared"
```

You need:

```smalltalk
| x |      "Declare x first"
x := 10.  "Now you can assign to it"
```

### Not Including Everything in One Selection

If you declare variables, you need to select the declarations too:

**Wrong:**
```smalltalk
| x |
x := 10.
```

If you only select `x := 10` and "Print it", you'll get an error because `x` isn't declared in that selection.

**Right:** Select both lines (the declaration and the assignment) together.

## Try This!

Time to experiment! Try each of these in your Playground:

1. **Calculator**: Calculate how many minutes you've been alive (approximately). Use your age in years, multiply by 365, then by 24, then by 60.

2. **String art**: Try these string manipulations:
   ```smalltalk
   'Racecar' reversed
   'listen' sorted
   'evil' reversed
   ```

3. **Variables**: Create variables for your name and age, then create a sentence:
   ```smalltalk
   | name age |
   name := 'Your Name'.
   age := 25.
   'My name is ' , name , ' and I am ' , age printString , ' years old.'
   ```

4. **Dates**: Explore dates:
   ```smalltalk
   Date today
   Date tomorrow
   Date yesterday
   Date today dayOfWeek
   Date today addDays: 100
   ```

5. **Inspector exploration**: Inspect a string:
   ```smalltalk
   'Hello, Smalltalk!' inspect
   ```
   In the Inspector, try sending it messages like `size`, `reversed`, `asUppercase`.

6. **Cause an error on purpose**: Try something nonsensical and see what happens:
   ```smalltalk
   'hello' + 5
   ```
   You can't add a number to a string! Close the debugger and try other experiments.

7. **Transcript art**: Make a pattern in the Transcript:
   ```smalltalk
   Transcript clear.
   Transcript show: '***'; cr.
   Transcript show: '***'; cr.
   Transcript show: '***'; cr.
   ```

## The Playground Philosophy

The Playground embodies Smalltalk's philosophy: **immediate feedback and exploration**.

You don't write code, save it, compile it, run it, and then see what happens. You write a line, execute it immediately, see the result, and keep going. This tight feedback loop is incredibly valuable for learning.

As you progress through this book, you'll use the Playground constantly. It's not just for beginners - expert Smalltalkers use it every day for:
- Testing ideas
- Exploring APIs
- Debugging
- Prototyping
- Quick calculations
- Learning new libraries

Get comfortable in the Playground. It's your laboratory, your sketchpad, and your assistant all in one.

## Coming Up Next

You now know how to use the Playground to experiment with Smalltalk code. But what IS this code, really? What are these "objects" and "messages" we keep talking about?

In Chapter 4, we'll dive deep into Smalltalk's fundamental concept: **everything is an object**. You'll learn what objects really are, how they communicate via messages, and why this approach is so powerful.

This is where Smalltalk really starts to click!

---

**Key Takeaways:**
- The Playground (Workspace) is your interactive programming environment
- **Do it** executes code, **Print it** shows the result, **Inspect it** opens an explorer
- Use `| varName |` to declare variables, `:=` to assign values
- The period `.` separates statements, the semicolon `;` chains messages to the same object
- Comments use double quotes `"like this"`
- You can't break anything in the Playground - experiment freely!
- The Spotter (`Shift+Enter`) is your quick search tool
- Every expression returns an object
- Select code before executing it with Do it, Print it, or Inspect it

---

[Previous: Chapter 2 - Your First Steps](chapter-02-your-first-steps.md) | [Next: Chapter 4 - Everything is an Object](chapter-04-everything-is-an-object.md)
