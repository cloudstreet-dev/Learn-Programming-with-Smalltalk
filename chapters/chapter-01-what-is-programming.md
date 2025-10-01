# Chapter 1: What is Programming (And Why Smalltalk?)

Welcome to your first chapter! If you've never programmed before, you're in for an exciting journey. If you *have* programmed before, you might be surprised by how different Smalltalk is from what you know. Either way, let's start at the very beginning.

## What is Programming?

At its heart, **programming** is the art of giving instructions to a computer. But that's like saying painting is "putting colors on canvas" - technically true, but it misses so much of what makes it interesting.

Programming is really about:

- **Solving problems** - You have a task, and you need to break it down into steps a computer can follow
- **Building tools** - Creating programs that help people do things faster, better, or in ways they couldn't before
- **Expressing ideas** - Converting thoughts and logic into a form that can be executed
- **Creating experiences** - Everything from websites to games to the operating system you're using right now

Think about your phone's calculator app. Someone had to:
1. Decide what buttons it should have
2. Determine what happens when you press each button
3. Figure out how to store numbers and perform calculations
4. Make it display results in a readable way
5. Handle errors (like dividing by zero)

That's programming! And every app, every website, every digital tool you've ever used started as someone sitting down and writing instructions for a computer to follow.

### Why Do We Need Programming Languages?

Computers, at their most basic level, only understand patterns of electrical signals - essentially ones and zeros. This is called **machine code**, and it looks something like this:

```
10110000 01100001
```

Humans aren't very good at thinking in ones and zeros. We need languages that let us express ideas in ways that make sense to *us*, which then get translated into machine code. That's what programming languages are for.

Different programming languages have different philosophies, strengths, and ways of thinking about problems. Some languages are:

- **Low-level** - Closer to how the computer actually works (like C or Assembly)
- **High-level** - Closer to how humans think (like Python or JavaScript)
- **Procedural** - Focused on sequences of instructions (like C or Pascal)
- **Object-oriented** - Focused on organizing code into objects (like Java or Ruby)
- **Functional** - Focused on mathematical functions (like Haskell or Lisp)

Smalltalk is a **high-level, object-oriented** language. But it's not just object-oriented - it was one of the *first* truly object-oriented languages, and it took the idea further than most languages that came after it.

## What Makes Smalltalk Special?

You might be wondering: "If I want to learn programming, why should I choose Smalltalk? Why not Python, JavaScript, or Java - languages I hear about more often?"

Great question! Here's why Smalltalk is an exceptional choice for learning programming:

### 1. **Everything is an Object**

In Smalltalk, literally everything is an object. Not "most things" or "well, except for these special cases" - *everything*.

The number `3`? That's an object.
The word `true`? That's an object.
A piece of code? That's an object too.

This consistency means there are fewer special cases to memorize. Once you understand how objects work, you understand how *everything* works.

In many other languages, you have "regular" values (like numbers) and "special" objects, and they follow different rules. Smalltalk says: "No, let's make everything follow the same rules." This makes learning much simpler.

### 2. **Ridiculously Simple Syntax**

Smalltalk's entire syntax - the grammatical rules of the language - fits on a postcard. Seriously. People have literally printed it on postcards.

Compare this to languages like Java or C++, where you need thick books just to cover the syntax. In Smalltalk, you can learn all the grammar rules in an afternoon and spend your time actually *learning to program* instead of memorizing syntax quirks.

Here's a complete list of Smalltalk's punctuation marks that have special meaning:
- `.` (period) - separates statements
- `;` (semicolon) - chains messages to the same object
- `|` (vertical bars) - declares variables
- `'` (single quotes) - marks strings
- `:=` (colon-equals) - assigns values
- `^` (caret) - returns from a method
- `[]` (square brackets) - creates blocks of code
- `()` (parentheses) - groups expressions

That's it. That's the whole language syntax. Everything else is just objects sending messages to each other.

### 3. **Live Programming**

Here's where Smalltalk gets really magical: you can modify your program *while it's running*.

In most programming languages, you:
1. Write code
2. Compile it or run it
3. See if it works
4. Stop the program
5. Make changes
6. Start over

In Smalltalk, you:
1. Start your program
2. Watch it run
3. Make changes while it runs
4. See the changes immediately
5. Keep working

It's like being able to repair and modify a car while it's driving down the highway. This immediate feedback makes learning faster and more intuitive. You can experiment, see results instantly, and understand cause and effect in real-time.

### 4. **The Image - Your Living Environment**

Smalltalk doesn't just save your code as text files. Instead, it saves your entire programming environment - every object, every value, everything - as a snapshot called an **image**.

Think of it like saving your game in a video game. When you load that save file, everything is exactly as you left it. Your character, your inventory, your location - all preserved.

Similarly, a Smalltalk image contains:
- All your code
- All your objects
- All your tools
- The entire state of your program

You can stop working on a Friday afternoon, save your image, go home, come back Monday morning, load the image, and pick up *exactly* where you left off. Every object is still there, every state is preserved.

This makes Smalltalk fantastic for learning because you can build up your environment gradually, experimenting and saving your work as you go.

### 5. **Incredible Development Tools**

Smalltalk comes with tools that many other languages are still trying to catch up to:

- **The Browser** - Navigate through all the code (yours and the system's)
- **The Inspector** - Look inside any object and see what it contains
- **The Debugger** - Not just for finding bugs, but for writing code and exploring
- **The Finder** - Search for code and examples throughout the system

These tools aren't add-ons or third-party plugins. They're built into the language itself, and they're first-class citizens. We'll explore each of these tools in depth later in the book.

### 6. **Learn by Exploring**

Because Smalltalk's entire system is made of objects, and you can inspect any object, you can learn by exploration. Wondering how strings work? Open a string in an inspector and look at its methods. Curious about how collections work? Browse the Collection class and see the implementation.

It's like learning biology by having access to real organisms to study, rather than just reading about them in a textbook. The code is right there, waiting for you to explore it.

### 7. **Pure Object-Oriented Design**

If you're going to learn object-oriented programming, why not learn it from the language that pioneered it and took it to its logical conclusion?

Smalltalk doesn't mix paradigms or make compromises. It's purely object-oriented, which means you learn the concepts in their clearest, most consistent form. Later, if you learn other languages like Java, Python, or Ruby, you'll understand *why* they do things the way they do - because you'll have seen the original vision.

## A Brief History (Don't Worry, This Won't Be Boring)

Smalltalk was created in the 1970s at Xerox PARC (Palo Alto Research Center) by a team led by Alan Kay. PARC was one of those magical places where the future was being invented - they also created the graphical user interface (GUI), the laser printer, and Ethernet networking.

Alan Kay had a vision: what if everything in computing was an object, and those objects communicated by sending messages to each other? What if the computer itself was a medium for exploring ideas, not just a calculator?

The team wanted to create a programming environment so simple that children could use it, yet powerful enough for professional programmers. They largely succeeded.

Here are some things that Smalltalk pioneered or popularized:

- **Object-oriented programming** - The pure form that influenced languages like Java, C++, Python, and Ruby
- **Graphical IDEs** - The idea of having windows, menus, and tools to help you program
- **Test-driven development** - The practice of writing tests before writing code
- **Refactoring tools** - Tools to help you restructure code safely
- **Model-View-Controller (MVC)** - A design pattern used in countless applications
- **The windowing system** - Multiple overlapping windows that you can move around

When Steve Jobs visited Xerox PARC in 1979, he saw Smalltalk's graphical interface and was so impressed that it influenced the design of the Macintosh. The windows, icons, and mouse-driven interface you use today? That came from Smalltalk.

## The Smalltalk Family Tree

Over the decades, Smalltalk has evolved into several different versions, or "dialects." The major ones you'll encounter are:

### Pharo

**Pharo** is a modern, clean, actively developed Smalltalk. It removes old cruft, adds modern features, and has an active, welcoming community. It's excellent for:
- Learning programming
- Web development (with frameworks like Seaside and Teapot)
- Data visualization (with Roassal)
- Industrial applications

Pharo is what we'll use for most examples in this book because it's free, well-documented, and has excellent tools.

### Squeak

**Squeak** is a highly portable Smalltalk that runs on almost anything. It's the direct descendant of the original Smalltalk-80. Squeak is known for:
- Multimedia capabilities
- Educational tools (like Etoys for kids)
- Being able to run on everything from phones to Raspberry Pis
- A focus on stability and compatibility

Squeak is also free and has a wonderful community focused on education and creativity.

### Glamorous Toolkit (GT)

**Glamorous Toolkit** is the newest member of the family and takes a revolutionary approach called "moldable development." Instead of having fixed tools, you create custom tools for your specific problems. GT is:
- Ultra-modern and innovative
- Focused on making environments moldable
- Excellent for exploring complex data
- Still evolving rapidly

GT represents the cutting edge of where Smalltalk is going.

### Others

There are other Smalltalks worth mentioning:
- **Cuis** - A minimalist, elegant Smalltalk focused on simplicity
- **GNU Smalltalk** - A command-line Smalltalk for Unix systems
- **VisualWorks** - A commercial Smalltalk used in industry
- **VA Smalltalk** - Another commercial version with enterprise features

We'll explore these different flavors later in the book (Part VIII), but don't worry about the differences yet. They all share the same fundamental concepts, which is what we'll be learning.

## What You Won't Learn (At Least Not Right Away)

This book focuses on fundamental programming concepts using Smalltalk. We're not going to:

- Rush you through just so we can say "you know programming"
- Assume you already understand computing concepts
- Skip explanations because "everyone knows that"
- Pretend that programming is either impossibly hard or trivially easy

Programming is a learnable skill. Some concepts will click immediately. Others will take time and practice. That's normal. We're going to take the time to build solid foundations.

## What to Expect

Here's what your journey will look like:

**Part I (Chapters 1-3)**: You'll install Smalltalk and get comfortable with the environment. You'll run your first programs in the Workspace, which is like a programming playground.

**Part II (Chapters 4-7)**: You'll learn about objects - what they are, how they work, and how to use the ones that come with Smalltalk (numbers, strings, collections, etc.).

**Part III (Chapters 8-10)**: You'll learn about making decisions (if this, then that) and repetition (loops), using Smalltalk's unique approach with blocks.

**Part IV (Chapters 11-15)**: You'll create your own objects by defining classes, methods, and using inheritance. This is where you become not just a user of code, but a creator.

**Part V (Chapters 16-18)**: You'll understand Smalltalk's unique image-based system and how to manage your code with version control.

**Part VI (Chapters 19-22)**: You'll master the development tools that make Smalltalk so productive.

**Part VII (Chapters 23-26)**: You'll learn intermediate concepts like polymorphism, error handling, testing, and organizing larger programs.

**Part VIII (Chapters 27-30)**: You'll explore the different Smalltalk environments in depth.

**Part IX (Chapters 31-35)**: You'll build three complete projects and work with files, graphics, and web servers.

**Part X (Chapters 36-40)**: You'll learn about design patterns, performance, the community, and where to go next.

## A Word of Encouragement

If you've never programmed before, you might feel intimidated. Don't be. Programming is just a skill, like cooking or playing an instrument. Nobody expects you to be a master chef after reading one cookbook, and nobody should expect you to be an expert programmer after one book.

What you *can* expect:
- By the end of this book, you'll be able to write real programs
- You'll understand fundamental concepts that apply to all programming languages
- You'll be able to explore and learn on your own
- You'll have a solid foundation to build upon

If you've programmed before in other languages, you might be tempted to skim chapters or skip ahead. I'd encourage you to resist that urge, at least for Part II and Part V. Smalltalk does things differently, and understanding *why* will make you a better programmer in any language.

## Your First Programming Wisdom

Let me leave you with something important: **Programming is not about memorizing syntax or commands.** Programming is about:

1. **Breaking problems into smaller problems** - Taking something complex and figuring out the steps
2. **Thinking logically** - Understanding cause and effect
3. **Recognizing patterns** - Seeing similarities between different problems
4. **Debugging** - Finding and fixing problems (which you'll do a lot!)
5. **Iterating** - Making something, trying it, improving it, repeat

The syntax - the specific words and punctuation you use - is just the way you express these ideas to the computer. You'll pick up syntax naturally as you practice. What matters is learning to think like a programmer.

And here's a secret: even experienced programmers look things up constantly. The goal isn't to memorize everything; it's to understand concepts well enough that you know what to look for when you need it.

## Try This!

Before moving to the next chapter, take a moment to think about:

1. **What do you want to create?** - A game? A tool to help with a hobby? Something to make your job easier? Having a goal in mind will motivate you.

2. **What programs do you use every day?** - Your web browser, your email client, your music player. Someone programmed each of those. Start thinking about how they might work under the hood.

3. **What questions do you have?** - Write them down. As you go through this book, you'll find many of them answered. The ones that aren't? Those are great questions to explore on your own or ask the Smalltalk community.

## Coming Up Next

In Chapter 2, we'll roll up our sleeves and install Smalltalk. You'll see the environment for the first time, learn what an "image" really is, and take your first look at the tools you'll be using.

Don't worry about installing anything yet - Chapter 2 will walk you through everything step by step, with instructions for Windows, macOS, and Linux.

Are you ready? Let's do this!

---

**Key Takeaways:**
- Programming is about solving problems by giving instructions to computers
- Smalltalk is a pure object-oriented language where *everything* is an object
- Smalltalk has remarkably simple syntax - you can learn it all quickly
- Live programming and the image system make learning interactive and forgiving
- Modern versions (Pharo, Squeak, GT) are free, powerful, and actively maintained
- Programming is a learnable skill that develops with practice
- The goal is understanding concepts, not memorizing syntax

---

[Previous: Table of Contents](../TABLE_OF_CONTENTS.md) | [Next: Chapter 2 - Your First Steps](chapter-02-your-first-steps.md)
