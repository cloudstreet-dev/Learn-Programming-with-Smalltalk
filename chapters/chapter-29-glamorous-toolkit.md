# Chapter 29: Glamorous Toolkit - Moldable Development

The **Glamorous Toolkit** (GT) is the most radical reimagining of development environments in decades. It's not just another Smalltalk - it's a fundamentally different way of working with software.

GT introduces **moldable development**: the idea that development tools should adapt to your specific domain, not the other way around. Every tool in GT can be customized for your particular problem, making complex systems understandable.

## What is Glamorous Toolkit?

**Glamorous Toolkit** is a Smalltalk environment focused on making sense of complex systems. Created by Tudor Gîrba and the feenk team, it's built on Pharo but diverges significantly in philosophy and tooling.

### Core Ideas:

1. **Moldable** - Tools adapt to your domain
2. **Visual** - Everything has custom visualizations
3. **Explainable** - Systems explain themselves
4. **Exploratory** - Navigate by understanding, not structure
5. **Context-aware** - Tools show what's relevant

GT believes: **Reading code is not enough. You need custom tools for your specific domain.**

## Why Glamorous Toolkit?

### Traditional Development:

```
Generic tools (IDE, debugger, profiler)
    ↓
Applied to all domains
    ↓
You adapt to the tools
```

One-size-fits-all approach.

### Moldable Development:

```
Your specific domain
    ↓
Custom tools created for this domain
    ↓
Tools adapt to you
```

Tools customized for your problem!

### The Problem GT Solves

Modern systems are **complex**:
- Thousands of classes
- Millions of lines of code
- Multiple frameworks
- Distributed architectures
- Legacy code

**Generic tools don't scale.** You need custom views, custom inspectors, custom debuggers for your specific system.

GT makes creating these custom tools **trivial**.

## Installing Glamorous Toolkit

1. Visit **gtoolkit.com**
2. Download for your platform (Windows, macOS, Linux)
3. Extract and run

GT is self-contained - everything you need is included!

### First Launch

When GT starts, you see:
- **Glamorous Toolkit** window
- **Coder** - The code browser
- **Playground** - Executable examples
- **Lepiter** - Knowledge management system

The interface is sleek, modern, and very different from traditional Smalltalk!

## Key Concepts

### 1. Everything is Moldable

Every tool can be customized:
- Inspectors show custom views
- Code browsers show domain-specific information
- Debuggers have custom actions
- Even the Playground is moldable

### 2. Examples as Documentation

Instead of comments, write **executable examples**:

```smalltalk
exampleEmptyList
    <gtExample>
    ^ OrderedCollection new
```

Examples are tests AND documentation!

### 3. Lepiter - Knowledge Management

**Lepiter** is a notebook system for capturing knowledge:
- Executable code snippets
- Explanations
- Diagrams
- Links between concepts

Think Jupyter notebooks, but moldable!

### 4. Custom Inspectors

Every object can define custom views:

```smalltalk
gtPointsFor: aView
    <gtView>
    ^ aView list
        title: 'Points';
        items: [ points ];
        itemText: [ :each | each printString ]
```

Now when you inspect this object, you see a custom "Points" tab!

## The Coder

GT's code browser is called **Coder**. It's fundamentally different from traditional browsers:

### Features:

- **Unified view** - Classes, methods, and packages in one place
- **Search-driven** - Find anything instantly
- **Preview** - See code without navigating
- **Context-aware** - Shows relevant information
- **Moldable** - Customize for your domain

### Navigation:

Instead of browsing packages → classes → methods, you **search** for what you need and GT shows context.

## Custom Views

The heart of GT is custom views. Create views for any object:

### Example: Visualizing a Graph

```smalltalk
Object subclass: #Graph
    instanceVariableNames: 'nodes edges'
    ...
```

Add a custom view:

```smalltalk
gtGraphFor: aView
    <gtView>
    ^ aView mondrian
        title: 'Graph';
        painting: [ :view |
            view nodes
                shape: [ :each | BlElement new size: 10@10; background: Color blue ];
                with: nodes.
            view edges
                fromCenterBottom; toCenterTop;
                connectFrom: #first toAll: #second.
            view layout tree ]
```

Now inspecting a Graph shows a visual graph!

### Example: Custom List View

```smalltalk
gtItemsFor: aView
    <gtView>
    ^ aView list
        title: 'Items';
        items: [ items ];
        itemText: [ :each | each name ];
        itemFormat: [ :each | each status = #done
            ifTrue: [ each name asRopedText foreground: Color gray ]
            ifFalse: [ each name asRopedText ] ]
```

Completed items appear in gray!

## Executable Examples

Replace comments with examples:

### Traditional Comment:

```smalltalk
"Create an empty order and add items:
  order := Order new.
  order addItem: (Item name: 'Book' price: 15).
  order total. -> 15"
```

Comments rot - they stop matching the code!

### GT Example:

```smalltalk
exampleOrderWithItem
    <gtExample>
    | order item |
    order := Order new.
    item := Item name: 'Book' price: 15.
    order addItem: item.
    self assert: order total equals: 15.
    ^ order
```

This is executable! If it doesn't work, the example fails.

### Chaining Examples:

```smalltalk
exampleEmptyOrder
    <gtExample>
    ^ Order new

exampleOrderWithItem
    <gtExample>
    | order |
    order := self exampleEmptyOrder.
    order addItem: (Item name: 'Book' price: 15).
    ^ order
```

Examples build on each other!

## Lepiter Notebooks

**Lepiter** is GT's knowledge management system.

### Create a Notebook:

1. GT Home → Lepiter
2. Click "+" to create a database
3. Add pages

### Lepiter Pages:

Pages contain:
- **Text** - Markdown-like formatting
- **Code snippets** - Executable Smalltalk
- **Queries** - Search and embed results
- **Links** - Connect pages and code

### Example Page:

```
# User Authentication

Our system uses JWT tokens for authentication.

## Implementation

The main class is:
[[User]]

Example usage:
```smalltalk
user := User named: 'alice'.
token := user generateToken.
token validate
```

See also: [[API Documentation]]
```

Double brackets `[[User]]` link to the User class!

## Visualization with Mondrian

GT includes **Mondrian** for data visualization:

```smalltalk
| view |
view := GtMondrian new.
view nodes
    shape: [ :each | BlElement new size: 10@10; background: Color random ];
    with: (1 to: 20).
view layout circle.
view
```

Creates a circle of colored nodes!

### Complex Visualization:

```smalltalk
gtDependenciesFor: aView
    <gtView>
    ^ aView mondrian
        title: 'Dependencies';
        painting: [ :view |
            view nodes
                shape: [ :each |
                    BlTextElement new text: each name asRopedText ];
                with: self allClasses.
            view edges
                connectFromAll: [ :each | each dependencies ].
            view layout force ]
```

Visualize class dependencies!

## The Inspector

GT's Inspector is radically different:

### Multiple Views:

Each object has many views:
- **Raw** - Instance variables
- **Print** - Text representation
- **Custom views** - Domain-specific

Click tabs to switch views!

### Live Editing:

Edit object state directly in the Inspector. Changes apply immediately!

### Embedded Navigation:

Click on values to navigate deeper. The Inspector tracks your path - navigate back easily!

## The Playground

GT's Playground is enhanced:

### Snippets:

Playground pages contain multiple snippets:

```smalltalk
"Snippet 1:"
orders := OrderedCollection new.

"Snippet 2:"
orders add: (Order new total: 100).

"Snippet 3:"
orders collect: #total
```

Each snippet remembers its results!

### Inline Visualizations:

```smalltalk
| data |
data := #(1 5 3 8 2 9 4).
data inspect
```

The Inspector opens **inline** in the Playground!

## Debugging in GT

GT's debugger has custom extensions:

### Custom Debug Actions:

Add domain-specific debugging actions:

```smalltalk
gtDebugActionResumeWith42
    <gtDebugAction>
    ^ GLMGenericAction new
        action: [ :debugger | debugger resumeWith: 42 ];
        icon: BrGlamorousVectorIcons accept;
        label: 'Resume with 42'
```

Now in the debugger, you have a "Resume with 42" button!

### Visualization While Debugging:

Inspect objects while debugging and see custom visualizations. Understand complex state visually!

## Moldable Development Workflow

### Traditional Workflow:

1. Read generic code
2. Guess what it does
3. Add print statements
4. Re-run
5. Repeat

### GT Workflow:

1. Create custom view for your domain
2. Visualize the system
3. Understand immediately
4. Create examples
5. Done!

## Practical Example: Understanding a Web API

You have a REST API with many endpoints. How do you understand it?

### Traditional Approach:

Read code, trace through methods, build mental model.

### GT Approach:

**Step 1: Create a custom view:**

```smalltalk
gtEndpointsFor: aView
    <gtView>
    ^ aView list
        title: 'Endpoints';
        items: [ self allEndpoints ];
        itemText: [ :each | each method, ' ', each path ]
```

**Step 2: Visualize:**

```smalltalk
gtEndpointMapFor: aView
    <gtView>
    ^ aView mondrian
        title: 'Endpoint Map';
        painting: [ :view |
            view nodes
                shape: [ :each | BlTextElement new text: each path asRopedText ];
                with: self allEndpoints.
            "... visualization logic ..." ]
```

**Step 3: Add examples:**

```smalltalk
exampleGetUser
    <gtExample>
    ^ self get: '/users/123'
```

Now you understand the API **visually** with **executable examples**!

## When to Use GT

### Great For:

- **Understanding complex systems** - Visualize architecture
- **Domain-specific development** - Custom tools for your domain
- **Documentation** - Lepiter notebooks
- **Teaching** - Explain with examples
- **Data analysis** - Visual exploration

### Consider Alternatives For:

- **Quick scripts** - Pharo is simpler
- **Traditional web development** - Pharo has better frameworks
- **Team with no GT experience** - Learning curve

## GT vs Pharo

| Feature | GT | Pharo |
|---------|-----|-------|
| Focus | Understanding systems | General development |
| Tools | Moldable | Standard |
| Inspector | Custom views | Standard views |
| Documentation | Lepiter notebooks | Comments |
| Examples | First-class | Optional |
| Visualization | Built-in (Mondrian) | Add-on (Roassal) |
| Learning Curve | Steeper | Gentler |
| Innovation | Cutting-edge | Stable |

**Use GT when** you need to understand complex systems and are willing to invest in creating custom tools.

**Use Pharo when** you want stable, well-supported, traditional Smalltalk development.

## Resources

### Official

- **gtoolkit.com** - Main site
- **Book** - Free online at gtoolkit.com/docs
- **Discord** - Active community
- **YouTube** - Tutorial videos

### Learning GT

Start with:
1. Download GT
2. Open the "GT Book" (included)
3. Follow the tutorials
4. Create custom views for your domain

## Try This!

Explore GT:

1. **Download GT**
   - Visit gtoolkit.com
   - Download and extract
   - Launch

2. **Open the GT Book**
   - GT Home → Lepiter → "Glamorous Toolkit Book"
   - Read and execute examples

3. **Create a Custom Inspector**
   ```smalltalk
   Object subclass: #Person
       instanceVariableNames: 'name age'
       ...

   gtDetailsFor: aView
       <gtView>
       ^ aView list
           title: 'Details';
           items: [
               { 'Name' -> name. 'Age' -> age } ];
           itemText: [ :assoc | assoc key, ': ', assoc value printString ]
   ```

   Inspect a Person - see the custom "Details" view!

4. **Write an Example**
   ```smalltalk
   examplePerson
       <gtExample>
       ^ Person new name: 'Alice'; age: 30; yourself
   ```

   Run it - see the result!

5. **Create a Lepiter Page**
   - Lepiter → New page
   - Add text and code snippets
   - Link to classes with `[[ClassName]]`

6. **Visualize Data**
   ```smalltalk
   | view data |
   data := #(5 8 3 9 2 7 4).
   view := GtMondrian new.
   view nodes
       shape: [ :each | BlElement new size: each * 10 @ 20; background: Color blue ];
       with: data.
   view layout horizontalLine.
   view
   ```

   See a bar chart!

## The Philosophy

GT believes:

1. **Reading code is insufficient** - You need custom tools
2. **Tools should be cheap** - Creating custom tools should be trivial
3. **Examples over comments** - Executable documentation
4. **Visual over textual** - Pictures worth 1000 words
5. **Moldable, not generic** - Adapt tools to your domain
6. **Understanding first** - Before changing, understand

## The Vision

GT aims to make **understanding** cheap. Currently:
- Understanding complex systems is expensive
- We spend more time reading than writing
- Generic tools don't help with domain complexity

GT makes understanding **orders of magnitude** cheaper by providing custom tools for your specific domain.

## Looking Ahead

You now understand Glamorous Toolkit - the moldable development environment! You know:
- What makes GT unique
- Moldable development philosophy
- Custom inspectors and views
- Lepiter knowledge management
- Executable examples
- When to use GT vs Pharo/Squeak

In Chapter 30, we'll survey **Other Smalltalks Worth Knowing**: VisualWorks, Smalltalk/X, VA Smalltalk, Cuis, and more. Each has unique strengths and use cases!

Part VIII completes with an understanding of the diverse Smalltalk ecosystem!

---

**Key Takeaways:**
- **Glamorous Toolkit** (GT) is a moldable development environment
- **Moldable** means tools adapt to your specific domain
- Created by Tudor Gîrba and feenk team, based on Pharo
- Focus on **understanding complex systems**
- **Custom inspectors** for any object via `<gtView>`
- **Executable examples** replace comments via `<gtExample>`
- **Lepiter** - Knowledge management with executable notebooks
- **Mondrian** - Built-in data visualization
- Tools should be **cheap to create** for your domain
- Radically different from traditional Smalltalks
- Steeper learning curve but powerful for complex systems
- Great for understanding, documenting, visualizing
- Reading code isn't enough - you need custom tools
- Free, open source, cross-platform

---

[Previous: Chapter 28 - Squeak](chapter-28-squeak.md) | [Next: Chapter 30 - Other Smalltalks Worth Knowing](chapter-30-other-smalltalks.md)
