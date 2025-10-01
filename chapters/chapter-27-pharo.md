# Chapter 27: Pharo - The Modern Smalltalk

Welcome to Part VIII: Exploring the Smalltalk Variations! You've learned Smalltalk fundamentals using examples that work across implementations. Now we'll explore specific Smalltalk systems, starting with **Pharo** - the modern, actively developed Smalltalk that's pushing the language forward.

Throughout this book, most examples have been Pharo-centric because it's accessible, well-documented, and actively maintained. Now let's explore what makes Pharo special and why it's an excellent choice for modern development.

## What is Pharo?

**Pharo** is a pure object-oriented dynamically typed language descending from Smalltalk-80. It began in 2008 as a fork of Squeak, with a focus on:

- **Clean, modern design** - Removing legacy cruft
- **Active development** - New releases regularly
- **Great tools** - Best-in-class development environment
- **Strong community** - Active, welcoming, growing
- **Industrial use** - Used in real companies for real products
- **Education** - Excellent for teaching and learning

Pharo is **open source** (MIT license) and runs on Windows, macOS, and Linux.

## Why Pharo?

### 1. Modern and Maintained

Pharo releases a new major version annually:
- **Pharo 11** (2023) - Latest stable
- **Pharo 12** (2024) - Current development
- Continuous improvements and modernization

### 2. Excellent Documentation

- **Pharo by Example** - Free online book
- **Deep into Pharo** - Advanced topics
- **Enterprise Pharo** - Web development
- **Mooc** - Free online courses
- Active forums and Discord

### 3. Best-in-Class Tools

- **System Browser** - Code navigation
- **Iceberg** - Git integration
- **Inspector** - Object exploration
- **Debugger** - Live debugging
- **Spotter** - Universal search
- **Critic Browser** - Code quality
- **Playground** - Interactive coding

### 4. Rich Ecosystem

Thousands of packages available:
- **Seaside** - Web framework
- **Voyage** - Object persistence
- **Roassal** - Data visualization
- **Pillar** - Documentation
- **Zinc HTTP** - HTTP client/server
- **NeoJSON** - JSON parsing
- **OpenAPI** - REST API support
- And many more!

### 5. Professional Quality

Used in:
- Financial services
- Industrial automation
- Research and education
- Data analysis and visualization
- Web applications
- IoT systems

## Installing Pharo

### Quick Install

1. Visit **pharo.org**
2. Click **Download**
3. Choose your platform:
   - **Windows**: Download and extract
   - **macOS**: Download .dmg, drag to Applications
   - **Linux**: Download and extract

4. Run the launcher or VM

That's it! You have a complete Pharo environment.

### PharoLauncher (Recommended)

The **PharoLauncher** manages multiple Pharo images:

1. Download from pharo.org
2. Install
3. Launch PharoLauncher
4. Select an image template (e.g., "Pharo 11.0 - 64bit (stable)")
5. Click "Create Image"
6. Launch the image

Now you can easily manage multiple projects and Pharo versions!

## First Steps in Pharo

### The Welcome Window

When Pharo opens, you see:
- **World menu** - Click background or click the Pharo logo
- **Taskbar** - Quick access to tools
- **Welcome window** - Links to documentation and tutorials

Close the welcome window for now.

### Opening Tools

Press shortcuts to open tools:
- **Ctrl+O W** (Cmd+O W on Mac) - Playground
- **Ctrl+O B** - System Browser
- **Shift+Enter** - Spotter (search)

Try opening a Playground and evaluate:

```smalltalk
'Welcome to Pharo!' inspect
```

An Inspector opens showing the string!

## Pharo's Philosophy

Pharo follows core principles:

### 1. Everything is an Object

Truly everything:
- Classes are objects
- Methods are objects
- Blocks are objects
- Even the compiler is an object!

### 2. Live Programming

The system is always running. You modify it while it runs. No compile-restart cycle.

### 3. Simple and Uniform

Small syntax, few keywords, uniform message sending. Easy to learn, powerful to use.

### 4. Reflective

The system can inspect and modify itself. You can browse, change, and extend everything.

### 5. Open and Clean

All source code is available and readable. Clean, modern codebase without legacy baggage.

## Key Pharo Features

### Modern UI Framework

Pharo uses **Spec 2** for building UIs:

```smalltalk
| presenter |
presenter := SpPresenter new.
presenter application: SpApplication new.
presenter layout: (SpBoxLayout newTopToBottom
    add: (SpLabelPresenter new label: 'Hello Pharo!'); yourself).
presenter open
```

Spec provides reusable, composable UI components.

### Iceberg (Git Integration)

Built-in Git support via Iceberg:
- Clone repositories
- Commit changes
- Push/pull
- Branch and merge
- All from within Pharo!

See Chapter 18 for details.

### Rich Standard Library

Pharo includes excellent collection classes:
- `Array`, `OrderedCollection`, `Set`, `Dictionary`
- `Bag`, `SortedCollection`, `LinkedList`
- Rich protocols: `do:`, `collect:`, `select:`, `reject:`, `detect:`, etc.

Strong support for:
- Strings and text processing
- Numbers (integers, floats, fractions)
- Dates and times
- File system access
- Network programming
- Parsing and compiling

### Quality Tools

- **Code Critics** - Detects code smells
- **Test Runner** - Run and manage tests
- **Profiler** - Performance analysis
- **Memory Monitor** - Track memory usage
- **Refactoring Tools** - Safe code transformations

### Documentation Browser

Access documentation from within Pharo:
- Class comments
- Method comments
- Help browser
- Example methods

## Pharo's Ecosystem

### Web Development

**Seaside** - Component-based web framework:

```smalltalk
renderContentOn: html
    html heading: 'Welcome!'.
    html paragraph: 'This is Seaside on Pharo.'
```

**Teapot** - Micro web framework:

```smalltalk
Teapot on
    GET: '/hello' -> 'Hello, Pharo!';
    start
```

### Data Visualization

**Roassal** - Powerful visualization library:

```smalltalk
| view |
view := RSCanvas new.
(1 to: 20) do: [ :i |
    view add: (RSEllipse new size: i * 5; yourself) ].
view open
```

### Database Access

**Voyage** - Object persistence:

```smalltalk
VOMongoRepository
    host: 'localhost'
    database: 'mydb'.

user := User new name: 'Alice'.
user save
```

### Machine Learning

**PolyMath** - Scientific computing:

```smalltalk
| matrix |
matrix := PMMatrix rows: #((1 2) (3 4)).
matrix determinant
```

### API Development

**OpenAPI** - REST API generation from specs:

```smalltalk
OpenAPIGenerator generate: '/path/to/openapi.yaml'
```

## Pharo Projects

Real projects built with Pharo:

### Commercial

- **4D** - Database management (uses Pharo for tooling)
- **Siemens** - Industrial automation
- **Thales** - Aerospace and defense systems
- Various fintech and trading systems

### Open Source

- **Dr. Geo** - Interactive geometry software
- **Moose** - Software analysis platform
- **PetitParser** - Parser framework
- **Pillar** - Document generator

### Research

Used in universities worldwide for:
- Software engineering research
- Programming language research
- Teaching object-oriented programming
- Data analysis and visualization

## Learning Resources

### Books (All Free Online!)

1. **Pharo by Example** - Comprehensive introduction
2. **Deep into Pharo** - Advanced topics
3. **Enterprise Pharo** - Web development
4. **Numerical Methods with Pharo** - Scientific computing

Available at: books.pharo.org

### Online Courses

**Pharo MOOC** - Free online course with videos:
- Week 1: Syntax and basics
- Week 2: Blocks and iterators
- Week 3: Classes and methods
- Week 4: Inheritance
- Week 5: Debugging
- Week 6: Advanced concepts

Available at: mooc.pharo.org

### Community

- **Discord** - Active chat (discord.gg/pharo)
- **Mailing lists** - pharo-users, pharo-dev
- **GitHub** - github.com/pharo-project
- **Stack Overflow** - Tag: pharo

## Unique Pharo Features

### Fluid Class Definition

Define classes with a fluent API:

```smalltalk
Object << #Point2D
    slots: { #x . #y };
    package: 'MyApp'
```

Cleaner than traditional syntax!

### Slots

Advanced instance variables with behavior:

```smalltalk
Object << #Person
    slots: { #name => ObservableSlot };
    package: 'MyApp'
```

Slots can trigger events, enforce constraints, etc.

### First-Class Instance Variables

Access instance variables reflectively:

```smalltalk
object instVarNamed: 'x' put: 42
```

### Metalinks

Intercept and modify behavior dynamically:

```smalltalk
method := MyClass >> #myMethod.
method ast link: (MetaLink new
    metaObject: [ :node | Transcript show: 'Executed!'; cr ];
    selector: #value:;
    yourself)
```

Now every call to `myMethod` logs "Executed!"

### Calypso Browser

The default browser in modern Pharo - faster, more extensible than older browsers.

## Development Workflow in Pharo

### Typical Session

1. **Launch Pharo**
2. **Open Playground** - Experiment with code
3. **Create classes** - Use System Browser
4. **Write tests** - Test-driven development
5. **Run tests** - Test Runner
6. **Debug** - Fix issues in Debugger
7. **Commit** - Save to Git via Iceberg
8. **Save Image** - Preserve your work

### Iterative Development

```
Write method → Test → Debug → Refactor → Repeat
```

All without restarting!

## Pharo vs Other Smalltalks

| Feature | Pharo | Squeak | Smalltalk/X | VisualWorks |
|---------|-------|--------|-------------|-------------|
| License | MIT | MIT | Various | Commercial |
| Active Dev | ✅ Very | ✅ Yes | ✅ Yes | ✅ Yes |
| Modern Tools | ✅ Excellent | ⚠️ Good | ⚠️ Good | ✅ Excellent |
| Git Integration | ✅ Built-in | ❌ Add-on | ⚠️ Limited | ⚠️ Limited |
| Documentation | ✅ Excellent | ⚠️ Good | ⚠️ Good | ✅ Excellent |
| Community | ✅ Large | ⚠️ Medium | ⚠️ Small | ⚠️ Small |
| Education | ✅ Great | ✅ Great | ⚠️ Good | ⚠️ Good |
| Commercial Use | ✅ Yes | ⚠️ Some | ✅ Yes | ✅ Yes |

Pharo excels in modern development practices and community support.

## When to Use Pharo

### Great For:

- **Learning** - Clean, modern, well-documented
- **Web development** - Seaside, Teapot
- **Data visualization** - Roassal
- **Rapid prototyping** - Fast, interactive
- **Research projects** - Reflective, moldable
- **Education** - Teaching OOP concepts

### Consider Alternatives For:

- **Embedded systems** - Use Smalltalk/X
- **Mobile apps** - Limited mobile support
- **High-performance number crunching** - C/Fortran better
- **Legacy Smalltalk projects** - May need VisualWorks or VA Smalltalk

## Installing Libraries

Use **Metacello** to install packages:

```smalltalk
Metacello new
    baseline: 'Roassal3';
    repository: 'github://ObjectProfile/Roassal3:master/src';
    load
```

Or use **Catalog Browser**:
- World menu → Tools → Catalog Browser
- Browse available packages
- Click to install

## Customizing Pharo

### Themes

Change the look:
- World menu → Settings → Appearance → Theme
- Choose: Pharo Light, Pharo Dark, etc.

### Fonts

- Settings → Appearance → Standard Fonts
- Choose size and font family

### Key Bindings

- Settings → Keymapping
- Customize shortcuts

### Startup Actions

Run code on startup:

```smalltalk
SessionManager default
    registerSystemClassNamed: #MyStartup
```

## Try This!

Explore Pharo:

1. **Install PharoLauncher**
   - Download from pharo.org
   - Create a new Pharo 11 image
   - Launch it

2. **Take the Tour**
   - Open Welcome window
   - Click "Interactive Tutorial"
   - Follow the guided tour

3. **Experiment with Tools**
   - Open Playground (`Ctrl+O W`)
   - Open Browser (`Ctrl+O B`)
   - Try Spotter (`Shift+Enter`)

4. **Build Something**
   - Create a class
   - Add methods
   - Write tests
   - Run them!

5. **Install a Package**
   ```smalltalk
   Metacello new
       baseline: 'PetitParser2';
       repository: 'github://kursjan/petitparser2';
       load
   ```

6. **Visualize Data**
   ```smalltalk
   Metacello new
       baseline: 'Roassal3';
       repository: 'github://ObjectProfile/Roassal3/src';
       load.

   | canvas |
   canvas := RSCanvas new.
   (1 to: 20) do: [ :i |
       canvas add: (RSBox new size: i * 3; yourself) ].
   RSFlowLayout on: canvas nodes.
   canvas open
   ```

7. **Read the Books**
   - Visit books.pharo.org
   - Read "Pharo by Example"

8. **Join the Community**
   - Join Discord: discord.gg/pharo
   - Ask questions
   - Share your projects!

## Pharo's Future

Pharo continues evolving:

### Recent Additions

- **Calypso** - New system browser
- **Commander** - Command pattern framework
- **NewTools** - Debugger and Inspector rewrite
- **Spec 2** - Modern UI framework
- **Microdown** - Documentation format

### Upcoming Features

- Better debugging tools
- Improved performance
- Enhanced Iceberg
- More packages in the catalog
- Better onboarding for newcomers

## The Pharo Philosophy

Pharo believes:

1. **Simplicity over complexity** - Small, clean, understandable
2. **Live programming** - Modify while running
3. **Everything is an object** - No exceptions
4. **Uniform syntax** - One way to do things
5. **Reflectivity** - Inspect and change everything
6. **Open source** - Community-driven development
7. **Education first** - Teaching is a priority
8. **Industrial strength** - Professional quality

## Looking Ahead

You now understand Pharo - the modern, actively developed Smalltalk! You know:
- What makes Pharo special
- Its philosophy and design principles
- Key features and tools
- The rich ecosystem
- Learning resources
- When to use Pharo

In Chapter 28, we'll explore **Squeak** - Pharo's parent, focused on multimedia and education. Squeak powers projects like Scratch and has unique capabilities for creating rich, interactive experiences.

Then Chapter 29 covers the **Glamorous Toolkit** - a moldable development environment that reimagines how we work with code and data.

Part VIII shows you the diversity and richness of the Smalltalk world!

---

**Key Takeaways:**
- **Pharo** is a modern, clean, actively developed Smalltalk
- Born in 2008 as a Squeak fork
- **Open source** (MIT license), cross-platform
- **Annual releases** with continuous improvements
- **Excellent tools**: Browser, Debugger, Inspector, Spotter, Iceberg
- **Rich ecosystem**: Seaside, Roassal, Voyage, and more
- **Great documentation**: Free books, MOOCs, active community
- **Industrial use**: Real companies, real products
- **Best for**: Modern development, web apps, data visualization, education
- **PharoLauncher** manages multiple images easily
- **Metacello** installs packages from GitHub
- Active, welcoming community on Discord and mailing lists
- Pharo pushes Smalltalk forward with modern features
- Clean design without legacy cruft

---

[Previous: Chapter 26 - Packages and Code Organization](chapter-26-packages.md) | [Next: Chapter 28 - Squeak - Multimedia and Education](chapter-28-squeak.md)
