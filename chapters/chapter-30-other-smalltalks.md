# Chapter 30: Other Smalltalks Worth Knowing

We've explored Pharo, Squeak, and Glamorous Toolkit in depth. But the Smalltalk family is larger and more diverse! From commercial powerhouses to minimalist experiments, each Smalltalk has unique strengths.

In this chapter, we'll survey other notable Smalltalks: VisualWorks, VA Smalltalk, Smalltalk/X, Cuis, Dolphin, GNU Smalltalk, and more. You'll learn what makes each special and which to choose for different purposes.

## The Smalltalk Family Tree

```
Smalltalk-80 (1980)
├─ VisualWorks (1990s)
├─ VisualAge (IBM, 1990s)
├─ Smalltalk/X (1990s)
├─ Squeak (1996)
│  ├─ Pharo (2008)
│  ├─ Cuis (2010s)
│  └─ Glamorous Toolkit (2017)
├─ Dolphin Smalltalk (1997)
└─ GNU Smalltalk (1980s/90s)
```

All descend from Smalltalk-80, but have evolved differently!

## VisualWorks

**VisualWorks** is the commercial flagship of Smalltalk, developed by Cincom.

### Overview:

- **Vendor**: Cincom Systems
- **License**: Commercial (free for non-commercial use)
- **Platforms**: Windows, macOS, Linux, Unix
- **Since**: 1990s (originally from ParcPlace)

### Strengths:

1. **Industrial-grade** - Battle-tested in enterprise
2. **Cross-platform** - True "write once, run anywhere"
3. **Performance** - Highly optimized VM
4. **Commercial support** - Professional support contracts
5. **Mature ecosystem** - Decades of packages and frameworks
6. **Database integration** - Excellent RDBMS support

### Features:

- **Store** - Version control system for Smalltalk
- **Refactoring Browser** - Originated here!
- **VisualWave** - Web application framework
- **CORBA support** - Distributed objects
- **Hot deployment** - Update running systems

### When to Use:

- **Enterprise applications** - Banks, insurance, telecom
- **Mission-critical systems** - Where stability matters
- **Need commercial support** - Professional backing
- **Cross-platform deployment** - True portability

### Getting Started:

1. Visit **cincomsmalltalk.com**
2. Download VisualWorks (free non-commercial license)
3. Install and launch
4. Sophisticated, professional environment

### Example:

```smalltalk
"VisualWorks syntax is standard Smalltalk"
| window |
window := ApplicationWindow new.
window label: 'Hello VisualWorks'.
window open
```

## VA Smalltalk (VisualAge)

**VA Smalltalk** is IBM's enterprise Smalltalk, now maintained by Instantiations.

### Overview:

- **Vendor**: Instantiations (originally IBM)
- **License**: Commercial
- **Platforms**: Windows, Linux, Unix
- **Since**: 1990s

### Strengths:

1. **Enterprise integration** - CICS, MQSeries, DB2
2. **Team development** - ENVY versioning system
3. **Scalability** - Large-scale applications
4. **IBM ecosystem** - Integration with IBM tools
5. **Proven track record** - Used in major corporations

### Features:

- **ENVY/Developer** - Configuration management
- **Web Enablement** - Seaside integration
- **Database frameworks** - Multiple DB backends
- **CORBA & COM** - Distributed computing
- **Packaging** - Shrink-wrapped applications

### When to Use:

- **IBM shops** - Already using IBM tools
- **Team development** - ENVY is powerful
- **Legacy systems** - Maintaining IBM code
- **Enterprise integration** - Need IBM connectivity

### Getting Started:

Visit **instantiations.com/va-smalltalk**

## Smalltalk/X

**Smalltalk/X** is a Smalltalk optimized for Unix/Linux environments and embedded systems.

### Overview:

- **Vendor**: eXept Software AG
- **License**: Free for non-commercial, commercial licenses available
- **Platforms**: Linux, Unix, Windows, embedded
- **Since**: 1990s

### Strengths:

1. **C integration** - Call C libraries directly
2. **Embedded systems** - IoT, industrial control
3. **Unix integration** - Native Unix/Linux feel
4. **Performance** - JIT compiler
5. **Small footprint** - Can run on constrained systems

### Features:

- **Class libraries** - Extensive libraries
- **GUI toolkits** - Multiple options
- **Foreign function interface** - Easy C integration
- **Web frameworks** - Including Seaside
- **Compiler technology** - Advanced optimizations

### When to Use:

- **Embedded systems** - IoT devices, industrial control
- **Unix/Linux development** - Native integration
- **Need C integration** - Calling C libraries
- **Small footprint** - Resource-constrained environments

### Getting Started:

Visit **exept.de**

### Example:

```smalltalk
"Calling C library:"
LibC system: 'ls -la'
```

## Cuis Smalltalk

**Cuis** is a minimalist, pure Smalltalk focused on simplicity and elegance.

### Overview:

- **Author**: Juan Vuletich
- **License**: MIT (open source)
- **Platforms**: Cross-platform
- **Since**: 2010s (forked from Squeak)

### Philosophy:

- **Simplicity** - Minimal, clean, small
- **Elegance** - Beautiful code
- **Purity** - True to Smalltalk ideals
- **Understandability** - Easy to comprehend

### Strengths:

1. **Small image** - ~10 MB (vs 50+ MB for Pharo)
2. **Clean code** - No cruft
3. **Fast** - Optimized for performance
4. **Educational** - Learn Smalltalk fundamentals
5. **Active development** - Constantly refined

### Features:

- **Morphic 3** - Clean graphics framework
- **Unicode support** - International text
- **Vector graphics** - Scalable UIs
- **Git integration** - File-based packages
- **Modular** - Load only what you need

### When to Use:

- **Learning** - Understand pure Smalltalk
- **Minimalism** - Prefer simplicity
- **Resource constraints** - Small images
- **Research** - Clean slate for experiments

### Getting Started:

1. Visit **cuis.st**
2. Download Cuis
3. Extract and run
4. Minimal, elegant environment

### Example:

```smalltalk
"Cuis uses standard Smalltalk syntax"
(1 to: 10) collect: [ :n | n squared ]
```

## Dolphin Smalltalk

**Dolphin** is a Windows-native Smalltalk with excellent Windows integration.

### Overview:

- **Platforms**: Windows only
- **License**: MIT (open source as of 2022)
- **Since**: 1997

### Strengths:

1. **Native Windows** - Feels like Windows
2. **ActiveX support** - COM integration
3. **Windows UI** - Native controls
4. **Performance** - Fast on Windows
5. **Professional IDE** - Excellent tools

### Features:

- **MVP framework** - Model-View-Presenter
- **Native controls** - Windows widgets
- **COM/ActiveX** - Windows automation
- **Database integration** - ADO support
- **Packaging** - Single .exe deployment

### When to Use:

- **Windows-only apps** - Native Windows development
- **COM integration** - ActiveX controls
- **Desktop applications** - Traditional Windows apps
- **Learning** - Clean, modern Smalltalk

### Getting Started:

Visit **github.com/dolphinsmalltalk**

## GNU Smalltalk

**GNU Smalltalk** is a command-line Smalltalk, part of the GNU project.

### Overview:

- **Vendor**: GNU Project
- **License**: GPL (open source)
- **Platforms**: Unix, Linux, macOS
- **Since**: 1980s/90s

### Strengths:

1. **Command-line** - Scripting-friendly
2. **Unix integration** - Pipes, stdin/stdout
3. **C integration** - FFI to C libraries
4. **Free as in freedom** - GPL licensed
5. **Package manager** - Install libraries easily

### Features:

- **File-based** - No image (uses source files)
- **Scripting** - Use in shell scripts
- **Libraries** - Many extension packages
- **Portable** - Runs on many systems

### When to Use:

- **Scripting** - System administration
- **Unix integration** - Command-line tools
- **No GUI needed** - Server-side code
- **Learning** - Simple, no image complexity

### Getting Started:

Install via package manager:

```bash
# Linux
sudo apt-get install gnu-smalltalk

# macOS
brew install gnu-smalltalk
```

### Example:

```bash
# Run Smalltalk script
gst script.st

# Interactive REPL
gst
```

```smalltalk
"In GNU Smalltalk:"
Transcript show: 'Hello from GNU Smalltalk'; nl
```

## Other Notable Smalltalks

### Amber Smalltalk

- **Browser-based** - Compiles to JavaScript
- **Web development** - Frontend applications
- **Open source**
- Visit: **amber-lang.net**

### Redline Smalltalk

- **JVM-based** - Runs on Java Virtual Machine
- **Java integration** - Use Java libraries
- **Experimental**

### Smalltalk MT

- **Multi-threaded** - True parallelism
- **Modern features** - Continuations, etc.
- **Less active**

### Strongtalk

- **Typed Smalltalk** - Optional type annotations
- **Research project** - From Sun Microsystems
- **Historical interest**

## Comparison Table

| Smalltalk | License | Platform | Best For | Active |
|-----------|---------|----------|----------|---------|
| **Pharo** | MIT | Cross | Modern dev | ✅ Very |
| **Squeak** | MIT | Cross | Multimedia, edu | ✅ Yes |
| **GT** | MIT | Cross | Complex systems | ✅ Yes |
| **VisualWorks** | Commercial | Cross | Enterprise | ✅ Yes |
| **VA Smalltalk** | Commercial | Cross | IBM integration | ✅ Yes |
| **Smalltalk/X** | Mixed | Cross | Embedded, C | ✅ Yes |
| **Cuis** | MIT | Cross | Minimalism | ✅ Yes |
| **Dolphin** | MIT | Windows | Windows apps | ⚠️ Limited |
| **GNU** | GPL | Unix | Scripting | ⚠️ Limited |
| **Amber** | MIT | Browser | Web frontend | ⚠️ Limited |

## Choosing a Smalltalk

### Decision Tree:

**1. What's your goal?**

- **Learning Smalltalk** → Pharo or Cuis
- **Building web apps** → Pharo
- **Teaching kids** → Squeak (or Scratch)
- **Understanding systems** → Glamorous Toolkit
- **Enterprise apps** → VisualWorks or VA Smalltalk
- **Embedded systems** → Smalltalk/X
- **Windows apps** → Dolphin
- **Unix scripting** → GNU Smalltalk
- **JavaScript apps** → Amber

**2. Budget?**

- **Free** → Pharo, Squeak, Cuis, GT, Dolphin, GNU
- **Commercial support needed** → VisualWorks, VA Smalltalk

**3. Platform?**

- **Windows only** → Dolphin excels
- **Cross-platform** → Pharo, Squeak, VW, Cuis
- **Linux/embedded** → Smalltalk/X
- **Browser** → Amber

**4. Community?**

- **Large, active** → Pharo, Squeak
- **Smaller, focused** → Cuis, GT
- **Professional** → VisualWorks, VA Smalltalk

## Migration Between Smalltalks

Code is largely portable, but differences exist:

### Common Across All:

```smalltalk
"Core Smalltalk:"
(1 to: 10) collect: [ :n | n * 2 ].
OrderedCollection new add: 'item'; yourself.
String new, 'text'
```

### Platform-Specific:

```smalltalk
"UI varies:"
Pharo: SpButton new label: 'Click'.
Squeak: SimpleButtonMorph new label: 'Click'.
VisualWorks: Button new label: 'Click'.
```

### Porting Strategy:

1. **Core logic** - Usually portable
2. **UI** - Rewrite for target platform
3. **Libraries** - Find equivalents
4. **File out code** - Use Tonel or Monticello
5. **File in** - Load into target Smalltalk
6. **Fix differences** - Update platform-specific code

## The Future of Smalltalk

Smalltalk remains relevant:

### Current Trends:

- **Pharo** - Modernization continues
- **GT** - Moldable development innovation
- **Squeak** - Educational impact (Scratch)
- **Commercial** - Stable, maintained enterprises

### Emerging Areas:

- **IoT** - Smalltalk/X for embedded
- **Data science** - PolyMath, Roassal
- **Web** - Seaside, Teapot
- **Knowledge work** - GT's Lepiter

Smalltalk isn't mainstream, but it's **healthy and evolving**.

## Try This!

Explore different Smalltalks:

1. **Download Multiple Smalltalks**
   - Install Pharo, Squeak, and Cuis
   - Compare the experiences
   - Notice similarities and differences

2. **Write Portable Code**
   ```smalltalk
   "This works everywhere:"
   Object subclass: #Calculator
       instanceVariableNames: 'result'
       ...

   add: number
       result := result + number.
       ^ result
   ```

   Load into multiple Smalltalks!

3. **Try GNU Smalltalk**
   ```bash
   echo "Transcript show: 'Hello'; nl" | gst
   ```

   Scripting with Smalltalk!

4. **Explore VisualWorks**
   - Download free non-commercial version
   - See commercial-grade tooling
   - Notice performance

5. **Read Comparison Articles**
   - Search "Smalltalk comparison"
   - Understand trade-offs
   - Make informed choices

## Community Resources

### Cross-Smalltalk:

- **comp.lang.smalltalk** - Usenet group (historical)
- **Reddit** - r/smalltalk
- **Stack Overflow** - Tag: smalltalk
- **Discord** - Pharo/Squeak server welcomes all

### Implementation-Specific:

- **Pharo**: pharo.org, discord
- **Squeak**: squeak.org, mailing list
- **GT**: gtoolkit.com, discord
- **VW**: cincomsmalltalk.com/community
- **Cuis**: cuis.st, mailing list

## The Smalltalk Spirit

Despite differences, all Smalltalks share:

1. **Everything is an object**
2. **Message passing** - Uniform communication
3. **Image-based** (mostly) - Live programming
4. **Reflective** - Inspect and modify everything
5. **Interactive** - Immediate feedback
6. **Simple syntax** - Easy to learn
7. **Powerful tools** - Excellent IDEs

Choose the implementation that fits your needs, but they're all **Smalltalk** at heart!

## Looking Ahead

You now understand the diverse Smalltalk ecosystem! You know:
- Major Smalltalk implementations
- Strengths and trade-offs of each
- When to use which Smalltalk
- How to choose for your needs

This completes Part VIII (Exploring the Smalltalk Variations)!

In Part IX (Building Real Things), we'll build complete projects:
- Chapter 31: Todo List Manager
- Chapter 32: Text Adventure Game
- Chapter 33: Simple Web Server
- Chapter 34: Working with Files and Streams
- Chapter 35: Graphics and UI Basics

You'll apply everything you've learned to create real, working applications!

---

**Key Takeaways:**
- Many Smalltalk implementations exist, each with unique strengths
- **VisualWorks** - Commercial, enterprise-grade, cross-platform
- **VA Smalltalk** - IBM integration, enterprise, ENVY versioning
- **Smalltalk/X** - Unix/Linux, embedded systems, C integration
- **Cuis** - Minimalist, pure, elegant, small footprint
- **Dolphin** - Windows-native, COM/ActiveX, open source
- **GNU Smalltalk** - Command-line, Unix scripting, GPL
- **Amber** - Browser-based, compiles to JavaScript
- Core Smalltalk code is largely portable across implementations
- Choose based on: goal, platform, budget, community
- All share the Smalltalk philosophy: objects, messages, simplicity
- The Smalltalk ecosystem is healthy and evolving
- Commercial options exist for enterprise needs
- Open source options for learning and development

---

[Previous: Chapter 29 - Glamorous Toolkit](chapter-29-glamorous-toolkit.md) | [Next: Chapter 31 - Project 1: A Todo List Manager](chapter-31-project-todo-list.md)
