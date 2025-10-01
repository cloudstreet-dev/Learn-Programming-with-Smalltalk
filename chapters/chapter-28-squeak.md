# Chapter 28: Squeak - Multimedia and Education

While Pharo focuses on clean, modern development, **Squeak** emphasizes multimedia, education, and creative expression. Squeak is where fun meets programming - a playground for building interactive, visual, animated experiences.

Squeak powers **Scratch** (used by millions of kids worldwide) and **eToys** (a multimedia authoring system). It's a Smalltalk that believes programming should be joyful, creative, and accessible to everyone.

## What is Squeak?

**Squeak** is a modern implementation of Smalltalk-80, created in 1996 by Alan Kay, Dan Ingalls, and others at Apple. Goals:

- **Multimedia-rich** - Graphics, sound, video, animation
- **Educational** - Teaching programming to everyone
- **Portable** - Runs everywhere, even on tiny devices
- **Self-sufficient** - Contains everything needed to modify itself
- **Open source** - MIT license, completely free

Squeak is Pharo's parent - Pharo forked from Squeak in 2008 to focus on modern development while Squeak continued emphasizing multimedia and education.

## Why Squeak?

### 1. Multimedia Power

Squeak excels at:
- **Graphics** - Drawing, animation, visual effects
- **Sound** - Music synthesis, audio processing
- **Video** - Playback and manipulation
- **3D** - Three-dimensional worlds
- **Animation** - Frame-by-frame or scripted

### 2. Educational Focus

Squeak powers educational tools:
- **Scratch** - Visual programming for kids (20+ million users)
- **eToys** - Multimedia authoring for children
- **DrGeo** - Interactive geometry
- **Snap!** - Browser-based visual programming

### 3. Morphic Graphics

**Morphic** is Squeak's direct-manipulation UI framework. Everything is a visual object you can grab, move, resize, and modify:

- Windows are morphs
- Buttons are morphs
- Text fields are morphs
- Your custom graphics are morphs

You build UIs by composing morphs!

### 4. Historical Significance

Squeak is the continuation of the original Smalltalk vision from Xerox PARC. It preserves the "Dynabook" dream - personal computing for creative expression and learning.

### 5. Portability

Squeak runs on:
- Windows, macOS, Linux
- Raspberry Pi
- Android (via SqueakJS)
- Browsers (via SqueakJS)
- Even old, low-powered machines

## Installing Squeak

### Desktop Installation

1. Visit **squeak.org**
2. Click **Download**
3. Choose your platform
4. Download the "All-in-One" package
5. Extract and run

You get:
- Squeak VM
- A fresh image
- Sources file
- Everything ready to go!

### SqueakJS (Browser)

Visit **squeak.js.org** and run Squeak **in your browser**! No installation needed.

## First Impressions

When Squeak launches, you see a colorful desktop with:
- Workspace windows
- World menu (click the desktop)
- Bright, playful aesthetics

Right-click (or click the desktop) to open the **World menu**.

### Opening Tools

World menu → Tools:
- **Workspace** - Like Playground
- **Browser** - System Browser
- **Transcript** - Output window
- **File List** - File browser

### Your First Graphics

Open a Workspace and evaluate:

```smalltalk
| morph |
morph := EllipseMorph new.
morph color: Color red.
morph openInWorld
```

A red circle appears! Click and drag it around. Right-click for a menu with options.

## Morphic - Direct Manipulation Graphics

Everything in Squeak's UI is a **Morph** - a graphical object.

### Creating Morphs

```smalltalk
"A rectangle:"
(RectangleMorph new
    color: Color blue;
    extent: 100@50;
    position: 200@200) openInWorld
```

```smalltalk
"A circle:"
(CircleMorph new
    color: Color yellow;
    extent: 80@80) openInWorld
```

```smalltalk
"An image:"
(ImageMorph new
    image: (Form extent: 50@50 depth: 32);
    color: Color green) openInWorld
```

### Manipulating Morphs

Once a morph is in the world:
- **Click and drag** - Move it
- **Drag corners** - Resize it
- **Right-click** - Open halo (control handles)
- **Halo buttons**:
  - Blue - Pick up
  - Yellow - Resize
  - Pink - Rotate
  - Red - Delete
  - Green - Duplicate

Try it! The UI is tactile and playful.

### Animated Morphs

Make morphs animate:

```smalltalk
| morph |
morph := EllipseMorph new color: Color red; openInWorld.

[ 100 timesRepeat: [
    morph position: morph position + (2@1).
    (Delay forMilliseconds: 50) wait ] ] fork
```

The circle moves across the screen!

## Building Interactive Graphics

### A Bouncing Ball

```smalltalk
| ball dx dy |
ball := EllipseMorph new
    color: Color blue;
    extent: 30@30;
    position: 100@100;
    openInWorld.

dx := 3.
dy := 2.

[ true ] whileTrue: [
    | newPos |
    newPos := ball position + (dx@dy).

    "Bounce off edges"
    (newPos x < 0 or: [ newPos x > 600 ]) ifTrue: [ dx := dx negated ].
    (newPos y < 0 or: [ newPos y > 400 ]) ifTrue: [ dy := dy negated ].

    ball position: ball position + (dx@dy).
    (Delay forMilliseconds: 20) wait ]
```

A ball bounces around the screen!

### Interactive Morph

Create a morph that responds to clicks:

```smalltalk
Morph subclass: #ClickableCircle
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MySqueakApp'
```

```smalltalk
initialize
    super initialize.
    self color: Color green.
    self extent: 50@50

mouseUp: anEvent
    self color: Color random.
    ^ true
```

```smalltalk
ClickableCircle new openInWorld
```

Click the circle - it changes color!

## Sound and Music

Squeak includes powerful sound synthesis:

### Playing Notes

```smalltalk
"Play a middle C:"
(SampledSound new
    pitch: 261.63;
    duration: 1.0;
    play)
```

### Simple Melody

```smalltalk
| notes |
notes := #(60 62 64 65 67 69 71 72).  "C major scale (MIDI notes)"
notes do: [ :note |
    (FMSound new
        setPitch: note;
        duration: 0.3;
        play).
    (Delay forMilliseconds: 300) wait ]
```

Hear the scale!

### Sound Synthesis

```smalltalk
| sound |
sound := FMSound new.
sound
    setPitch: 440;  "A440"
    duration: 2.0;
    modulation: 5.0;
    play
```

FM synthesis with Squeak!

## eToys - Visual Programming

**eToys** is a multimedia authoring environment built in Squeak.

### Launching eToys

In Squeak:
1. World menu → **eToys**
2. Or load eToys image (from squeak.org)

eToys provides:
- Visual scripting (tile-based programming)
- Easy animation
- Interactive simulations
- Great for kids and non-programmers

### eToys Example

In eToys:
1. Click "Supplies" flap
2. Drag out a "Star"
3. Click the star's menu → "View Script"
4. Add tiles: "Forward by 5", "Turn by 5"
5. Set to run "ticking" (every frame)
6. Watch the star spiral!

eToys makes programming visual and immediate.

## Squeak vs Pharo

| Feature | Squeak | Pharo |
|---------|--------|-------|
| Focus | Multimedia, education | Modern development |
| Graphics | Morphic (rich, playful) | Morphic (streamlined) |
| UI | Colorful, tactile | Clean, professional |
| Sound | Excellent synthesis | Basic |
| 3D | Built-in (Croquet) | Limited |
| eToys | Included | Not included |
| Tools | Classic, stable | Modern, evolving |
| Git Integration | Add-on (Monticello) | Built-in (Iceberg) |
| Community | Education-focused | Development-focused |
| Use Cases | Teaching, multimedia | Web, business apps |

**Choose Squeak if:**
- You want multimedia/graphics capabilities
- You're teaching kids or beginners
- You love eToys and visual programming
- You need sound/music synthesis
- You want the "classic" Smalltalk experience

**Choose Pharo if:**
- You're building web applications
- You want modern dev tools (Iceberg, etc.)
- You prefer clean, minimal aesthetics
- You're doing professional software development

Both are excellent! Many developers use both for different purposes.

## Educational Uses

Squeak is used in education worldwide:

### Scratch

**Scratch** (scratch.mit.edu) is built on Squeak. It teaches programming to millions of children using:
- Visual blocks (drag and drop)
- Sprites and costumes
- Sounds and music
- Interactive stories and games

Scratch's backend is Squeak!

### OLPC (One Laptop Per Child)

Squeak powered educational software on OLPC laptops distributed to children in developing countries.

### Universities

Many universities use Squeak to teach:
- Object-oriented programming
- Software engineering
- Human-computer interaction
- Multimedia programming

## 3D Graphics

Squeak includes 3D capabilities via **Croquet**:

```smalltalk
| world |
world := Alice2World new.
world addMorph: (Sphere new radius: 1.0; color: Color red).
world open
```

Build 3D scenes and interactive worlds!

## Creating Games

Squeak is great for game development:

### Simple Game Example

```smalltalk
Morph subclass: #PaddleGame
    instanceVariableNames: 'ball paddle'
    classVariableNames: ''
    package: 'MyGame'
```

```smalltalk
initialize
    super initialize.
    self extent: 400@300.
    self color: Color black.

    ball := EllipseMorph new color: Color white; extent: 10@10.
    paddle := RectangleMorph new color: Color green; extent: 60@10.

    self addMorph: ball.
    self addMorph: paddle.

    self startGame

startGame
    [ true ] whileTrue: [
        self updateBall.
        (Delay forMilliseconds: 20) wait ]
```

Build Pong, platformers, puzzle games!

## Books and Resources

### Books

- **Squeak by Example** - Free online book
- **Powerful Ideas in the Classroom** - Using Squeak in education

Available at squeak.org

### Community

- **Mailing list** - squeak-dev
- **Swiki** - wiki.squeak.org
- **Discord** - Squeak/Pharo shared server
- **IRC** - #squeak on Libera.Chat

### Examples

Squeak ships with many example projects:
- World menu → **Open** → **Sample Projects**
- Explore games, simulations, demos

## Squeak Projects

Cool projects built with Squeak:

### Educational

- **Scratch** - Visual programming for kids
- **eToys** - Multimedia authoring
- **Snap!** - Browser-based Scratch
- **Dr. Geo** - Interactive geometry

### Creative

- **BPM** - Music composition
- **Kedama** - Particle simulations
- **Nebraska** - Collaborative environments

### Research

- **Croquet** - 3D collaborative spaces
- **OpenCobalt** - Virtual world platform

## Extending Squeak

### Installing Packages

Squeak uses **Metacello** (like Pharo):

```smalltalk
Installer squeaksource
    project: 'MyPackage';
    install: 'MyPackage'
```

Or use **SqueakMap** (World menu → Tools → SqueakMap Package Loader).

### External Plugins

Squeak supports external plugins for:
- Additional sound codecs
- Video playback
- Hardware interfacing
- More!

## Try This!

Explore Squeak:

1. **Install Squeak**
   - Download from squeak.org
   - Launch the All-in-One

2. **Create Morphs**
   ```smalltalk
   (EllipseMorph new color: Color random; extent: 50@50) openInWorld.
   (RectangleMorph new color: Color random; extent: 100@30) openInWorld
   ```

   Drag them around, resize them, play!

3. **Animate Something**
   ```smalltalk
   | morph |
   morph := StarMorph new openInWorld.
   [ 360 timesRepeat: [
       morph rotationDegrees: morph rotationDegrees + 1.
       (Delay forMilliseconds: 10) wait ] ] fork
   ```

4. **Make Music**
   ```smalltalk
   #(60 64 67 72) do: [ :note |
       (PluckedSound new setPitch: note; duration: 0.5; play).
       (Delay forMilliseconds: 500) wait ]
   ```

5. **Explore eToys**
   - World menu → eToys
   - Drag out objects
   - Script them with tiles
   - Build something fun!

6. **Browse Examples**
   - World menu → Open → Sample Projects
   - Explore the pre-built demos

7. **Build a Simple Game**
   - Create a paddle and ball
   - Make the ball bounce
   - Detect collisions
   - Add scoring!

8. **Try SqueakJS**
   - Visit squeak.js.org
   - Run Squeak in your browser
   - No installation needed!

## Squeak's Philosophy

Squeak embodies:

1. **Computing for Everyone** - Not just professionals, but kids, artists, teachers
2. **Direct Manipulation** - See it, touch it, change it
3. **Multimedia-Rich** - Not just text, but images, sound, animation
4. **Self-Sufficient** - The system can evolve itself
5. **Playful** - Programming should be fun!
6. **Accessible** - Runs on anything, costs nothing
7. **Educational** - Learning is the primary goal

## The Legacy

Squeak continues Smalltalk's original vision:

- **Personal computing** - Computers for creative expression
- **Dynabook** - Alan Kay's vision of a computer for children
- **Literacy** - Programming as a basic skill for everyone
- **Authorship** - Users as creators, not just consumers

## Looking Ahead

You now understand Squeak - the multimedia-rich, education-focused Smalltalk! You know:
- What makes Squeak special
- Morphic direct-manipulation graphics
- Sound synthesis and music
- eToys visual programming
- Educational applications
- When to choose Squeak vs Pharo

In Chapter 29, we'll explore the **Glamorous Toolkit** - a radical reimagining of development environments. GT is moldable, extensible, and designed for understanding complex systems.

Then Chapter 30 covers other Smalltalks: VisualWorks, Smalltalk/X, Cuis, and more!

Part VIII is showing you the diversity and richness of the Smalltalk ecosystem!

---

**Key Takeaways:**
- **Squeak** emphasizes multimedia, graphics, sound, and education
- Created in 1996 by Alan Kay, Dan Ingalls, and others
- **Pharo's parent** - Pharo forked from Squeak in 2008
- **Morphic** - Direct-manipulation graphics framework
- Every UI element is a morph you can grab and manipulate
- **Excellent sound synthesis** - Music and audio processing
- **eToys** - Visual programming for kids and beginners
- Powers **Scratch** - used by 20+ million kids worldwide
- **Educational focus** - Teaching programming to everyone
- **SqueakJS** - Runs in browsers, no installation
- Great for games, simulations, interactive art
- Playful, colorful, tactile interface
- Open source (MIT), runs everywhere
- Choose Squeak for multimedia, education, creative projects
- Choose Pharo for modern web/business development

---

[Previous: Chapter 27 - Pharo](chapter-27-pharo.md) | [Next: Chapter 29 - Glamorous Toolkit](chapter-29-glamorous-toolkit.md)
