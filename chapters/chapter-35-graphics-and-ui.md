# Chapter 35: Graphics and UI Basics

Welcome to the final chapter of Part IX! You've built todo lists, games, web servers, and mastered file I/O. Now let's explore **graphics and user interfaces** - the visual side of Smalltalk programming.

Smalltalk pioneered graphical user interfaces at Xerox PARC. While modern Smalltalks have evolved, they retain powerful graphics capabilities. This chapter covers **Morphic** (Smalltalk's graphics framework) and shows you how to create interactive visual applications!

## Graphics in Smalltalk

Smalltalk has a rich graphics heritage:

- **1970s**: First bitmap graphics and windows (Xerox Alto)
- **1980s**: MVC (Model-View-Controller) pattern invented
- **1990s**: Morphic - direct manipulation graphics
- **2000s+**: Modern UI frameworks (Spec 2, Bloc)

Today's Smalltalk offers multiple approaches:
- **Morphic** - Classic direct-manipulation graphics (Squeak/Pharo)
- **Spec 2** - Modern widget framework (Pharo)
- **Bloc** - New vector graphics engine (Pharo/GT)
- **Native widgets** - Platform-specific UIs (Dolphin)

This chapter focuses on **Morphic basics** and **Spec 2** (covered in Chapter 31).

## Understanding Morphic

**Morphic** is Smalltalk's graphics framework where everything visible is a **Morph** - a graphical object you can see, move, and interact with.

### Morphic Philosophy

1. **Direct manipulation** - Click, drag, resize anything
2. **Everything is an object** - Windows, buttons, shapes are all morphs
3. **Live development** - Modify the UI while it runs
4. **Composable** - Build complex UIs from simple morphs
5. **Self-describing** - Right-click any morph for a menu

## Your First Morph

Let's create something visible!

### Simple Rectangle

```smalltalk
| morph |
morph := Morph new.
morph color: Color red.
morph extent: 100@100.
morph position: 200@200.
morph openInWorld
```

A red square appears! Try this:
- **Click and drag** it around
- **Right-click** for a menu
- **Shift+click** to get handles
- **Click the X** to close it

### Understanding Points

Points represent 2D coordinates:

```smalltalk
100@200         "x=100, y=200"

| point |
point := 50@75.
point x.        "-> 50"
point y.        "-> 75"

"Point arithmetic"
(100@100) + (50@25).    "-> 150@125"
(100@100) * 2.          "-> 200@200"
(100@100) / 2.          "-> 50@50"
```

### Understanding Rectangles

Rectangles define areas:

```smalltalk
| rect |
rect := 10@10 corner: 110@60.  "Origin and corner"

rect origin.        "-> 10@10"
rect corner.        "-> 110@60"
rect extent.        "-> 100@50 (width@height)"
rect center.        "-> 60@35"
rect area.          "-> 5000"

"Contains point?"
rect containsPoint: 50@30.  "-> true"
```

## Colors

Rich color support:

```smalltalk
"Named colors"
Color red.
Color blue.
Color green.
Color yellow.
Color white.
Color black.
Color gray.
Color transparent.

"RGB colors (0-1 range)"
Color r: 1 g: 0 b: 0.        "Red"
Color r: 0.5 g: 0.5 b: 1.    "Light blue"

"With alpha (transparency)"
Color r: 1 g: 0 b: 0 alpha: 0.5.  "Semi-transparent red"

"From hex"
Color fromHexString: '#FF5733'.

"Random color"
Color random.

"Color mixing"
Color red mixed: 0.5 with: Color blue.  "Purple"

"Lighter/darker"
Color blue lighter.
Color blue darker.
```

## Basic Morphs

Smalltalk includes many built-in morphs:

### RectangleMorph

```smalltalk
| rect |
rect := RectangleMorph new.
rect color: Color blue.
rect borderColor: Color black.
rect borderWidth: 3.
rect extent: 120@80.
rect openInWorld
```

### EllipseMorph

```smalltalk
| circle |
circle := EllipseMorph new.
circle color: Color yellow.
circle extent: 80@80.
circle openInWorld
```

### TextMorph

```smalltalk
| text |
text := TextMorph new.
text contents: 'Hello Smalltalk!'.
text fontName: 'Arial' size: 24.
text color: Color blue.
text openInWorld
```

### ImageMorph

```smalltalk
| image form |

"Create a simple form (bitmap)"
form := Form extent: 50@50 depth: 32.
form fillColor: Color red.

image := ImageMorph new.
image image: form.
image openInWorld
```

### StringMorph

```smalltalk
StringMorph new
    contents: 'Click me!';
    color: Color red;
    openInWorld
```

## Creating Custom Morphs

Let's build our own morphs!

### Simple Custom Morph

```smalltalk
Morph subclass: #ColorBox
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyGraphics'
```

```smalltalk
initialize
    super initialize.
    self color: Color random.
    self extent: 100@100

drawOn: aCanvas
    "Draw the morph"
    super drawOn: aCanvas.

    "Draw a border"
    aCanvas
        frameRectangle: self bounds
        width: 2
        color: Color black
```

Usage:

```smalltalk
ColorBox new openInWorld
```

Each ColorBox appears with a random color!

### Interactive Morph

Add mouse interaction:

```smalltalk
Morph subclass: #ClickableBox
    instanceVariableNames: 'clickCount'
    classVariableNames: ''
    package: 'MyGraphics'
```

```smalltalk
initialize
    super initialize.
    clickCount := 0.
    self color: Color lightBlue.
    self extent: 120@80

handlesMouseDown: event
    "Answer whether we want mouse down events"
    ^ true

mouseDown: event
    "Handle mouse click"
    clickCount := clickCount + 1.
    self updateColor.
    self changed  "Trigger redraw"

updateColor
    | colors |
    colors := { Color red. Color green. Color blue. Color yellow. Color orange }.
    self color: (colors at: (clickCount \\ colors size) + 1)

drawOn: aCanvas
    super drawOn: aCanvas.

    "Draw click count"
    aCanvas
        drawString: 'Clicks: ', clickCount asString
        at: self position + (10@10)
        font: nil
        color: Color black
```

Try it:

```smalltalk
ClickableBox new openInWorld
```

Click the box repeatedly - it changes color and counts clicks!

### Animated Morph

```smalltalk
Morph subclass: #BouncingBall
    instanceVariableNames: 'velocity'
    classVariableNames: ''
    package: 'MyGraphics'
```

```smalltalk
initialize
    super initialize.
    self color: Color red.
    self extent: 30@30.
    velocity := 5@3.  "Velocity in x and y"
    self startStepping

step
    "Called repeatedly for animation"
    | newPosition bounds |

    "Calculate new position"
    newPosition := self position + velocity.
    bounds := self world bounds.

    "Bounce off edges"
    (newPosition x < bounds left or: [ newPosition x > bounds right - self width ]) ifTrue: [
        velocity := velocity x negated @ velocity y ].

    (newPosition y < bounds top or: [ newPosition y > bounds bottom - self height ]) ifTrue: [
        velocity := velocity x @ velocity y negated ].

    "Update position"
    self position: self position + velocity

stepTime
    "Milliseconds between steps"
    ^ 30  "~33 FPS"
```

Launch it:

```smalltalk
BouncingBall new openInWorld
```

The ball bounces around the screen!

## Drawing with Canvas

The **Canvas** provides drawing primitives:

### Custom Drawing

```smalltalk
Morph subclass: #CustomDrawing
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyGraphics'
```

```smalltalk
initialize
    super initialize.
    self color: Color white.
    self extent: 300@300

drawOn: aCanvas
    | center radius |
    super drawOn: aCanvas.

    center := self bounds center.
    radius := 50.

    "Draw circles"
    1 to: 5 do: [ :i |
        aCanvas
            frameOval: (Rectangle center: center extent: (radius * i)@(radius * i))
            color: (Color r: i/5 g: 0 b: 1 - (i/5))
            borderWidth: 2 ].

    "Draw lines"
    0 to: 360 by: 30 do: [ :angle |
        | endPoint |
        endPoint := center + (Point r: 100 theta: angle degreesToRadians).
        aCanvas
            line: center
            to: endPoint
            width: 1
            color: Color black ].

    "Draw text"
    aCanvas
        drawString: 'Custom Drawing'
        at: self position + (10@10)
        font: (LogicalFont familyName: 'Arial' pointSize: 16)
        color: Color black
```

```smalltalk
CustomDrawing new openInWorld
```

Beautiful concentric circles with radiating lines!

### Canvas Drawing Methods

```smalltalk
"Lines"
aCanvas line: startPoint to: endPoint width: 2 color: Color red.

"Rectangles"
aCanvas fillRectangle: aRectangle color: Color blue.
aCanvas frameRectangle: aRectangle width: 2 color: Color black.

"Circles/Ovals"
aCanvas fillOval: aRectangle color: Color green.
aCanvas frameOval: aRectangle color: Color red borderWidth: 2.

"Polygons"
aCanvas drawPolygon: { 10@10. 50@10. 30@50 } color: Color yellow borderWidth: 1 borderColor: Color black.

"Text"
aCanvas drawString: 'Hello' at: 10@10 font: nil color: Color black.

"Images"
aCanvas drawImage: aForm at: 10@10.
```

## Composite Morphs

Build complex UIs by combining morphs:

```smalltalk
Morph subclass: #ColorPalette
    instanceVariableNames: 'colors'
    classVariableNames: ''
    package: 'MyGraphics'
```

```smalltalk
initialize
    super initialize.
    self color: Color white.
    self extent: 220@120.
    colors := { Color red. Color green. Color blue. Color yellow.
                Color orange. Color purple. Color brown. Color pink }.
    self buildUI

buildUI
    | x y |
    x := 10.
    y := 10.

    colors do: [ :color |
        | box |
        box := Morph new
            color: color;
            extent: 40@40;
            position: self position + (x@y);
            yourself.

        box on: #mouseDown send: #colorClicked: to: self with: color.

        self addMorph: box.

        x := x + 50.
        x > 200 ifTrue: [
            x := 10.
            y := y + 50 ] ]

colorClicked: aColor
    "User clicked a color"
    Transcript show: 'Selected: ', aColor asString; cr.
    self color: aColor
```

```smalltalk
ColorPalette new openInWorld
```

Click the color squares to change the palette's background!

## Form and BitBlt

**Forms** are bitmaps - pixel grids you can draw on:

```smalltalk
| form |

"Create a 100x100 form"
form := Form extent: 100@100 depth: 32.

"Fill with color"
form fillColor: Color lightGray.

"Draw a rectangle"
form fillRectangle: (20@20 corner: 80@80) color: Color blue.

"Display it"
form asMorph openInWorld
```

### Drawing on Forms

```smalltalk
| form canvas |

form := Form extent: 200@200 depth: 32.
canvas := FormCanvas on: form.

"Draw shapes"
canvas fillRectangle: (10@10 corner: 190@190) color: Color white.
canvas fillOval: (50@50 corner: 150@150) color: Color red.
canvas fillOval: (70@70 corner: 90@90) color: Color white.  "Eye"
canvas fillOval: (110@110 corner: 130@130) color: Color white.  "Eye"
canvas line: 80@140 to: 120@140 width: 3 color: Color white.  "Smile"

form asMorph openInWorld
```

### Saving Forms

```smalltalk
| form |
form := Form extent: 100@100 depth: 32.
form fillColor: Color red.

"Save as PNG"
PNGReadWriter putForm: form onFileNamed: 'my-image.png'
```

## Building a Simple Drawing App

Let's create a mini paint program:

```smalltalk
Morph subclass: #SimplePaint
    instanceVariableNames: 'canvas drawing currentColor'
    classVariableNames: ''
    package: 'MyGraphics'
```

```smalltalk
initialize
    super initialize.
    self extent: 400@400.
    self color: Color white.
    currentColor := Color black.
    drawing := false.
    self buildUI

buildUI
    "Add color buttons at the top"
    | colors x |
    colors := { Color black. Color red. Color blue. Color green. Color yellow }.
    x := 10.

    colors do: [ :color |
        | button |
        button := Morph new
            color: color;
            extent: 30@30;
            position: self position + (x@10);
            yourself.

        button on: #mouseDown send: #selectColor: to: self with: color.
        self addMorph: button.

        x := x + 40 ]

handlesMouseDown: event
    ^ true

mouseDown: event
    drawing := true.
    self drawDot: event position

handlesMouseMove: event
    ^ true

mouseMove: event
    drawing ifTrue: [
        self drawDot: event position ]

handlesMouseUp: event
    ^ true

mouseUp: event
    drawing := false

drawDot: position
    "Draw a dot at the position"
    | dot |
    dot := EllipseMorph new
        color: currentColor;
        extent: 10@10;
        position: position - (5@5);
        yourself.
    self addMorph: dot

selectColor: aColor
    currentColor := aColor.
    Transcript show: 'Color changed to: ', aColor asString; cr
```

```smalltalk
SimplePaint new openInWorld
```

Click and drag to draw! Click color buttons to change colors!

## UI Layouts

Organize morphs with layouts:

### TableLayout

```smalltalk
| container buttons |
container := Morph new.
container color: Color lightGray.
container extent: 200@100.
container layoutPolicy: TableLayout new.

"Add buttons in a grid"
1 to: 6 do: [ :i |
    | button |
    button := SimpleButtonMorph new
        label: 'Button ', i asString;
        yourself.
    container addMorph: button ].

container openInWorld
```

### ProportionalLayout

```smalltalk
| container top bottom |
container := Morph new.
container extent: 300@200.
container color: Color white.
container layoutPolicy: ProportionalLayout new.

"Top half"
top := Morph new color: Color lightBlue.
top layoutFrame: (LayoutFrame fractions: (0@0 corner: 1@0.5)).
container addMorph: top.

"Bottom half"
bottom := Morph new color: Color lightGreen.
bottom layoutFrame: (LayoutFrame fractions: (0@0.5 corner: 1@1)).
container addMorph: bottom.

container openInWorld
```

## Events and Interaction

Morphs respond to many events:

### Mouse Events

```smalltalk
handlesMouseDown: event        "Want mouse down?"
mouseDown: event               "Handle mouse down"
handlesMouseMove: event        "Want mouse move?"
mouseMove: event               "Handle mouse move"
handlesMouseUp: event          "Want mouse up?"
mouseUp: event                 "Handle mouse up"
mouseEnter: event              "Mouse entered"
mouseLeave: event              "Mouse left"
handlesMouseOver: event        "Want mouse over?"
```

### Keyboard Events

```smalltalk
handlesKeyboard: event         "Want keyboard events?"
keyDown: event                 "Key pressed"
keyUp: event                   "Key released"
keyStroke: event               "Key typed"
```

Example:

```smalltalk
Morph subclass: #KeyboardMorph
    instanceVariableNames: 'text'
    classVariableNames: ''
    package: 'MyGraphics'
```

```smalltalk
initialize
    super initialize.
    self color: Color white.
    self extent: 300@100.
    text := 'Type something...'.
    self on: #keyStroke send: #handleKey: to: self

handlesKeyboard: event
    ^ true

keyStroke: event
    text := text, event keyCharacter asString.
    self changed

drawOn: aCanvas
    super drawOn: aCanvas.
    aCanvas
        drawString: text
        at: self position + (10@40)
        font: nil
        color: Color black
```

```smalltalk
KeyboardMorph new openInWorld
```

Click it and type!

## Drag and Drop

Implement draggable morphs:

```smalltalk
Morph subclass: #DraggableMorph
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyGraphics'
```

```smalltalk
initialize
    super initialize.
    self color: Color random.
    self extent: 80@80

handlesMouseDown: event
    ^ true

mouseDown: event
    event hand grabMorph: self
```

```smalltalk
3 timesRepeat: [
    DraggableMorph new openInWorld ]
```

Click and drag the morphs around!

## Game Example: Simple Pong

Let's build a mini Pong game:

```smalltalk
Morph subclass: #PongGame
    instanceVariableNames: 'ball paddle score'
    classVariableNames: ''
    package: 'MyGraphics'
```

```smalltalk
initialize
    super initialize.
    self extent: 400@500.
    self color: Color black.
    score := 0.
    self buildGame.
    self startStepping

buildGame
    "Create ball"
    ball := Morph new
        color: Color white;
        extent: 15@15;
        position: self center;
        yourself.

    ball velocity: 4@4.  "Custom property for movement"
    self addMorph: ball.

    "Create paddle"
    paddle := Morph new
        color: Color blue;
        extent: 80@10;
        position: self bottomCenter - (40@30);
        yourself.
    self addMorph: paddle

handlesMouseMove: event
    ^ true

mouseMove: event
    "Move paddle with mouse"
    | newX |
    newX := (event position x - (paddle width // 2)) max: self left min: (self right - paddle width).
    paddle position: newX @ paddle position y

step
    "Update game state"
    | newPos |

    "Move ball"
    newPos := ball position + ball velocity.

    "Bounce off walls"
    (newPos x < self left or: [ newPos x > self right - ball width ]) ifTrue: [
        ball velocity: ball velocity x negated @ ball velocity y ].

    "Bounce off top"
    newPos y < self top ifTrue: [
        ball velocity: ball velocity x @ ball velocity y negated ].

    "Bounce off paddle"
    ((ball bounds intersects: paddle bounds) and: [ ball velocity y > 0 ]) ifTrue: [
        ball velocity: ball velocity x @ ball velocity y negated.
        score := score + 1.
        self changed ].

    "Miss - game over"
    newPos y > self bottom ifTrue: [
        self gameOver.
        ^ self ].

    ball position: ball position + ball velocity

stepTime
    ^ 30

gameOver
    self stopStepping.
    self color: Color red.
    Transcript show: 'Game Over! Score: ', score asString; cr

drawOn: aCanvas
    super drawOn: aCanvas.

    "Draw score"
    aCanvas
        drawString: 'Score: ', score asString
        at: self position + (10@10)
        font: (LogicalFont familyName: 'Arial' pointSize: 16)
        color: Color white
```

```smalltalk
PongGame new openInWorld
```

Move the mouse to control the paddle!

## Graphics Tips

### Performance

For smooth animation:
- Keep `step` methods fast
- Use `changed` sparingly
- Cache computed values
- Use appropriate step times

### Coordinate Systems

```smalltalk
morph bounds           "Rectangle in world coordinates"
morph position         "Top-left corner"
morph center           "Center point"
morph extent           "Width@height"
```

### Z-Order (Layering)

```smalltalk
container addMorph: morph.           "Add to front"
container addMorphBack: morph.       "Add to back"
morph comeToFront.                   "Bring forward"
morph goBehind.                      "Send backward"
```

### Visibility

```smalltalk
morph visible: false.    "Hide"
morph visible: true.     "Show"
```

## Try This!

Enhance your graphics skills:

1. **Color Mixer**
   ```smalltalk
   "Create RGB sliders that mix colors in real-time"
   ```

2. **Analog Clock**
   ```smalltalk
   "Draw clock hands that update every second"
   step
       | now center |
       now := Time now.
       center := self bounds center.
       "Draw hour, minute, second hands..."
   ```

3. **Snake Game**
   ```smalltalk
   "Classic snake game with keyboard controls"
   ```

4. **Particle System**
   ```smalltalk
   "Animated particles for effects"
   ```

5. **Graph Visualizer**
   ```smalltalk
   "Visualize data as bars or pie charts"
   ```

6. **Tile Puzzle**
   ```smalltalk
   "Sliding tile puzzle game"
   ```

7. **Paint Program**
   ```smalltalk
   "Add brush sizes, eraser, fill tool, save/load"
   ```

8. **Sprite Animation**
   ```smalltalk
   "Animate character walking using sprite frames"
   ```

## Modern Alternatives

While Morphic is powerful, modern Pharo offers:

### Spec 2

Widget-based framework (used in Chapter 31):

```smalltalk
SpPresenter subclass: #MyApp
    instanceVariableNames: 'button label'

initializePresenters
    button := self newButton label: 'Click'.
    label := self newLabel label: 'Hello'.

    button action: [ label label: 'Clicked!' ]
```

### Bloc

Modern vector graphics:

```smalltalk
| element |
element := BlElement new
    background: Color red;
    size: 100@100;
    yourself
```

### Roassal

Data visualization (great for charts and graphs):

```smalltalk
| view |
view := RSCanvas new.
(1 to: 20) do: [ :i |
    view add: (RSBox new size: i * 5) ].
view open
```

## What You Learned

Exploring graphics and UI, you've mastered:

1. **Morphic Basics**
   - Creating and positioning morphs
   - Colors, points, rectangles
   - Opening morphs in the world

2. **Custom Morphs**
   - Subclassing Morph
   - Drawing with Canvas
   - Handling events

3. **Animation**
   - Stepping for animation
   - Smooth movement
   - Game loops

4. **Interaction**
   - Mouse events
   - Keyboard input
   - Drag and drop

5. **Composition**
   - Building complex UIs
   - Parent-child relationships
   - Layouts

6. **Graphics Primitives**
   - Lines, rectangles, circles
   - Colors and transparency
   - Text rendering

7. **Real Applications**
   - Drawing programs
   - Games
   - Interactive visualizations

## Graphics in Smalltalk

Smalltalk excels at graphics because:
- **Everything is an object** - Even visual elements
- **Live programming** - Modify graphics while running
- **Direct manipulation** - Click and interact with anything
- **Composable** - Build complex from simple
- **Rich protocols** - Uniform interfaces

## Looking Ahead

You've completed Part IX (Building Real Things)! You built:
- Chapter 31: Todo List Manager (desktop app)
- Chapter 32: Text Adventure Game (interactive fiction)
- Chapter 33: Simple Web Server (network programming)
- Chapter 34: Files and Streams (data I/O)
- Chapter 35: Graphics and UI (visual programming)

Part IX showed Smalltalk's versatility across domains!

Now entering **Part X: Next Steps** with advanced topics:
- Chapter 36: Design Patterns in Smalltalk
- Chapter 37: Performance and Optimization
- Chapter 38: The Smalltalk Community
- Chapter 39: Beyond Smalltalk
- Chapter 40: Your Smalltalk Journey

You're in the home stretch!

---

**Key Takeaways:**
- **Morphic** is Smalltalk's direct-manipulation graphics framework
- Every visible thing is a **Morph** object
- **Points** (x@y) represent positions
- **Colors** have rich manipulation methods
- **Canvas** provides drawing primitives
- **Custom morphs** created by subclassing Morph
- **Event handling** for mouse and keyboard
- **Animation** via stepping (step method called repeatedly)
- **Composite morphs** built from simple morphs
- **Forms** are bitmaps you can draw on
- **Layouts** organize morph positioning
- Build **games, drawing apps, visualizations**
- **Live development** - modify graphics while running
- Modern alternatives: **Spec 2, Bloc, Roassal**
- Graphics demonstrate Smalltalk's **object-oriented** power

---

[Previous: Chapter 34 - Working with Files and Streams](chapter-34-files-and-streams.md) | [Next: Chapter 36 - Design Patterns in Smalltalk](chapter-36-design-patterns.md)
