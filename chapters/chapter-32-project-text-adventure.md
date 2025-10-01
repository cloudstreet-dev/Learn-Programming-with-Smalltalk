# Chapter 32: Project 2 - A Text Adventure Game

Welcome to our second real-world project! In Chapter 31, we built a practical todo list manager. Now we'll create something completely different: a **text adventure game** - the kind of interactive fiction that captivated players in the 1980s.

This project explores different aspects of Smalltalk: **string parsing**, **state management**, **narrative design**, **command processing**, and **object-oriented game design**. You'll build a playable adventure where players explore rooms, collect items, solve puzzles, and win the game!

Think *Zork*, *Colossal Cave Adventure*, or modern interactive fiction - but built from scratch in Smalltalk!

## What We're Building

A **text adventure game engine** with:

- **Room navigation** - Move between connected locations
- **Item management** - Pick up, drop, examine items
- **Inventory system** - Carry items with weight limits
- **Puzzles** - Locked doors, keys, challenges
- **NPC interactions** - Talk to characters
- **Game state** - Save and load progress
- **Parser** - Understand natural language commands
- **Descriptions** - Rich, atmospheric text
- **Win/lose conditions** - Complete objectives

We'll create a sample game: **"The Lost Artifact"** - a short adventure where you explore an ancient temple to find a legendary treasure!

## Project Structure

```
AdventureGame-Core
├─ Game (main game loop)
├─ Room (locations)
├─ Item (objects in the game)
├─ Player (player state)
├─ Connection (links between rooms)
└─ Command (parsed commands)

AdventureGame-Parser
├─ CommandParser (text → commands)
└─ Vocabulary (recognized words)

AdventureGame-Content
├─ TempleAdventure (our sample game)
└─ ContentBuilder (DSL for creating games)

AdventureGame-Tests
└─ Various test classes
```

## Step 1: Create the Packages

```smalltalk
RPackageOrganizer default createPackageNamed: 'AdventureGame-Core'.
RPackageOrganizer default createPackageNamed: 'AdventureGame-Parser'.
RPackageOrganizer default createPackageNamed: 'AdventureGame-Content'.
RPackageOrganizer default createPackageNamed: 'AdventureGame-Tests'
```

## Step 2: The Room Class

Rooms are locations players can visit:

```smalltalk
Object subclass: #Room
    instanceVariableNames: 'name description items connections visited'
    classVariableNames: ''
    package: 'AdventureGame-Core'
```

### Initialize

```smalltalk
initialize
    super initialize.
    name := 'Unnamed Room'.
    description := 'There is nothing special here.'.
    items := OrderedCollection new.
    connections := Dictionary new.
    visited := false

name
    ^ name

name: aString
    name := aString

description
    ^ description

description: aString
    description := aString

items
    ^ items

connections
    ^ connections

isVisited
    ^ visited

markVisited
    visited := true
```

### Navigation

```smalltalk
connectTo: aRoom direction: aSymbol
    "Create a one-way connection to another room"
    connections at: aSymbol put: aRoom

connectBidirectionalTo: aRoom direction: aSymbol oppositeDirection: oppositeSymbol
    "Create a two-way connection"
    self connectTo: aRoom direction: aSymbol.
    aRoom connectTo: self direction: oppositeSymbol

roomAt: direction
    "Answer the room in the given direction, or nil"
    ^ connections at: direction ifAbsent: [ nil ]

availableDirections
    "Answer a collection of valid directions"
    ^ connections keys

canGoToward: direction
    "Answer whether you can go in this direction"
    ^ connections includesKey: direction
```

### Items

```smalltalk
addItem: anItem
    "Add an item to this room"
    items add: anItem.
    anItem location: self

removeItem: anItem
    "Remove an item from this room"
    items remove: anItem ifAbsent: [ ]

findItem: itemName
    "Find an item by name (case-insensitive)"
    ^ items
        detect: [ :item | item name asLowercase = itemName asLowercase ]
        ifNone: [ nil ]

hasItem: itemName
    "Answer whether this room contains the named item"
    ^ (self findItem: itemName) notNil

itemNames
    "Answer a list of item names"
    ^ items collect: [ :item | item name ]
```

### Display

```smalltalk
fullDescription
    "Answer a complete description including items and exits"
    ^ String streamContents: [ :stream |
        "Room name"
        stream
            nextPutAll: '=== ';
            nextPutAll: name;
            nextPutAll: ' ==='; cr; cr.

        "Description"
        stream nextPutAll: description; cr.

        "Items"
        items ifNotEmpty: [
            stream cr; nextPutAll: 'You see: '.
            items
                do: [ :item | stream nextPutAll: item name ]
                separatedBy: [ stream nextPutAll: ', ' ].
            stream nextPut: $.
        ].

        "Exits"
        connections ifNotEmpty: [
            stream cr; nextPutAll: 'Exits: '.
            self availableDirections
                do: [ :dir | stream nextPutAll: dir asString ]
                separatedBy: [ stream nextPutAll: ', ' ].
            stream nextPut: $.
        ] ]

printOn: aStream
    aStream nextPutAll: name
```

### Convenience Class Methods

```smalltalk
"Class side:"
named: aString
    ^ self new
        name: aString;
        yourself

named: aString description: descString
    ^ self new
        name: aString;
        description: descString;
        yourself
```

## Step 3: The Item Class

Items are objects in the game world:

```smalltalk
Object subclass: #Item
    instanceVariableNames: 'name description weight takeable location'
    classVariableNames: ''
    package: 'AdventureGame-Core'
```

### Initialize

```smalltalk
initialize
    super initialize.
    name := 'item'.
    description := 'An ordinary item.'.
    weight := 1.
    takeable := true.
    location := nil

name
    ^ name

name: aString
    name := aString

description
    ^ description

description: aString
    description := aString

weight
    ^ weight

weight: aNumber
    weight := aNumber

isTakeable
    ^ takeable

makeTakeable
    takeable := true

makeUntakeable
    takeable := false

location
    ^ location

location: aRoom
    location := aRoom
```

### Display

```smalltalk
examine
    "Answer a detailed description"
    ^ description

printOn: aStream
    aStream nextPutAll: name
```

### Convenience Class Methods

```smalltalk
"Class side:"
named: aString
    ^ self new
        name: aString;
        yourself

named: aString description: descString
    ^ self new
        name: aString;
        description: descString;
        yourself

scenery: aString description: descString
    "Create an untakeable scenery item"
    ^ self new
        name: aString;
        description: descString;
        makeUntakeable;
        yourself
```

## Step 4: The Player Class

The player represents the game state:

```smalltalk
Object subclass: #Player
    instanceVariableNames: 'currentRoom inventory maxWeight'
    classVariableNames: ''
    package: 'AdventureGame-Core'
```

### Initialize

```smalltalk
initialize
    super initialize.
    inventory := OrderedCollection new.
    maxWeight := 10.
    currentRoom := nil

currentRoom
    ^ currentRoom

currentRoom: aRoom
    currentRoom := aRoom.
    currentRoom markVisited

inventory
    ^ inventory

maxWeight
    ^ maxWeight

maxWeight: aNumber
    maxWeight := aNumber
```

### Movement

```smalltalk
moveTo: direction
    "Move in the given direction if possible"
    | nextRoom |
    nextRoom := currentRoom roomAt: direction.
    nextRoom ifNil: [ ^ 'You cannot go that way.' ].

    self currentRoom: nextRoom.
    ^ nextRoom fullDescription

canMoveTo: direction
    ^ currentRoom canGoToward: direction
```

### Inventory Management

```smalltalk
takeItem: itemName
    "Pick up an item from the current room"
    | item |
    item := currentRoom findItem: itemName.
    item ifNil: [ ^ 'You don''t see that here.' ].

    item isTakeable ifFalse: [
        ^ 'You cannot take the ', item name, '.' ].

    (self canCarry: item) ifFalse: [
        ^ 'The ', item name, ' is too heavy to carry.' ].

    currentRoom removeItem: item.
    inventory add: item.
    item location: nil.
    ^ 'You take the ', item name, '.'

dropItem: itemName
    "Drop an item from inventory into the current room"
    | item |
    item := self findInInventory: itemName.
    item ifNil: [ ^ 'You don''t have that.' ].

    inventory remove: item.
    currentRoom addItem: item.
    ^ 'You drop the ', item name, '.'

hasItem: itemName
    "Answer whether the player is carrying this item"
    ^ (self findInInventory: itemName) notNil

findInInventory: itemName
    "Find an item in inventory by name"
    ^ inventory
        detect: [ :item | item name asLowercase = itemName asLowercase ]
        ifNone: [ nil ]

canCarry: anItem
    "Answer whether the player can carry this item"
    ^ self currentWeight + anItem weight <= maxWeight

currentWeight
    "Answer total weight of carried items"
    ^ inventory sum: [ :item | item weight ]

inventoryDescription
    "Answer a description of carried items"
    inventory ifEmpty: [ ^ 'You are carrying nothing.' ].

    ^ String streamContents: [ :stream |
        stream nextPutAll: 'You are carrying: '.
        inventory
            do: [ :item | stream nextPutAll: item name ]
            separatedBy: [ stream nextPutAll: ', ' ].
        stream
            nextPut: $.;
            cr;
            nextPutAll: '(';
            print: self currentWeight;
            nextPutAll: '/';
            print: maxWeight;
            nextPutAll: ' kg)' ]
```

### Examining

```smalltalk
examine: itemName
    "Examine an item in the room or inventory"
    | item |

    "Check inventory first"
    item := self findInInventory: itemName.
    item ifNotNil: [ ^ item examine ].

    "Check current room"
    item := currentRoom findItem: itemName.
    item ifNotNil: [ ^ item examine ].

    ^ 'You don''t see that here.'
```

## Step 5: The Command Parser

Parse player input into commands:

```smalltalk
Object subclass: #Command
    instanceVariableNames: 'verb noun'
    classVariableNames: ''
    package: 'AdventureGame-Parser'
```

```smalltalk
initialize
    super initialize.
    verb := nil.
    noun := nil

verb
    ^ verb

verb: aSymbol
    verb := aSymbol

noun
    ^ noun

noun: aString
    noun := aString

"Class side:"
verb: aSymbol noun: aString
    ^ self new
        verb: aSymbol;
        noun: aString;
        yourself

verb: aSymbol
    ^ self new
        verb: aSymbol;
        yourself
```

### CommandParser

```smalltalk
Object subclass: #CommandParser
    instanceVariableNames: 'vocabulary'
    classVariableNames: ''
    package: 'AdventureGame-Parser'
```

```smalltalk
initialize
    super initialize.
    self initializeVocabulary

initializeVocabulary
    "Map synonyms to canonical verbs"
    vocabulary := Dictionary new.

    "Movement"
    #('go' 'move' 'walk' 'run') do: [ :word |
        vocabulary at: word put: #go ].

    "Taking items"
    #('take' 'get' 'grab' 'pick') do: [ :word |
        vocabulary at: word put: #take ].

    "Dropping items"
    #('drop' 'leave' 'put') do: [ :word |
        vocabulary at: word put: #drop ].

    "Examining"
    #('examine' 'look' 'inspect' 'x') do: [ :word |
        vocabulary at: word put: #examine ].

    "Inventory"
    #('inventory' 'inv' 'i') do: [ :word |
        vocabulary at: word put: #inventory ].

    "Help"
    #('help' 'h' '?') do: [ :word |
        vocabulary at: word put: #help ].

    "Quit"
    #('quit' 'exit' 'q') do: [ :word |
        vocabulary at: word put: #quit ].

    "Directions"
    #('north' 'n') do: [ :word | vocabulary at: word put: #north ].
    #('south' 's') do: [ :word | vocabulary at: word put: #south ].
    #('east' 'e') do: [ :word | vocabulary at: word put: #east ].
    #('west' 'w') do: [ :word | vocabulary at: word put: #west ].
    #('up' 'u') do: [ :word | vocabulary at: word put: #up ].
    #('down' 'd') do: [ :word | vocabulary at: word put: #down ]

parse: inputString
    "Parse input into a Command"
    | words firstWord restWords |

    words := inputString trimBoth substrings.
    words ifEmpty: [ ^ nil ].

    firstWord := words first asLowercase.

    "Check if it's a direction"
    (vocabulary at: firstWord ifAbsent: [ nil ]) ifNotNil: [ :verb |
        (#(north south east west up down) includes: verb) ifTrue: [
            ^ Command verb: #go noun: verb asString ] ].

    "Check if it's a one-word command"
    (#('inventory' 'help' 'quit' 'look') includes: firstWord) ifTrue: [
        ^ Command verb: (vocabulary at: firstWord ifAbsent: [ firstWord asSymbol ]) ].

    "Two-word command: verb noun"
    words size < 2 ifTrue: [
        ^ Command verb: (vocabulary at: firstWord ifAbsent: [ firstWord asSymbol ]) ].

    restWords := words allButFirst.
    ^ Command
        verb: (vocabulary at: firstWord ifAbsent: [ firstWord asSymbol ])
        noun: (restWords joinUsing: ' ')
```

## Step 6: The Game Class

The main game engine:

```smalltalk
Object subclass: #Game
    instanceVariableNames: 'player rooms parser running startRoom'
    classVariableNames: ''
    package: 'AdventureGame-Core'
```

### Initialize

```smalltalk
initialize
    super initialize.
    player := Player new.
    rooms := OrderedCollection new.
    parser := CommandParser new.
    running := false.
    startRoom := nil

player
    ^ player

rooms
    ^ rooms

startRoom: aRoom
    startRoom := aRoom.
    player currentRoom: aRoom

addRoom: aRoom
    rooms add: aRoom
```

### Game Loop

```smalltalk
start
    "Start the game"
    running := true.
    self showIntroduction.
    Transcript show: player currentRoom fullDescription; cr; cr.

    [ running ] whileTrue: [
        self processInput ]

processInput
    "Get and process one command"
    | input command response |

    Transcript show: '> '.
    input := UIManager default request: 'Command:'.
    input ifNil: [ ^ self quit ].

    Transcript show: input; cr.

    command := parser parse: input.
    command ifNil: [
        Transcript show: 'I don''t understand that.'; cr; cr.
        ^ self ].

    response := self executeCommand: command.
    Transcript show: response; cr; cr

executeCommand: aCommand
    "Execute a parsed command and return the response"
    aCommand verb = #go ifTrue: [
        ^ player moveTo: aCommand noun asSymbol ].

    aCommand verb = #take ifTrue: [
        ^ player takeItem: aCommand noun ].

    aCommand verb = #drop ifTrue: [
        ^ player dropItem: aCommand noun ].

    aCommand verb = #examine ifTrue: [
        aCommand noun ifNil: [ ^ player currentRoom fullDescription ].
        ^ player examine: aCommand noun ].

    aCommand verb = #inventory ifTrue: [
        ^ player inventoryDescription ].

    aCommand verb = #help ifTrue: [
        ^ self showHelp ].

    aCommand verb = #quit ifTrue: [
        ^ self quit ].

    aCommand verb = #look ifTrue: [
        ^ player currentRoom fullDescription ].

    ^ 'I don''t know how to do that.'

quit
    "End the game"
    running := false.
    ^ 'Thanks for playing!'
```

### Display Methods

```smalltalk
showIntroduction
    "Override in subclasses for specific games"
    Transcript
        show: '================================='; cr;
        show: '   TEXT ADVENTURE ENGINE'; cr;
        show: '================================='; cr; cr;
        show: 'Type "help" for commands.'; cr; cr

showHelp
    ^ 'Commands:
  north/south/east/west (or n/s/e/w) - Move
  take <item> - Pick up an item
  drop <item> - Drop an item
  examine <item> - Look at something
  inventory (or i) - Show what you''re carrying
  look - Look around
  help - Show this help
  quit - Exit the game'
```

## Step 7: Build a Sample Game

Now let's create "The Lost Artifact"!

```smalltalk
Game subclass: #TempleAdventure
    instanceVariableNames: 'doorUnlocked'
    classVariableNames: ''
    package: 'AdventureGame-Content'
```

### Building the World

```smalltalk
buildWorld
    "Create rooms, items, and connections"
    | entrance hall corridor treasureRoom key door statue torch |

    "Create rooms"
    entrance := Room
        named: 'Temple Entrance'
        description: 'You stand before the entrance to an ancient temple. Vines cover the weathered stone walls. A dark passage leads north into the temple.'.

    hall := Room
        named: 'Great Hall'
        description: 'A vast hall with towering pillars. Faded murals depict forgotten gods. The air is thick with dust. Passages lead north and east. A heavy stone door blocks the way west.'.

    corridor := Room
        named: 'East Corridor'
        description: 'A narrow corridor lit by cracks in the ceiling. Strange symbols cover the walls.'.

    treasureRoom := Room
        named: 'Treasure Chamber'
        description: 'The legendary treasure chamber! Gold glitters in the dim light. In the center, on a pedestal, rests the Lost Artifact!'.

    "Connect rooms"
    entrance connectBidirectionalTo: hall
        direction: #north oppositeDirection: #south.

    hall connectBidirectionalTo: corridor
        direction: #east oppositeDirection: #west.

    "West is blocked initially"
    doorUnlocked := false.

    "Create items"
    torch := Item
        named: 'torch'
        description: 'A sturdy wooden torch. It still burns with a dim flame.'.
    torch weight: 2.

    key := Item
        named: 'golden key'
        description: 'An ornate golden key with mysterious symbols etched into it.'.
    key weight: 1.

    statue := Item
        scenery: 'statue'
        description: 'A weathered stone statue of an ancient guardian. Its empty eyes seem to follow you.'.

    door := Item
        scenery: 'door'
        description: 'A massive stone door covered in intricate carvings. There is a keyhole in the center.'.

    "Place items"
    entrance addItem: torch.
    corridor addItem: key.
    hall addItem: statue.
    hall addItem: door.

    "Add rooms to game"
    self addRoom: entrance.
    self addRoom: hall.
    self addRoom: corridor.
    self addRoom: treasureRoom.

    "Set starting room"
    self startRoom: entrance

initialize
    super initialize.
    doorUnlocked := false.
    self buildWorld
```

### Custom Commands

Override command execution for special interactions:

```smalltalk
executeCommand: aCommand
    "Add custom commands for this game"

    "Use key on door"
    (aCommand verb = #use and: [ aCommand noun includesSubstring: 'key' ]) ifTrue: [
        ^ self unlockDoor ].

    (aCommand verb = #unlock and: [ aCommand noun includesSubstring: 'door' ]) ifTrue: [
        ^ self unlockDoor ].

    "Take artifact - win condition!"
    (aCommand verb = #take and: [ aCommand noun includesSubstring: 'artifact' ]) ifTrue: [
        ^ self winGame ].

    "Default commands"
    ^ super executeCommand: aCommand

unlockDoor
    "Unlock the door to the treasure room"
    doorUnlocked ifTrue: [ ^ 'The door is already unlocked.' ].

    (player currentRoom name = 'Great Hall') ifFalse: [
        ^ 'There is no door here to unlock.' ].

    (player hasItem: 'golden key') ifFalse: [
        ^ 'You need a key to unlock the door.' ].

    doorUnlocked := true.

    "Create connection to treasure room"
    player currentRoom connectBidirectionalTo: (rooms detect: [ :r | r name = 'Treasure Chamber' ])
        direction: #west oppositeDirection: #east.

    ^ 'You insert the golden key into the door. With a deep rumbling sound, the massive door swings open, revealing a passage to the west!'

winGame
    "Player found the artifact!"
    running := false.
    ^ '
╔════════════════════════════════════════╗
║                                        ║
║          CONGRATULATIONS!              ║
║                                        ║
║   You have found the Lost Artifact!    ║
║                                        ║
║        You win the game!               ║
║                                        ║
╚════════════════════════════════════════╝

The legendary artifact glows in your hands. Your adventure is complete!

Thanks for playing!'

showIntroduction
    Transcript
        show: '╔════════════════════════════════════════╗'; cr;
        show: '║       THE LOST ARTIFACT                ║'; cr;
        show: '║       A Text Adventure                 ║'; cr;
        show: '╚════════════════════════════════════════╝'; cr; cr;
        show: 'You are an adventurer seeking the legendary Lost Artifact,'; cr;
        show: 'said to be hidden in an ancient temple deep in the jungle.'; cr; cr;
        show: 'Can you navigate the temple''s mysteries and claim the treasure?'; cr; cr;
        show: 'Type "help" for commands.'; cr; cr
```

### Playing the Game

```smalltalk
"Class side:"
play
    "Start a new game"
    ^ self new start
```

## Step 8: Play Your Game!

Now let's play:

```smalltalk
TempleAdventure play
```

Sample gameplay:

```
╔════════════════════════════════════════╗
║       THE LOST ARTIFACT                ║
║       A Text Adventure                 ║
╚════════════════════════════════════════╝

...

=== Temple Entrance ===

You stand before the entrance to an ancient temple...

You see: torch.
Exits: north.

> take torch
You take the torch.

> north

=== Great Hall ===

A vast hall with towering pillars...

You see: statue, door.
Exits: north, east, south.

> examine statue
A weathered stone statue of an ancient guardian...

> east

=== East Corridor ===

A narrow corridor lit by cracks in the ceiling...

You see: golden key.
Exits: west.

> take key
You take the golden key.

> west

=== Great Hall ===

> unlock door
You insert the golden key into the door...

> west

=== Treasure Chamber ===

The legendary treasure chamber!...

> take artifact

╔════════════════════════════════════════╗
║          CONGRATULATIONS!              ║
...
```

🎉 You've created a playable text adventure!

## Step 9: Testing

Write tests for game mechanics:

```smalltalk
TestCase subclass: #RoomTest
    instanceVariableNames: 'room item'
    classVariableNames: ''
    package: 'AdventureGame-Tests'
```

```smalltalk
setUp
    room := Room named: 'Test Room'.
    item := Item named: 'test item'

testAddingItems
    self assert: room items isEmpty.
    room addItem: item.
    self assert: room items size equals: 1.
    self assert: (room hasItem: 'test item')

testFindingItems
    room addItem: item.
    self assert: (room findItem: 'test item') equals: item.
    self assert: (room findItem: 'nonexistent') isNil

testConnections
    | otherRoom |
    otherRoom := Room named: 'Other Room'.
    room connectTo: otherRoom direction: #north.

    self assert: (room canGoToward: #north).
    self deny: (room canGoToward: #south).
    self assert: (room roomAt: #north) equals: otherRoom

testBidirectionalConnections
    | otherRoom |
    otherRoom := Room named: 'Other Room'.
    room connectBidirectionalTo: otherRoom
        direction: #north
        oppositeDirection: #south.

    self assert: (room roomAt: #north) equals: otherRoom.
    self assert: (otherRoom roomAt: #south) equals: room
```

```smalltalk
TestCase subclass: #PlayerTest
    instanceVariableNames: 'player room item'
    classVariableNames: ''
    package: 'AdventureGame-Tests'
```

```smalltalk
setUp
    player := Player new.
    room := Room named: 'Test Room'.
    item := Item named: 'test item'.
    player currentRoom: room.
    room addItem: item

testTakingItems
    player takeItem: 'test item'.
    self assert: (player hasItem: 'test item').
    self deny: (room hasItem: 'test item')

testDroppingItems
    player takeItem: 'test item'.
    player dropItem: 'test item'.
    self deny: (player hasItem: 'test item').
    self assert: (room hasItem: 'test item')

testWeightLimit
    | heavyItem |
    player maxWeight: 5.
    heavyItem := Item named: 'boulder'.
    heavyItem weight: 10.
    room addItem: heavyItem.

    player takeItem: 'boulder'.
    self deny: (player hasItem: 'boulder')

testMovement
    | northRoom |
    northRoom := Room named: 'North Room'.
    room connectTo: northRoom direction: #north.

    player moveTo: #north.
    self assert: player currentRoom equals: northRoom
```

Run tests:

```smalltalk
RoomTest suite run.
PlayerTest suite run
```

## Step 10: Enhancements

Let's add more features!

### NPCs (Non-Player Characters)

```smalltalk
Object subclass: #NPC
    instanceVariableNames: 'name description conversation location'
    classVariableNames: ''
    package: 'AdventureGame-Core'
```

```smalltalk
initialize
    super initialize.
    name := 'stranger'.
    description := 'A mysterious figure.'.
    conversation := 'The figure nods silently.'.
    location := nil

name: aString
    name := aString

description: aString
    description := aString

conversation: aString
    conversation := aString

talkTo
    ^ conversation

examine
    ^ description
```

Add NPCs to rooms:

```smalltalk
Room >> addNPC: anNPC
    npcs add: anNPC.
    anNPC location: self
```

### Combat System

Add simple combat:

```smalltalk
Object subclass: #Monster
    instanceVariableNames: 'name health damage'
    classVariableNames: ''
    package: 'AdventureGame-Core'
```

```smalltalk
initialize
    super initialize.
    name := 'monster'.
    health := 10.
    damage := 2

isAlive
    ^ health > 0

takeDamage: amount
    health := health - amount.
    ^ health <= 0

attack: aPlayer
    aPlayer takeDamage: damage.
    ^ name, ' attacks for ', damage asString, ' damage!'
```

### Save/Load System

Save game state:

```smalltalk
Game >> saveGame
    "Save game state to file"
    | data |
    data := Dictionary new
        at: 'playerRoom' put: player currentRoom name;
        at: 'inventory' put: (player inventory collect: #name);
        at: 'visitedRooms' put: (rooms select: #isVisited thenCollect: #name);
        at: 'doorUnlocked' put: doorUnlocked;
        yourself.

    'adventure_save.ston' asFileReference writeStreamDo: [ :stream |
        STON put: data onStream: stream ].

    ^ 'Game saved!'

Game >> loadGame
    "Load game state from file"
    | data |
    'adventure_save.ston' asFileReference exists ifFalse: [
        ^ 'No saved game found.' ].

    data := 'adventure_save.ston' asFileReference readStreamDo: [ :stream |
        STON fromStream: stream ].

    "Restore state..."
    ^ 'Game loaded!'
```

### Multiple Endings

Track player choices:

```smalltalk
TempleAdventure >> initialize
    super initialize.
    choices := Dictionary new.
    self buildWorld

executeCommand: aCommand
    "Track important choices"
    (aCommand verb = #take and: [ aCommand noun = 'cursed idol' ]) ifTrue: [
        choices at: 'tookIdol' put: true.
        ^ self badEnding ].

    ^ super executeCommand: aCommand

badEnding
    running := false.
    ^ 'As you touch the cursed idol, you feel a chill...
    The temple begins to crumble around you!

    GAME OVER

    (You got the bad ending. Try again!)'
```

### Puzzle System

Create puzzles:

```smalltalk
Object subclass: #Puzzle
    instanceVariableNames: 'description solution solved rewardText'
    classVariableNames: ''
    package: 'AdventureGame-Core'
```

```smalltalk
attempt: answer
    "Check if answer solves the puzzle"
    solved ifTrue: [ ^ 'You already solved this.' ].

    (answer asLowercase = solution asLowercase) ifTrue: [
        solved := true.
        ^ rewardText ].

    ^ 'That doesn''t work.'
```

Add to game:

```smalltalk
"In TempleAdventure:"
riddleRoom := Room named: 'Chamber of Riddles'
    description: 'An ancient voice echoes: "What has keys but no locks, space but no room, and you can enter but not go in?"'.

executeCommand: aCommand
    (aCommand verb = #answer) ifTrue: [
        ^ self solveRiddle: aCommand noun ].
    ^ super executeCommand: aCommand

solveRiddle: answer
    (answer = 'keyboard') ifTrue: [
        ^ self unlockSecretPassage ].
    ^ 'Incorrect. The riddle remains unsolved.'
```

## Try This!

Expand the game:

1. **Add More Rooms**
   Create a larger temple with multiple areas:
   ```smalltalk
   cryptRoom := Room named: 'Ancient Crypt' description: '...'.
   library := Room named: 'Dusty Library' description: '...'.
   ```

2. **Implement Light Sources**
   Some rooms require a torch:
   ```smalltalk
   Room >> isDark
       ^ dark ifNil: [ false ]

   Player >> moveTo: direction
       nextRoom isDark ifTrue: [
           (self hasItem: 'torch') ifFalse: [
               ^ 'It''s too dark to go that way!' ] ]
   ```

3. **Add Time Limits**
   Create urgency:
   ```smalltalk
   Game >> initialize
       ...
       turnCount := 0.
       maxTurns := 50

   processInput
       ...
       turnCount := turnCount + 1.
       turnCount >= maxTurns ifTrue: [ ^ self timeOut ]
   ```

4. **Container Items**
   Items that hold other items:
   ```smalltalk
   chest := Container named: 'wooden chest'.
   chest addContent: (Item named: 'gold coins')
   ```

5. **Score System**
   Track player achievements:
   ```smalltalk
   Player >> addScore: points
       score := score + points.
       ^ 'You earned ', points asString, ' points!'
   ```

6. **Map Command**
   Show a text map:
   ```smalltalk
   showMap
       ^ '
             [Treasure]
                  |
         [Corridor]-[Hall]-[Secret]
                  |
              [Entrance]'
   ```

7. **Hint System**
   Provide contextual hints:
   ```smalltalk
   showHint
       player currentRoom name = 'Great Hall' ifTrue: [
           ^ 'Perhaps you should explore to the east...' ]
   ```

8. **Achievement System**
   Track accomplishments:
   ```smalltalk
   achievements := Set new.

   unlockAchievement: name
       (achievements includes: name) ifTrue: [ ^ self ].
       achievements add: name.
       ^ '🏆 Achievement unlocked: ', name
   ```

## Architecture Lessons

This project demonstrates:

### Object-Oriented Design
- **Encapsulation**: Rooms manage their items, players manage inventory
- **Single Responsibility**: Each class has one clear purpose
- **Composition**: Game composed of rooms, items, player

### State Management
- Game state tracked through objects
- Mutable state (room connections, inventory) handled cleanly
- State changes trigger updates

### String Processing
- Parsing natural language input
- Handling synonyms and variations
- Building formatted output

### Domain-Specific Language
- Fluent interfaces for building games
- Declarative room/item creation
- Readable game definition

## What You Learned

Building this text adventure, you practiced:

1. **Complex Object Interactions**
   - Objects communicating and modifying each other
   - Managing relationships (player in room, items in inventory)

2. **State Machines**
   - Game state transitions
   - Tracking conditions (locked/unlocked doors)
   - Win/lose conditions

3. **String Manipulation**
   - Parsing input
   - Building formatted output
   - Text generation

4. **Collections**
   - Managing dynamic collections
   - Searching and filtering
   - Set operations

5. **Design Patterns**
   - Command pattern (parsed commands)
   - Composite pattern (rooms contain items)
   - Strategy pattern (different command handlers)

6. **User Experience**
   - Clear feedback
   - Helpful error messages
   - Atmospheric descriptions

## Interactive Fiction in Smalltalk

Smalltalk is excellent for text adventures because:
- **Objects map naturally to game entities** - Rooms, items, characters are objects
- **Live development** - Test as you build, no restart needed
- **Introspection** - Debug by inspecting game state
- **Rapid iteration** - Quickly add features and content
- **Clean abstraction** - Separate engine from content

## Looking Ahead

You've built a complete text adventure engine and game! You understand:
- Complex object-oriented design
- State management
- String processing and parsing
- Game development in Smalltalk
- Creating interactive experiences

In Chapter 33, we'll build a **Simple Web Server** - handling HTTP, serving pages, and creating web applications in pure Smalltalk!

Then Chapter 34 covers **Files and Streams** - reading, writing, and processing data!

Part IX is showing you Smalltalk's versatility across different domains!

---

**Key Takeaways:**
- Built a complete **text adventure game engine**
- Created **rooms, items, player, and game state** objects
- Implemented **natural language parser** for commands
- Developed **"The Lost Artifact"** - a playable game
- Applied **object-oriented design** to game development
- Managed **complex state** (inventory, connections, puzzles)
- Used **collections** extensively for game data
- Demonstrated **string processing** and output formatting
- Created **extensible architecture** for adding features
- Added **NPCs, combat, puzzles, and saving**
- Showed how Smalltalk excels at **interactive systems**
- Built **testable game mechanics**
- Used **live coding** for rapid game development
- Created engaging **player experience** with text
- Proved Smalltalk's power for **creative projects**

---

[Previous: Chapter 31 - Project 1: A Todo List Manager](chapter-31-project-todo-list.md) | [Next: Chapter 33 - Project 3: A Simple Web Server](chapter-33-project-web-server.md)
