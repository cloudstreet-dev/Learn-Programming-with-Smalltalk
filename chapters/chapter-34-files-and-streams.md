# Chapter 34: Working with Files and Streams

After building complete applications in Chapters 31-33, let's explore a fundamental topic: **files and streams**. Every real program needs to read and write data - configuration files, logs, user documents, CSV data, and more.

Smalltalk has an elegant, object-oriented approach to I/O. Instead of procedural file operations, you work with **streams** - objects that represent flowing data. This chapter teaches you to master files and streams for all your data processing needs!

## Why Streams?

### Traditional Approach (Procedural)

```c
// C-style
FILE* file = fopen("data.txt", "r");
char buffer[100];
fgets(buffer, 100, file);
fclose(file);
```

Clunky, error-prone, manual memory management.

### Smalltalk Approach (Object-Oriented)

```smalltalk
'data.txt' asFileReference readStreamDo: [ :stream |
    stream nextLine ]
```

Clean, safe, automatic cleanup!

**Streams** are objects representing sequential data flow. They provide a uniform interface whether you're reading from:
- Files
- Network sockets
- Strings
- Collections
- Standard input/output

## Understanding Streams

A **stream** is like a tape player:
- **Read streams** play data (input)
- **Write streams** record data (output)
- **Position** tracks where you are
- Can **peek** ahead or **skip** data

### Stream Hierarchy

```
Stream (abstract)
├─ PositionableStream
│  ├─ ReadStream
│  ├─ WriteStream
│  └─ ReadWriteStream
└─ FileStream
```

All streams share common protocols!

## Reading from Strings

The simplest streams are **string streams** - great for learning!

### ReadStream Basics

```smalltalk
| stream |
stream := 'Hello World' readStream.

stream next.        "-> $H"
stream next.        "-> $e"
stream next: 3.     "-> 'llo'"
stream upTo: $W.    "-> ' '"
stream upToEnd.     "-> 'orld'"
```

Each `next` advances the position!

### Position and Peeking

```smalltalk
| stream |
stream := 'ABCDEF' readStream.

stream position.           "-> 0 (at start)"
stream next.               "-> $A"
stream position.           "-> 1"

stream peek.               "-> $B (doesn't advance)"
stream next.               "-> $B (now it advances)"

stream skip: 2.            "Skip C and D"
stream next.               "-> $E"

stream atEnd.              "-> false"
stream next.               "-> $F"
stream atEnd.              "-> true"
```

### Reading Lines

```smalltalk
| stream |
stream := 'Line 1
Line 2
Line 3' readStream.

stream nextLine.    "-> 'Line 1'"
stream nextLine.    "-> 'Line 2'"
stream nextLine.    "-> 'Line 3'"
stream atEnd.       "-> true"
```

### Useful Reading Methods

```smalltalk
| stream |
stream := 'apple,banana,cherry' readStream.

"Read until delimiter"
stream upTo: $,.       "-> 'apple'"
stream upTo: $,.       "-> 'banana'"
stream upToEnd.        "-> 'cherry'"
```

```smalltalk
| stream |
stream := '42 is the answer' readStream.

"Read matching characters"
stream upToAnyOf: ' '.  "-> '42'"
stream skip: 1.         "Skip space"
stream upToEnd.         "-> 'is the answer'"
```

## Writing to Strings

**WriteStreams** build strings incrementally:

### WriteStream Basics

```smalltalk
| stream |
stream := WriteStream on: String new.

stream nextPut: $H.
stream nextPut: $i.
stream nextPut: $!.

stream contents.    "-> 'Hi!'"
```

More commonly, use `nextPutAll:`:

```smalltalk
| stream |
stream := WriteStream on: String new.

stream nextPutAll: 'Hello'.
stream space.
stream nextPutAll: 'World'.
stream cr.
stream nextPutAll: 'How are you?'.

stream contents.
"-> 'Hello World
How are you?'"
```

### Building Complex Strings

```smalltalk
| stream |
stream := WriteStream on: String new.

stream
    nextPutAll: 'Name: ';
    nextPutAll: 'Alice';
    cr;
    nextPutAll: 'Age: ';
    print: 30;
    cr.

stream contents.
"-> 'Name: Alice
Age: 30
'"
```

### Stream Convenience

Many objects support streaming:

```smalltalk
String streamContents: [ :stream |
    stream
        nextPutAll: 'Numbers: ';
        print: (1 to: 5);
        cr;
        nextPutAll: 'Done!' ]

"-> 'Numbers: (1 to: 5)
Done!'"
```

## File References

Smalltalk uses **FileReference** objects to represent files and directories:

### Creating File References

```smalltalk
"Relative path"
'data.txt' asFileReference.

"Absolute path"
'/Users/alice/documents/notes.txt' asFileReference.

"Home directory"
FileLocator home / 'documents' / 'data.txt'.

"Working directory"
FileLocator workingDirectory / 'config.ini'.

"Image directory"
FileLocator imageDirectory / 'resources' / 'icon.png'.
```

### File Operations

```smalltalk
| file |
file := 'example.txt' asFileReference.

"Check existence"
file exists.        "-> true/false"

"File info"
file size.          "-> size in bytes"
file isFile.        "-> true"
file isDirectory.   "-> false"
file basename.      "-> 'example.txt'"
file extension.     "-> 'txt'"

"Create"
file ensureCreateFile.

"Delete"
file ensureDelete.

"Rename/move"
file renameTo: 'new-name.txt'.
```

### Directory Operations

```smalltalk
| dir |
dir := 'my-folder' asFileReference.

"Create directory"
dir ensureCreateDirectory.

"List contents"
dir children.       "-> collection of FileReferences"
dir files.          "-> only files"
dir directories.    "-> only directories"

"Navigate"
dir / 'subfolder' / 'file.txt'.

"Parent directory"
dir parent.

"Delete (must be empty)"
dir ensureDelete.

"Delete recursively"
dir deleteAll.
```

## Reading Files

Now let's read real files!

### Read Entire File

```smalltalk
| file contents |
file := 'data.txt' asFileReference.

"Read as string"
contents := file contents.

"Read as bytes"
contents := file binaryReadStream contents.
```

### Read File with Stream

The safe way - automatically closes the file:

```smalltalk
'data.txt' asFileReference readStreamDo: [ :stream |
    stream contents ]
```

### Read Line by Line

```smalltalk
'data.txt' asFileReference readStreamDo: [ :stream |
    [ stream atEnd ] whileFalse: [
        | line |
        line := stream nextLine.
        Transcript show: line; cr ] ]
```

Or collect lines:

```smalltalk
| lines |
lines := OrderedCollection new.

'data.txt' asFileReference readStreamDo: [ :stream |
    [ stream atEnd ] whileFalse: [
        lines add: stream nextLine ] ].

lines
```

### Process Large Files Efficiently

Don't load entire file into memory:

```smalltalk
"Count lines in large file"
| count |
count := 0.

'huge-file.txt' asFileReference readStreamDo: [ :stream |
    [ stream atEnd ] whileFalse: [
        stream nextLine.
        count := count + 1 ] ].

count
```

```smalltalk
"Find lines containing a word"
| matches |
matches := OrderedCollection new.

'log.txt' asFileReference readStreamDo: [ :stream |
    [ stream atEnd ] whileFalse: [
        | line |
        line := stream nextLine.
        (line includesSubstring: 'ERROR') ifTrue: [
            matches add: line ] ] ].

matches
```

## Writing Files

### Write String to File

```smalltalk
| file |
file := 'output.txt' asFileReference.

"Write (overwrites existing file)"
file writeStreamDo: [ :stream |
    stream
        nextPutAll: 'Hello World!';
        cr;
        nextPutAll: 'This is line 2.' ]
```

Simpler:

```smalltalk
'output.txt' asFileReference writeStream: 'Hello World!'
```

### Append to File

```smalltalk
'log.txt' asFileReference appendStreamDo: [ :stream |
    stream
        nextPutAll: DateAndTime now asString;
        nextPutAll: ': Application started';
        cr ]
```

### Write Collection to File

```smalltalk
| data |
data := OrderedCollection new
    add: 'Line 1';
    add: 'Line 2';
    add: 'Line 3';
    yourself.

'output.txt' asFileReference writeStreamDo: [ :stream |
    data do: [ :line |
        stream nextPutAll: line; cr ] ]
```

### Binary Files

Write binary data:

```smalltalk
| bytes |
bytes := #[65 66 67 68 69].  "ABCDE in ASCII"

'binary.dat' asFileReference binaryWriteStreamDo: [ :stream |
    stream nextPutAll: bytes ]
```

Read binary data:

```smalltalk
'binary.dat' asFileReference binaryReadStreamDo: [ :stream |
    stream contents ]
"-> #[65 66 67 68 69]"
```

## Practical Examples

### CSV File Processing

Read a CSV file:

```smalltalk
Object subclass: #CSVReader
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyApp'
```

```smalltalk
"Class side:"
readFile: filePath
    "Read CSV file and return rows"
    | rows |
    rows := OrderedCollection new.

    filePath asFileReference readStreamDo: [ :stream |
        [ stream atEnd ] whileFalse: [
            | line fields |
            line := stream nextLine.
            fields := line splitOn: $,.
            rows add: fields ] ].

    ^ rows

readFileWithHeaders: filePath
    "Read CSV with first row as headers"
    | headers data |
    data := OrderedCollection new.

    filePath asFileReference readStreamDo: [ :stream |
        stream atEnd ifFalse: [
            headers := stream nextLine splitOn: $, ].

        [ stream atEnd ] whileFalse: [
            | line fields row |
            line := stream nextLine.
            fields := line splitOn: $,.

            row := Dictionary new.
            headers with: fields do: [ :header :value |
                row at: header put: value ].

            data add: row ] ].

    ^ data
```

Usage:

```smalltalk
"Read CSV file"
| data |
data := CSVReader readFileWithHeaders: 'users.csv'.

"data is now a collection of dictionaries"
data first at: 'name'.    "-> 'Alice'"
data first at: 'age'.     "-> '30'"
```

Write CSV:

```smalltalk
"Class side:"
writeFile: filePath data: rows
    "Write data as CSV"
    filePath asFileReference writeStreamDo: [ :stream |
        rows do: [ :row |
            row
                do: [ :field | stream nextPutAll: field asString ]
                separatedBy: [ stream nextPut: $, ].
            stream cr ] ]
```

### JSON File Processing

Read JSON:

```smalltalk
| file data |
file := 'config.json' asFileReference.

data := file readStreamDo: [ :stream |
    STON fromStream: stream ].

"Access data"
data at: 'server'.
data at: 'port'.
```

Write JSON:

```smalltalk
| data |
data := Dictionary new
    at: 'server' put: 'localhost';
    at: 'port' put: 8080;
    at: 'debug' put: true;
    yourself.

'config.json' asFileReference writeStreamDo: [ :stream |
    STON put: data onStream: stream ]
```

### Log File Writer

```smalltalk
Object subclass: #Logger
    instanceVariableNames: 'logFile'
    classVariableNames: ''
    package: 'MyApp'
```

```smalltalk
initialize
    super initialize.
    logFile := 'application.log' asFileReference

log: message
    "Append message to log file"
    logFile appendStreamDo: [ :stream |
        stream
            nextPutAll: DateAndTime now asString;
            nextPutAll: ' - ';
            nextPutAll: message;
            cr ]

info: message
    self log: 'INFO: ', message

error: message
    self log: 'ERROR: ', message

warning: message
    self log: 'WARNING: ', message

clear
    "Clear log file"
    logFile ensureDelete.
    logFile ensureCreateFile
```

Usage:

```smalltalk
| logger |
logger := Logger new.

logger info: 'Application started'.
logger warning: 'Low memory'.
logger error: 'Connection failed'.
```

### Configuration File

```smalltalk
Object subclass: #Config
    instanceVariableNames: 'settings configFile'
    classVariableNames: ''
    package: 'MyApp'
```

```smalltalk
initialize
    super initialize.
    configFile := 'config.ini' asFileReference.
    settings := Dictionary new.
    self load

load
    "Load configuration from file"
    configFile exists ifFalse: [ ^ self ].

    configFile readStreamDo: [ :stream |
        [ stream atEnd ] whileFalse: [
            | line parts key value |
            line := stream nextLine trimBoth.

            "Skip empty lines and comments"
            (line isEmpty or: [ line beginsWith: '#' ]) ifFalse: [
                parts := line splitOn: $=.
                parts size = 2 ifTrue: [
                    key := parts first trimBoth.
                    value := parts second trimBoth.
                    settings at: key put: value ] ] ] ]

save
    "Save configuration to file"
    configFile writeStreamDo: [ :stream |
        settings keysAndValuesDo: [ :key :value |
            stream
                nextPutAll: key;
                nextPut: $=;
                nextPutAll: value asString;
                cr ] ]

at: key
    ^ settings at: key ifAbsent: [ nil ]

at: key put: value
    settings at: key put: value.
    self save
```

Usage:

```smalltalk
| config |
config := Config new.

config at: 'server' put: 'localhost'.
config at: 'port' put: 8080.

"Later..."
config at: 'server'.  "-> 'localhost'"
```

### Text File Analysis

Count word frequencies:

```smalltalk
| file frequencies |
file := 'document.txt' asFileReference.
frequencies := Bag new.

file readStreamDo: [ :stream |
    [ stream atEnd ] whileFalse: [
        | line words |
        line := stream nextLine.
        words := line substrings.  "Split on whitespace"
        words do: [ :word |
            frequencies add: word asLowercase ] ] ].

"Most common words"
frequencies sortedCounts first: 10
```

Find and replace in file:

```smalltalk
| file content newContent |
file := 'document.txt' asFileReference.

"Read file"
content := file contents.

"Replace text"
newContent := content copyReplaceAll: 'old-text' with: 'new-text'.

"Write back"
file writeStream: newContent
```

## Working with Paths

### Path Manipulation

```smalltalk
| path |
path := '/Users/alice/documents/work/report.txt' asFileReference.

path basename.          "-> 'report.txt'"
path basenameWithoutExtension.  "-> 'report'"
path extension.         "-> 'txt'"
path parent.            "-> /Users/alice/documents/work"
path parent basename.   "-> 'work'"
path fullName.          "-> '/Users/alice/documents/work/report.txt'"

"Build paths"
path parent / 'backup' / 'report-backup.txt'.
```

### Temporary Files

```smalltalk
| tempFile |

"Create temporary file"
tempFile := FileLocator temp / 'my-temp-file.txt'.
tempFile writeStream: 'Temporary data'.

"Use it..."

"Clean up"
tempFile ensureDelete
```

Or use automatic cleanup:

```smalltalk
FileLocator temp / 'temp-data.txt' writeStreamDo: [ :stream |
    stream nextPutAll: 'Temporary data'.
    "File automatically cleaned up when done" ]
```

### File Copying and Moving

```smalltalk
| source destination |
source := 'original.txt' asFileReference.
destination := 'copy.txt' asFileReference.

"Copy file"
source copyTo: destination.

"Move/rename file"
source moveTo: destination.

"Copy directory recursively"
'source-dir' asFileReference copyAllTo: 'dest-dir' asFileReference
```

## Buffered I/O

For performance, use buffered streams:

```smalltalk
| file |
file := 'large-file.txt' asFileReference.

"Buffered reading"
file readStreamDo: [ :stream |
    | bufferedStream |
    bufferedStream := (BufferedStream on: stream) next: 8192.

    [ bufferedStream atEnd ] whileFalse: [
        "Process in chunks..."
        | chunk |
        chunk := bufferedStream next: 1024.
        "Process chunk..." ] ]
```

## Error Handling

Always handle file errors:

```smalltalk
[ 'data.txt' asFileReference readStreamDo: [ :stream |
      stream contents ] ]
    on: FileDoesNotExistException
    do: [ :ex |
        Transcript show: 'File not found!'; cr.
        '' ]
```

More comprehensive:

```smalltalk
| file content |
file := 'data.txt' asFileReference.

[
    content := file readStreamDo: [ :stream |
        stream contents ].
]
    on: FileDoesNotExistException
    do: [ :ex |
        Transcript show: 'File not found: ', file fullName; cr.
        content := '' ].

[
    file writeStream: 'New content'.
]
    on: CannotDeleteFileException
    do: [ :ex |
        Transcript show: 'Cannot write file: ', ex messageText; cr ]
```

## Stream Protocols

Common stream methods you'll use:

### Reading

```smalltalk
next                "Read one element"
next: n             "Read n elements"
peek                "Look ahead without consuming"
upTo: delimiter     "Read until delimiter"
upToEnd             "Read everything remaining"
nextLine            "Read until newline"
atEnd               "Are we at the end?"
```

### Writing

```smalltalk
nextPut: element        "Write one element"
nextPutAll: collection  "Write multiple elements"
cr                      "Write newline"
space                   "Write space"
tab                     "Write tab"
print: object           "Write object's printString"
```

### Position

```smalltalk
position            "Current position"
position: n         "Set position"
reset               "Go to start"
skip: n             "Skip n elements"
skipSeparators      "Skip whitespace"
```

## Memory-Mapped Files

For very large files, use memory mapping:

```smalltalk
| file mapped |
file := 'huge-file.dat' asFileReference.
mapped := file binaryReadStream.

"Access data efficiently without loading entire file"
mapped position: 1000000.
mapped next: 1024
```

## Standard Streams

Access standard input/output:

```smalltalk
"Standard output"
Transcript show: 'Hello!'; cr.

"Standard input (in command-line mode)"
Stdio stdin nextLine.

"Standard error"
Stdio stderr nextPutAll: 'Error!'; cr
```

## Try This!

Practice with files and streams:

1. **Build a Line Counter**
   ```smalltalk
   countLines: filePath
       | count |
       count := 0.
       filePath asFileReference readStreamDo: [ :stream |
           [ stream atEnd ] whileFalse: [
               stream nextLine.
               count := count + 1 ] ].
       ^ count
   ```

2. **File Backup Utility**
   ```smalltalk
   backup: filePath
       | file backupFile |
       file := filePath asFileReference.
       backupFile := file parent / (file basenameWithoutExtension, '-backup.', file extension).
       file copyTo: backupFile
   ```

3. **Directory Tree Printer**
   ```smalltalk
   printTree: directory indent: level
       level timesRepeat: [ Transcript space; space ].
       Transcript show: directory basename; cr.

       directory directories do: [ :subdir |
           self printTree: subdir indent: level + 1 ]
   ```

4. **File Search**
   ```smalltalk
   findFiles: pattern in: directory
       "Find all files matching pattern"
       | matches |
       matches := OrderedCollection new.
       directory allFiles do: [ :file |
           (file basename matchesRegex: pattern) ifTrue: [
               matches add: file ] ].
       ^ matches
   ```

5. **Merge Files**
   ```smalltalk
   mergeFiles: fileList into: outputFile
       outputFile asFileReference writeStreamDo: [ :out |
           fileList do: [ :file |
               file asFileReference readStreamDo: [ :in |
                   out nextPutAll: in contents; cr ] ] ]
   ```

6. **File Encryption (Simple)**
   ```smalltalk
   encrypt: filePath key: key
       | file content encrypted |
       file := filePath asFileReference.
       content := file binaryReadStream contents.
       encrypted := content collect: [ :byte | byte bitXor: key ].
       file binaryWriteStream: encrypted
   ```

7. **Build a Simple Database**
   ```smalltalk
   Object subclass: #SimpleDB
       instanceVariableNames: 'file records'

   add: record
       records add: record.
       self save

   save
       file writeStreamDo: [ :stream |
           records do: [ :record |
               STON put: record onStream: stream.
               stream cr ] ]

   load
       records := OrderedCollection new.
       file readStreamDo: [ :stream |
           [ stream atEnd ] whileFalse: [
               records add: (STON fromStream: stream) ] ]
   ```

8. **File Monitor**
   ```smalltalk
   Object subclass: #FileMonitor

   watch: filePath
       | lastModified |
       lastModified := filePath asFileReference modificationTime.

       [ true ] whileTrue: [
           (Delay forSeconds: 1) wait.
           filePath asFileReference modificationTime > lastModified ifTrue: [
               self handleFileChanged: filePath.
               lastModified := filePath asFileReference modificationTime ] ]
   ```

## Best Practices

### Always Use Block Forms

**Good:**
```smalltalk
file readStreamDo: [ :stream |
    stream contents ]
```

Automatically closes the stream!

**Bad:**
```smalltalk
stream := file readStream.
content := stream contents.
stream close.
```

Can leak resources if error occurs!

### Check File Existence

```smalltalk
file exists ifTrue: [
    file readStreamDo: [ :stream |
        "Process file..." ] ]
```

### Handle Large Files Efficiently

Don't:
```smalltalk
content := file contents.  "Loads entire file into memory!"
```

Do:
```smalltalk
file readStreamDo: [ :stream |
    [ stream atEnd ] whileFalse: [
        | chunk |
        chunk := stream next: 1024.
        "Process chunk..." ] ]
```

### Use Appropriate File Formats

- **Text files** - Human-readable, easy to edit
- **STON** - Smalltalk objects, preserves structure
- **JSON** - Interoperability with other systems
- **Binary** - Compact, fast, not human-readable
- **CSV** - Spreadsheet data

## Common Patterns

### Read-Process-Write

```smalltalk
| input output |
input := 'input.txt' asFileReference.
output := 'output.txt' asFileReference.

input readStreamDo: [ :in |
    output writeStreamDo: [ :out |
        [ in atEnd ] whileFalse: [
            | line processed |
            line := in nextLine.
            processed := self process: line.
            out nextPutAll: processed; cr ] ] ]
```

### Accumulate Results

```smalltalk
| results |
results := OrderedCollection new.

file readStreamDo: [ :stream |
    [ stream atEnd ] whileFalse: [
        results add: (self parseLine: stream nextLine) ] ].

^ results
```

### Filter Files

```smalltalk
directory files select: [ :file |
    file extension = 'txt' and: [ file size > 1000 ] ]
```

## What You Learned

Working with files and streams, you've mastered:

1. **Stream Abstraction**
   - Uniform interface for all I/O
   - Sequential data processing
   - Position management

2. **File Operations**
   - Reading and writing files
   - Binary and text modes
   - File metadata

3. **Directory Management**
   - Creating and navigating directories
   - Listing contents
   - Path manipulation

4. **Practical Patterns**
   - CSV/JSON processing
   - Logging
   - Configuration files
   - Text analysis

5. **Error Handling**
   - Graceful failure
   - Resource cleanup
   - Exception handling

6. **Performance**
   - Buffered I/O
   - Streaming large files
   - Memory efficiency

## Streams Are Everywhere

Streams aren't just for files! Use them for:

- **Network sockets** - Read/write network data
- **Strings** - Build and parse strings
- **Collections** - Process sequential data
- **Compression** - Zip/unzip on the fly
- **Encryption** - Encrypt/decrypt streams
- **Parsing** - Parse structured data

The stream protocol unifies all these use cases!

## Looking Ahead

You now understand files and streams - the foundation of data persistence! You've learned:
- Stream abstraction and protocols
- File reading and writing
- Directory operations
- CSV, JSON, and binary formats
- Practical file processing patterns
- Error handling and best practices

In Chapter 35, we'll explore **Graphics and UI Basics** - creating visual applications with Morphic!

Then we move to Part X (Next Steps) with advanced topics like design patterns, performance optimization, and the Smalltalk community!

---

**Key Takeaways:**
- **Streams** provide uniform interface for sequential I/O
- **ReadStream** reads data, **WriteStream** writes data
- **FileReference** represents files and directories
- Use `readStreamDo:` and `writeStreamDo:` for automatic cleanup
- Process large files **line-by-line** or **in chunks**
- Handle **CSV, JSON, binary** formats appropriately
- Always use **error handling** for file operations
- **Path manipulation** with FileReference methods
- **Temporary files** for transient data
- Stream protocols work for **files, strings, collections, networks**
- Build practical utilities: **logging, configuration, data processing**
- **Memory efficiency** with streaming vs loading entire files
- File operations are **object-oriented** in Smalltalk
- Streams **automatically close** when using block forms
- Rich protocols for **reading, writing, positioning**

---

[Previous: Chapter 33 - Project 3: A Simple Web Server](chapter-33-project-web-server.md) | [Next: Chapter 35 - Graphics and UI Basics](chapter-35-graphics-and-ui.md)
