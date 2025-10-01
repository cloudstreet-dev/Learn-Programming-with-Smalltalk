# Chapter 8: Conditionals - Making Choices

Programs need to make decisions: "If the user is logged in, show their profile. Otherwise, show the login page." "If the temperature is below freezing, warn about ice." "If the list is empty, do nothing."

These are **conditionals** - code that chooses what to do based on whether something is true or false.

In most programming languages, conditionals are special syntax built into the language (`if`, `else`, `switch`, etc.). In Smalltalk, they're just **messages sent to boolean objects**.

This is where Smalltalk's "everything is an object" philosophy really shines. Let's explore!

## Boolean Objects: `true` and `false`

Remember from Chapter 5: `true` and `false` aren't keywords or special values - they're objects. Specifically, they're the only instances of the classes `True` and `False`.

```smalltalk
true class   "True"
false class  "False"
```

These boolean objects understand messages that implement conditional logic. When you send a conditional message to a boolean, it decides what to do based on whether it's `true` or `false`.

Mind-blowing, right? Control flow isn't special syntax - it's just objects responding to messages!

## `ifTrue:` - Do Something If True

The simplest conditional message is `ifTrue:`. It takes a block of code and executes it only if the receiver is `true`.

```smalltalk
true ifTrue: [ Transcript show: 'This will run'; cr ]
```

Try this - check your Transcript, and you'll see the message!

Now try:

```smalltalk
false ifTrue: [ Transcript show: 'This will NOT run'; cr ]
```

Nothing happens! The block is never executed because `false` received the `ifTrue:` message and decided not to run the block.

### Real Example

```smalltalk
| age |
age := 25.
(age >= 18) ifTrue: [
    Transcript show: 'You are an adult'; cr ]
```

Let's break this down:

1. `age >= 18` evaluates to `true` or `false` (a boolean object)
2. That boolean receives the `ifTrue:` message
3. If the boolean is `true`, it executes the block
4. If the boolean is `false`, it ignores the block

The parentheses are important! They ensure `age >= 18` is evaluated first, producing a boolean, which then receives the `ifTrue:` message.

## `ifFalse:` - Do Something If False

The opposite of `ifTrue:` is `ifFalse:`:

```smalltalk
false ifFalse: [ Transcript show: 'This WILL run'; cr ]
```

```smalltalk
true ifFalse: [ Transcript show: 'This will NOT run'; cr ]
```

### Real Example

```smalltalk
| temperature |
temperature := -5.
(temperature < 0) ifFalse: [
    Transcript show: 'No freezing warning needed'; cr ]
```

## `ifTrue:ifFalse:` - Choose Between Two Actions

Often you want to do one thing if a condition is true, and something else if it's false. Use `ifTrue:ifFalse:`:

```smalltalk
| age |
age := 15.
(age >= 18)
    ifTrue: [ Transcript show: 'Adult'; cr ]
    ifFalse: [ Transcript show: 'Minor'; cr ]
```

Check your Transcript - it shows "Minor" because the age is less than 18.

### Another Example

```smalltalk
| score grade |
score := 85.
grade := (score >= 60)
    ifTrue: [ 'Pass' ]
    ifFalse: [ 'Fail' ].
grade  "Returns 'Pass'"
```

Notice: the `ifTrue:ifFalse:` message returns the value of whichever block executed! This makes it perfect for assigning conditional values.

## `ifFalse:ifTrue:` - Reversed Order

You can also write `ifFalse:ifTrue:` if it reads better:

```smalltalk
| temperature |
temperature := 25.
(temperature < 0)
    ifFalse: [ Transcript show: 'Warm enough'; cr ]
    ifTrue: [ Transcript show: 'Freezing!'; cr ]
```

Use whichever order makes your code more readable.

## Comparison Operators Produce Booleans

You've been using comparison operators. They all return boolean objects:

```smalltalk
5 > 3    "true"
5 < 3    "false"
5 = 5    "true"
5 ~= 6   "true (not equal)"
5 >= 5   "true (greater than or equal)"
5 <= 6   "true (less than or equal)"
```

These booleans can then receive conditional messages:

```smalltalk
(5 > 3) ifTrue: [ Transcript show: 'Five is greater than three'; cr ]
```

## Logical Operators: `&`, `|`, and `not`

You can combine boolean conditions using logical operators.

### AND: `&`

Both conditions must be true:

```smalltalk
true & true    "true"
true & false   "false"
false & true   "false"
false & false  "false"
```

Real example:

```smalltalk
| age hasLicense |
age := 25.
hasLicense := true.
(age >= 16 & hasLicense)
    ifTrue: [ Transcript show: 'Can drive'; cr ]
    ifFalse: [ Transcript show: 'Cannot drive'; cr ]
```

### OR: `|`

At least one condition must be true:

```smalltalk
true | true    "true"
true | false   "true"
false | true   "true"
false | false  "false"
```

Real example:

```smalltalk
| isWeekend isHoliday |
isWeekend := false.
isHoliday := true.
(isWeekend | isHoliday)
    ifTrue: [ Transcript show: 'Day off!'; cr ]
    ifFalse: [ Transcript show: 'Work day'; cr ]
```

### NOT: `not`

Reverses a boolean:

```smalltalk
true not   "false"
false not  "true"
```

Real example:

```smalltalk
| isRaining |
isRaining := false.
isRaining not
    ifTrue: [ Transcript show: 'Good weather for a walk!'; cr ]
```

## Eager vs Lazy Evaluation

The operators `&` and `|` are **eager** - they evaluate both sides even if they don't need to.

```smalltalk
true | (1 / 0)  "This causes an error even though true | anything is true"
```

For **lazy evaluation** (short-circuit evaluation), use `and:` and `or:`:

```smalltalk
true or: [ 1 / 0 ]  "No error! The second part never evaluates"
```

### `and:` - Lazy AND

```smalltalk
false and: [ Transcript show: 'This never runs'; cr. true ]
"Returns false without executing the block"
```

```smalltalk
true and: [ Transcript show: 'This DOES run'; cr. true ]
"Returns true, block executes"
```

### `or:` - Lazy OR

```smalltalk
true or: [ Transcript show: 'This never runs'; cr. false ]
"Returns true without executing the block"
```

```smalltalk
false or: [ Transcript show: 'This DOES run'; cr. true ]
"Returns true, block executes"
```

### When to Use Which

- Use `&` and `|` for simple boolean combinations: `(x > 5) & (y < 10)`
- Use `and:` and `or:` when the second condition is expensive or might cause errors:

```smalltalk
| collection |
collection := #().
(collection notEmpty) and: [ collection first > 10 ]
```

If `collection` is empty, `collection first` would cause an error. But with `and:`, the second part never executes, so it's safe!

## Nested Conditionals

You can nest conditionals inside each other:

```smalltalk
| score grade |
score := 85.
grade := (score >= 90)
    ifTrue: [ 'A' ]
    ifFalse: [
        (score >= 80)
            ifTrue: [ 'B' ]
            ifFalse: [
                (score >= 70)
                    ifTrue: [ 'C' ]
                    ifFalse: [
                        (score >= 60)
                            ifTrue: [ 'D' ]
                            ifFalse: [ 'F' ] ] ] ].
grade
```

This works, but it's getting ugly! We'll see cleaner ways to handle multiple conditions soon.

## Multiple Conditions with `caseOf:`

For multiple conditions, collections have a `caseOf:` message that's cleaner than nested `ifTrue:ifFalse:`:

```smalltalk
| day message |
day := 'Monday'.
message := day caseOf: {
    [ 'Monday' ] -> [ 'Start of work week' ].
    [ 'Friday' ] -> [ 'Almost weekend!' ].
    [ 'Saturday' ] -> [ 'Weekend!' ].
    [ 'Sunday' ] -> [ 'Weekend!' ]
} otherwise: [ 'Regular work day' ].
message
```

Actually, let me correct that. The more common pattern in Smalltalk is using a Dictionary or a series of tests. Let me show you a better approach:

```smalltalk
| score grade |
score := 85.
grade := (score >= 90) ifTrue: [ 'A' ] ifFalse: [
    (score >= 80) ifTrue: [ 'B' ] ifFalse: [
        (score >= 70) ifTrue: [ 'C' ] ifFalse: [
            (score >= 60) ifTrue: [ 'D' ] ifFalse: [ 'F' ]
        ]
    ]
].
grade
```

Or more elegantly, use `cond` from some libraries, or simply extract it into a method (which we'll learn in Chapter 12).

## Pattern: Guard Clauses

A common pattern is to check for special cases first and handle them immediately:

```smalltalk
| numbers average |
numbers := #(10 20 30 40 50).

"Guard clause - handle empty case"
numbers isEmpty ifTrue: [ 'No numbers to average' ] ifFalse: [
    average := numbers sum / numbers size.
    average ]
```

This is cleaner than deeply nested conditionals.

## Returning Values from Conditionals

Remember: conditionals are messages that return values!

```smalltalk
| age status |
age := 25.
status := (age >= 18) ifTrue: [ 'adult' ] ifFalse: [ 'minor' ].
status  "Returns 'adult'"
```

The entire `ifTrue:ifFalse:` expression evaluates to the result of whichever block executed.

This means you can use conditionals in larger expressions:

```smalltalk
| price discount finalPrice |
price := 100.
discount := (price > 50) ifTrue: [ 0.10 ] ifFalse: [ 0 ].
finalPrice := price * (1 - discount).
finalPrice  "Returns 90.0"
```

## Conditionals with Side Effects

Blocks can contain multiple statements:

```smalltalk
| age |
age := 30.
(age >= 18) ifTrue: [
    Transcript show: 'You are an adult'; cr.
    Transcript show: 'You can vote'; cr.
    Transcript show: 'You have full legal rights'; cr ]
```

All three lines execute if the condition is true.

## Implicit Returns

In Smalltalk, blocks return the value of their last expression:

```smalltalk
| x result |
x := 10.
result := (x > 5) ifTrue: [
    Transcript show: 'X is big'; cr.
    'big' ] ifFalse: [
    Transcript show: 'X is small'; cr.
    'small' ].
result  "Returns 'big'"
```

The `'big'` and `'small'` strings are the return values of their respective blocks.

## Testing Multiple Conditions

Let's look at more complex condition combinations:

```smalltalk
| age income hasGoodCredit approved |
age := 30.
income := 50000.
hasGoodCredit := true.

approved := (age >= 25) and: [
    (income >= 40000) and: [ hasGoodCredit ] ].
approved  "Returns true"
```

All three conditions must be true for approval.

```smalltalk
| isWeekend isHoliday isVacation dayOff |
isWeekend := false.
isHoliday := true.
isVacation := false.

dayOff := isWeekend or: [ isHoliday or: [ isVacation ] ].
dayOff  "Returns true"
```

Any one condition being true gives a day off.

## The `ifNil:` and `ifNotNil:` Messages

Objects (not just booleans) understand conditional messages for testing `nil`:

```smalltalk
| x |
x := nil.
x ifNil: [ Transcript show: 'x is nil'; cr ]
```

```smalltalk
| x |
x := 42.
x ifNotNil: [ Transcript show: 'x is not nil'; cr ]
```

Combined:

```smalltalk
| x |
x := 42.
x ifNil: [ Transcript show: 'No value'; cr ]
  ifNotNil: [ Transcript show: 'Value is: ' , x printString; cr ]
```

### With Block Arguments

`ifNotNil:` can pass the value to the block:

```smalltalk
| x |
x := 42.
x ifNotNil: [ :value |
    Transcript show: 'The value is: ' , value printString; cr ]
```

This is handy when the expression is complex:

```smalltalk
someComplexExpression ifNotNil: [ :result |
    "Use result without recomputing the expression" ]
```

## The `ifEmpty:` and `ifNotEmpty:` Messages

Collections understand conditional messages for testing emptiness:

```smalltalk
#() ifEmpty: [ Transcript show: 'Empty array'; cr ]
```

```smalltalk
#(1 2 3) ifNotEmpty: [ Transcript show: 'Has elements'; cr ]
```

```smalltalk
| list |
list := OrderedCollection new.
list ifEmpty: [ Transcript show: 'No items yet'; cr ]
```

Combined:

```smalltalk
| list |
list := #(1 2 3).
list
    ifEmpty: [ Transcript show: 'Nothing to process'; cr ]
    ifNotEmpty: [ Transcript show: 'Processing ' , list size printString , ' items'; cr ]
```

## Practical Examples

### Example 1: Grade Calculator

```smalltalk
| score grade |
score := 87.

grade := (score >= 90) ifTrue: [ 'A' ] ifFalse: [
    (score >= 80) ifTrue: [ 'B' ] ifFalse: [
        (score >= 70) ifTrue: [ 'C' ] ifFalse: [
            (score >= 60) ifTrue: [ 'D' ] ifFalse: [ 'F' ]
        ]
    ]
].

Transcript show: 'Grade: ' , grade; cr.
```

### Example 2: Leap Year

```smalltalk
| year isLeapYear |
year := 2024.

isLeapYear := (year \\ 4 = 0) and: [
    (year \\ 100 ~= 0) or: [ year \\ 400 = 0 ] ].

isLeapYear
    ifTrue: [ Transcript show: year printString , ' is a leap year'; cr ]
    ifFalse: [ Transcript show: year printString , ' is not a leap year'; cr ]
```

A year is a leap year if:
- It's divisible by 4, AND
- Either it's not divisible by 100, OR it's divisible by 400

### Example 3: Discount Calculator

```smalltalk
| price quantity discount total |
price := 10.
quantity := 15.

discount := (quantity >= 20) ifTrue: [ 0.20 ]
    ifFalse: [ (quantity >= 10) ifTrue: [ 0.10 ]
    ifFalse: [ 0 ] ].

total := price * quantity * (1 - discount).

Transcript show: 'Subtotal: ' , (price * quantity) printString; cr.
Transcript show: 'Discount: ' , (discount * 100) printString , '%'; cr.
Transcript show: 'Total: ' , total printString; cr.
```

### Example 4: Input Validation

```smalltalk
| email valid |
email := 'user@example.com'.

valid := (email notNil) and: [
    (email notEmpty) and: [
        (email includesSubstring: '@') and: [
            (email includesSubstring: '.') ] ] ].

valid
    ifTrue: [ Transcript show: 'Email looks valid'; cr ]
    ifFalse: [ Transcript show: 'Invalid email'; cr ]
```

### Example 5: Safety Check

```smalltalk
| collection firstElement |
collection := #(10 20 30).

firstElement := collection
    ifEmpty: [ nil ]
    ifNotEmpty: [ collection first ].

firstElement ifNotNil: [ :value |
    Transcript show: 'First element is: ' , value printString; cr ]
```

This safely handles empty collections.

## Common Patterns

### Early Return Pattern (Using Guard Clauses)

```smalltalk
| numbers |
numbers := #().
numbers isEmpty ifTrue: [ 'No numbers' ] ifFalse: [
    "Process numbers here"
    numbers sum ]
```

### Default Value Pattern

```smalltalk
| config timeout |
config := Dictionary new.
timeout := config at: 'timeout' ifAbsent: [ 30 ].
timeout  "Returns 30 if 'timeout' key doesn't exist"
```

### Validation Pattern

```smalltalk
| username password valid |
username := 'alice'.
password := 'secret123'.

valid := (username notEmpty) and: [
    (password notEmpty) and: [
        password size >= 8 ] ].

valid ifTrue: [ 'Login successful' ] ifFalse: [ 'Invalid credentials' ]
```

### Toggle Pattern

```smalltalk
| flag |
flag := true.
flag := flag not.  "Toggle the flag"
flag  "Returns false"
```

## How It Really Works

Let's peek under the hood. When you write:

```smalltalk
true ifTrue: [ Transcript show: 'Hello' ]
```

The object `true` (an instance of class `True`) receives the message `ifTrue:` with a block as an argument. The `True` class has a method that looks something like:

```smalltalk
ifTrue: aBlock
    "Execute the block if receiver is true"
    ^ aBlock value
```

It executes the block!

Meanwhile, `false` (an instance of class `False`) has a different implementation:

```smalltalk
ifTrue: aBlock
    "Do nothing if receiver is false"
    ^ nil
```

It ignores the block!

This is polymorphism in action: the same message (`ifTrue:`) sent to different objects (`true` vs `false`) produces different behavior.

You can actually browse these methods:
1. Open the Spotter (`Shift+Enter`)
2. Type "True"
3. Browse to the `ifTrue:` method
4. See the actual implementation!

This transparency is one of Smalltalk's teaching superpowers.

## Try This!

Practice with conditionals:

1. **Simple conditional:**
   ```smalltalk
   | temperature |
   temperature := 25.
   (temperature > 20)
       ifTrue: [ Transcript show: 'Warm day'; cr ]
       ifFalse: [ Transcript show: 'Cool day'; cr ]
   ```

2. **Logical operators:**
   ```smalltalk
   | x |
   x := 15.
   ((x > 10) & (x < 20))
       ifTrue: [ Transcript show: 'x is between 10 and 20'; cr ]
   ```

3. **Grade calculator:**
   ```smalltalk
   | scores grades |
   scores := #(95 78 82 91 67).
   grades := scores collect: [ :score |
       (score >= 90) ifTrue: [ 'A' ] ifFalse: [
           (score >= 80) ifTrue: [ 'B' ] ifFalse: [
               (score >= 70) ifTrue: [ 'C' ] ifFalse: [
                   (score >= 60) ifTrue: [ 'D' ] ifFalse: [ 'F' ] ] ] ] ].
   grades
   ```

4. **Nil handling:**
   ```smalltalk
   | value result |
   value := nil.
   result := value ifNil: [ 'No value' ] ifNotNil: [ value printString ].
   result
   ```

5. **Collection conditional:**
   ```smalltalk
   | numbers |
   numbers := #(1 2 3 4 5).
   numbers
       ifEmpty: [ 'No numbers!' ]
       ifNotEmpty: [ 'Sum: ' , numbers sum printString ]
   ```

6. **Even/odd checker:**
   ```smalltalk
   (1 to: 10) do: [ :n |
       n even
           ifTrue: [ Transcript show: n printString , ' is even'; cr ]
           ifFalse: [ Transcript show: n printString , ' is odd'; cr ] ]
   ```

7. **Lazy evaluation:**
   ```smalltalk
   | list |
   list := #().
   (list notEmpty) and: [ list first > 10 ]
   ```
   Why doesn't this cause an error?

8. **Complex validation:**
   ```smalltalk
   | password valid |
   password := 'MyP@ssw0rd'.
   valid := (password size >= 8) and: [
       (password anySatisfy: [ :char | char isDigit ]) and: [
           password anySatisfy: [ :char | char isLetter ] ] ].
   valid
       ifTrue: [ 'Strong password' ]
       ifFalse: [ 'Weak password' ]
   ```

## Common Mistakes

### Forgetting Parentheses

```smalltalk
age >= 18 ifTrue: [ 'adult' ]  "Wrong! Operator precedence issue"
```

The comparison happens first (correct), but then `18` receives the `ifTrue:` message (wrong!).

Correct:

```smalltalk
(age >= 18) ifTrue: [ 'adult' ]
```

### Using Assignment Instead of Comparison

```smalltalk
(x := 5) ifTrue: [ 'yes' ]  "Wrong! := is assignment, not comparison"
```

Correct:

```smalltalk
(x = 5) ifTrue: [ 'yes' ]  "= is comparison"
```

### Not Understanding Block Return Values

```smalltalk
| result |
result := true ifTrue: [
    Transcript show: 'Hello'; cr ].
result  "What is this?"
```

The block returns the result of its last expression. `cr` returns a character, so that's what `result` becomes!

If you want the block to return something specific:

```smalltalk
| result |
result := true ifTrue: [
    Transcript show: 'Hello'; cr.
    'success' ].  "Explicit return value"
result  "Returns 'success'"
```

## Coming Up Next

You now understand how Smalltalk implements conditionals as messages to boolean objects. This is elegant, consistent, and demonstrates the power of object-oriented design.

But we've been using these square brackets `[ ]` without fully explaining them. What ARE blocks, really? How do they work? Why are they so powerful?

In Chapter 9, we'll dive deep into **Blocks** - one of Smalltalk's most important and powerful features. Blocks are objects that contain code, and you can pass them around, store them, and execute them whenever you want.

Blocks are what make Smalltalk's conditionals and loops possible, and they're also used for many other purposes. Understanding blocks will unlock a new level of Smalltalk mastery.

Let's explore!

---

**Key Takeaways:**
- Conditionals in Smalltalk are **messages sent to boolean objects**
- `true` and `false` are objects, not keywords
- Use `ifTrue:`, `ifFalse:`, and `ifTrue:ifFalse:` for conditional logic
- Logical operators: `&` and `|` (eager), `and:` and `or:` (lazy)
- Use `not` to negate a boolean
- Objects understand `ifNil:`, `ifNotNil:`, `ifEmpty:`, `ifNotEmpty:`
- Conditionals return values - the result of the executed block
- Always use parentheses around comparisons: `(x > 5) ifTrue: [...]`
- Control flow is implemented through polymorphism, not special syntax
- This approach is elegant, consistent, and discoverable

---

[Previous: Chapter 7 - Variables](chapter-07-variables.md) | [Next: Chapter 9 - Blocks](chapter-09-blocks.md)
