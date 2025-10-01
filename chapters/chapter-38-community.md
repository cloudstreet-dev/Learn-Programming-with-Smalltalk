# Chapter 38: The Smalltalk Community

You've mastered Smalltalk fundamentals, built real applications, learned design patterns, and optimized code. Now let's explore the most important resource: **the Smalltalk community** - the people who keep Smalltalk alive, innovative, and welcoming!

Smalltalk has one of the friendliest, most helpful programming communities. Whether you're stuck on a problem, want to contribute, or just chat about elegant code, the Smalltalk community is there for you. This chapter shows you how to connect, contribute, and become part of this vibrant ecosystem!

## Why Community Matters

Programming isn't solitary - it's collaborative! The community provides:

1. **Help and support** - When you're stuck
2. **Learning resources** - Books, tutorials, examples
3. **Libraries and tools** - Thousands of packages
4. **Inspiration** - See what others are building
5. **Collaboration** - Work on projects together
6. **Jobs and opportunities** - Professional connections
7. **Friendship** - Meet fellow enthusiasts

**You're not alone!**

## The Smalltalk Spirit

The Smalltalk community has a unique culture:

### Values

- **Openness** - Share knowledge freely
- **Excellence** - Pursue elegant solutions
- **Curiosity** - Explore and experiment
- **Respect** - Welcome all skill levels
- **Collaboration** - Build together
- **Fun** - Programming should be joyful!

### History

Smalltalk people remember their roots:
- **Xerox PARC** in the 1970s - Where it all began
- **Alan Kay's vision** - Personal computing for everyone
- **Dan Ingalls' implementation** - Technical brilliance
- **Adele Goldberg's leadership** - Community building

This heritage shapes the community today!

## Where to Find Smalltalkers

### Discord Servers

**Pharo Discord** - Most active Smalltalk community
- **URL**: https://discord.gg/QewZMZa
- **Activity**: Very high - responses within hours
- **Topics**: Pharo help, general Smalltalk, projects
- **Channels**: #beginners, #pharo-help, #show-and-tell, #jobs

**Squeak/Etoys Discord**
- Focus on Squeak and educational uses
- Friendly for newcomers

**Join Discord** - Best place for real-time help!

### Mailing Lists

**Pharo Users**
- Email: pharo-users@lists.pharo.org
- Subscribe: https://lists.pharo.org/mailman/listinfo/pharo-users
- Archives: Searchable history
- Traffic: Moderate - 5-10 posts/day

**Pharo Dev**
- Email: pharo-dev@lists.pharo.org
- For Pharo core development
- Technical discussions

**Squeak Mailing Lists**
- squeak-dev: Development
- beginners: For newcomers

**Cuis List**
- For Cuis Smalltalk users
- Small but active

### Forums

**Reddit**
- r/smalltalk - General Smalltalk discussion
- r/pharo - Pharo-specific
- Active, friendly community

**Stack Overflow**
- Tag: [smalltalk]
- Tag: [pharo]
- Good for Q&A format

### IRC (Still Active!)

**Libera.Chat**
- #pharo - Pharo discussion
- #squeak - Squeak discussion
- Old-school but responsive

### Social Media

**Twitter/X**
- #Pharo hashtag
- #Smalltalk hashtag
- Follow: @pharoproject, @SqueakOrg

**Mastodon**
- fosstodon.org - Many Smalltalkers here

**LinkedIn**
- Pharo group
- Smalltalk professionals

## Getting Help

### Asking Good Questions

When seeking help:

1. **Be specific**
   - ❌ "My code doesn't work"
   - ✅ "I get 'MessageNotUnderstood: #size' when I call myMethod"

2. **Show code**
   ```smalltalk
   "What I'm trying:"
   | collection |
   collection := OrderedCollection new.
   collection size.  "This works"
   collection add: 42.
   collection size.  "This fails - why?"
   ```

3. **Describe what you tried**
   - "I tried X, but got error Y"
   - "I expected Z to happen"

4. **Include environment**
   - "Pharo 11 on macOS"
   - "Using Spec 2"

5. **Be patient and polite**
   - People volunteer their time
   - Say thank you!

### Example Good Question

> **Subject:** How to sort Dictionary by values in Pharo 11?
>
> Hi! I'm trying to sort a Dictionary by its values (not keys). I have:
>
> ```smalltalk
> dict := Dictionary new.
> dict at: 'apple' put: 5.
> dict at: 'banana' put: 2.
> dict at: 'cherry' put: 8.
> ```
>
> I want: #('cherry' 'apple' 'banana') (sorted by values descending)
>
> I tried `dict sorted` but it doesn't work. I'm using Pharo 11 on Linux.
>
> Thanks for any help!

**This will get quick, helpful responses!**

## Contributing Back

Once you're comfortable, give back to the community!

### Answer Questions

**Where:**
- Discord #pharo-help
- Stack Overflow
- Reddit

**How:**
- Watch for questions you can answer
- Test your answer before posting
- Be encouraging to beginners

### Share Your Projects

**Show what you built!**

**Discord #show-and-tell:**
```
Just finished my Sudoku solver in Pharo! 🎉
Uses backtracking algorithm.
GitHub: https://github.com/username/pharo-sudoku
```

**Blog about it:**
- Write tutorials
- Share lessons learned
- Document your journey

### Report Bugs

Found a bug? Report it!

**Pharo bugs:**
- https://github.com/pharo-project/pharo/issues
- Provide: Steps to reproduce, expected vs actual behavior
- Include Pharo version

**Example report:**
```
Title: Inspector crashes when inspecting huge array

Steps:
1. Create array: Array new: 10000000
2. Inspect it
3. Try to scroll

Expected: Inspector shows array
Actual: Image freezes

Pharo: 11.0 build 742
OS: macOS 13.2
```

### Write Documentation

**Help others learn!**

- Improve class comments
- Write tutorials
- Create examples
- Fix typos in documentation

**Contribute to Pharo:**
```bash
# Fork pharo-project/pharo on GitHub
# Fix documentation
# Submit pull request
```

### Create Libraries

**Built something reusable?**

Package it and share:

```smalltalk
"Create a baseline:"
BaselineOf subclass: #BaselineOfMyLibrary
```

**Publish to GitHub:**
```bash
git init
git add .
git commit -m "Initial commit"
git push origin main
```

**Announce it:**
- Discord #show-and-tell
- Pharo mailing list
- Reddit r/pharo

### Contribute to Core

**Advanced: Contribute to Pharo itself!**

1. **Read contributing guide**: https://github.com/pharo-project/pharo/blob/Pharo11/CONTRIBUTING.md
2. **Find an issue**: Look for "good first issue" label
3. **Fork and clone**: Work on your fork
4. **Make changes**: Fix bug or add feature
5. **Test**: Ensure tests pass
6. **Pull request**: Submit for review

**Small fixes welcome!**

## Learning Resources

### Official Documentation

**Pharo Books (Free!):**
- **Pharo by Example** - Best introduction
  - https://books.pharo.org/
- **Deep into Pharo** - Advanced topics
- **Enterprise Pharo** - Web development
- **Numerical Methods in Pharo** - Scientific computing

**Download all books** - They're excellent!

### MOOCs (Online Courses)

**Pharo MOOC:**
- https://mooc.pharo.org/
- Free video lectures
- Exercises and quizzes
- 7 weeks of content
- Perfect for beginners!

### YouTube Channels

**Pharo Project**
- Official tutorials
- Conference talks
- Feature demonstrations

**Community Members**
- Various Smalltalk tutorials
- Project walkthroughs
- Tips and tricks

### Blogs

**Active Smalltalk blogs:**
- https://pharoweekly.wordpress.com/ - Weekly news
- https://astares.blogspot.com/ - Astares' Smalltalk blog
- Individual developer blogs

**Planet Smalltalk:**
- Aggregates Smalltalk blogs
- One-stop reading

### Podcasts

Search for:
- Smalltalk interviews
- Pharo discussions
- Alan Kay talks (not Smalltalk-specific but inspiring)

## Events and Conferences

### ESUG (European Smalltalk User Group)

**Annual conference:**
- Usually August/September
- Europe (location varies)
- Talks, workshops, coding
- Very welcoming!

**Website:** https://esug.github.io/

### Smalltalks Conference

**South American Smalltalk conference:**
- Annual event
- Spanish/Portuguese focus
- Growing community

### Local Meetups

**Find meetups:**
- Check Meetup.com
- Discord #events channel
- Ask on mailing lists

**Or start one yourself!**

### Virtual Events

**Online Pharo Days:**
- Periodic online conferences
- Free to attend
- Recorded for later viewing

**Twitch/YouTube Streams:**
- Live coding sessions
- Q&A with experts

## Companies Using Smalltalk

**Job opportunities exist!**

### Industries

- **Financial services** - Trading systems, risk analysis
- **Healthcare** - Medical records, imaging
- **Telecommunications** - Network management
- **Manufacturing** - Factory automation
- **Research** - Scientific computing
- **Education** - Learning platforms

### Notable Companies

- **JPMorgan Chase** - Financial systems
- **UBS** - Banking applications
- **Desjardins** - Insurance
- **4D** - Database tools (built with Pharo!)
- **Various startups** - Web applications

### Finding Jobs

**Where to look:**
- Discord #jobs channel
- Pharo mailing list (occasional postings)
- LinkedIn (search "Smalltalk" or "Pharo")
- Company websites directly
- Freelance platforms

**Skills in demand:**
- Seaside web development
- Legacy system maintenance
- Pharo application development
- VA Smalltalk (insurance companies)

## Notable Community Members

### Core Contributors

**Pharo:**
- Stéphane Ducasse - Project leader, professor
- Esteban Lorenzano - VM expert
- Marcus Denker - Reflectivity expert
- Guille Polito - Tools developer
- Many more active contributors!

**Squeak:**
- Vanessa Freudenberg - Squeak/Croquet
- Tim Rowledge - VM development
- Craig Latta - Various projects

**Glamorous Toolkit:**
- Tudor Gîrba - Creator, moldable development

### You Can Meet Them!

The Smalltalk community is **accessible**:
- Ask questions on Discord
- Attend conferences
- Collaborate on GitHub
- Join video calls

**No rockstars - just helpful people!**

## Giving Back: Ideas

Start small, grow from there!

### Beginner Level

- Answer questions on Discord
- Fix typos in documentation
- Share your learning journey (blog)
- Star projects on GitHub
- Thank people who help you

### Intermediate Level

- Write tutorials
- Create example projects
- Package a library
- Contribute bug fixes
- Present at local meetup

### Advanced Level

- Core contributions
- Library maintenance
- Organize events
- Mentor newcomers
- Speak at conferences

## The Pharo Consortium

**Support Pharo's development!**

**Pharo Consortium:**
- Industrial consortium
- Funds Pharo development
- Member companies
- https://consortium.pharo.org/

**Individual supporters:**
- Donations welcome
- Support continued development

## Community Projects

### Awesome Pharo

**Curated list of libraries:**
- https://github.com/pharo-open-documentation/awesome-pharo
- Categorized packages
- Updated regularly

### PharoThings

**Internet of Things with Pharo:**
- Run Pharo on Raspberry Pi
- Control hardware
- https://github.com/pharo-iot/PharoThings

### PolyMath

**Scientific computing:**
- Numerical methods
- Statistics
- Machine learning
- https://github.com/PolyMathOrg/PolyMath

### Roassal

**Data visualization:**
- Charts and graphs
- Network diagrams
- Interactive visualizations
- https://github.com/ObjectProfile/Roassal3

### Glamorous Toolkit

**Moldable development:**
- https://gtoolkit.com/
- Active development
- Innovation in tooling

## Etiquette and Culture

### Do's

✅ **Be respectful** - Everyone's learning
✅ **Be patient** - Volunteers help when they can
✅ **Search first** - Question might be answered already
✅ **Share knowledge** - Help others when you can
✅ **Give credit** - Acknowledge helpers
✅ **Stay positive** - Encourage, don't criticize
✅ **Have fun** - Enjoy programming!

### Don'ts

❌ **Don't demand** - People volunteer
❌ **Don't ask to ask** - Just ask your question
❌ **Don't cross-post** - Pick one venue
❌ **Don't hijack threads** - Start new topic
❌ **Don't disparage** - Respect all languages
❌ **Don't give up** - Community wants to help!

## Your Community Journey

### Week 1: Observer

- Join Discord
- Read recent conversations
- Watch interactions
- Learn the culture

### Week 2-4: Lurker

- Follow discussions
- Read documentation
- Try suggested solutions
- Build understanding

### Month 2-3: Participant

- Ask your first question
- Thank helpers
- Try answering easy questions
- Share what you're building

### Month 4+: Contributor

- Answer questions regularly
- Share code/libraries
- Report bugs
- Contribute fixes
- Help newcomers

### Year 1+: Community Member

- Known in community
- Regular contributor
- Mentor others
- Possibly present at events

**Everyone follows this path!**

## Success Stories

### From Beginners to Contributors

Many started knowing nothing:
- Asked basic questions
- Built simple projects
- Shared their work
- Now core contributors!

**You can too!**

### Projects That Started Small

- Someone's weekend project → popular library
- "Just learning" code → production system
- Hobby experiment → startup company

**Your project could be next!**

## Staying Connected

### Regular Check-ins

**Daily:**
- Check Discord
- Read new threads

**Weekly:**
- Read Pharo Weekly blog
- Check GitHub activity
- Try new packages

**Monthly:**
- Review learning progress
- Contribute something
- Update your projects

### Building Your Network

- Connect on LinkedIn
- Follow on Twitter/Mastodon
- Attend virtual meetups
- Collaborate on projects
- Share your work

**Programming is social!**

## Try This!

Get involved today:

1. **Join Discord**
   - https://discord.gg/QewZMZa
   - Introduce yourself in #introductions
   - Tell us what you're learning!

2. **Ask a Question**
   - Something you're stuck on
   - See how helpful the community is!

3. **Star Some Projects**
   - https://github.com/pharo-project/pharo
   - https://github.com/ObjectProfile/Roassal3
   - Show your appreciation!

4. **Read Pharo by Example**
   - If you haven't already
   - Best learning resource

5. **Share This Book**
   - Tell others about it
   - Help grow the community

6. **Build Something**
   - Start a project
   - Share it when ready

7. **Say Thanks**
   - To someone who helped you
   - Good karma!

## What You Learned

Exploring the community, you've discovered:

1. **Where Smalltalkers Gather**
   - Discord (best for real-time)
   - Mailing lists (traditional)
   - Reddit, Stack Overflow
   - Conferences and meetups

2. **How to Get Help**
   - Ask good questions
   - Be specific
   - Show your work
   - Be patient and polite

3. **Ways to Contribute**
   - Answer questions
   - Share projects
   - Report bugs
   - Write documentation
   - Create libraries

4. **Learning Resources**
   - Free books
   - MOOCs
   - YouTube tutorials
   - Blogs and podcasts

5. **Professional Opportunities**
   - Companies using Smalltalk
   - Job boards
   - Freelance work

6. **Community Values**
   - Openness and respect
   - Excellence and curiosity
   - Collaboration and fun

## The Heart of Smalltalk

The community **is** Smalltalk:
- Without people, it's just code
- With people, it's a movement
- **You make it vibrant!**

Smalltalk survived decades because:
- Dedicated community
- Mutual support
- Shared values
- Love of elegant code

**You're part of this now!**

## Looking Ahead

You now understand the Smalltalk community! You know:
- Where to find Smalltalkers
- How to get help and give back
- Learning resources available
- How to contribute
- Community culture and values

In Chapter 39, we'll explore **Beyond Smalltalk** - taking your skills to other languages and paradigms!

Then Chapter 40 concludes with **Your Smalltalk Journey** - where to go next!

You're almost at the finish line!

---

**Key Takeaways:**
- **Community is vital** - You're not alone
- **Discord is most active** - Join for real-time help
- **Mailing lists** still important for deeper discussions
- **Ask good questions** - Be specific, show code, describe attempts
- **Give back** - Answer questions, share projects, contribute
- **Free learning resources** - Books, MOOCs, tutorials all available
- **Conferences exist** - ESUG, Smalltalks, virtual events
- **Jobs available** - Financial, healthcare, telecom sectors
- **Be respectful and patient** - Volunteers help when they can
- **Start small** - Observer → Participant → Contributor
- **Pharo Consortium** supports development
- **Community projects** - PharoThings, PolyMath, Roassal
- **You can meet core contributors** - Accessible, friendly people
- **Success stories abound** - Beginners became experts
- **Your contribution matters** - Every bit helps!

---

[Previous: Chapter 37 - Performance and Optimization](chapter-37-performance.md) | [Next: Chapter 39 - Beyond Smalltalk - Taking Your Skills Further](chapter-39-beyond-smalltalk.md)
