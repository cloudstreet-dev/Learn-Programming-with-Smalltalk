# Appendix A: Installation Guide Details

This appendix provides detailed installation instructions for all major Smalltalk implementations on all major platforms. Use this as a comprehensive reference when setting up your Smalltalk environment.

## Pharo Installation

### Windows

**Method 1: PharoLauncher (Recommended)**

1. Visit https://pharo.org/download
2. Download "PharoLauncher for Windows"
3. Extract the ZIP file
4. Run `PharoLauncher.exe`
5. In the launcher:
   - Click "New" or the "+" button
   - Select "Official distributions" → "Pharo 11.0 - 64bit (stable)"
   - Name your image (e.g., "My Project")
   - Click "Create image"
6. Double-click the image to launch

**Method 2: Direct Download**

1. Visit https://pharo.org/download
2. Download "Pharo 11 - 64bit (stable)"
3. Extract the ZIP file
4. Run `Pharo.exe`

**Troubleshooting:**
- **Windows Defender warning**: Click "More info" → "Run anyway"
- **Missing DLLs**: Install Visual C++ Redistributable from Microsoft
- **Won't start**: Run as Administrator once

### macOS

**Method 1: PharoLauncher (Recommended)**

1. Visit https://pharo.org/download
2. Download "PharoLauncher for macOS"
3. Open the DMG file
4. Drag PharoLauncher to Applications
5. Open PharoLauncher from Applications
6. First launch: Right-click → "Open" (to bypass Gatekeeper)
7. Create and launch images as described in Windows section

**Method 2: Direct Download**

1. Visit https://pharo.org/download
2. Download "Pharo 11 - 64bit (stable) for macOS"
3. Open the DMG file
4. Drag Pharo to Applications
5. Right-click Pharo → "Open"

**Troubleshooting:**
- **"Cannot open" error**: Right-click → "Open" (don't double-click)
- **Gatekeeper issues**: System Preferences → Security → "Open Anyway"
- **Crashes on Apple Silicon**: Use native ARM64 version or Rosetta

### Linux

**Method 1: PharoLauncher (Recommended)**

1. Visit https://pharo.org/download
2. Download "PharoLauncher for Linux"
3. Extract: `tar xzf PharoLauncher-linux.tar.gz`
4. Run: `cd pharo-launcher && ./pharo-launcher`
5. Create images as described above

**Method 2: ZeroConf Script**

```bash
# Download and run
curl https://get.pharo.org/64/ | bash

# Or with wget
wget -O- https://get.pharo.org/64/ | bash

# Launch
./pharo-ui Pharo.image
```

**Method 3: Package Manager (Ubuntu/Debian)**

```bash
# Add PPA (if available)
sudo add-apt-repository ppa:pharo/stable
sudo apt-get update
sudo apt-get install pharo-launcher

# Or build from source
git clone https://github.com/pharo-project/pharo-launcher.git
cd pharo-launcher
./build.sh
```

**Troubleshooting:**
- **Missing libraries**: `sudo apt-get install libssl-dev libssh2-1-dev`
- **32-bit on 64-bit**: `sudo dpkg --add-architecture i386`
- **Display issues**: Try different window managers
- **Segmentation fault**: Update graphics drivers

## Squeak Installation

### Windows

1. Visit https://squeak.org/downloads/
2. Download "All-in-One" for Windows
3. Extract the ZIP file
4. Run `Squeak.exe`

**Optional - Install separately:**
1. Download VM and Image separately
2. Extract both
3. Drag image onto VM executable

### macOS

1. Visit https://squeak.org/downloads/
2. Download "All-in-One" for macOS
3. Open DMG and drag to Applications
4. Right-click → "Open" first time
5. Run Squeak

### Linux

```bash
# Ubuntu/Debian
sudo apt-get install squeak-vm squeak-image

# Or download from squeak.org
wget http://files.squeak.org/6.0/Squeak6.0-22104-64bit-All-in-One.zip
unzip Squeak6.0*.zip
cd Squeak6.0*
./squeak Squeak6.0*.image
```

## Glamorous Toolkit Installation

### Windows

1. Visit https://gtoolkit.com/download/
2. Download "GT for Windows"
3. Extract ZIP file
4. Run `GlamorousToolkit.exe`
5. Wait for initial load (first time takes a moment)

### macOS

1. Visit https://gtoolkit.com/download/
2. Download "GT for macOS"
3. Open DMG file
4. Drag GlamorousToolkit to Applications
5. Right-click → "Open" first time
6. Launch GT

**Apple Silicon:**
- Native ARM64 version available
- Or use Intel version with Rosetta

### Linux

```bash
# Download
wget https://dl.feenk.com/gt/GlamorousToolkit-x86_64-v*-linux.zip
unzip GlamorousToolkit*.zip
cd GlamorousToolkit*

# Run
./bin/GlamorousToolkit
```

## Cuis Smalltalk Installation

### All Platforms

1. Visit https://cuis.st/
2. Download latest release (e.g., Cuis6.0-5033.zip)
3. Extract the archive
4. You need a compatible VM:

**Windows:**
```
Download Cog VM from https://opensmalltalk.org/
Place in Cuis directory
Run: CogVM.exe Cuis6.0-5033.image
```

**macOS:**
```bash
# Download and place Cog VM
# Run:
./Squeak.app/Contents/MacOS/Squeak Cuis6.0-5033.image
```

**Linux:**
```bash
# Install VM
sudo apt-get install squeak-vm

# Run
squeak Cuis6.0-5033.image
```

## GNU Smalltalk Installation

### Ubuntu/Debian

```bash
sudo apt-get update
sudo apt-get install gnu-smalltalk
```

### macOS

```bash
# Using Homebrew
brew install gnu-smalltalk

# Or MacPorts
sudo port install gst
```

### Windows

Use WSL (Windows Subsystem for Linux):
```bash
wsl --install
# Then install as in Ubuntu/Debian
```

### From Source

```bash
# Download
wget http://ftp.gnu.org/gnu/smalltalk/smalltalk-3.2.5.tar.gz
tar xzf smalltalk-3.2.5.tar.gz
cd smalltalk-3.2.5

# Build
./configure
make
sudo make install
```

## VisualWorks Installation

### All Platforms

1. Visit https://www.cincomsmalltalk.com/
2. Click "Download VisualWorks"
3. Register (free for non-commercial use)
4. Download for your platform
5. Run installer
6. Launch VisualWorks

**Note**: Commercial product, free non-commercial license

## Dolphin Smalltalk Installation (Windows Only)

1. Visit https://github.com/dolphinsmalltalk/Dolphin
2. Download latest release
3. Extract ZIP file
4. Run `Dolphin.exe`
5. Open workspace and start coding

## Smalltalk/X Installation

### Windows

1. Visit https://www.exept.de/
2. Download Windows installer
3. Run installer
4. Launch Smalltalk/X

### Linux

```bash
# Download from exept.de
# Or use package if available
```

## System Requirements

### Minimum Requirements

**Pharo:**
- RAM: 2 GB minimum, 4 GB recommended
- Disk: 500 MB free space
- OS: Windows 7+, macOS 10.12+, Linux kernel 3.x+
- Display: 1024x768 minimum

**Squeak:**
- RAM: 512 MB minimum, 2 GB recommended
- Disk: 200 MB free space
- Very low requirements - runs on older hardware

**Glamorous Toolkit:**
- RAM: 4 GB minimum, 8 GB recommended
- Disk: 1 GB free space
- Modern system recommended

### Recommended Setup

- **RAM**: 8 GB or more
- **Disk**: SSD for better performance
- **Display**: 1920x1080 or higher
- **Mouse**: 3-button mouse helpful (middle-click)
- **Keyboard**: Full keyboard (numeric keypad useful)

## Virtual Machine Notes

### OpenSmalltalk VM (Cog)

Most modern Smalltalks (Pharo, Squeak, Cuis) use Cog VM:
- JIT compilation
- Fast performance
- Multiple platforms
- Actively developed

**Get it**: https://opensmalltalk.org/

### SqueakJS

Run Squeak in a browser:
1. Visit https://squeak.js.org/
2. Click "Run Squeak"
3. No installation needed!

## Multiple Smalltalk Installations

You can install multiple Smalltalks simultaneously:

```
~/Smalltalks/
  ├── pharo/
  │   ├── image1.image
  │   ├── image2.image
  │   └── PharoLauncher/
  ├── squeak/
  │   └── Squeak6.0/
  ├── gt/
  │   └── GlamorousToolkit/
  └── cuis/
      └── Cuis6.0/
```

They don't interfere with each other!

## Environment Variables

### Pharo

```bash
# Set image directory
export PHARO_IMAGE_DIR="$HOME/pharo-images"

# VM options
export PHARO_VM_OPTS="-memory 2g"
```

### Squeak

```bash
export SQUEAK_IMAGE="$HOME/squeak/MyProject.image"
```

## Command-Line Usage

### Pharo

```bash
# Launch with specific image
./pharo-ui MyProject.image

# Headless execution
./pharo MyProject.image eval "1 + 1"

# Run script
./pharo MyProject.image st script.st

# Save and quit
./pharo MyProject.image save myNewName
```

### Squeak

```bash
# Launch
squeak MyProject.image

# Headless
squeak -headless MyProject.image script.st
```

### GNU Smalltalk

```bash
# REPL
gst

# Run script
gst script.st

# Execute code
gst -e "1 to: 10 do: [:i | i printNl]"
```

## Updating Smalltalk

### Pharo

**Within image:**
```smalltalk
"Check for updates"
UpdateChecker checkForUpdates.

"Download and install updates"
UpdateDownloader new downloadLatestUpdates.
```

**Or use PharoLauncher** to download new versions

### Squeak

**Within image:**
```smalltalk
"Update from server"
MCMcmUpdater updateFromServer.
```

### Glamorous Toolkit

Download new releases from https://gtoolkit.com/download/

## Backup and Migration

### Backup Your Images

```bash
# Copy image files
cp MyProject.image MyProject-backup-$(date +%Y%m%d).image
cp MyProject.changes MyProject-backup-$(date +%Y%m%d).changes

# Or use tar
tar czf MyProject-backup-$(date +%Y%m%d).tar.gz MyProject.image MyProject.changes
```

### Migrate Between Machines

1. **Copy image files** (.image and .changes)
2. **Ensure compatible VM** version
3. **Copy any external resources** (files, databases)
4. **Test** on new machine

## Troubleshooting Common Issues

### Image Won't Open

- **Check VM compatibility**: Match image version to VM version
- **Corrupt image**: Restore from backup
- **Missing changes file**: Image may open but changes lost

### Slow Performance

- **Increase memory**: Edit VM parameters
- **Close unused windows**: Reduce visual overhead
- **Garbage collect**: `Smalltalk garbageCollect`
- **Check background processes**: May be running expensive code

### Graphics Issues

- **Linux**: Try different window manager
- **macOS Retina**: May need to adjust scaling
- **Windows**: Update graphics drivers

### Network Issues

- **Firewall**: Allow Smalltalk through firewall
- **Proxy**: Configure proxy settings if needed
- **SSL certificates**: May need to update certificates

## Getting Help

If installation fails:

1. **Check system requirements**
2. **Read error messages carefully**
3. **Search Discord/mailing lists** for similar issues
4. **Ask on Discord** with:
   - Your OS and version
   - Smalltalk version
   - Error message
   - What you've tried

**Discord**: https://discord.gg/QewZMZa

## Quick Start Commands

After installation, try these:

```smalltalk
"Hello World"
Transcript show: 'Hello, Smalltalk!'; cr.

"Inspect an object"
42 inspect.

"Browse a class"
Collection browse.

"Run tests"
TestRunner open.

"Open a workspace"
Workspace open.
```

**You're ready to code!**

---

[Previous: Chapter 40 - Your Smalltalk Journey](../chapters/chapter-40-your-journey.md) | [Next: Appendix B - Keyboard Shortcuts Reference](appendix-b-shortcuts.md)
