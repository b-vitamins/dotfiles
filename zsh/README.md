# Zsh Configuration User Manual

This manual teaches you modern zsh features through direct comparisons with traditional approaches.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Modern Command Replacements](#modern-command-replacements)
3. [Navigation](#navigation)
4. [Git Shortcuts](#git-shortcuts)
5. [Guix Package Management](#guix-package-management)
6. [Development Tools](#development-tools)
7. [Interactive Search (FZF)](#interactive-search-fzf)
8. [Prompt Features](#prompt-features)
9. [Key Bindings](#key-bindings)
10. [Utility Functions](#utility-functions)

## Quick Start

Instead of:
```bash
ls -la
```
Do:
```bash
ea
```

Instead of:
```bash
cd ../../projects/webapp/src
```
Do:
```bash
z webapp
```

Instead of:
```bash
history | grep "git pull" | tail -20
```
Do:
```bash
# Press ctrl+r and type "git pull"
```

## Modern Command Replacements

### File Listing (eza replaces ls)

Instead of:
```bash
ls
```
Do:
```bash
e
```

Instead of:
```bash
ls -l
```
Do:
```bash
ee
```

Instead of:
```bash
ls -la
```
Do:
```bash
ea
```

Instead of:
```bash
ls -1
```
Do:
```bash
e1
```

Instead of:
```bash
tree
```
Do:
```bash
et
```

Instead of:
```bash
ls -la ~/Documents/
```
Do:
```bash
ea ~/Documents
```

Instead of:
```bash
tree -L 2 -a
```
Do:
```bash
et -L 2 -a
```

Instead of:
```bash
find . -type f -name "*.pdf" -exec ls -la {} \;
```
Do:
```bash
fd -e pdf -x ee
```

### File Viewing (bat replaces cat)

Instead of:
```bash
cat file.txt
```
Do:
```bash
b file.txt
```

Instead of:
```bash
cat -n file.txt
```
Do:
```bash
bn file.txt
```

Instead of:
```bash
cat file.txt | grep -A 5 -B 5 "error"
```
Do:
```bash
b file.txt | rg -C 5 "error"
```

Instead of:
```bash
cat > /tmp/output.txt
```
Do:
```bash
bp > /tmp/output.txt  # Plain output, no colors
```

Instead of:
```bash
head -n 50 script.py | cat -n
```
Do:
```bash
b --line-range=:50 script.py
```

### Searching (ripgrep and fd)

Instead of:
```bash
grep -r "TODO" .
```
Do:
```bash
rg "TODO"
```

Instead of:
```bash
grep -ri "error" . --include="*.log"
```
Do:
```bash
rg -i "error" -g "*.log"
```

Instead of:
```bash
find . -name "*.py"
```
Do:
```bash
fd "*.py"
```

Instead of:
```bash
find . -name "*test*" -type f
```
Do:
```bash
fd test
```

Instead of:
```bash
find ~/projects -name "*.rs" -type f | head -20
```
Do:
```bash
fd -e rs . ~/projects | head -20
```

Instead of:
```bash
grep -r "function.*login" . --include="*.js"
```
Do:
```bash
rg "function.*login" -g "*.js"
```

Instead of:
```bash
find . -name "*.json" -exec grep -l "version" {} \;
```
Do:
```bash
fd -e json -x rg -l "version"
```

### System Monitoring

Instead of:
```bash
top
```
Do:
```bash
btop
```

Instead of:
```bash
df -h
```
Do:
```bash
duf
```

Instead of:
```bash
du -sh * | sort -h
```
Do:
```bash
dust
```

Instead of:
```bash
ps aux | grep python
```
Do:
```bash
procs python
```

Instead of:
```bash
ps aux | grep -E "(CPU|python)" | grep -v grep
```
Do:
```bash
procs -p python
```

### Text Manipulation

Instead of:
```bash
sed 's/old/new/g' file.txt
```
Do:
```bash
sd "old" "new" file.txt
```

Instead of:
```bash
sed -i.bak 's/localhost/127.0.0.1/g' config.ini
```
Do:
```bash
sd -p "localhost" "127.0.0.1" config.ini
```

Instead of:
```bash
find . -name "*.md" -exec sed -i 's/Github/GitHub/g' {} \;
```
Do:
```bash
fd -e md -x sd "Github" "GitHub"
```

## Navigation

### Quick Directory Changes

Instead of:
```bash
cd ..
```
Do:
```bash
..
```

Instead of:
```bash
cd ../..
```
Do:
```bash
...
```

Instead of:
```bash
cd ../../..
```
Do:
```bash
....
```

Instead of:
```bash
cd -  # Previous directory
```
Do:
```bash
-
```

Instead of:
```bash
pushd ~/projects && pushd src && pushd tests && dirs
```
Do:
```bash
cd ~/projects && cd src && cd tests && d
```

Instead of:
```bash
cd ~/projects/website/src/components  # After showing dirs with 'd'
```
Do:
```bash
3  # If it's position 3 in the stack
```

### Smart Jumping with Zoxide

Instead of:
```bash
cd ~/projects/my-awesome-webapp/src/components
```
Do:
```bash
z webapp
```

Instead of:
```bash
cd ~/Documents/Research/2024/machine-learning/
```
Do:
```bash
z machine
```

Instead of:
```bash
find ~ -type d -name "*config*" | grep -i vim | head -1 | xargs cd
```
Do:
```bash
zi  # Then type "vim config"
```

Instead of:
```bash
cd ~/very/long/path/to/frequently/used/directory
```
Do:
```bash
z frequently  # After visiting once
```

## Git Shortcuts

### Basic Git Commands

Instead of:
```bash
git status
```
Do:
```bash
gs
```

Instead of:
```bash
git add file1.txt file2.txt
```
Do:
```bash
ga file1.txt file2.txt
```

Instead of:
```bash
git diff
```
Do:
```bash
gd
```

Instead of:
```bash
git diff --staged
```
Do:
```bash
gds
```

Instead of:
```bash
git commit -v
```
Do:
```bash
gc
```

Instead of:
```bash
git push origin main
```
Do:
```bash
gp
```

Instead of:
```bash
git pull origin main
```
Do:
```bash
gpu
```

Instead of:
```bash
git checkout -b feature-branch
```
Do:
```bash
gcb feature-branch
```

### Advanced Git Workflows

Instead of:
```bash
git log --oneline --graph --decorate -20
```
Do:
```bash
gl
```

Instead of:
```bash
git log --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit
```
Do:
```bash
glog
```

Instead of:
```bash
git add -A && git commit -m "Work in progress"
```
Do:
```bash
gwip
```

Instead of:
```bash
git reset HEAD~1 --mixed
```
Do:
```bash
gundo
```

Instead of:
```bash
git branch -a | grep -i feature | xargs git checkout
```
Do:
```bash
gco  # Then search for "feature"
```

Instead of:
```bash
git log --oneline | fzf | cut -d' ' -f1 | xargs git show
```
Do:
```bash
gshow  # Interactive commit browser with preview
```

### Combined Git Operations

Instead of:
```bash
git status && git diff && git add -p && git diff --staged && git commit -v
```
Do:
```bash
gs && gd && ga -p && gds && gc
```

Instead of:
```bash
git stash && git checkout main && git pull && git checkout - && git stash pop
```
Do:
```bash
git stash && g checkout main && gpu && - && git stash pop
```

## Guix Package Management

### Package Operations

Instead of:
```bash
guix search python | less
```
Do:
```bash
gse python | b
```

Instead of:
```bash
guix package --install python-numpy
```
Do:
```bash
gi python-numpy
```

Instead of:
```bash
guix package --remove python-numpy
```
Do:
```bash
gr python-numpy
```

Instead of:
```bash
guix package --upgrade
```
Do:
```bash
gu
```

Instead of:
```bash
guix pull && guix package --upgrade
```
Do:
```bash
gpl && gu
```

### System Management

Instead of:
```bash
sudo guix system reconfigure /etc/config.scm
```
Do:
```bash
gsy reconfigure /etc/config.scm
```

Instead of:
```bash
guix gc --free-space=50G
```
Do:
```bash
gca -F 50G
```

Instead of:
```bash
guix package --list-generations | less
```
Do:
```bash
guix-generations
```

Instead of:
```bash
guix gc --delete-generations=1m && guix gc --free-space=50G
```
Do:
```bash
guix-clean 50G
```

Instead of:
```bash
guix describe
```
Do:
```bash
gde
```

### Development Environments

Instead of:
```bash
guix shell -D -f guix.scm
```
Do:
```bash
guix-shell-here
```

Instead of:
```bash
guix shell python python-numpy python-matplotlib python-pandas
```
Do:
```bash
gsh python python-numpy python-matplotlib python-pandas
```

Instead of:
```bash
GUIX_PROFILE="/home/user/.guix-extra-profiles/music/music" && source "$GUIX_PROFILE/etc/profile"
```
Do:
```bash
fenv  # Select profile interactively
```

Instead of:
```bash
guix search . | grep -i machine | grep -i learning | less
```
Do:
```bash
fguix  # Then type "machine learning"
```

## Development Tools

### Editors and Basic Tools

Instead of:
```bash
emacs file.txt
```
Do:
```bash
e file.txt
```

Instead of:
```bash
vim file.txt
```
Do:
```bash
v file.txt
```

Instead of:
```bash
python3
```
Do:
```bash
py
```

Instead of:
```bash
python3 -m venv venv
```
Do:
```bash
venv venv
```

Instead of:
```bash
source venv/bin/activate
```
Do:
```bash
activate
```

Instead of:
```bash
make -j$(nproc)
```
Do:
```bash
mk
```

Instead of:
```bash
make -j$(($(nproc) - 2))
```
Do:
```bash
mk  # Already optimized
```

Instead of:
```bash
cmake -G Ninja .
```
Do:
```bash
cninja .
```

### Docker Operations

Instead of:
```bash
docker ps
```
Do:
```bash
dkps
```

Instead of:
```bash
docker ps -a
```
Do:
```bash
dkpsa
```

Instead of:
```bash
docker exec -it container_name bash
```
Do:
```bash
dke container_name bash
```

Instead of:
```bash
docker logs -f container_name
```
Do:
```bash
dkl -f container_name
```

Instead of:
```bash
docker rm $(docker ps -aq)
```
Do:
```bash
dkrm $(dkpsa -q)
```

Instead of:
```bash
docker compose up -d
```
Do:
```bash
dkc up -d
```

### System Services

Instead of:
```bash
systemctl status nginx
```
Do:
```bash
sc status nginx
```

Instead of:
```bash
systemctl --user enable emacs
```
Do:
```bash
scu enable emacs
```

Instead of:
```bash
journalctl -u nginx -f
```
Do:
```bash
jc -u nginx -f
```

Instead of:
```bash
journalctl --user -u emacs -n 50
```
Do:
```bash
jcu -u emacs -n 50
```

## Interactive Search (FZF)

### Command History

Instead of:
```bash
history | grep "docker" | tail -20
```
Do:
```bash
# Press ctrl+r, type "docker"
```

Instead of:
```bash
!423  # Run command 423 from history
```
Do:
```bash
# Press ctrl+r, find command, press enter
```

Instead of:
```bash
history | grep "git commit" | grep "fix" | tail -10
```
Do:
```bash
# Press ctrl+r, type "git commit fix"
```

### File Operations

Instead of:
```bash
find . -name "*.md" | xargs less
```
Do:
```bash
fd -e md | fzf | xargs b
```

Instead of:
```bash
ls -la | grep -i config
```
Do:
```bash
e | fzf  # Type "config"
```

Instead of:
```bash
find . -type f | head -50 | xargs cat | less
```
Do:
```bash
preview
```

### Process Management

Instead of:
```bash
ps aux | grep firefox | awk '{print $2}' | xargs kill -9
```
Do:
```bash
fkill  # Type "firefox", select, enter
```

Instead of:
```bash
ps aux | grep -E "python|node" | less
```
Do:
```bash
fkill  # Type "python node" to filter
```

### Git with FZF

Instead of:
```bash
git branch -a | grep feature | head -5
```
Do:
```bash
gco  # Type "feature"
```

Instead of:
```bash
git log --oneline | head -20 | grep fix | xargs git show
```
Do:
```bash
gshow  # Type "fix", press enter on commit
```

### Advanced FZF Combinations

Instead of:
```bash
grep -r "TODO" . | cut -d: -f1 | sort | uniq | xargs less
```
Do:
```bash
rgi "TODO"  # Shows files with preview
```

Instead of:
```bash
find . -name "*.py" -exec grep -l "import numpy" {} \; | xargs vim
```
Do:
```bash
fd -e py -x rg -l "import numpy" | fzf | xargs v
```

Instead of:
```bash
guix search python | grep -i science | grep -i data | less
```
Do:
```bash
fguix  # Type "python science data"
```

## Prompt Features

### Understanding Prompt States

Instead of:
```bash
PS1="$ "  # Boring prompt
```
Do:
```bash
# λ mileva ~ ›  # Rich contextual prompt
```

Instead of:
```bash
echo $?  # Check last command status
```
Do:
```bash
# › turns red after failed command
```

Instead of:
```bash
pwd  # Check current directory
```
Do:
```bash
# Look at prompt: λ mileva ~/projects ›
```

Instead of:
```bash
git branch && git status -s  # Check git state
```
Do:
```bash
# Look at right prompt: [main*] src/components
```

Instead of:
```bash
time long-running-command  # Time execution
```
Do:
```bash
long-running-command  # Shows "12s" in prompt if > 3s
```

Instead of:
```bash
echo $SSH_CONNECTION  # Check if SSH session
```
Do:
```bash
# Hostname turns yellow in SSH sessions
```

Instead of:
```bash
echo $VIRTUAL_ENV  # Check virtual environment
```
Do:
```bash
# λ turns red when in virtual env
```

### Prompt Customization

Instead of:
```bash
# Static prompt all the time
```
Do:
```bash
ctrl+space  # Toggle between minimal and full prompt
```

## Key Bindings

### Command Line Editing

Instead of:
```bash
# Delete whole line and retype
```
Do:
```bash
ctrl+u  # Kill to beginning of line
```

Instead of:
```bash
# Use arrow keys to go to start
```
Do:
```bash
ctrl+a  # Jump to beginning
```

Instead of:
```bash
# Delete character by character
```
Do:
```bash
ctrl+w  # Delete previous word
```

Instead of:
```bash
cd /home/user/projects/webapp  # Delete "webapp" char by char
```
Do:
```bash
cd /home/user/projects/webapp  # Press alt+backspace
# Result: cd /home/user/projects/
```

Instead of:
```bash
# Retype long command with small edit
```
Do:
```bash
ctrl+x ctrl+e  # Edit in your editor
```

### Word Navigation

Instead of:
```bash
# Use arrow keys to navigate
```
Do:
```bash
alt+f  # Forward word
alt+b  # Backward word
```

Instead of:
```bash
# Delete forwards character by character
```
Do:
```bash
alt+d  # Delete word forward
```

### History Navigation

Instead of:
```bash
# Press up arrow many times
```
Do:
```bash
ctrl+p  # Previous command
ctrl+n  # Next command
```

Instead of:
```bash
# Retype the filename from last command
```
Do:
```bash
alt+.  # Insert last argument
```

Instead of:
```bash
git add very/long/path/to/file.txt
vim very/long/path/to/file.txt  # Retyping path
```
Do:
```bash
git add very/long/path/to/file.txt
vim alt+.  # Inserts: very/long/path/to/file.txt
```

### Autosuggestions

Instead of:
```bash
# Type entire command you used before
```
Do:
```bash
# Start typing, press shift+tab to accept suggestion
```

Instead of:
```bash
docker exec -it my-very-long-container-name bash  # Type it all
```
Do:
```bash
docker ex[shift+tab]  # Completes from history
```

## Utility Functions

### File Operations

Instead of:
```bash
mkdir new-project && cd new-project
```
Do:
```bash
mkcd new-project
```

Instead of:
```bash
cp important.conf important.conf.backup
```
Do:
```bash
bak important.conf  # Creates: important.conf.bak.20240115-143022
```

Instead of:
```bash
tar -xzf archive.tar.gz
```
Do:
```bash
extract archive.tar.gz
```

Instead of:
```bash
tar -xjf archive.tar.bz2 && tar -xf other.tar && unzip third.zip
```
Do:
```bash
extract archive.tar.bz2 && extract other.tar && extract third.zip
```

Instead of:
```bash
gunzip file.gz && tar -xf file.tar && unzip file.zip
```
Do:
```bash
extract file.gz && extract file.tar && extract file.zip
```

### Temporary Workspaces

Instead of:
```bash
cd /tmp && mkdir test-123 && cd test-123
```
Do:
```bash
tmp test
```

Instead of:
```bash
mkdir /tmp/experiment-$(date +%s) && cd /tmp/experiment-*
```
Do:
```bash
tmp experiment
```

### System Information

Instead of:
```bash
netstat -tulpn | grep LISTEN
```
Do:
```bash
ports
```

Instead of:
```bash
echo $PATH | sed 's/:/\n/g'
```
Do:
```bash
path
```

Instead of:
```bash
curl -s https://ipinfo.io/ip && echo
```
Do:
```bash
myip
```

Instead of:
```bash
curl -s https://wttr.in | head -17
```
Do:
```bash
weather
```

Instead of:
```bash
history | tail -30
```
Do:
```bash
h
```

### Configuration Management

Instead of:
```bash
vim ~/.config/zsh/.zshrc
```
Do:
```bash
zc
```

Instead of:
```bash
vim ~/.config/zsh/.zshenv
```
Do:
```bash
ze
```

Instead of:
```bash
source ~/.config/zsh/.zshrc
```
Do:
```bash
reload
```

Instead of:
```bash
exit && zsh  # Full restart
```
Do:
```bash
exec zsh
```

## Advanced Compositions

### Multi-Level Tool Combinations

Instead of:
```bash
find . -name "*.log" -exec grep -l "ERROR" {} \; | xargs ls -la | sort -k5 -n
```
Do:
```bash
fd -e log -x rg -l "ERROR" | xargs ee | sort -k5 -n
```

Instead of:
```bash
ps aux | grep python | awk '{print $2}' | xargs -I{} sh -c 'echo "PID: {}" && lsof -p {} | head -5'
```
Do:
```bash
procs python | fzf | awk '{print $1}' | xargs -I{} lsof -p {}
```

Instead of:
```bash
git log --oneline | grep -i fix | head -10 | cut -d' ' -f1 | xargs -I{} git diff {}^..{}
```
Do:
```bash
glog | rg -i fix | head -10 | cut -d' ' -f1 | xargs -I{} gd {}^..{}
```

Instead of:
```bash
docker ps --format "table {{.Names}}\t{{.Status}}" | grep -i running | cut -f1 | xargs -I{} docker logs {} | tail -20
```
Do:
```bash
dkps | rg -i running | cut -f1 -d' ' | xargs -I{} dkl {} | tail -20
```

### Workflow Compositions

Instead of:
```bash
cd ~/projects && ls -la && find . -name ".git" -type d | cut -d'/' -f2 | sort
```
Do:
```bash
z projects && ea && fd -H -t d "^\.git$" -x dirname | sort
```

Instead of:
```bash
history | grep "guix install" | tail -20 | cut -d' ' -f4- | sort | uniq
```
Do:
```bash
# Press ctrl+r, type "gi " (note the space), browse all installs
```

Instead of:
```bash
vim $(find . -name "*.py" | grep -v __pycache__ | grep test | head -1)
```
Do:
```bash
v $(fd -e py test | head -1)
```

Instead of:
```bash
cd /tmp && mkdir test-$$ && cd test-$$ && git init && echo "test" > README.md
```
Do:
```bash
tmp git-test && g init && echo "test" > README.md
```

### Research Workflow Examples

Instead of:
```bash
find ~/Documents -name "*.pdf" | xargs -I{} sh -c 'echo "File: {}" && pdfinfo {} | grep -E "(Title|Author|Pages)"' | less
```
Do:
```bash
fd -e pdf . ~/Documents -x sh -c 'echo "File: {}" && pdfinfo {} | rg "(Title|Author|Pages)"' | b
```

Instead of:
```bash
grep -r "hypothesis" ~/research --include="*.tex" | cut -d: -f1 | sort | uniq | xargs wc -l
```
Do:
```bash
rg "hypothesis" ~/research -g "*.tex" -l | xargs wc -l
```

Instead of:
```bash
find ~/projects -name "requirements.txt" -exec dirname {} \; | xargs -I{} sh -c 'echo "Project: {}" && cat {}/requirements.txt | grep -E "(numpy|pandas|torch)"'
```
Do:
```bash
fd requirements.txt ~/projects -x dirname | xargs -I{} sh -c 'echo "Project: {}" && b {}/requirements.txt | rg "(numpy|pandas|torch)"'
```

## Diagnostic Commands

Instead of:
```bash
which ls && type ls && whereis ls
```
Do:
```bash
which e  # For our alias
```

Instead of:
```bash
echo "Startup time test" && time zsh -i -c exit
```
Do:
```bash
# Uncomment line 2 in .zshrc, then:
zsh  # New shell
zprof  # See profiling data
```

Instead of:
```bash
env | grep -E "(PATH|GUIX|VIRTUAL)" | sort
```
Do:
```bash
env | rg "(PATH|GUIX|VIRTUAL)" | sort | b
```

## Emergency Fixes

Instead of:
```bash
# Shell is broken, can't type commands
```
Do:
```bash
/bin/bash  # Fallback to bash
/usr/bin/zsh -f  # Zsh without config
```

Instead of:
```bash
# Prompt is messed up
```
Do:
```bash
ctrl+l  # Clear and redraw
ctrl+space  # Toggle prompt mode
```

Instead of:
```bash
# Completion is hanging
```
Do:
```bash
ctrl+c  # Cancel
rm ~/.cache/zsh/zcompdump*  # Clear completion cache
reload
```