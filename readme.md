**LazGitGUI**

This is yet another git tool, resembling git gui with some additional features.
I started this tool because it was not available, by default, under mac os, as
it is under linux and windows (when you install git by your own).

It was built also as a test for doing some git commands, with the ultimate goal
of doing a Lazarus integration.

**Screenshots.**

![main](images/main.png)

![log](images/log.png)

![filehistory](images/filehistory.png)

**Starting it.**

At start, the program tries to find the git executable in the system (or user)
path, once located, it gets stored in the LazGitGui config file. LazGitGui does
this only once, the next time the program is started, the git executable path
is read from the config file. 

In the case LazGitGui was unable to locate the git program, one have to edit the
config file and introduce the git location there. At the moment, LazGitGui
doesn't have a config screen.

The config file can be found in the following locations:

    Windows:    %LOCALAPPDATA%\appdata\lazgitgui\config\lazgitgui.cfg
    Linux/mac:  $HOME/.config/lazgitgui\lazgitgui.cfg

for example (this works under linux):

    [lazgitgui.cfg]
    git=/usr/bin/git

**How to use.**

LazGitGui requires a path passed as the last parameter in the command line, it can be a directory or a file residing within the working area of the repository. Some suggestions for invoking LazGitGUI:

From Lazarus, an external tool can be configured with the following properties:

(linux + windows)

    Title: LazGitGui
    Program Filename: PATH/TO/LazGitGui executable
    Parameters: --logfile=PATH/TO/LazGitGui/ide.log $EdFile()
    Shortcut: CTRL+ALT+G

(macos)

    Title: LazGitGui
    Program Filename:
    Parameters: --logfile=PATH/TO/LazGitGui/ide.log $EdFile()
    Shortcut:

    





