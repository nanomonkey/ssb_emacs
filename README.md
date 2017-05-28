# ssb_emacs
Secure Scuttlebutt for Emacs
****

**What is scuttlebutt?!**

Learn more at [scuttlebutt](https://www.scuttlebutt.nz/)

**Install**
- Put the ssb_emacs.el file where the sun don't shine.

**Usage and Commands**

Render via Patchfoo and EWW

| Interactive Command | Description|
|-------------------|------------|
| M-x ssb-start-patchfoo | Starts Patchfoo server process |
|M-x ssb-eww-patchfoo | Opens up localhost via eww browser|
|M-x ssb-stop-patchfoo | Stops patchfoo process |


Render via emacs (work in progress)

| Interactive Command | Description|
|-------------------|------------|
|M-x ssb-start-server| Starts sbot server process|
|M-x ssb-stop-server | Stops sbot server process |
|M-x ssb-whoami | gets id |
|M-x ssb-join-pub | Join a pub with code |
|M-x ssb-quick-message | Send a one line message |

There are functions for viewing and creating longer messages, but they are
still being ironed out.  Pull requests welcome.

...more to come

