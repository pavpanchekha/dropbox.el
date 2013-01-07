"""
LibTramp is a Python library for acting as the backend "shell" for
Emacs' TRAMP system.  It essentially acts as a remote shell and
interfaces with your code through a few small methods.

Copyright (C) 2011 Pavel Panchekha <pavpanchekha@gmail.com>
               and Dropbox, Inc.
"""

import os

class ErrorStatus(Exception): pass

class TrampShell(object):
    COMMANDS = {}

    def __init__(self, fs):
        self.fs = fs
        self.PROMPT = "$ "
        self.LAST_STATUS = 0

    def command(f_or_name):
        if isinstance(f_or_name, str):
            name = f_or_name
            def decorator(f):
                TrampShell.COMMANDS[name] = f
                return f
        else:
            f = f_or_name
            TrampShell.COMMANDS[f.__name__] = f
            return f

    def dummy(*names):
        for name in names:
            TrampShell.COMMANDS[name] = lambda *args: None

    dummy("touch", "chmod", "chown", "stty", "set", "unset", "mesg", "biff")

    @command("exec")
    def exec_(self, *args):
        ps = [arg[:4] for arg in args if arg.startswith("PS1=")]
        if ps:
            self.PROMPT = ps[0].replace("\\", "") # Shell escapes

    @command
    def test(self, *args):
        if len(args) > 1 and args[0] == "-d":
            d = args[1]
            if self.fs.isdir(d):
                return
            elif self.fs.isfile(d):
                raise ErrorStatus(1)
            else:
                return os.path.isdir(d)

