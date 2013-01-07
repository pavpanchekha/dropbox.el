#!/usr/bin/env python
# This is just here for C-x C-e'ing: (tramp-cleanup-all-connections)

from dropbox import client, session # for API
import re # For haxxx
import sys # For std{in,out}
import os # For path name operations

print "HEllo, I'm dropbox-sh, my args are", sys.argv

log = open("/tmp/tramp-db.log", "w")

KEY = "iybkzcei2msvtfs"
SECRET = "h0i6bjickvpk0nt"

def connect():
    sess = session.DropboxSession(KEY, SECRET, root="dropbox")
    api = client.DropboxClient(sess)
    sess.set_token(*sys.argv[1:])
    return api
API = connect()

class ErrorStatus(Exception): pass

def read():
    res = sys.stdin.readline()
    log.write(res)
    log.flush()
    return res
def write(line):
    log.write(line)
    log.flush()
    sys.stdout.write(line)

def list_dir(dir):
    # Returns two lists, of files and of directories
    # or None if fail
    if dir[-2:] == "/.":
        dir = dir[:-2]

    files = []
    dirs = ["."] if dir == "/" else [".", ".."]
    try:
        resp = API.metadata(dir)
    except:
        return None

    if 'contents' not in resp.data: # File
        return None

    for f in resp.data["contents"]:
        fname = os.path.basename(f["path"])
        if f["is_dir"]:
            dirs.append(fname)
        else:
            files.append(fname)

    return files, dirs

def dir_exists(dir):
    try:
        resp = API.metadata(dir)
    except:
        return False

    return "is_dir" in resp.data and resp.data["is_dir"]

def file_exists(file):
    try:
        resp = API.metadata(file)
    except:
        return False

    return "is_dir" in resp.data and not resp.data["is_dir"]

def file_size(file):
    try:
        resp = API.metadata(file)
    except:
        return -1

    if "is_dir" in resp.data and resp.data["is_dir"]:
        return 4096 # I don't think Emacs actually needs this, but my eyes are trained to look for 4096
    elif "bytes" in resp.data:
        return resp.data["bytes"]
    else:
        return -1

COMMANDS = {}
def command(name):
    def decorator(f):
        COMMANDS[name] = f
        return f
    return decorator

PROMPT = "dbsh $"
LAST_STATUS = 0
PWD = "/"

@command("exec")
def my_exec(*args):
    global PROMPT
    ps = [arg for arg in args if arg.startswith("PS1=")]
    if ps: # Emacs demands a certain prompt, usually '#$'
        PROMPT = ps[0][4:].replace("\\", "") # Emacs shell escapes it
    return

@command("test")
def my_test(*args):
    if len(args) > 1 and  args[0] == "-d":
        dir = args[1]
        if dir_exists(dir): # Dirs stay dirs
            pass
        if dir.startswith("/usr/bin/") and len(dir) > 5: # Files in /bin are likely programs Emacs pretends to need
            raise ErrorStatus(2)
        if os.path.isdir(dir): # Real dirs are dirs
            pass
        else: # Everything else is a file
            raise ErrorStatus(1)

@command("echo")
def my_echo(*args):
    def special_cases(arg):
        if ">" in arg[0:2]:
            return None
        elif "\"`getconf" == arg:
            return '"/bin:/usr/bin"'
        elif "PATH`\"" == arg:
            return None
        elif "\"`uname" == arg:
            return '"Dropbox API 1.0"'
        elif "-sr`\"" == arg:
            return None
        elif "\"`tty`\"" == arg:
            return "/dev/ttys1337"
        elif arg == "$?":
            return str(LAST_STATUS)
        elif arg[0] == "~":
            return "/usr/" + arg[1:]
        elif arg == ")":
            return None
        elif arg[:2] == '"`' and arg[-2:] == '`"':
            write('"')
            run_cmd(shlex.split(arg[2:-2]))
            write('"\n')
        else:
            return arg
    write(" ".join(filter(bool, [special_cases(arg) for arg in args])) + "\n")

@command("(")
def my_special(*args):
    #write(repr(args) + "\n")
    if args[2:7] == ("|", "base64", "|", "base64", "-d") and args[0] == "echo":
        write(args[1] + "\n")
    elif args[:2] == ("base64", "</dev/null"):
        return
    elif "-d" in args:
        write("!!!\n")
        return
    else:
        write("!!!\n")
        return
        
@command("cp")
def my_cp(*args):
    args = [arg for arg in args if not arg.startswith("-")] # filter out flags
    src, dst = args[:2]
    try:
        f = API.get_file(src)
        open(dst, "w").write(f.read())
    except:
        f = open(src)
        API.put_file(os.path.dirname(dst), f, name=os.path.basename(dst), overwrite=True)

@command("touch")
def my_touch(*args):
    pass

@command("chmod")
def my_chmod(*args): pass
@command("chown")
def my_chown(*args): pass

@command("cd")
def my_cd(arg):
    global PWD
    if arg != "~":
        PWD = arg
    else: # Trick Emacs into thinking we're a real user
        PWD = "/usr/pavpanchekha" # Fuck it, make it look like we're on plan 9

@command("stty")
def my_stty(*args):
    # That's very nice, Emacs.  I'm sure to take all that into account
    pass

@command("pwd")
def my_pwd():
    write(PWD + "\n")

@command("set")
def my_set(*args):
    # You set... vi and emacs mode?!
    pass

@command("unset")
def my_unset(*args):
    # Technically correct, since the variables are afterwards not set
    pass

@command("export")
def my_export(*args):
    # Can't export to cuba
    if args[0] == "cuba":
        write("ERREMBARGO\n")
    else:
        pass

@command("which")
def my_which(*args):
    # Which is piped to wc -c
    write("\t1\n")

@command("stat")
def my_stat(*args):
    # Emacs only does this to directories...
    write('("%s" %i)\n' % (args[2], file_size(args[2])))

@command("readlink")
def my_readlink(canon_flag, arg, *args):
    write(arg + "\n")

@command("mesg")
def my_mesg(*args):
    # Don't worry, there's no one here to *send* you messages
    pass

@command("biff")
def my_biff(*args):
    # Don't worry, there's no one here to *send* you mail
    pass

@command("tramp_vc_registered_read_file_names")
def my_vc(*args):
    write("(\n")
    while True:
        fname = read()[:-1]
        if fname == "EOF":
            break
        write('("%s" "file-exists-p" nil)\n' % fname)
        write('("%s" "file-readable-p" nil)\n' % fname)
    write(")\n")
    read() # Some more shit ignored here
    write("tramp_exit_status 0\n")

@command("ls")
def my_ls(*args):
    global LAST_STATUS
    if len(args) > 1 and args[0] == "-lnd":
        dir = args[1]
        if dir_exists(dir):
            # Print "directory info"
            write("drwxr-xr-x 1 0 0 4096 1992-08-20 17:23 %s\n" % dir)
    elif len(args) > 3 and args[:3] == ("--color=never", "-al", "/dev/null"):
        # Tell Emacs /dev/null exists
        write("crw-rw-rw- 1 pavpanchekha pavpanchekha 1, 3 1992-08-20 17:23 /dev/null\n")
    elif len(args) > 4 and args[:4] == ("--color=never", "--dired", "-al", "/dev/null"):
        # Tell Emacs /dev/null exists
        write("  crw-rw-rw- 1 pavpanchekha pavpanchekha 1, 3 1992-08-20 17:23 /dev/null\n")
        write("//DIRED// 63 72\n")
        write("//DIRED-OPTIONS// --quoting-style=literal\n")
    elif "--dired" in args:
        try:
            files, dirs = list_dir(args[-2]) # -2 is the 2>/dev/null
        except TypeError:
            write("//DIRED-OPTIONS// --quoting-style=literal\n")
            raise ErrorStatus(2)
        lines = []
        for dir in dirs:
            lines.append("  drw------- 1 pavpanchekha pavpanchekha 4096 1992-08-20 17:23 %s\n" % dir)
        for file in files:
            lines.append("  -rw------- 1 pavpanchekha pavpanchekha %i 1992-08-20 17:23 %s\n" % (file_size(file), file))
        write("".join(lines))
        bytes = []
        bytecount = 0
        for line in lines:
            parts = line.strip().split(" ", 7)
            # -1 is newline
            bytes.append(bytecount + len(line) - len(parts[-1]) - 1)
            bytecount += len(line)
            bytes.append(bytecount - 1)
        write("//DIRED// " + " ".join(map(str, bytes)) + "\n")
        write("//DIRED-OPTIONS// --quoting-style literal\n")
    else:
        try:
            files, dirs = list_dir(args[-2]) # -2 is the 2>/dev/null
        except TypeError:
            write("//DIRED-OPTIONS// --quoting-style=literal\n")
            raise ErrorStatus(2)
        lines = []
        for dir in dirs:
            lines.append("drw------- 1 pavpanchekha pavpanchekha 4096 1992-08-20 17:23 %s\n" % dir)
        for file in files:
            lines.append("-rw------- 1 pavpanchekha pavpanchekha %s 1992-08-20 17:23 %s\n" % (file_size(file), file))
        write("".join(lines))

@command("tramp_perl_file_attributes")
def my_file_attributes(file, *args):
    if dir_exists(file):
        write("(t 23 0 0 (19962 30605) (19962 20114) (19962 20114) 4096.0 16877 t (0 . 511) -1)\n")
    elif file_exists(file):
        write("(nil 23 0 0 (19962 30605) (19962 20114) (19962 20114) %i.0 16877 t (0 . 511) -1)\n" % file_size(file))
    else:
        write("nil\n")

@command("tramp_perl_file_truename")
def my_truename(file, *args):
    write('"%s"\n' % file)

def classify_perl(s):
    if s == """'use File::Spec;' 2>/dev/null; echo tramp_exit_status $?\n""":
        write("tramp_exit_status 0\n") # Uhuh, File:Spec, got it, yeah
    elif s == """'use Cwd "realpath";' 2>/dev/null; echo tramp_exit_status $?\n""":
        write("tramp_exit_status 0\n") # Cwd? Sure
    else:
        write("...\n")

@command("tramp_perl_file_name_all_completions")
def my_complete(dir, start, lowercase):
    if lowercase:
        lc = lambda s: s.lower()
    else:
        lc = lambda s: s

    files, dirs = list_dir(dir)

    for file in files:
        if file.startswith(start):
            write(lc(file + "\n"))
    for dir in dirs:
        if dir.startswith(start):
            write(lc(dir + "/" + "\n"))
    write("ok\n")

@command("id")
def my_id(*args):
    write("0\n") # Everyone's root!  Yay!


def run_cmd(args):
    global LAST_STATUS, PROMPT
    try:
        if "/" in args[0]:
            args[0] = args[0].split("/")[-1] # Screw paths
        if args[0][0] == "\\":
            args[0] = args[1:] # What the hell, emacs...
        if ">" in args[0]:
            return # A pipeout, ignore it
    except:
        return

    if looks_like_variable_assignment.match(cmd):
        if cmd[:4] == "PS1=":
            PROMPT = cmd[4:].replace("\\", "")[:-1] # Shell escapes and end of line
        else:
            return # Emacs doesn't actually assign any other important variables
    elif args[0] in COMMANDS:
        try:
            COMMANDS[args[0]](*args[1:])
        except ErrorStatus, e:
            LAST_STATUS = e.args[0]
        else:
            LAST_STATUS = 0
    elif args[0] == "exit":
        sys.exit()
    else:
        write("...\n")

WHILING_AWAY = False
FUNCTIONAL = 0
if __name__ == "__main__":
    import shlex
    looks_like_variable_assignment = re.compile("^[0-9A-Za-z_]+=.*$")
    looks_like_were_looking_for_the_path_of = re.compile("while read d; do if test -x \\$d/(\\w+) -a -f \\$d/\\1; then echo tramp_executable \\$d/\\1; break; fi; done <<'EOF'\n")

    while True:
        write(PROMPT if not WHILING_AWAY and not FUNCTIONAL else "")
        cmd = read()

        if looks_like_were_looking_for_the_path_of.match(cmd): # TRAMP uses this to find program absolute names
            cmd = looks_like_were_looking_for_the_path_of.match(cmd).groups()[0]
            WHILING_AWAY = cmd
            continue # No emacs while loops are useful, trust me
        elif cmd[:3] == "EOF":
            WHILING_AWAY = False
            continue
        elif WHILING_AWAY:
            write("tramp_executable " + cmd[:-1] + "/" + WHILING_AWAY + "\n") # :-1 for newline
            continue
        
        if "{" in cmd:
            FUNCTIONAL += 1
            continue
        elif "}" in cmd:
            FUNCTIONAL -= 1
            if not FUNCTIONAL and ";" in cmd: # Extra command after end of function
                cmd = cmd[cmd.find(";")+1:]
            else:
                continue
        elif FUNCTIONAL:
            continue

        if cmd[:9] == "\\perl5 -e":
            classify_perl(cmd[10:])
            continue

        if ";" in cmd:
            cmds = cmd.split(";")
        else:
            cmds = [cmd]
        for cmd in cmds:
            try:
                args = shlex.split(cmd)
            except:
                continue
            
            run_cmd(args)
