<!-- -*- mode: markdown -*- -->

`dropbox.el`, A Dropbox Client for Emacs
========================================

[![MELPA](https://melpa.org/packages/dropbox-badge.svg)](https://melpa.org/#/dropbox)

`dropbox.el` provides lets your read and write Dropbox files from
Emacs.

Setting Up
----------

After downloading `dropbox.el`, you'll need to authorize it to connect
to your Dropbox. Run `customize-group`, with group `dropbox`; you
should see a field for your Dropbox "access token".

To create this token, navigate to the [Dropbox developers
console](https://www.dropbox.com/developers/apps/create) and click
through the questions to create a new Dropbox app. (Choose the
"Dropbox API" option and give it access to the full Dropbox.)

Click "Create App", and you'll be taken to the new app's details page.
Find the text "Generate Access Token" and click the "Generate" button
below it. Copy and paste the long random string you get in response
into the "access token" field in Emacs. Save the buffer, and you're
ready to use Dropbox.

Using `dropbox.el`
------------------

Once you have saved your access token, you can use `dropbox.el` by
simply attempting to open a file in any folder beginning with `/db:`.

For example, if you open the path `/db:`, you'll get a Dired window in
the root of your Dropbox folder. You can use many of the usual Dired
commands from here; you can also open files to edit them.

Unsupported
-----------

`dropbox.el` supports much of the functionality you'd expect from an
Emacs Dropbox client. A few otherwise-reasonable features aren't yet
supported.

+ Some file operations are still in progress (like file and directory copy and rename)
+ None of the `shell-command` or `process-file` variants are supported.
+ Not all optional arguments are implemented for many functions.
+ Symbolic and hard links aren't handled, though Dropbox must somehow handle them internally.
+ Race conditions abound. Don't use `dropbox.el` simultaneously on multiple machines.
+ The setup for this package is incredibly confusing.
