<!-- -*- mode: markdown -*- -->

`dropbox.el`, A Dropbox Client for Emacs
========================================

It's often joked that Emacs is an operating system; well, it might as well have a Dropbox client.

`dropbox.el` provides a partial Dropbox client for Emacs.  You can log in to Dropbox and read and write your files.  Since it implements handlers for all of the normal Emacs file operations, `dropbox.el` can also be considered a Dropbox API client in Emacs Lisp.

`dropbox.el` is available in the [Marmalade](https://marmalade-repo.org) repository, under the name "`dropbox`".

Setting Up
----------

The first step is to get `dropbox.el`; the easiest way is to use the Marmalade repository, but you can also just download the `dropbox.el` file from this very project.  However, *before you use it*, you'll need to set up a few configuration variables.  `customize-group`, with group `dropbox`, will take you to a configuration screen.

The key step is to set up the "Consumer Key" and "Consumer Secret" variables. Unfortunately, Dropbox uses OAuth 1.0, which isn't particularly friendly to open source applications since it requires keeping a key secret.  Instead, you'll have to create your own, private application key and secret, and tell `dropbox.el` to use these.

To do this, navigate over to the [Dropbox developers console](https://www.dropbox.com/developers/apps/create).  You'll want to create an application for the "Core" API.  Any name at all works for the application name, and you want to give it access to your full Dropbox.

Click "Create App", and you'll be taken to a page that includes the "App key" and "App secret".  You'll want to enter these into the corresponding fields in the Customize window, and save the buffer.  You are, of course, free to ignore Customize and just stick the necessary settings into your `init.el` instead.

Once you have a consumer key and secret set up (and don't worry, you only need to do this once), you can run `dropbox-connect` to get a special token from Dropbox and begin using the API.  The token is stored, by default, in `~/.emacs.d/dropbox-token`, but you can change this with the `dropbox-token-file` variable.  `dropbox-connect` will cause you to browse to a web page asking you to allow the application to use your Dropbox; allow it to, and you're done.

Using `dropbox.el`
------------------

Once connected, you can use `dropbox.el` by simply attempting to open a file in any folder beginning with `/db:`.  In particular, if you browse to `/db:`, you'll get a Dired window open onto the root of your Dropbox folder.  You can use many of the usual Dired commands from here; you can also open files and read and write them.

Unsupported
-----------

`dropbox.el` supports much of the functionality you'd expect from an Emacs Dropbox client.  A few otherwise-reasonable features aren't yet supported.

 + None of the `shell-command` or `process-file` variants are supported, though it might be possible to implement them by downloading local copies.
 + Not all optional arguments are implemented for many function, though the most commonly-used functionality is there.
 + Image files seem to have strange problems.
 + Symbolic and hard links aren't handled, though Dropbox must somehow handle them internally.
 + Operations aren't guaranteed to have strange race conditions.  Dropbox doesn't yet provide sufficient API support to properly fix this.
 + The setup for this package is incredibly confusing.
