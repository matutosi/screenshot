Hi @karl-an and @stefansmr,

Sorry for the long wait, and thank you both for the reports. @stefansmr was
right: the command was not quoted, so a path with a space was split by the
shell. This is why the package failed when it was installed under
`C:/Program Files/...`, on a server and on a desktop alike.

I also have to correct my earlier suggestion. The `quote = TRUE` argument did
not fix it. It wrapped the whole command in single quotes:

```
'"C:/Program Files/.../screenshot.exe" "C:/.../sc_xxx.png"'
```

On Windows `system()` runs the command with `cmd.exe`, which does not treat a
single quote as a quoting character, so this failed as well. I confirmed both
failures here before fixing them.

The command is now built with `shQuote()` for the shell of the platform, which
gives this on Windows, and it works:

```
"C:/Program Files/.../screenshot.exe" "C:/.../sc_xxx.png"
```

The fix is in the development version. It will be in 0.9.3 on CRAN.

```r
remotes::install_github("matutosi/screenshot", ref = "main")
screenshot::screenshot()
```

`quote` is now deprecated and ignored, since the command is always quoted.
Passing it only warns, so old code keeps working.

## On running it on a server

@karl-an, about your original question. `screenshot.exe` captures the screen
with GDI, so it needs an interactive desktop session. On an AWS Windows
server:

* It works while you are logged in over RDP and run R inside that session.
  A machine with no physical display is fine, because the RDP session
  provides the desktop.
* It does not work after you disconnect the RDP session. The session is
  locked, and you get a black image or an error. If you need this, look into
  redirecting the session to the console with `tscon`.
* It does not work from a Windows service, or from a scheduled task set to
  "Run whether user is logged on or not". Those run in session 0, which has
  no interactive desktop. Use "Run only when user is logged on" instead.

On Linux, `gnome-screenshot` needs an X or Wayland session, so a headless
server needs something like `Xvfb`.

In short: it is fine while you are working in an RDP session, but it is not
suited to unattended, headless automation.

Please try the development version and let me know if anything is still off.
