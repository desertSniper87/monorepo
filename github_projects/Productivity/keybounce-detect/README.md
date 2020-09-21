# keydebounce-detect

A Linux utility to detect "bouncing" keys on computer keyboards.

Very small and simple, but could be of use to someone else who also has a keyboards suffering from "bouncing" keys.

## Background

Computer keyboards -- at least those with Cherry mechanical
keyswitches -- apparently tend to develop keys that "bounce" or
"chatter" -- that send two instances of a keystroke.

I've noticed it after about four years of using a Leopold FC200RT, but
this is not specific to this keyboard and is common across a number of
keyboards using Cherry keyswitches.

It's common for switches to electrically have a certain amount of
"bounce".  However, normally this is filtered out by the keyboard
controller -- I believe that with Cherry keyswitches, bounce is
normally expected to be under 5 milliseconds.  Whatever mechanical
issue arises with older Cherry keyswitches is not being caught by the
keyboard controller.

The issue arises on a key-by-key basis.  Some keys may not be
affected, and some may be.

There are several techniques for diagnosing the problem.  One
easily-done manual one is to simply hit a key, then the space key.
It's reasonably easy for a user to just hit one and then the other
fairly quickly, and then a bouncing key becomes easy to see:

Correctly-working keyboard:

> l l l l l l l l l l l l l l l l l l l l l l l l l l l l l l 

Keyboard with a bouncing 'l' key:

> l l l l l l l l ll l l l l l l l l l l l l l l l l l l l l l 

Unfortunately, this is still somewhat-error prone.  I also wanted a utility that could pick up on it without as much user effort.  Windows has built-in functionality ("Filter Keys") intended for handicapped users that can detect rapid double presses and ignore them; "real" keypresses come in at only a limited rate.  Xorg on Linux has similar functionality (accessed via various software packages calling XkbSetBounceKeysDelay()).

keydebounce-detect simply reads keypresses, and waits for rapid
duplicate keypresses.

## Compile

Type "make".

## Usage

Run "./keybounce-detect".

Then start whacking keys on the keyboard.  If one bounces, keybounce-detect will provide the name of the key and the delay between the duplicate keypresses.

I saw a delay of ~200000 microseconds on my bouncing key.

The default minimum-time-between-keypresses of 1000000 microseconds worked for me, but if you can hit your key more than ten times a second, you might want to use a different number; pass it as follows:

"./keybounce-detect 50000"

Once you've determined the time on the bounce, you can select an appropriate setting to have the current session of Xorg filter out the keybouncing via "xkbset bouncekeys <milliseconds>".

## Caveats

It's a terminal application, so it can't look for modifier keys.  Could be possible to make an X11 version.
