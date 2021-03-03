# SpotifyConnectionVerifier

In 2020 there were a number of episodes where trying to change the active Spotify playback device would return 400.  I contacted Spotify a number of times
over Twitter, but most of the time they told me if was user error, even though their own api was returning 400 when requested from their own app.

I decided to make this app to list the devices and run through them, activating them one by one and reporting success or failure.
I started building it in Elm because I wanted to see what a functional UI language was like.

**Status 2020:** The app logs in and displays all the devices.  There is little to no styling right now.

### Disclaimer

The entire authentication flow was copied from the elm/oath2 package github then modified for this apps needs. 
