# Key Clash! A typing duel game.

Key Clash is like a real-time, two-player Tetris battle but for typing. Two players race through a document on separate computers, typing as fast and accurately as possible. As they progress, they build up Super Power, which can be unleashed to send a Challenge line to their opponent, making their job harder.

<video autoplay muted playsinline controls width="640">
  <source src="output.mp4" type="video/mp4">
  Your browser does not support the video tag.
</video>

This is a beta version with very minimal UI. Built in Erlang, Key Clash explores real-time concurrency and networking in a simple, terminal-based game. If you're learning Erlang or want to contribute, come join us and help level up the game!

Here's a [short rant](https://princengoc.github.io/key_clash.html) on my experience learning Erlang while coding this up. 

## How to run the game on your home computer network

You need two computers with [Erlang](https://www.erlang.org/) installed. 

1. Check ip addresses of the two computers

```
ip addr show
```
look for the ``inet6 192.168....`` line. Suppose node1 is at ``192.168.1.12`` and node2 is at ``192.168.1.70``. 

2. Start Erlang with node names and cookies. 

For example, on ``node1``, do
```
erl -name node1@192.168.1.12 -setcookie awesome
```
The cookie functions as a handshake password so that two nodes with the same cookie can communicate. 

On ``node2``, do
```
erl -name node2@192.168.1.70 -setcookie awesome
```

3. On each computer, compile
```
c(utils).
c(game_server).
```

4. Start the game in battle mode. 
```
game_server:start(battle).
```
then follow the instructions.

You can also start the game in single player mode. Then it is just a plain typing game. 
```
game_server:start(single). 
```

## Improvement ideas

* [ ] package with ``rebar3`` and simplify the startup process
* [ ] dynamically check the ``data/*.txt`` files instead of hard-code the list of available texts
* [ ] make the other player's screen visible on RHS
* [ ] validate the Challenge line, eg, only allow words from valid dictionary
* [ ] improve UI
* [ ] codes refactor