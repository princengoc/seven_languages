# Dorothy the Wizard Hunter: ``pacman`` on an infinite board
s
Dorothy is looking for the Wizard in the land of Oz, while monsters lurk about. Would she be able to find the Wizard in time? 

This game is a simple treasure hunt on an infinite board. I'm inspired by the minimal graphics and monster movments of of ``pacman``. 

It is really a ``Clojure`` capstone project, where concurrency is dealt with using ``atom`` and ``add-watch``. And here's a rant on [my Clojure experience](https://princengoc.github.io/dorothy.html). 

**To run**

``lein trampoline run``

the trampoline option is needed for ``jline3`` to work with ``lein``, as [explained here](https://stackoverflow.com/questions/56416242/lein-causes-jline3-terminal-to-be-dumb). 


## Improvement ideas

* [ ] Make the wizard block bigger
* [ ] Add the logic to count the number of steps
* [ ] Generate a variant with fixed maze, pacman-style
* [ ] Do the refs/agents implementation for comparison
* [ ] Figure out why sometimes the screen did not clear before updates