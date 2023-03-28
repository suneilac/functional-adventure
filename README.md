
# Functional Adventure Final Project
### Suneil Acharya

### PROJECT STRUCTURE
```
.
├── app
│   └── Main.hs
├── functional-adventure.cabal
├── instructions.md
├── package.yaml
├── README.txt
├── src
│   ├── Command.hs
│   ├── Direction.hs
│   ├── Example.hs
│   ├── GameIO.hs
│   ├── GameState.hs
│   ├── Item.hs
│   ├── Main.hs
│   ├── Player.hs
│   └── Room.hs
├── stack.yaml
├── stack.yaml.lock
└── test
    └── Spec.hs
```

### INSTRUCTIONS
The feature I chose to add was a lantern and dark rooms, where you cannot see the items in the room without a lantern.
The first level of the game plays normally, and no lantern is implemented. After completing the first level (taking the
jug to the yard), the second level starts you in a dark room. If you try to "look" in the room, you cannot see the name
of the room nor can you see the items in the room. Once you pick up the lantern, you will be able to see both these features
and the game will play similarly to level 1. The objective of this level is the same as the previous level - to take the 
jug to the yard. However, since the lantern weighs as much as the max weight the player can hold, you have to find the
jug and yard first, and then put down the lantern to pick up the jug and take it to the yard. Putting down the lantern
will make the room dark again.