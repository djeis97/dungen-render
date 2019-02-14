(in-package :dungen-render)

(b:define-prefab "dungeon"
  ("camera"
   (b:camera () :mode :isometric
                :clip-near -1024
                :clip-far 1024
                :zoom 1))
  (("world" (:link "/world"))
   (("player" (:link "/player")))))

(b:define-scenes
  (dungeon "dungeon"))
