(in-package :dungen-render)

(b:define-options ()
  :application-name 'dungen-render
  :title "Dungeon Renderer"
  :debug nil
  :window-width 1280
  :window-height 760
  :log-file-enable nil
  :default-scene 'dungeon)

(b:define-resource-paths dungen-render
  (:log "log"
   :image "data"
   :mesh "data"
   :misc "data"))

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
