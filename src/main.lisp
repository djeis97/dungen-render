(in-package :dungen-render)

(b:define-project dungen-render
  :title "Dungeon Renderer"
  :debug t
  :vsync :on
  :window-width 1280
  :window-height 760
  :physics-delta 1/20
  :log-file-enable nil
  :default-scene 'dungeon)

(b:define-resource-paths dungen-render
  (:log "log"
   :image "data"
   :mesh "data"
   :misc "data"))

