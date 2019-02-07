(in-package :dungen-render)

(b:define-component player-movement (:after (camera))
  (transform nil)
  (stage nil)
  (coords (m:vec3 1 2 0)))

(defmethod b:on-component-create ((self player-movement))
  (with-accessors ((game-state b:game-state)) self
    (setf (stage self) (b:cache-lookup game-state :world :stage1))))

(defmethod b:on-component-attach ((self player-movement))
  (with-accessors ((entity b:entity)) self
    (with-slots (%transform) self
      (b::switch-camera-target entity)
      (setf %transform (b:get-entity-component-by-type (b:entity self) 'b:transform)))))

(defmethod b:on-component-update ((self player-movement)))

;;; Definitions

(b:define-material sphere ()
  (:shader bs:sphere
   :uniforms (list :light.position (m:vec3 0 0.25 -1)
                   :light.ambient (m:vec4 0.01)
                   :light.diffuse (m:vec4 0.5)
                   :light.specular (m:vec4 1)
                   :material.ambient (m:vec4 0.01)
                   :material.diffuse (m:vec4 0.1 0.5 1)
                   :material.specular (m:vec4 1)
                   :material.shininess 40
                   :opacity 1.0)))

(b:define-prefab "player"
  (b:transform () :translate (m:vec3 2 1 1))
  ("body"
   (b:transform () :scale 0.75)
   (b:mesh () :file "sphere.glb")
   (b:render () :material 'sphere
                :mode :mesh)
   (player-movement ())))
