(in-package :dungen-render)

(b:define-component player-movement (:after (transform))
  (stage nil))

(defmethod b:on-component-create ((self player-movement))
  (with-accessors ((game-state b:game-state)) self
    (setf (stage self) (b:cache-lookup game-state :world :stage1))))

(defmethod b:on-component-attach ((self player-movement))
  (with-accessors ((game-state b:game-state) (entity b:entity)) self
    (b:switch-camera-target entity)
    (b:make-action entity
                   :type 'b:action/translate
                   :repeat-p t
                   :duration 0.8
                   :shape 'm:sine-in-out
                   :axis :z
                   :offset 0.2)))

(defmethod b:on-component-update ((self player-movement))
  (with-accessors ((game-state b:game-state)) self
    (cond
      ((b:input-enter-p game-state '(:key :w))
       nil)
      ((b:input-enter-p game-state '(:key :a))
       nil)
      ((b:input-enter-p game-state '(:key :s))
       nil)
      ((b:input-enter-p game-state '(:key :d))
       nil))))

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
