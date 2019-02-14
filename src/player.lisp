(in-package :dungen-render)

(b:define-component player-movement (:after (transform))
  (stage nil)
  (transform nil))

(defmethod b:on-component-create ((self player-movement))
  (with-accessors ((core b:core)) self
    (setf (stage self) (b:cache-lookup core :world :stage1))))

(defmethod b:on-component-attach ((self player-movement))
  (with-accessors ((core b:core) (entity b:entity)) self
    (setf (transform self) (b:get-entity-component entity 'b:transform))
    (b:switch-camera-target entity)
    (b:make-action entity
                   :type 'b:action/translate
                   :repeat-p t
                   :duration 0.8
                   :shape 'm:sine-in-out
                   :axis :z
                   :offset 0.2)))

(defmethod b:on-component-update ((self player-movement))
  (with-accessors ((core b:core) (entity b:entity)) self
    (with-slots (%stage %transform) self
      (au:when-let* ((direction
                      (cond
                        ((b:input-enter-p core '(:key :w))
                         (m:vec2 0 1))
                        ((b:input-enter-p core '(:key :a))
                         (m:vec2 -1 0))
                        ((b:input-enter-p core '(:key :s))
                         (m:vec2 0 -1))
                        ((b:input-enter-p core '(:key :d))
                         (m:vec2 1 0))))
                     (coords (m:vec2 (b:get-translation %transform))))
        (m:with-vec2 ((v (m:+ coords direction)))
          (when (and (not (bloom:find-action entity 'b:action/move-tile))
                     (dungen:carved-p (dungen:get-cell %stage v.x v.y)))
            (b:make-action entity
                           :type 'b:action/move-tile
                           :duration 1
                           :blocking-p t
                           :direction direction)))))))

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
  (b:transform () :translate (m:vec3 1 1 0.5)
                  :scale 0.5)
  (player-movement ())
  (b:mesh () :file "sphere.glb")
  (b:render () :material 'sphere
               :mode :mesh))
