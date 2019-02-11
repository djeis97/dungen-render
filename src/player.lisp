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
                   :duration 0.6
                   :shape 'm:sine-in-out
                   :axis :z
                   :offset 0.1)))
(defun player-close-to-wall-p (stage position direction)
  (game-math:with-vec3 ((pos position)
                        (dir direction))
    (let* ((wall.x (round (+ pos.x dir.x)))
           (wall.y (round (+ pos.y dir.y)))
           (tile (dungen:get-cell stage wall.x wall.y)))
      (if (dungen:feature-present-p tile :wall)
          (if (/= dir.x 0)
              (< (abs (- pos.x wall.x)) 0.75)
              (< (abs (- pos.y wall.y)) 0.75))))))

(defmethod b:on-component-update ((self player-movement))
  (with-accessors ((game-state b:game-state)
                   (stage stage)
                   (entity b:entity))
      self
    (let* ((transform (b:get-entity-component entity 'b:transform))
           (position (b::current (b::translation transform))))
      (game-math:with-vec2 ((tile (m:vec2 (m:round position))))
        (when (b:input-enabled-p game-state '(:key :w))
          (let ((dir (m:vec3 0 1 0)))
            (when (not (player-close-to-wall-p stage position dir))
              (b:translate-transform transform dir))))
        (when (b:input-enabled-p game-state '(:key :a))
          (let ((dir (m:vec3 -1 0 0)))
            (when (not (player-close-to-wall-p stage position dir))
              (b:translate-transform transform dir))))
        (when (b:input-enabled-p game-state '(:key :s))
          (let ((dir (m:vec3 0 -1 0)))
            (when (not (player-close-to-wall-p stage position dir))
              (b:translate-transform transform dir))))
        (when (b:input-enabled-p game-state '(:key :d))
          (let ((dir (m:vec3 1 0 0)))
            (when (not (player-close-to-wall-p stage position dir))
              (b:translate-transform transform dir))))))))

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
  (b:transform () :translate (m:vec3 2 1 0.5)
                  :scale 0.5)
  (player-movement ())
  (b:mesh () :file "sphere.glb")
  (b:render () :material 'sphere
               :mode :mesh))
