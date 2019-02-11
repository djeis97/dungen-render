(in-package :dungen-render)

(b:define-component world (:before (b:mesh))
  (name :world)
  (options nil)
  (cell-counts nil)
  (geometry nil)
  (shaders nil)
  (data nil))

(defclass map-cells ()
  ((%floors :accessor floors
            :initform nil)
   (%walls :accessor walls
           :initform nil)
   (%doors/v :accessor doors/v
             :initform nil)
   (%doors/h :accessor doors/h
             :initform nil)))

(defun get-cell-count (game-state cell-type)
  (let ((cell-counts (au:href (b:storage game-state) 'world-cell-counts)))
    (getf cell-counts cell-type)))

(defun get-cell-index (game-state cell-type)
  (declare (ignore game-state))
  (ecase cell-type
    (:floor 0)
    (:wall 1)
    (:door/v 2)
    (:door/h 3)))

(defun analyze-map-data (world)
  (with-accessors ((game-state b:game-state) (data data)) world
    (with-accessors ((grid dungen:grid) (options dungen:options)) data
      (let ((width (dungen:width options))
            (height (dungen:height options))
            (cells (make-instance 'map-cells)))
        (dotimes (x width)
          (dotimes (y height)
            (let* ((index (+ (* y width) x))
                   (cell (aref grid index))
                   (vec (m:vec2 x y)))
              (push vec (floors cells))
              (cond
                ((dungen:feature-present-p cell :wall)
                 (push vec (walls cells)))
                ((dungen:feature-present-p cell :door-vertical)
                 (push vec (doors/v cells)))
                ((dungen:feature-present-p cell :door-horizontal)
                 (push vec (doors/h cells)))))))
        (setf (au:href (b:storage game-state) 'world-cell-counts)
              (list :floor (length (floors cells))
                    :wall (length (walls cells))
                    :door/v (length (doors/v cells))
                    :door/h (length (doors/h cells))))
        cells))))

(defmethod b:write-shader-buffer (buffer (object world))
  (destructuring-bind (&key width height &allow-other-keys) (options object)
    (let ((cells (analyze-map-data object)))
      (shadow:write-buffer-path buffer :size (list (m:vec2 width height)))
      (shadow:write-buffer-path buffer :floors (floors cells))
      (shadow:write-buffer-path buffer :walls (walls cells))
      (shadow:write-buffer-path buffer :doors/v (doors/v cells))
      (shadow:write-buffer-path buffer :doors/h (doors/h cells)))))

(defun make-world-data (world)
  (with-accessors ((game-state b:game-state)) world
    (with-slots (%name %shaders %options %data) world
      (setf %data (apply #'dungen:make-stage %options))
      (au:mvlet ((blocks binding (b:make-shader-blocks game-state %shaders :world)))
        (b:make-shader-buffer %name (first blocks) binding world)
        %data))))

;;; Component event hooks

(defmethod b:on-component-create ((component world))
  (with-accessors ((game-state b:game-state)) component
    (with-slots (%name %geometry %data) component
      (setf %data (b:cache-lookup game-state :world %name
                    (make-world-data component))))))

;;; Definitions

(b:define-material world ()
  (:shader bs:world
   :uniforms (list :light.position (m:vec3 0.25 -0.125 -1)
                   :light.ambient (m:vec4 0.01)
                   :light.diffuse (m:vec4 0.5)
                   :light.specular (m:vec4 0.5)
                   :material.ambient (m:vec4 0.01)
                   :material.diffuse (m:vec4 1)
                   :material.specular (m:vec4 1)
                   :material.shininess 30
                   :opacity 1.0
                   :generate-texture-p nil)))

(b:define-material floor (world)
  (:uniforms (list :cell-type (lambda (x) (get-cell-index x :floor)))))

(b:define-material wall (world)
  (:uniforms (list :cell-type (lambda (x) (get-cell-index x :wall)))))

(b:define-prefab "world-tile"
  (b:transform () :translate (m:vec3 0 0 0.75)
                  :scale (m:vec3 0.5 0.5 0.75))
  (b:mesh () :file "wall.glb"
             :instances (lambda (x) (get-cell-count x :wall)))
  (b:render () :material 'wall
               :mode :mesh))

(b:define-prefab "world"
  (b:transform () :scale 120)
  (world () :name :stage1
            :shaders '(bs:world)
            :options '(:width 49 :height 49 :seed 5))
  (("walls" (:link "/world-tile")))
  (("floors" (:link "/world-tile"))
   (b:transform (:policy :new-type)
                :scale (m:vec3 0.5 0.5 0.1))
   (b:mesh (:policy :new-type)
           :file "floor.glb"
           :instances (lambda (x) (get-cell-count x :floor)))
   (b:render (:policy :new-args)
             :material 'floor)))
