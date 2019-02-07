(in-package :bloom.shader)

(define-struct world-data
  (size :vec2 :accessor size)
  (walls (:vec2 65536))
  (floors (:vec2 65536))
  (doors/v (:vec2 65536))
  (doors/h (:vec2 65536)))

(define-function get-cell-coords ((world world-data)
                                  (cell-type :int))
  (with-slots (walls floors doors/v doors/h) world
    (case cell-type
      (1 (aref walls gl-instance-id))
      (2 (aref doors/v gl-instance-id))
      (3 (aref doors/h gl-instance-id))
      (otherwise (aref floors gl-instance-id)))))

(define-struct light/directional
  (position :vec3 :accessor position)
  (ambient :vec4 :accessor ambient)
  (diffuse :vec4 :accessor diffuse)
  (specular :vec4 :accessor specular))

(define-struct material-data
  (ambient :vec4 :accessor ambient)
  (diffuse :vec4 :accessor diffuse)
  (specular :vec4 :accessor specular)
  (shininess :float :accessor shininess))

(define-function get-lighting-vectors ((view :mat4)
                                       (model :mat4)
                                       (normal :vec3))
  (let ((normal-mat (transpose (inverse (mat3 (* view model))))))
    (values (normalize (* normal-mat normal))
            (normalize (.xyz (aref (inverse view) 3))))))

(define-function world/v ((mesh-attrs mesh-attrs)
                          &uniform
                          (model :mat4)
                          (view :mat4)
                          (proj :mat4)
                          (cell-type :int)
                          (world world-data :ssbo :std-430))
  (with-slots (mesh/pos mesh/normal mesh/uv1) mesh-attrs
    (mvlet* ((normal to-camera (get-lighting-vectors view model mesh/normal))
             (coords (get-cell-coords world cell-type))
             (offset (vec3 (* coords 2) 0))
             (model-pos (vec3 (* model (vec4 (+ offset mesh/pos) 1)))))
      (values (* proj view (vec4 model-pos 1))
              mesh/uv1
              model-pos
              normal
              to-camera))))

(define-function calculate-lighting ((light light/directional)
                                     (material material-data)
                                     (to-camera :vec3)
                                     (normal :vec3))
  (with-accessors ((light/position position)
                   (light/ambient ambient)
                   (light/diffuse diffuse)
                   (light-specular specular))
      light
    (with-accessors ((material/ambient ambient)
                     (material/diffuse diffuse)
                     (material/specular specular)
                     (material/shininess shininess))
        material
      (let* ((light-from-dir (- (normalize light/position)))
             (normal-dir (normalize normal))
             (camera-dir (normalize to-camera))
             (n-dot-l (dot normal-dir light-from-dir))
             (ambient (* material/ambient light/ambient))
             (diffuse-factor (clamp n-dot-l 0 1))
             (diffuse (* material/diffuse light/diffuse diffuse-factor))
             (specular-factor 0.0))
        (when (plusp n-dot-l)
          (let ((light-camera-half (normalize (+ light-from-dir camera-dir))))
            (setf specular-factor (pow (dot normal-dir light-camera-half)
                                       material/shininess))))
        (let ((specular (* light-specular specular-factor)))
          (vec3 (+ ambient diffuse specular)))))))

(define-function generate-texture/floor ((pos :vec3))
  (let* ((base-1 (vec3 (+ (* 0.25 (umbra.noise:perlin-surflet (* pos 0.014)))
                          (* 0.4 (umbra.noise:cellular-fast (* pos 0.08)))
                          (* 0.4 (umbra.noise:cellular-fast (* pos 0.06)))
                          (* 0.3 (umbra.noise:simplex-perlin (* pos 0.5))))))
         (base-1 (umbra.color:set-brightness base-1 -0.4))
         (base-1 (umbra.color:set-gamma base-1 3))
         (base-1 (umbra.color:set-contrast base-1 0.9))
         (base-2 (vec3 (umbra.noise:simplex-perlin (* pos 0.0088))))
         (base-2 (umbra.color:set-contrast base-2 0.24))
         (base (mix base-1 base-2 0.5))
         (base-color (* base (vec3 0 0.17 0.29)))
         (base-color (umbra.color:set-saturation base-color 0.65))
         (base-color (umbra.color:set-contrast base-color 1.3)))
    (+ base base-color)))

(define-function generate-texture/wall ((pos :vec3))
  (let* ((base-1 (vec3 (+ (* 0.55
                             (umbra.noise:simplex-perlin (vec3 (* pos 0.18))))
                          (* 0.25
                             (umbra.noise:simplex-perlin (vec3 (* pos 0.58))))
                          (* 0.65
                             (umbra.noise:cellular-fast (vec3 (* pos 0.046)))))))
         (base-2 (vec3 (* 0.5
                          (umbra.noise:simplex-perlin (vec3 (* pos 0.0028))))))
         (base-2 (umbra.color:set-contrast base-2 0.4))
         (base (mix base-1 base-2 0.5))
         (base-color (umbra.color:set-saturation
                      (* base (vec3 0 0.21 0.29)) 0.6))
         (base-color (umbra.color:set-contrast base-color 1.25)))
    (umbra.color:set-contrast
     (+ base base-color)
     1.5)))

(define-function generate-texture ((cell-type :int)
                                   (frag-pos :vec3)
                                   (generate-texture-p :bool))
  (case cell-type
    (0 (if generate-texture-p
           (generate-texture/floor frag-pos)
           (vec3 0.4)))
    (1 (if generate-texture-p
           (generate-texture/wall frag-pos)
           (vec3 0.2)))
    (otherwise (vec3 0))))

(define-function world/f ((uv :vec2)
                          (pos :vec3)
                          (normal :vec3)
                          (to-camera :vec3)
                          &uniform
                          (cell-type :int)
                          (light light/directional)
                          (material material-data)
                          (opacity :float)
                          (generate-texture-p :bool))
  (let ((lighting (calculate-lighting light material to-camera normal))
        (texture (generate-texture cell-type pos generate-texture-p)))
    (vec4 (mix texture lighting 0.5) opacity)))

(define-shader world ()
  (:vertex (world/v mesh-attrs))
  (:fragment (world/f :vec2 :vec3 :vec3 :vec3)))

(define-function sphere/v ((mesh-attrs mesh-attrs)
                           &uniform
                           (model :mat4)
                           (view :mat4)
                           (proj :mat4))
  (with-slots (mesh/pos mesh/uv1 mesh/normal) mesh-attrs
    (mvlet* ((normal to-camera (get-lighting-vectors view model mesh/normal)))
      (values (* proj view model (vec4 mesh/pos 1))
              mesh/uv1
              normal
              to-camera))))

(define-function sphere/f ((uv :vec2)
                           (normal :vec3)
                           (to-camera :vec3)
                           &uniform
                           (light light/directional)
                           (material material-data)
                           (opacity :float))
  (let ((lighting (calculate-lighting light material to-camera normal)))
    (vec4 (mix (vec3 0 0.4 1) lighting 0.5) opacity)))

(define-shader sphere ()
  (:vertex (sphere/v mesh-attrs))
  (:fragment (sphere/f :vec2 :vec3 :vec3)))
