#include <math.h>
#include <float.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
//#include "utils.h"
#include "graphene-matrix.h"

#define TRUE 1
#define FALSE 0

typedef struct _graphene_vec2_t         graphene_vec2_t;
typedef struct _graphene_vec3_t         graphene_vec3_t;
typedef struct _graphene_vec4_t         graphene_vec4_t;

typedef struct _graphene_matrix_t       graphene_matrix_t;

typedef struct _graphene_point_t        graphene_point_t;
typedef struct _graphene_size_t         graphene_size_t;
typedef struct _graphene_rect_t         graphene_rect_t;

typedef struct _graphene_point3d_t      graphene_point3d_t;
typedef struct _graphene_quad_t         graphene_quad_t;
typedef struct _graphene_quaternion_t   graphene_quaternion_t;
typedef struct _graphene_euler_t        graphene_euler_t;

typedef struct _graphene_plane_t        graphene_plane_t;
typedef struct _graphene_frustum_t      graphene_frustum_t;
typedef struct _graphene_sphere_t       graphene_sphere_t;
typedef struct _graphene_box_t          graphene_box_t;
typedef struct _graphene_triangle_t     graphene_triangle_t;
typedef struct _graphene_ray_t          graphene_ray_t;

#include "graphene-simd4f.h"

#define GRAPHENE_PI_2           1.5707963267948966192313217f
#define GRAPHENE_PI M_PI
#define GRAPHENE_DEG_TO_RAD(x)          ((x) * (GRAPHENE_PI / 180.f))
#define GRAPHENE_RAD_TO_DEG(x)          ((x) * (180.f / GRAPHENE_PI))
#define GRAPHENE_FLOAT_EPSILON FLT_EPSILON

/**
 * graphene_point3d_init:
 * @p: the #graphene_point3d_t to initialize
 * @x: the X coordinate of the point
 * @y: the Y coordinate of the point
 * @z: the Z coordinate of the point
 *
 * Initializes a #graphene_point3d_t with the given coordinates.
 *
 * Returns: (transfer none): the initialized #graphene_point3d_t
 *
 * Since: 1.0
 */
graphene_point3d_t *
graphene_point3d_init (graphene_point3d_t *p,
                       float               x,
                       float               y,
                       float               z)
{
  p->x = x;
  p->y = y;
  p->z = z;
  return p;
}

graphene_vec4_t *       graphene_vec4_init              (graphene_vec4_t       *v,
                                                         float                  x,
                                                         float                  y,
                                                         float                  z,
                                                         float                  w);

graphene_vec4_t *
graphene_vec4_init (graphene_vec4_t *v,
                    float            x,
                    float            y,
                    float            z,
                    float            w)
{
  v->value = graphene_simd4f_init (x, y, z, w);

  return v;
}

void
graphene_simd4x4f_transpose_in_place (graphene_simd4x4f_t *s)
{
  graphene_simd4x4f_t m = *s;

  s->x.x = m.x.x;
  s->x.y = m.y.x;
  s->x.z = m.z.x;
  s->x.w = m.w.x;

  s->y.x = m.x.y;
  s->y.y = m.y.y;
  s->y.z = m.z.y;
  s->y.w = m.w.y;

  s->z.x = m.x.z;
  s->z.y = m.y.z;
  s->z.z = m.z.z;
  s->z.w = m.w.z;

  s->w.x = m.x.w;
  s->w.y = m.y.w;
  s->w.z = m.z.w;
  s->w.w = m.w.w;
}

/**
 * graphene_simd4x4f_t:
 *
 * A SIMD-based matrix type that uses four #graphene_simd4f_t vectors.
 *
 * The matrix is treated as row-major, i.e. the x, y, z, and w vectors
 * are rows, and elements of each vector are a column:
 *
 * |[<!-- language="C" -->
 *   graphene_simd4x4f_t = {
 *     x.x, x.y, x.z, x.w,
 *     y.x, y.y, y.z, y.w,
 *     z.x, z.y, z.z, z.w,
 *     w.x, w.y, w.z, w.w
 *   }
 * ]|
 *
 * The contents of the #graphene_simd4x4f_t type are private and
 * cannot be accessed directly; use the provided API instead.
 *
 * Since: 1.0
 */

/**
 * graphene_simd4x4f_init:
 * @x: a #graphene_simd4f_t for the first row
 * @y: a #graphene_simd4f_t for the second row
 * @z: a #graphene_simd4f_t for the third row
 * @w: a #graphene_simd4f_t for the fourth row
 *
 * Creates a new #graphene_simd4x4f_t using the given row vectors
 * to initialize it.
 *
 * Returns: the newly created #graphene_simd4x4f_t
 *
 * Since: 1.0
 */
static inline graphene_simd4x4f_t 
graphene_simd4x4f_init (graphene_simd4f_t x,
                        graphene_simd4f_t y,
                        graphene_simd4f_t z,
                        graphene_simd4f_t w)
{
  graphene_simd4x4f_t s;

  s.x = x;
  s.y = y;
  s.z = z;
  s.w = w;

  return s;
}

/**
 * graphene_simd4x4f_init_identity:
 * @m: a #graphene_simd4x4f_t
 *
 * Initializes @m to be the identity matrix.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_init_identity (graphene_simd4x4f_t *m)
{
  *m = graphene_simd4x4f_init (graphene_simd4f_init (1.0f, 0.0f, 0.0f, 0.0f),
                               graphene_simd4f_init (0.0f, 1.0f, 0.0f, 0.0f),
                               graphene_simd4f_init (0.0f, 0.0f, 1.0f, 0.0f),
                               graphene_simd4f_init (0.0f, 0.0f, 0.0f, 1.0f));
}

/**
 * graphene_simd4x4f_init_from_float:
 * @m: a #graphene_simd4x4f_t
 * @f: (array fixed-size=16): an array of 16 floating point values
 *
 * Initializes a #graphene_simd4x4f_t with the given array
 * of floating point values.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_init_from_float (graphene_simd4x4f_t *m,
                                   const float         *f)
{
  m->x = graphene_simd4f_init_4f (f +  0);
  m->y = graphene_simd4f_init_4f (f +  4);
  m->z = graphene_simd4f_init_4f (f +  8);
  m->w = graphene_simd4f_init_4f (f + 12);
}

/**
 * graphene_simd4x4f_to_float:
 * @m: a #graphene_sidm4x4f_t
 * @v: (out caller-allocates) (array fixed-size=16): a floating
 *   point values vector capable of holding at least 16 values
 *
 * Copies the content of @m in a float array.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_to_float (const graphene_simd4x4f_t *m,
                            float                     *v)
{
  graphene_simd4f_dup_4f (m->x, v +  0);
  graphene_simd4f_dup_4f (m->y, v +  4);
  graphene_simd4f_dup_4f (m->z, v +  8);
  graphene_simd4f_dup_4f (m->w, v + 12);
}

/**
 * graphene_simd4x4f_sum:
 * @a: a #graphene_simd4f_t
 * @res: (out): return location for the sum vector
 *
 * Adds all the row vectors of @a.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_sum (const graphene_simd4x4f_t *a,
                       graphene_simd4f_t         *res)
{
  graphene_simd4f_t s = graphene_simd4f_add (a->x, a->y);
  s = graphene_simd4f_add (s, a->z);
  s = graphene_simd4f_add (s, a->w);
  *res = s;
}

/**
 * graphene_simd4x4f_vec4_mul:
 * @a: a #graphene_simd4x4f_t
 * @b: a #graphene_simd4f_t
 * @res: (out): return location for a #graphene_simd4f_t
 *
 * Multiplies the given #graphene_simd4x4f_t with the given
 * #graphene_simd4f_t using a dot product.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_vec4_mul (const graphene_simd4x4f_t *a,
                            const graphene_simd4f_t   *b,
                            graphene_simd4f_t         *res)
{
  const graphene_simd4f_t v = *b;
  const graphene_simd4f_t v_x = graphene_simd4f_splat_x (v);
  const graphene_simd4f_t v_y = graphene_simd4f_splat_y (v);
  const graphene_simd4f_t v_z = graphene_simd4f_splat_z (v);
  const graphene_simd4f_t v_w = graphene_simd4f_splat_w (v);

  *res = graphene_simd4f_add (graphene_simd4f_add (graphene_simd4f_mul (a->x, v_x),
                                                   graphene_simd4f_mul (a->y, v_y)),
                              graphene_simd4f_add (graphene_simd4f_mul (a->z, v_z),
                                                   graphene_simd4f_mul (a->w, v_w)));
}

/**
 * graphene_simd4x4f_vec3_mul:
 * @m: a #graphene_simd4x4f_t
 * @v: a #graphene_simd4f_t
 * @res: (out): return location for a #graphene_simd4f_t
 *
 * Multiplies the given #graphene_simd4x4f_t with the given
 * #graphene_simd4f_t, using only the first three row vectors
 * of the matrix, and the first three components of the vector.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_vec3_mul (const graphene_simd4x4f_t *m,
                            const graphene_simd4f_t   *v,
                            graphene_simd4f_t         *res)
{
  const graphene_simd4f_t v_x = graphene_simd4f_splat_x (*v);
  const graphene_simd4f_t v_y = graphene_simd4f_splat_y (*v);
  const graphene_simd4f_t v_z = graphene_simd4f_splat_z (*v);

  *res = graphene_simd4f_add (graphene_simd4f_add (graphene_simd4f_mul (m->x, v_x),
                                                   graphene_simd4f_mul (m->y, v_y)),
                              graphene_simd4f_mul (m->z, v_z));
}

/**
 * graphene_simd4x4f_point3_mul:
 * @m: a #graphene_simd4x4f_t
 * @p: a #graphene_simd4f_t
 * @res: (out): return location for a #graphene_simd4f_t
 *
 * Multiplies the given #graphene_simd4x4f_t with the given
 * #graphene_simd4f_t.
 *
 * Unlike graphene_simd4x4f_vec3_mul(), this function will
 * also use the fourth row vector of the matrix.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_point3_mul (const graphene_simd4x4f_t *m,
                              const graphene_simd4f_t   *p,
                              graphene_simd4f_t         *res)
{
  const graphene_simd4f_t v = *p;
  const graphene_simd4f_t v_x = graphene_simd4f_splat_x (v);
  const graphene_simd4f_t v_y = graphene_simd4f_splat_y (v);
  const graphene_simd4f_t v_z = graphene_simd4f_splat_z (v);

  *res = graphene_simd4f_add (graphene_simd4f_add (graphene_simd4f_mul (m->x, v_x),
                                                   graphene_simd4f_mul (m->y, v_y)),
                              graphene_simd4f_add (graphene_simd4f_mul (m->z, v_z),
                                                   m->w));
}

/**
 * graphene_simd4x4f_transpose:
 * @s: a #graphene_simd4x4f_t
 * @res: (out): return location for the transposed matrix
 *
 * Transposes the given #graphene_simd4x4f_t.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_transpose (const graphene_simd4x4f_t *s,
                             graphene_simd4x4f_t       *res)
{
  *res = *s;
  graphene_simd4x4f_transpose_in_place (res);
}

/**
 * graphene_simd4x4f_inv_ortho_vec3_mul:
 * @a: a #graphene_simd4x4f_t
 * @b: a #graphene_simd4f_t
 * @res: (out): return location for the transformed vector
 *
 * Performs the inverse orthographic transformation of the first
 * three components in the given vector, using the first three
 * row vectors of the given SIMD matrix.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_inv_ortho_vec3_mul (const graphene_simd4x4f_t *a,
                                      const graphene_simd4f_t   *b,
                                      graphene_simd4f_t         *res)
{
  graphene_simd4x4f_t transpose = *a;
  graphene_simd4f_t translation = *b;

  transpose.w = graphene_simd4f_init (0.f, 0.f, 0.f, 0.f);
  graphene_simd4x4f_transpose_in_place (&transpose);

  graphene_simd4x4f_vec3_mul (&transpose, &translation, res);
}

/**
 * graphene_simd4x4f_inv_ortho_point3_mul:
 * @a: a #graphene_simd4x4f_t
 * @b: a #graphene_simd4x4f_t
 * @res: (out): return location for the result vector
 *
 * Performs the inverse orthographic transformation of the first
 * three components in the given vector, using the given SIMD
 * matrix.
 *
 * Unlike graphene_simd4x4f_inv_ortho_vec3_mul(), this function
 * will also use the fourth row vector of the SIMD matrix.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_inv_ortho_point3_mul (const graphene_simd4x4f_t *a,
                                        const graphene_simd4f_t   *b,
                                        graphene_simd4f_t         *res)
{
  graphene_simd4f_t translation = graphene_simd4f_sub (*b, a->w);
  graphene_simd4x4f_t transpose = *a;

  transpose.w = graphene_simd4f_init (0.f, 0.f, 0.f, 0.f);
  graphene_simd4x4f_transpose_in_place (&transpose);

  graphene_simd4x4f_point3_mul (&transpose, &translation, res);
}

/**
 * graphene_simd4x4f_matrix_mul:
 * @a: a #graphene_simd4x4f_t
 * @b: a #graphene_simd4x4f_t
 * @res: (out): return location for the result
 *
 * Multiplies the two matrices.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_matrix_mul (const graphene_simd4x4f_t *a,
                              const graphene_simd4x4f_t *b,
                              graphene_simd4x4f_t       *res)
{
  /* this is an optimized version of the matrix multiplication, using
   * four dot products for each row vector. this yields drastically
   * better numbers while retaining the same correct results as above:
   * the scalar implementation now clocks at 91ns; the GCC vector
   * implementation is 19ns; and the SSE implementation is 16ns.
   *
   * the order is correct if we want to multiply A with B; remember
   * that matrix multiplication is non-commutative.
   */
  graphene_simd4x4f_vec4_mul (b, &a->x, &res->x);
  graphene_simd4x4f_vec4_mul (b, &a->y, &res->y);
  graphene_simd4x4f_vec4_mul (b, &a->z, &res->z);
  graphene_simd4x4f_vec4_mul (b, &a->w, &res->w);
}

/**
 * graphene_simd4x4f_init_perspective:
 * @m: a #graphene_simd4x4f_t
 * @fovy_rad: the angle of the field of vision, in radians
 * @aspect: the aspect value
 * @z_near: the depth of the near clipping plane
 * @z_far: the depth of the far clipping plane
 *
 * Initializes a #graphene_simd4x4f_t with a perspective projection.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_init_perspective (graphene_simd4x4f_t *m,
                                    float                fovy_rad,
                                    float                aspect,
                                    float                z_near,
                                    float                z_far)
{
  float delta_z = z_far - z_near;
  float cotangent = tanf (GRAPHENE_PI_2 - fovy_rad * 0.5f);

  float a = cotangent / aspect;
  float b = cotangent;
  float c = -(z_far + z_near) / delta_z;
  float d = -2 * z_near * z_far / delta_z;

  m->x = graphene_simd4f_init (   a, 0.0f, 0.0f,  0.0f);
  m->y = graphene_simd4f_init (0.0f,    b, 0.0f,  0.0f);
  m->z = graphene_simd4f_init (0.0f, 0.0f,    c, -1.0f );
  m->w = graphene_simd4f_init (0.0f, 0.0f,    d,  0.0f);
}

/**
 * graphene_simd4x4f_init_ortho:
 * @m: a #graphene_simd4x4f_t
 * @left: edge of the left clipping plane
 * @right: edge of the right clipping plane
 * @bottom: edge of the bottom clipping plane
 * @top: edge of the top clipping plane
 * @z_near: depth of the near clipping plane
 * @z_far: depth of the far clipping plane
 *
 * Initializes the given SIMD matrix with an orthographic projection.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_init_ortho (graphene_simd4x4f_t *m,
                              float                left,
                              float                right,
                              float                bottom,
                              float                top,
                              float                z_near,
                              float                z_far)
{
  float delta_x = right - left;
  float delta_y = top - bottom;
  float delta_z = z_far - z_near;

  float a = 2.0f / delta_x;
  float b = -(right + left) / delta_x;
  float c = 2.0f / delta_y;
  float d = -(top + bottom) / delta_y;
  float e = -2.0f / delta_z;
  float f = -(z_far + z_near) / delta_z;

  m->x = graphene_simd4f_init (   a, 0.0f, 0.0f, 0.0f);
  m->y = graphene_simd4f_init (0.0f,    c, 0.0f, 0.0f);
  m->z = graphene_simd4f_init (0.0f, 0.0f,    e, 0.0f);
  m->w = graphene_simd4f_init (   b,    d,    f, 1.0f);
}

/**
 * graphene_simd4x4f_init_look_at:
 * @m: a #graphene_simd4x4f_t
 * @eye: vector for the camera coordinates
 * @center: vector for the object coordinates
 * @up: vector for the upwards direction
 *
 * Initializes a SIMD matrix with the projection necessary for
 * the camera at the @eye coordinates to look at the object at
 * the @center coordinates. The top of the camera is aligned to
 * the @up vector.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_init_look_at (graphene_simd4x4f_t *m,
                                graphene_simd4f_t    eye,
                                graphene_simd4f_t    center,
                                graphene_simd4f_t    up)
{
  const graphene_simd4f_t z_axis = graphene_simd4f_normalize3 (graphene_simd4f_sub (center, eye));
  const graphene_simd4f_t x_axis = graphene_simd4f_normalize3 (graphene_simd4f_cross3 (z_axis, up));
  const graphene_simd4f_t y_axis = graphene_simd4f_cross3 (x_axis, z_axis);
  m->x = x_axis;
  m->y = y_axis;
  m->z = graphene_simd4f_neg(z_axis);
  m->w = graphene_simd4f_init (0,0,0,0);
  graphene_simd4x4f_transpose_in_place (m);
  m->w = graphene_simd4f_init (-graphene_simd4f_dot3_scalar (x_axis,eye),
			       -graphene_simd4f_dot3_scalar (y_axis,eye),
			       graphene_simd4f_dot3_scalar (z_axis,eye),
			       1.f);
}

/**
 * graphene_simd4x4f_init_frustum:
 * @m: a #graphene_simd4x4f_t
 * @left: distance of the left clipping plane
 * @right: distance of the right clipping plane
 * @bottom: distance of the bottom clipping plane
 * @top: distance of the top clipping plane
 * @z_near: distance of the near clipping plane
 * @z_far: distance of the far clipping plane
 *
 * Initializes a SIMD matrix with a frustum described by the distances
 * of six clipping planes.
 *
 * Since: 1.2
 */
static inline void
graphene_simd4x4f_init_frustum (graphene_simd4x4f_t *m,
                                float                left,
                                float                right,
                                float                bottom,
                                float                top,
                                float                z_near,
                                float                z_far)
{
  float x = 2.f * z_near / (right - left);
  float y = 2.f * z_near / (top - bottom);

  float a = (right + left) / (right - left);
  float b = (top + bottom) / (top - bottom);
  float c = -1.f * (z_far + z_near) / (z_far - z_near);
  float d = -2.f * z_far * z_near / (z_far - z_near);

  m->x = graphene_simd4f_init (  x, 0.f, 0.f,  0.f);
  m->y = graphene_simd4f_init (0.f,   y, 0.f,  0.f);
  m->z = graphene_simd4f_init (  a,   b,   c, -1.f);
  m->w = graphene_simd4f_init (0.f, 0.f,   d,  0.f);
}

/**
 * graphene_simd4x4f_perspective:
 * @m: a #graphene_simd4x4f_t
 * @depth: depth of the perspective
 *
 * Adds a perspective transformation for the given @depth.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_perspective (graphene_simd4x4f_t *m,
                               float                depth)
{
#if 1
  const float m_xw = graphene_simd4f_get_w (m->x);
  const float m_yw = graphene_simd4f_get_w (m->y);
  const float m_zw = graphene_simd4f_get_w (m->z);
  const float m_ww = graphene_simd4f_get_w (m->w);

  const float p0 = graphene_simd4f_get_z (m->x) + -1.0f / depth * m_xw;
  const float p1 = graphene_simd4f_get_z (m->y) + -1.0f / depth * m_yw;
  const float p2 = graphene_simd4f_get_z (m->z) + -1.0f / depth * m_zw;
  const float p3 = graphene_simd4f_get_z (m->w) + -1.0f / depth * m_ww;

  const graphene_simd4f_t p_x = graphene_simd4f_merge_w (m->x, m_xw + p0);
  const graphene_simd4f_t p_y = graphene_simd4f_merge_w (m->y, m_yw + p1);
  const graphene_simd4f_t p_z = graphene_simd4f_merge_w (m->z, m_zw + p2);
  const graphene_simd4f_t p_w = graphene_simd4f_merge_w (m->w, m_ww + p3);
#else
  /* this is equivalent to the operations above, but trying to inline
   * them into SIMD registers as much as possible by transposing the
   * original matrix and operating on the resulting column vectors. it
   * should warrant a micro benchmark, because while the above code is
   * dominated by single channel reads, the code below has a transpose
   * operation.
   */
  graphene_simd4x4f_t t;
  const graphene_simd4f_t f, p;
  const graphene_simd4f_t p_x, p_y, p_z, p_w;

  graphene_simd4x4f_transpose (m, &t);

  f = graphene_simd4f_neg (graphene_simd4f_reciprocal (graphene_simd4f_splat (depth)));
  p = graphene_simd4f_sum (t.w, graphene_simd4f_sum (t.z, graphene_simd4f_mul (f, t.w)));
  p_x = graphene_simd4f_merge_w (m->x, graphene_simd4f_get_x (p));
  p_y = graphene_simd4f_merge_w (m->y, graphene_simd4f_get_y (p));
  p_z = graphene_simd4f_merge_w (m->z, graphene_simd4f_get_z (p));
  p_w = graphene_simd4f_merge_w (m->w, graphene_simd4f_get_w (p));
#endif

  *m = graphene_simd4x4f_init (p_x, p_y, p_z, p_w);
}

/**
 * graphene_simd4x4f_translation:
 * @m: a #graphene_simd4x4f_t
 * @x: coordinate of the X translation
 * @y: coordinate of the Y translation
 * @z: coordinate of the Z translation
 *
 * Initializes @m to contain a translation to the given coordinates.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_translation (graphene_simd4x4f_t *m,
                               float                x,
                               float                y,
                               float                z)
{
  *m = graphene_simd4x4f_init (graphene_simd4f_init (1.0f, 0.0f, 0.0f, 0.0f),
                               graphene_simd4f_init (0.0f, 1.0f, 0.0f, 0.0f),
                               graphene_simd4f_init (0.0f, 0.0f, 1.0f, 0.0f),
                               graphene_simd4f_init (   x,    y,    z, 1.0f));
}

/**
 * graphene_simd4x4f_scale:
 * @m: a #graphene_simd4x4f_t
 * @x: scaling factor on the X axis
 * @y: scaling factor on the Y axis
 * @z: scaling factor on the Z axis
 *
 * Initializes @m to contain a scaling transformation with the
 * given factors.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_scale (graphene_simd4x4f_t *m,
                         float                x,
                         float                y,
                         float                z)
{
  *m = graphene_simd4x4f_init (graphene_simd4f_init (   x, 0.0f, 0.0f, 0.0f),
                               graphene_simd4f_init (0.0f,    y, 0.0f, 0.0f),
                               graphene_simd4f_init (0.0f, 0.0f,    z, 0.0f),
                               graphene_simd4f_init (0.0f, 0.0f, 0.0f, 1.0f));

}

/**
 * graphene_simd4x4f_rotation:
 * @m: a #graphene_simd4x4f_t
 * @rad: the rotation, in radians
 * @axis: the vector of the axis of rotation
 *
 * Initializes @m to contain a rotation of the given angle
 * along the given axis.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_rotation (graphene_simd4x4f_t *m,
                            float                rad,
                            graphene_simd4f_t    axis)
{
  float sine, cosine;
  float x, y, z;
  float ab, bc, ca;
  float tx, ty, tz;
  graphene_simd4f_t i, j, k;

  rad = -rad;
  axis = graphene_simd4f_normalize3 (axis);

  /* We cannot use graphene_sincos() because it's a private function, whereas
   * graphene-simd4x4f.h is a public header
   */
  sine = sinf (rad);
  cosine = cosf (rad);

  x = graphene_simd4f_get_x (axis);
  y = graphene_simd4f_get_y (axis);
  z = graphene_simd4f_get_z (axis);

  ab = x * y * (1.0f - cosine);
  bc = y * z * (1.0f - cosine);
  ca = z * x * (1.0f - cosine);

  tx = x * x;
  ty = y * y;
  tz = z * z;

  i = graphene_simd4f_init (tx + cosine * (1.0f - tx), ab - z * sine, ca + y * sine, 0.f);
  j = graphene_simd4f_init (ab + z * sine, ty + cosine * (1.0f - ty), bc - x * sine, 0.f);
  k = graphene_simd4f_init (ca - y * sine, bc + x * sine, tz + cosine * (1.0f - tz), 0.f);

  *m = graphene_simd4x4f_init (i, j, k, graphene_simd4f_init (0.0f, 0.0f, 0.0f, 1.0f));
}

/**
 * graphene_simd4x4f_add:
 * @a: a #graphene_simd4x4f_t
 * @b: a #graphene_simd4x4f_t
 * @res: (out caller-allocates): return location for a #graphene_simd4x4f_t
 *
 * Adds each row vector of @a and @b and places the results in @res.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_add (const graphene_simd4x4f_t *a,
                       const graphene_simd4x4f_t *b,
                       graphene_simd4x4f_t *res)
{
  res->x = graphene_simd4f_add (a->x, b->x);
  res->y = graphene_simd4f_add (a->y, b->y);
  res->z = graphene_simd4f_add (a->z, b->z);
  res->w = graphene_simd4f_add (a->w, b->w);
}

/**
 * graphene_simd4x4f_sub:
 * @a: a #graphene_simd4x4f_t
 * @b: a #graphene_simd4x4f_t
 * @res: (out caller-allocates): return location for a #graphene_simd4x4f_t
 *
 * Subtracts each row vector of @a and @b and places the results in @res.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_sub (const graphene_simd4x4f_t *a,
                       const graphene_simd4x4f_t *b,
                       graphene_simd4x4f_t *res)
{
  res->x = graphene_simd4f_sub (a->x, b->x);
  res->y = graphene_simd4f_sub (a->y, b->y);
  res->z = graphene_simd4f_sub (a->z, b->z);
  res->w = graphene_simd4f_sub (a->w, b->w);
}

/**
 * graphene_simd4x4f_mul:
 * @a: a #graphene_simd4x4f_t
 * @b: a #graphene_simd4x4f_t
 * @res: (out caller-allocates): return location for a #graphene_simd4x4f_t
 *
 * Multiplies each row vector of @a and @b and places the results in @res.
 *
 * You most likely want graphene_simd4x4f_matrix_mul() instead.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_mul (const graphene_simd4x4f_t *a,
                       const graphene_simd4x4f_t *b,
                       graphene_simd4x4f_t *res)
{
  res->x = graphene_simd4f_mul (a->x, b->x);
  res->y = graphene_simd4f_mul (a->y, b->y);
  res->z = graphene_simd4f_mul (a->z, b->z);
  res->w = graphene_simd4f_mul (a->w, b->w);
}

/**
 * graphene_simd4x4f_div:
 * @a: a #graphene_simd4x4f_t
 * @b: a #graphene_simd4x4f_t
 * @res: (out caller-allocates): return location for a #graphene_simd4x4f_t
 *
 * Divides each row vector of @a and @b and places the results in @res.
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_div (const graphene_simd4x4f_t *a,
                       const graphene_simd4x4f_t *b,
                       graphene_simd4x4f_t *res)
{
  res->x = graphene_simd4f_div (a->x, b->x);
  res->y = graphene_simd4f_div (a->y, b->y);
  res->z = graphene_simd4f_div (a->z, b->z);
  res->w = graphene_simd4f_div (a->w, b->w);
}

/**
 * graphene_simd4x4f_inverse:
 * @m: a #graphene_simd4x4f_t
 * @res: (out): return location for the inverse matrix
 *
 * Inverts the given #graphene_simd4x4f_t.
 *
 * Returns: `true` if the matrix was invertible
 *
 * Since: 1.0
 */
static inline bool
graphene_simd4x4f_inverse (const graphene_simd4x4f_t *m,
                           graphene_simd4x4f_t       *res)
{
  /* split rows */
  const graphene_simd4f_t r0 = m->x;
  const graphene_simd4f_t r1 = m->y;
  const graphene_simd4f_t r2 = m->z;
  const graphene_simd4f_t r3 = m->w;

  /* cofactors */
  const graphene_simd4f_t r0_wxyz = graphene_simd4f_shuffle_wxyz (r0);
  const graphene_simd4f_t r0_zwxy = graphene_simd4f_shuffle_zwxy (r0);
  const graphene_simd4f_t r0_yzwx = graphene_simd4f_shuffle_yzwx (r0);

  const graphene_simd4f_t r1_wxyz = graphene_simd4f_shuffle_wxyz (r1);
  const graphene_simd4f_t r1_zwxy = graphene_simd4f_shuffle_zwxy (r1);
  const graphene_simd4f_t r1_yzwx = graphene_simd4f_shuffle_yzwx (r1);

  const graphene_simd4f_t r2_wxyz = graphene_simd4f_shuffle_wxyz (r2);
  const graphene_simd4f_t r2_zwxy = graphene_simd4f_shuffle_zwxy (r2);
  const graphene_simd4f_t r2_yzwx = graphene_simd4f_shuffle_yzwx (r2);

  const graphene_simd4f_t r3_wxyz = graphene_simd4f_shuffle_wxyz (r3);
  const graphene_simd4f_t r3_zwxy = graphene_simd4f_shuffle_zwxy (r3);
  const graphene_simd4f_t r3_yzwx = graphene_simd4f_shuffle_yzwx (r3);

  const graphene_simd4f_t r0_wxyz_x_r1 = graphene_simd4f_mul (r0_wxyz, r1);
  const graphene_simd4f_t r0_wxyz_x_r1_yzwx = graphene_simd4f_mul (r0_wxyz, r1_yzwx);
  const graphene_simd4f_t r0_wxyz_x_r1_zwxy = graphene_simd4f_mul (r0_wxyz, r1_zwxy);

  const graphene_simd4f_t r2_wxyz_x_r3 = graphene_simd4f_mul (r2_wxyz, r3);
  const graphene_simd4f_t r2_wxyz_x_r3_yzwx = graphene_simd4f_mul (r2_wxyz, r3_yzwx);
  const graphene_simd4f_t r2_wxyz_x_r3_zwxy = graphene_simd4f_mul (r2_wxyz, r3_zwxy);

  const graphene_simd4f_t ar1 = graphene_simd4f_sub (graphene_simd4f_shuffle_wxyz (r2_wxyz_x_r3_zwxy),
                                                     graphene_simd4f_shuffle_zwxy (r2_wxyz_x_r3));
  const graphene_simd4f_t ar2 = graphene_simd4f_sub (graphene_simd4f_shuffle_zwxy (r2_wxyz_x_r3_yzwx),
                                                     r2_wxyz_x_r3_yzwx);
  const graphene_simd4f_t ar3 = graphene_simd4f_sub (r2_wxyz_x_r3_zwxy,
                                                     graphene_simd4f_shuffle_wxyz (r2_wxyz_x_r3));

  const graphene_simd4f_t br1 = graphene_simd4f_sub (graphene_simd4f_shuffle_wxyz (r0_wxyz_x_r1_zwxy),
                                                     graphene_simd4f_shuffle_zwxy (r0_wxyz_x_r1));
  const graphene_simd4f_t br2 = graphene_simd4f_sub (graphene_simd4f_shuffle_zwxy (r0_wxyz_x_r1_yzwx),
                                                     r0_wxyz_x_r1_yzwx);
  const graphene_simd4f_t br3 = graphene_simd4f_sub (r0_wxyz_x_r1_zwxy,
                                                     graphene_simd4f_shuffle_wxyz (r0_wxyz_x_r1));

  const graphene_simd4f_t r0_sum =
    graphene_simd4f_madd (r0_yzwx, ar3,
                          graphene_simd4f_madd (r0_zwxy, ar2,
                                                graphene_simd4f_mul (r0_wxyz, ar1)));
  const graphene_simd4f_t r1_sum =
    graphene_simd4f_madd (r1_wxyz, ar1,
                          graphene_simd4f_madd (r1_zwxy, ar2,
                                                graphene_simd4f_mul (r1_yzwx, ar3)));
  const graphene_simd4f_t r2_sum =
    graphene_simd4f_madd (r2_yzwx, br3,
                          graphene_simd4f_madd (r2_zwxy, br2,
                                                graphene_simd4f_mul (r2_wxyz, br1)));
  const graphene_simd4f_t r3_sum =
    graphene_simd4f_madd (r3_yzwx, br3,
                          graphene_simd4f_madd (r3_zwxy, br2,
                                                graphene_simd4f_mul (r3_wxyz, br1)));

  /* determinant and its inverse */
  const graphene_simd4f_t d0 = graphene_simd4f_mul (r1_sum, r0);
  const graphene_simd4f_t d1 = graphene_simd4f_add (d0, graphene_simd4f_merge_high (d0, d0));
  const graphene_simd4f_t det = graphene_simd4f_sub (d1, graphene_simd4f_splat_y (d1));
  if (graphene_simd4f_get_x (det) != 0.f)
    {
      const graphene_simd4f_t invdet = graphene_simd4f_splat_x (graphene_simd4f_div (graphene_simd4f_splat (1.0f), det));

      const graphene_simd4f_t o0 = graphene_simd4f_mul (graphene_simd4f_flip_sign_0101 (r1_sum), invdet);
      const graphene_simd4f_t o1 = graphene_simd4f_mul (graphene_simd4f_flip_sign_1010 (r0_sum), invdet);
      const graphene_simd4f_t o2 = graphene_simd4f_mul (graphene_simd4f_flip_sign_0101 (r3_sum), invdet);
      const graphene_simd4f_t o3 = graphene_simd4f_mul (graphene_simd4f_flip_sign_1010 (r2_sum), invdet);

      graphene_simd4x4f_t mt = graphene_simd4x4f_init (o0, o1, o2, o3);

      /* transpose the resulting matrix */
      graphene_simd4x4f_transpose (&mt, res);

      return TRUE;
    }

  return FALSE;
}

/**
 * graphene_simd4x4f_determinant:
 * @m: a #graphene_simd4x4f_t
 * @det_r: (out): return location for the matrix determinant
 * @invdet_r: (out): return location for the inverse of the matrix
 *   determinant
 *
 * Computes the determinant (and its inverse) of the given matrix
 *
 * Since: 1.0
 */
static inline void
graphene_simd4x4f_determinant (const graphene_simd4x4f_t *m,
                               graphene_simd4f_t         *det_r,
                               graphene_simd4f_t         *invdet_r)
{
  /* split rows */
  const graphene_simd4f_t r0 = m->x;
  const graphene_simd4f_t r1 = m->y;
  const graphene_simd4f_t r2 = m->z;
  const graphene_simd4f_t r3 = m->w;

  /* cofactors */
  const graphene_simd4f_t r1_wxyz = graphene_simd4f_shuffle_wxyz (r1);
  const graphene_simd4f_t r1_zwxy = graphene_simd4f_shuffle_zwxy (r1);
  const graphene_simd4f_t r1_yzwx = graphene_simd4f_shuffle_yzwx (r1);

  const graphene_simd4f_t r2_wxyz = graphene_simd4f_shuffle_wxyz (r2);

  const graphene_simd4f_t r3_zwxy = graphene_simd4f_shuffle_zwxy (r3);
  const graphene_simd4f_t r3_yzwx = graphene_simd4f_shuffle_yzwx (r3);

  const graphene_simd4f_t r2_wxyz_x_r3 = graphene_simd4f_mul (r2_wxyz, r3);
  const graphene_simd4f_t r2_wxyz_x_r3_yzwx = graphene_simd4f_mul (r2_wxyz, r3_yzwx);
  const graphene_simd4f_t r2_wxyz_x_r3_zwxy = graphene_simd4f_mul (r2_wxyz, r3_zwxy);

  const graphene_simd4f_t ar1 = graphene_simd4f_sub (graphene_simd4f_shuffle_wxyz (r2_wxyz_x_r3_zwxy),
                                                     graphene_simd4f_shuffle_zwxy (r2_wxyz_x_r3));
  const graphene_simd4f_t ar2 = graphene_simd4f_sub (graphene_simd4f_shuffle_zwxy (r2_wxyz_x_r3_yzwx),
                                                     r2_wxyz_x_r3_yzwx);
  const graphene_simd4f_t ar3 = graphene_simd4f_sub (r2_wxyz_x_r3_zwxy,
                                                     graphene_simd4f_shuffle_wxyz (r2_wxyz_x_r3));

  const graphene_simd4f_t r1_sum =
    graphene_simd4f_madd (r1_wxyz, ar1,
                          graphene_simd4f_madd (r1_zwxy, ar2,
                                                graphene_simd4f_mul (r1_yzwx, ar3)));

  /* determinant and its inverse */
  const graphene_simd4f_t d0 = graphene_simd4f_mul (r1_sum, r0);
  const graphene_simd4f_t d1 = graphene_simd4f_add (d0, graphene_simd4f_merge_high (d0, d0));

  const graphene_simd4f_t det = graphene_simd4f_sub (d1, graphene_simd4f_splat_y (d1));

  const graphene_simd4f_t invdet = graphene_simd4f_splat_x (graphene_simd4f_div (graphene_simd4f_splat (1.0f), det));

  if (det_r != NULL)
    *det_r = det;

  if (invdet_r != NULL)
    *invdet_r = invdet;
}

/**
 * graphene_simd4x4f_is_identity:
 * @m: a #graphene_simd4x4f_t
 *
 * Checks whether the given matrix is the identity matrix.
 *
 * Returns: `TRUE` if the matrix is the identity matrix
 *
 * Since: 1.0
 */
static inline bool
graphene_simd4x4f_is_identity (const graphene_simd4x4f_t *m)
{
  const graphene_simd4f_t r0 = graphene_simd4f_init (1.0f, 0.0f, 0.0f, 0.0f);
  const graphene_simd4f_t r1 = graphene_simd4f_init (0.0f, 1.0f, 0.0f, 0.0f);
  const graphene_simd4f_t r2 = graphene_simd4f_init (0.0f, 0.0f, 1.0f, 0.0f);
  const graphene_simd4f_t r3 = graphene_simd4f_init (0.0f, 0.0f, 0.0f, 1.0f);

  return graphene_simd4f_cmp_eq (m->x, r0) &&
         graphene_simd4f_cmp_eq (m->y, r1) &&
         graphene_simd4f_cmp_eq (m->z, r2) &&
         graphene_simd4f_cmp_eq (m->w, r3);
}

/**
 * graphene_simd4x4f_is_2d:
 * @m: a #graphene_simd4x4f_t
 *
 * Checks whether the given matrix is compatible with an affine
 * transformation matrix.
 *
 * Returns: `TRUE` if the matrix is compatible with an affine
 *   transformation matrix
 *
 * Since: 1.0
 */
static inline bool
graphene_simd4x4f_is_2d (const graphene_simd4x4f_t *m)
{
  float f[4];

  if (!(fabsf (graphene_simd4f_get_z (m->x)) < FLT_EPSILON && fabsf (graphene_simd4f_get_w (m->x)) < FLT_EPSILON))
    return FALSE;

  if (!(fabsf (graphene_simd4f_get_z (m->y)) < FLT_EPSILON && fabsf (graphene_simd4f_get_w (m->y)) < FLT_EPSILON))
    return FALSE;

  graphene_simd4f_dup_4f (m->z, f);
  if (!(fabsf (f[0]) < FLT_EPSILON &&
        fabsf (f[1]) < FLT_EPSILON &&
        1.f - fabsf (f[2]) < FLT_EPSILON &&
        fabsf (f[3]) < FLT_EPSILON))
    return FALSE;

  if (!(fabsf (graphene_simd4f_get_z (m->w)) < FLT_EPSILON && 1.f - fabsf (graphene_simd4f_get_w (m->w)) < FLT_EPSILON))
    return FALSE;

  return TRUE;
}

/**
 * graphene_matrix_to_float:
 * @m: a #graphene_matrix_t
 * @v: (array fixed-size=16) (out caller-allocates): return location
 *   for an array of floating point values. The array must be capable
 *   of holding at least 16 values.
 *
 * Converts a #graphene_matrix_t to an array of floating point
 * values.
 *
 * Since: 1.0
 */
void
graphene_matrix_to_float (const graphene_matrix_t *m,
                          float                   *v)
{
  graphene_simd4x4f_to_float (&m->value, v);
}

static const float graphene_identity_matrix_floats[16] = {
  1.f, 0.f, 0.f, 0.f,
  0.f, 1.f, 0.f, 0.f,
  0.f, 0.f, 1.f, 0.f,
  0.f, 0.f, 0.f, 1.f,
};

/**
 * graphene_matrix_init_identity:
 * @m: a #graphene_matrix_t
 *
 * Initializes a #graphene_matrix_t with the identity matrix.
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.0
 */
graphene_matrix_t *
graphene_matrix_init_identity (graphene_matrix_t *m)
{
  graphene_simd4x4f_init_from_float (&m->value, graphene_identity_matrix_floats);

  return m;
}

/**
 * graphene_matrix_init_from_float:
 * @m: a #graphene_matrix_t
 * @v: (array fixed-size=16): an array of at least 16 floating
 *   point values
 *
 * Initializes a #graphene_matrix_t with the given array of floating
 * point values.
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.0
 */
graphene_matrix_t *
graphene_matrix_init_from_float (graphene_matrix_t *m,
                                 const float       *v)
{
  graphene_simd4x4f_init_from_float (&m->value, v);

  return m;
}

/**
 * graphene_matrix_init_from_vec4:
 * @m: a #graphene_matrix_t
 * @v0: the first row vector
 * @v1: the second row vector
 * @v2: the third row vector
 * @v3: the fourth row vector
 *
 * Initializes a #graphene_matrix_t with the given four row
 * vectors.
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.0
 */
graphene_matrix_t *
graphene_matrix_init_from_vec4 (graphene_matrix_t     *m,
                                const graphene_vec4_t *v0,
                                const graphene_vec4_t *v1,
                                const graphene_vec4_t *v2,
                                const graphene_vec4_t *v3)
{
  m->value = graphene_simd4x4f_init (v0->value,
                                     v1->value,
                                     v2->value,
                                     v3->value);

  return m;
}

/**
 * graphene_matrix_init_from_matrix:
 * @m: a #graphene_matrix_t
 * @src: a #graphene_matrix_t
 *
 * Initializes a #graphene_matrix_t using the values of the
 * given matrix.
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.0
 */
graphene_matrix_t *
graphene_matrix_init_from_matrix (graphene_matrix_t       *m,
                                  const graphene_matrix_t *src)
{
  m->value = src->value;

  return m;
}

/**
 * graphene_matrix_init_perspective:
 * @m: a #graphene_matrix_t
 * @fovy: the field of view angle, in degrees
 * @aspect: the aspect value
 * @z_near: the near Z plane
 * @z_far: the far Z plane
 *
 * Initializes a #graphene_matrix_t with a perspective projection.
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.0
 */
graphene_matrix_t *
graphene_matrix_init_perspective (graphene_matrix_t *m,
                                  float              fovy,
                                  float              aspect,
                                  float              z_near,
                                  float              z_far)
{
  float fovy_rad = GRAPHENE_DEG_TO_RAD (fovy);

  graphene_simd4x4f_init_perspective (&m->value, fovy_rad, aspect, z_near, z_far);

  return m;
}

/**
 * graphene_matrix_init_ortho:
 * @m: a #graphene_matrix_t
 * @left: the left edge of the clipping plane
 * @right: the right edge of the clipping plane
 * @top: the top edge of the clipping plane
 * @bottom: the bottom edge of the clipping plane
 * @z_near: the distance of the near clipping plane
 * @z_far: the distance of the far clipping plane
 *
 * Initializes a #graphene_matrix_t with an orthographic projection.
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.0
 */
graphene_matrix_t *
graphene_matrix_init_ortho (graphene_matrix_t *m,
                            float              left,
                            float              right,
                            float              top,
                            float              bottom,
                            float              z_near,
                            float              z_far)
{
  graphene_simd4x4f_init_ortho (&m->value, left, right, top, bottom, z_near, z_far);

  return m;
}

/**
 * graphene_matrix_init_look_at:
 * @m: a #graphene_matrix_t
 * @eye: the vector describing the position to look from
 * @center: the vector describing the position to look at
 * @up: the vector describing the world's upward direction; usually,
 *   this is the graphene_vec3_y_axis() vector
 *
 * Initializes a #graphene_matrix_t so that it positions the "camera"
 * at the given @eye coordinates towards an object at the @center
 * coordinates. The top of the camera is aligned to the direction
 * of the @up vector.
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.0
 */
graphene_matrix_t *
graphene_matrix_init_look_at (graphene_matrix_t     *m,
                              const graphene_vec3_t *eye,
                              const graphene_vec3_t *center,
                              const graphene_vec3_t *up)
{
  graphene_simd4x4f_init_look_at (&m->value, eye->value, center->value, up->value);

  return m;
}

/**
 * graphene_matrix_init_frustum:
 * @m: a #graphene_matrix_t
 * @left: distance of the left clipping plane
 * @right: distance of the right clipping plane
 * @bottom: distance of the bottom clipping plane
 * @top: distance of the top clipping plane
 * @z_near: distance of the near clipping plane
 * @z_far: distance of the far clipping plane
 *
 * Initializes a #graphene_matrix_t compatible with #graphene_frustum_t.
 *
 * See also: graphene_frustum_init_from_matrix()
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.2
 */
graphene_matrix_t *
graphene_matrix_init_frustum (graphene_matrix_t *m,
                              float              left,
                              float              right,
                              float              bottom,
                              float              top,
                              float              z_near,
                              float              z_far)
{
  graphene_simd4x4f_init_frustum (&m->value, left, right, bottom, top, z_near, z_far);

  return m;
}

/**
 * graphene_matrix_init_scale:
 * @m: a #graphene_matrix_t
 * @x: the scale factor on the X axis
 * @y: the scale factor on the Y axis
 * @z: the scale factor on the Z axis
 *
 * Initializes a #graphene_matrix_t with the given scaling factors.
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.0
 */
graphene_matrix_t *
graphene_matrix_init_scale (graphene_matrix_t *m,
                            float              x,
                            float              y,
                            float              z)
{
  m->value =
    graphene_simd4x4f_init (graphene_simd4f_init (   x, 0.0f, 0.0f, 0.0f),
                            graphene_simd4f_init (0.0f,    y, 0.0f, 0.0f),
                            graphene_simd4f_init (0.0f, 0.0f,    z, 0.0f),
                            graphene_simd4f_init (0.0f, 0.0f, 0.0f, 1.0f));

  return m;
}

/**
 * graphene_matrix_init_translate:
 * @m: a #graphene_matrix_t
 * @p: the translation coordinates
 *
 * Initializes a #graphene_matrix_t with a translation to the
 * given coordinates.
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.0
 */
graphene_matrix_t *
graphene_matrix_init_translate (graphene_matrix_t        *m,
                                const graphene_point3d_t *p)
{
  m->value =
    graphene_simd4x4f_init (graphene_simd4f_init (1.0f, 0.0f, 0.0f, 0.0f),
                            graphene_simd4f_init (0.0f, 1.0f, 0.0f, 0.0f),
                            graphene_simd4f_init (0.0f, 0.0f, 1.0f, 0.0f),
                            graphene_simd4f_init (p->x, p->y, p->z, 1.0f));

  return m;
}

/**
 * graphene_matrix_init_skew:
 * @m: a #graphene_matrix_t
 * @x_skew: skew factor on the X axis
 * @y_skew: skew factor on the Y axis
 *
 * Initializes a #graphene_matrix_t with a skew transformation
 * with the given factors.
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.0
 */
graphene_matrix_t *
graphene_matrix_init_skew (graphene_matrix_t *m,
                           float              x_skew,
                           float              y_skew)
{
  float t_x, t_y;

  t_x = tanf (x_skew);
  t_y = tanf (y_skew);

  m->value =
    graphene_simd4x4f_init (graphene_simd4f_init (1.0f,  t_y, 0.0f, 0.0f),
                            graphene_simd4f_init ( t_x, 1.0f, 0.0f, 0.0f),
                            graphene_simd4f_init (0.0f, 0.0f, 1.0f, 0.0f),
                            graphene_simd4f_init (0.0f, 0.0f, 0.0f, 1.0f));

  return m;
}

/**
 * graphene_matrix_init_rotate
 * @m: a #graphene_matrix_t
 * @angle: the rotation angle, in degrees
 * @axis: the axis vector as a #graphene_vec3_t
 *
 * Initializes @m to represent a rotation of @angle degrees on
 * the axis represented by the @axis vector.
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.0
 */
graphene_matrix_t *
graphene_matrix_init_rotate (graphene_matrix_t     *m,
                             float                  angle,
                             const graphene_vec3_t *axis)
{
  float rad = GRAPHENE_DEG_TO_RAD (angle);

  graphene_simd4x4f_rotation (&m->value, rad, axis->value);

  return m;
}

/**
 * graphene_matrix_is_identity:
 * @m: a #graphene_matrix_t
 *
 * Checks whether the given #graphene_matrix_t is the identity matrix.
 *
 * Returns: `TRUE` if the matrix is the identity matrix
 *
 * Since: 1.0
 */
bool
graphene_matrix_is_identity (const graphene_matrix_t *m)
{
  return graphene_simd4x4f_is_identity (&m->value);
}

/**
 * graphene_matrix_is_2d:
 * @m: a #graphene_matrix_t
 *
 * Checks whether the given #graphene_matrix_t is compatible with an
 * a 2D affine transformation matrix.
 *
 * Returns: `TRUE` if the matrix is compatible with an affine
 *   transformation matrix
 *
 * Since: 1.0
 */
bool
graphene_matrix_is_2d (const graphene_matrix_t *m)
{
#if 0
  float res[4];

  graphene_simd4f_dup_4f (m->value.x, res);
  if (!(graphene_fuzzy_equals (res[2], 0.f, 0.000001) &&
        graphene_fuzzy_equals (res[3], 0.f, 0.000001)))
    return FALSE;

  graphene_simd4f_dup_4f (m->value.y, res);
  if (!(graphene_fuzzy_equals (res[2], 0.f, 0.000001) &&
        graphene_fuzzy_equals (res[3], 0.f, 0.000001)))
    return FALSE;

  graphene_simd4f_dup_4f (m->value.z, res);
  if (!(graphene_fuzzy_equals (res[0], 0.f, 0.000001) &&
        graphene_fuzzy_equals (res[1], 0.f, 0.000001) &&
        graphene_fuzzy_equals (res[2], 1.f, 0.000001) &&
        graphene_fuzzy_equals (res[3], 0.f, 0.000001)))
    return FALSE;

  graphene_simd4f_dup_4f (m->value.w, res);
  if (!(graphene_fuzzy_equals (res[2], 0.f, 0.000001) &&
        graphene_fuzzy_equals (res[3], 1.f, 0.000001)))
    return FALSE;

  return TRUE;
#else
  return graphene_simd4x4f_is_2d (&m->value);
#endif
}

/**
 * graphene_matrix_is_backface_visible:
 * @m: a #graphene_matrix_t
 *
 * Checks whether a #graphene_matrix_t has a visible back face.
 *
 * Returns: `TRUE` if the back face of the matrix is visible
 *
 * Since: 1.0
 */
bool
graphene_matrix_is_backface_visible (const graphene_matrix_t *m)
{
  graphene_simd4x4f_t tmp;

  if (!graphene_simd4x4f_inverse (&m->value, &tmp))
    return FALSE;

  /* inverse.zz < 0 */
  return graphene_simd4f_get_z (tmp.z) < 0.f;
}

/**
 * graphene_matrix_is_singular:
 * @m: a #graphene_matrix_t
 *
 * Checks whether a matrix is singular.
 *
 * Returns: `TRUE` if the matrix is singular
 *
 * Since: 1.0
 */
bool
graphene_matrix_is_singular (const graphene_matrix_t *m)
{
  graphene_simd4f_t det;

  graphene_simd4x4f_determinant (&m->value, &det, NULL);

  return fabsf (graphene_simd4f_get_x (det)) <= GRAPHENE_FLOAT_EPSILON;
}

/**
 * graphene_matrix_init_from_2d:
 * @m: a #graphene_matrix_t
 * @xx: the xx member
 * @yx: the yx member
 * @xy: the xy member
 * @yy: the yy member
 * @x_0: the x0 member
 * @y_0: the y0 member
 *
 * Initializes a #graphene_matrix_t from the values of an affine
 * transformation matrix.
 *
 * The arguments map to the following matrix layout:
 *
 * |[<!-- language="plain" -->
 *   | xx yx |   |  a  b  0 |
 *   | xy yy | = |  c  d  0 |
 *   | x0 y0 |   | tx ty  1 |
 * ]|
 *
 * This function can be used to convert between a matrix type from
 * other libraries and a #graphene_matrix_t.
 *
 * Returns: (transfer none): the initialized matrix
 *
 * Since: 1.0
 */
graphene_matrix_t *
graphene_matrix_init_from_2d (graphene_matrix_t *m,
                              double             xx,
                              double             yx,
                              double             xy,
                              double             yy,
                              double             x_0,
                              double             y_0)
{
  m->value = graphene_simd4x4f_init (graphene_simd4f_init ( xx,  yx, 0.f, 0.f),
                                     graphene_simd4f_init ( xy,  yy, 0.f, 0.f),
                                     graphene_simd4f_init (0.f, 0.f, 1.f, 0.f),
                                     graphene_simd4f_init (x_0, y_0, 0.f, 1.f));

  return m;
}


#define graphene_fuzzy_equals(n1,n2,epsilon)			\
  (((n1) > (n2) ? ((n1) - (n2)) : ((n2) - (n1))) < (epsilon))

static inline bool
graphene_approx_val (float a, float b)
{
  return graphene_fuzzy_equals (a, b, FLT_EPSILON);
}


/**
 * graphene_matrix_to_2d:
 * @m: a #graphene_matrix_t
 * @xx: (out): return location for the xx member
 * @yx: (out): return location for the yx member
 * @xy: (out): return location for the xy member
 * @yy: (out): return location for the yy member
 * @x_0: (out): return location for the x0 member
 * @y_0: (out): return location for the y0 member
 *
 * Converts a #graphene_matrix_t to an affine transformation
 * matrix, if the given matrix is compatible.
 *
 * The returned values have the following layout:
 *
 * |[<!-- language="plain" -->
 *   | xx yx |   |  a  b  0 |
 *   | xy yy | = |  c  d  0 |
 *   | x0 y0 |   | tx ty  1 |
 * ]|
 *
 * This function can be used to convert between a #graphene_matrix_t
 * and a matrix type from other libraries.
 *
 * Returns: `TRUE` if the matrix is compatible with an affine
 *   transformation matrix
 *
 * Since: 1.0
 */
bool
graphene_matrix_to_2d (const graphene_matrix_t *m,
                       double                  *xx,
                       double                  *yx,
                       double                  *xy,
                       double                  *yy,
                       double                  *x_0,
                       double                  *y_0)
{
  float res[4];

  graphene_simd4f_dup_4f (m->value.x, res);
  if (!graphene_approx_val (res[2], 0.f) &&
      !graphene_approx_val (res[3], 0.f))
    return FALSE;

  if (xx != NULL)
    *xx = res[0];
  if (yx != NULL)
    *yx = res[1];

  graphene_simd4f_dup_4f (m->value.y, res);
  if (!graphene_approx_val (res[2], 0.f) &&
      !graphene_approx_val (res[3], 0.f))
    return FALSE;

  if (xy != NULL)
    *xy = res[0];
  if (yy != NULL)
    *yy = res[1];

  graphene_simd4f_dup_4f (m->value.z, res);
  if (!graphene_approx_val (res[0], 0.f) &&
      !graphene_approx_val (res[1], 0.f) &&
      !graphene_approx_val (res[2], 1.f) &&
      !graphene_approx_val (res[3], 0.f))
    return FALSE;

  graphene_simd4f_dup_4f (m->value.w, res);
  if (!graphene_approx_val (res[2], 0.f) &&
      !graphene_approx_val (res[3], 1.f))
    return FALSE;

  if (x_0 != NULL)
    *x_0 = res[0];
  if (y_0 != NULL)
    *y_0 = res[1];

  return TRUE;
}

/**
 * graphene_matrix_get_row:
 * @m: a #graphene_matrix_t
 * @index_: the index of the row vector, between 0 and 3
 * @res: (out caller-allocates): return location for the #graphene_vec4_t
 *   that is used to store the row vector
 *
 * Retrieves the given row vector at @index_ inside a matrix.
 *
 * Since: 1.0
 */
void
graphene_matrix_get_row (const graphene_matrix_t *m,
                         unsigned int             index_,
                         graphene_vec4_t         *res)
{
  switch (index_)
    {
    case 0:
      res->value = m->value.x;
      break;

    case 1:
      res->value = m->value.y;
      break;

    case 2:
      res->value = m->value.z;
      break;

    case 3:
      res->value = m->value.w;
      break;

    default:
      res->value = graphene_simd4f_init_zero ();
      break;
    }
}

/**
 * graphene_matrix_get_value:
 * @m: a #graphene_matrix_t
 * @row: the row index
 * @col: the column index
 *
 * Retrieves the value at the given @row and @col index.
 *
 * Returns: the value at the given indices
 *
 * Since: 1.0
 */
float
graphene_matrix_get_value (const graphene_matrix_t *m,
                           unsigned int             row,
                           unsigned int             col)
{
  graphene_simd4f_t r;

  if (row > 3 || col > 3)
    return 0.f;

  switch (row)
    {
    case 0:
      r = m->value.x;
      break;

    case 1:
      r = m->value.y;
      break;

    case 2:
      r = m->value.z;
      break;

    case 3:
      r = m->value.w;
      break;

    default:
      return 0.f;
    }

  switch (col)
    {
    case 0:
      return graphene_simd4f_get (r, 0);

    case 1:
      return graphene_simd4f_get (r, 1);

    case 2:
      return graphene_simd4f_get (r, 2);

    case 3:
      return graphene_simd4f_get (r, 3);

    default:
      return 0.f;
    }

  return 0.f;
}

/**
 * graphene_matrix_print:
 * @m: The matrix to print
 *
 * Prints the contents of a matrix.
 *
 * Since: 1.0
 */
void
graphene_matrix_print (const graphene_matrix_t *m)
{
  for (int i = 0; i < 4; i++)
    {
      fprintf (stderr,
               "| %+.6f %+.6f %+.6f %+.6f |\n",
               graphene_matrix_get_value (m, i, 0),
               graphene_matrix_get_value (m, i, 1),
               graphene_matrix_get_value (m, i, 2),
               graphene_matrix_get_value (m, i, 3));
    }
}

/**
 * graphene_matrix_multiply:
 * @a: a #graphene_matrix_t
 * @b: a #graphene_matrix_t
 * @res: (out caller-allocates): return location for the matrix
 *   result
 *
 * Multiplies two #graphene_matrix_t.
 *
 * Remember: matrix multiplication is not commutative, except for the
 * identity matrix; the product of this multiplication is (A * B).
 *
 * Since: 1.0
 */
void
graphene_matrix_multiply (const graphene_matrix_t *a,
                          const graphene_matrix_t *b,
                          graphene_matrix_t       *res)
{
  graphene_simd4x4f_matrix_mul (&a->value, &b->value, &res->value);
}

/**
 * graphene_matrix_determinant:
 * @m: a #graphene_matrix_t
 *
 * Computes the determinant of the given matrix.
 *
 * Returns: the value of the determinant
 *
 * Since: 1.0
 */
float
graphene_matrix_determinant (const graphene_matrix_t *m)
{
  graphene_simd4f_t det;

  graphene_simd4x4f_determinant (&m->value, &det, NULL);

  return graphene_simd4f_get_x (det);
}

/**
 * graphene_matrix_translate:
 * @m: a #graphene_matrix_t
 * @pos: a #graphene_point3d_t
 *
 * Adds a translation transformation to @m using the coordinates
 * of the given #graphene_point3d_t.
 *
 * Since: 1.0
 */
void
graphene_matrix_translate (graphene_matrix_t        *m,
                           const graphene_point3d_t *pos)
{
  graphene_simd4x4f_t trans_m;

  graphene_simd4x4f_translation (&trans_m, pos->x, pos->y, pos->z);
  graphene_simd4x4f_matrix_mul (&m->value, &trans_m, &m->value);
}


static inline void
graphene_matrix_rotate_internal (graphene_simd4x4f_t     *m,
                                 float                    rad,
                                 const graphene_simd4f_t  axis)
{
  graphene_simd4x4f_t rot_m;

  graphene_simd4x4f_rotation (&rot_m, rad, axis);
  graphene_simd4x4f_matrix_mul (m, &rot_m, m);
}

/**
 * graphene_matrix_rotate:
 * @m: a #graphene_matrix_t
 * @angle: the rotation angle, in degrees
 * @axis: the rotation axis, as a #graphene_vec3_t
 *
 * Adds a rotation transformation to @m, using the given @angle
 * and @axis vector.
 *
 * Since: 1.0
 */
void
graphene_matrix_rotate (graphene_matrix_t     *m,
                        float                  angle,
                        const graphene_vec3_t *axis)
{
  graphene_matrix_rotate_internal (&m->value, GRAPHENE_DEG_TO_RAD (angle), axis->value);
}

/**
 * graphene_matrix_rotate_x:
 * @m: a #graphene_matrix_t
 * @angle: the rotation angle, in degrees
 *
 * Adds a rotation transformation around the X axis to @m, using
 * the given @angle.
 *
 * Since: 1.0
 */
void
graphene_matrix_rotate_x (graphene_matrix_t *m,
                          float              angle)
{
  graphene_matrix_rotate_internal (&m->value, GRAPHENE_DEG_TO_RAD (angle),
                                   graphene_simd4f_init (1.f, 0.f, 0.f, 0.f));
}

/**
 * graphene_matrix_rotate_y:
 * @m: a #graphene_matrix_t
 * @angle: the rotation angle, in degrees
 *
 * Adds a rotation transformation around the Y axis to @m, using
 * the given @angle.
 *
 * Since: 1.0
 */
void
graphene_matrix_rotate_y (graphene_matrix_t *m,
                          float              angle)
{
  graphene_matrix_rotate_internal (&m->value, GRAPHENE_DEG_TO_RAD (angle),
                                   graphene_simd4f_init (0.f, 1.f, 0.f, 0.f));
}

/**
 * graphene_matrix_rotate_z:
 * @m: a #graphene_matrix_t
 * @angle: the rotation angle, in degrees
 *
 * Adds a rotation transformation around the Z axis to @m, using
 * the given @angle.
 *
 * Since: 1.0
 */
void
graphene_matrix_rotate_z (graphene_matrix_t *m,
                          float              angle)
{
  graphene_matrix_rotate_internal (&m->value, GRAPHENE_DEG_TO_RAD (angle),
                                   graphene_simd4f_init (0.f, 0.f, 1.f, 0.f));
}

/**
 * graphene_matrix_scale:
 * @m: a #graphene_matrix_t
 * @factor_x: scaling factor on the X axis
 * @factor_y: scaling factor on the Y axis
 * @factor_z: scaling factor on the Z axis
 *
 * Adds a scaling transformation to @m, using the three
 * given factors.
 *
 * Since: 1.0
 */
void
graphene_matrix_scale (graphene_matrix_t *m,
                       float              factor_x,
                       float              factor_y,
                       float              factor_z)
{
  graphene_simd4x4f_t scale_m;

  graphene_simd4x4f_scale (&scale_m, factor_x, factor_y, factor_z);
  graphene_simd4x4f_matrix_mul (&m->value, &scale_m, &m->value);
}

/**
 * graphene_matrix_skew_xy:
 * @m: a #graphene_matrix_t
 * @factor: skew factor
 *
 * Adds a skew of @factor on the X and Y axis to the given matrix.
 *
 * Since: 1.0
 */
void
graphene_matrix_skew_xy (graphene_matrix_t *m,
                         float              factor)
{
  graphene_simd4f_t m_x, m_y;

  m_x = m->value.x;
  m_y = m->value.y;

  m->value.y = graphene_simd4f_madd (m_x, graphene_simd4f_splat (factor), m_y);
}

/**
 * graphene_matrix_skew_xz:
 * @m: a #graphene_matrix_t
 * @factor: skew factor
 *
 * Adds a skew of @factor on the X and Z axis to the given matrix.
 *
 * Since: 1.0
 */
void
graphene_matrix_skew_xz (graphene_matrix_t *m,
                         float              factor)
{
  graphene_simd4f_t m_x, m_z;

  m_x = m->value.x;
  m_z = m->value.z;

  m->value.z = graphene_simd4f_madd (m_x, graphene_simd4f_splat (factor), m_z);
}

/**
 * graphene_matrix_skew_yz:
 * @m: a #graphene_matrix_t
 * @factor: skew factor
 *
 * Adds a skew of @factor on the Y and Z axis to the given matrix.
 *
 * Since: 1.0
 */
void
graphene_matrix_skew_yz (graphene_matrix_t *m,
                         float              factor)
{
  graphene_simd4f_t m_y, m_z;

  m_y = m->value.y;
  m_z = m->value.z;

  m->value.z = graphene_simd4f_madd (m_y, graphene_simd4f_splat (factor), m_z);
}

static inline void
graphene_matrix_transpose_transform_vec4 (const graphene_matrix_t *m,
                                          const graphene_vec4_t   *v,
                                          graphene_vec4_t         *res)
{
  float x, y, z, w;

  x = graphene_simd4f_get_x (graphene_simd4f_dot4 (m->value.x, v->value));
  y = graphene_simd4f_get_x (graphene_simd4f_dot4 (m->value.y, v->value));
  z = graphene_simd4f_get_x (graphene_simd4f_dot4 (m->value.z, v->value));
  w = graphene_simd4f_get_x (graphene_simd4f_dot4 (m->value.w, v->value));
  
  graphene_vec4_init (res, x, y, z, w);
}

/**
 * graphene_matrix_transpose:
 * @m: a #graphene_matrix_t
 * @res: (out caller-allocates): return location for the
 *   transposed matrix
 *
 * Transposes the given matrix.
 *
 * Since: 1.0
 */
void
graphene_matrix_transpose (const graphene_matrix_t *m,
                           graphene_matrix_t       *res)
{
  graphene_simd4x4f_transpose (&m->value, &res->value);
}

/**
 * graphene_matrix_inverse:
 * @m: a #graphene_matrix_t
 * @res: (out caller-allocates): return location for the
 *   inverse matrix
 *
 * Inverts the given matrix.
 *
 * Returns: `TRUE` if the matrix is invertible
 *
 * Since: 1.0
 */
bool
graphene_matrix_inverse (const graphene_matrix_t *m,
                         graphene_matrix_t       *res)
{
  return graphene_simd4x4f_inverse (&m->value, &res->value);
}

/**
 * graphene_matrix_perspective:
 * @m: a #graphene_matrix_t
 * @depth: the depth of the perspective
 * @res: (out caller-allocates): return location for the
 *   perspective matrix
 *
 * Applies a perspective of @depth to the matrix.
 *
 * Since: 1.0
 */
void
graphene_matrix_perspective (const graphene_matrix_t *m,
                             float                    depth,
                             graphene_matrix_t       *res)
{

  res->value = m->value;

  graphene_simd4x4f_perspective (&res->value, depth);
}

/**
 * graphene_matrix_normalize:
 * @m: a #graphene_matrix_t
 * @res: (out caller-allocates): return location for the normalized matrix
 *
 * Normalizes the given #graphene_matrix_t.
 *
 * Since: 1.0
 */
void
graphene_matrix_normalize (const graphene_matrix_t *m,
                           graphene_matrix_t       *res)
{

  float ww = graphene_simd4f_get_w (m->value.w);
  if (graphene_approx_val (ww, 0.f))
    return;

  graphene_simd4f_t n = graphene_simd4f_splat (1.f / ww);

  res->value.x = graphene_simd4f_mul (m->value.x, n);
  res->value.y = graphene_simd4f_mul (m->value.y, n);
  res->value.z = graphene_simd4f_mul (m->value.z, n);
  res->value.w = graphene_simd4f_mul (m->value.w, n);
}

/**
 * graphene_matrix_get_x_scale:
 * @m: a #graphene_matrix_t
 *
 * Retrieves the scaling factor on the X axis in @m.
 *
 * Returns: the value of the scaling factor
 *
 * Since: 1.0
 */
float
graphene_matrix_get_x_scale (const graphene_matrix_t *m)
{
  return graphene_simd4f_get_x (m->value.x);
}

/**
 * graphene_matrix_get_y_scale:
 * @m: a #graphene_matrix_t
 *
 * Retrieves the scaling factor on the Y axis in @m.
 *
 * Returns: the value of the scaling factor
 *
 * Since: 1.0
 */
float
graphene_matrix_get_y_scale (const graphene_matrix_t *m)
{
  return graphene_simd4f_get_y (m->value.y);
}

/**
 * graphene_matrix_get_z_scale:
 * @m: a #graphene_matrix_t
 *
 * Retrieves the scaling factor on the Z axis in @m.
 *
 * Returns: the value of the scaling factor
 *
 * Since: 1.0
 */
float
graphene_matrix_get_z_scale (const graphene_matrix_t *m)
{
  return graphene_simd4f_get_z (m->value.z);
}


/**
 * graphene_vec3_init:
 * @v: a #graphene_vec3_t
 * @x: the X field of the vector
 * @y: the Y field of the vector
 * @z: the Z field of the vector
 *
 * Initializes a #graphene_vec3_t using the given values.
 *
 * This function can be called multiple times.
 *
 * Returns: (transfer none): a pointer to the initialized
 *   vector
 *
 * Since: 1.0
 */
graphene_vec3_t *
graphene_vec3_init (graphene_vec3_t *v,
                    float            x,
                    float            y,
                    float            z)
{
  v->value = graphene_simd4f_init (x, y, z, 0.f);

  return v;
}



/**
 * test function 
 */

void show(float *v,const char *str)
{
  int i,j;
  printf("matrix: %s\n",str);
  for (i = 0 ; i < 4; i++)
    {
      for (j = 0 ; j < 4; j++)
	printf("%f, ",v[i+4*j]);
      printf("\n");
    }
  printf("------------------------\n");
}
