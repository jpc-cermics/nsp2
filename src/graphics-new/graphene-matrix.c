#include <math.h>
#include <float.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
//#include "utils.h"

#include "graphene-matrix.h"
#include "graphene-simd4f.h"

#define TRUE 1
#define FALSE 0

typedef struct _graphene_vec3_t         graphene_vec3_t;
typedef struct _graphene_vec4_t         graphene_vec4_t;

typedef struct _graphene_matrix_t       graphene_matrix_t;

typedef struct _graphene_size_t         graphene_size_t;
typedef struct _graphene_rect_t         graphene_rect_t;

#include "graphene-simd4f.h"

#define GRAPHENE_PI_2           1.5707963267948966192313217f
#define GRAPHENE_PI M_PI
#define GRAPHENE_DEG_TO_RAD(x)          ((x) * (GRAPHENE_PI / 180.f))
#define GRAPHENE_RAD_TO_DEG(x)          ((x) * (180.f / GRAPHENE_PI))
#define GRAPHENE_FLOAT_EPSILON FLT_EPSILON


static void
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
 * SECTION:graphene-simd4f
 * @Title: SIMD vector
 * @short_description: Low level floating point 4-sized vector
 *
 * The #graphene_simd4f_t type wraps a platform specific implementation of
 * a vector of four floating point values.
 *
 * Graphene can be compiled to use different implementations of the SIMD
 * types, and will generally prefer the faster hardware-backed implementation
 * if one is available.
 *
 * The #graphene_simd4f_t should be treated as an opaque, integral type;
 * you cannot access its components directly, and you can only operate on
 * all components at the same time.
 */

// #include "graphene-simd4f.h"

/**
 * graphene_simd4f_t:
 *
 * A vector type containing four floating point values.
 *
 * The contents of the #graphene_simd4f_t type are private and
 * cannot be directly accessed; use the provided API instead.
 *
 * Since: 1.0
 */

graphene_simd4f_t
(graphene_simd4f_init) (float x,
                        float y,
                        float z,
                        float w)
{
  graphene_simd4f_t s = { x, y, z, w };
  return s;
}

graphene_simd4f_t
(graphene_simd4f_init_zero) (void)
{
  return graphene_simd4f_init (0.f, 0.f, 0.f, 0.f);
}

void
(graphene_simd4f_dup_4f) (const graphene_simd4f_t  s,
                          float                   *v)
{
  memcpy (v, &s, sizeof (float) * 4);
}

float
(graphene_simd4f_get_x) (const graphene_simd4f_t s)
{
  return s.x;
}

float
(graphene_simd4f_get_y) (const graphene_simd4f_t s)
{
  return s.y;
}

float
(graphene_simd4f_get_z) (const graphene_simd4f_t s)
{
  return s.z;
}

graphene_simd4f_t
(graphene_simd4f_splat) (float v)
{
  graphene_simd4f_t s = { v, v, v, v };
  return s;
}

graphene_simd4f_t
(graphene_simd4f_splat_x) (graphene_simd4f_t v)
{
  graphene_simd4f_t s = { v.x, v.x, v.x, v.x };
  return s;
}

graphene_simd4f_t
(graphene_simd4f_splat_y) (graphene_simd4f_t v)
{
  graphene_simd4f_t s = { v.y, v.y, v.y, v.y };
  return s;
}

graphene_simd4f_t
(graphene_simd4f_splat_z) (graphene_simd4f_t v)
{
  graphene_simd4f_t s = { v.z, v.z, v.z, v.z };
  return s;
}

graphene_simd4f_t
(graphene_simd4f_splat_w) (graphene_simd4f_t v)
{
  graphene_simd4f_t s = { v.w, v.w, v.w, v.w };
  return s;
}

graphene_simd4f_t
(graphene_simd4f_sqrt) (graphene_simd4f_t v)
{
  graphene_simd4f_t s = {
    sqrtf (v.x),
    sqrtf (v.y),
    sqrtf (v.z),
    sqrtf (v.w)
  };
  return s;
}

graphene_simd4f_t
(graphene_simd4f_rsqrt) (graphene_simd4f_t v)
{
  graphene_simd4f_t s = {
    v.x != 0.f ? 1.0f / sqrtf (v.x) : 0.f,
    v.y != 0.f ? 1.0f / sqrtf (v.y) : 0.f,
    v.z != 0.f ? 1.0f / sqrtf (v.z) : 0.f,
    v.w != 0.f ? 1.0f / sqrtf (v.w) : 0.f
  };
  return s;
}

graphene_simd4f_t
(graphene_simd4f_add) (const graphene_simd4f_t a,
                       const graphene_simd4f_t b)
{
  graphene_simd4f_t s = {
    a.x + b.x,
    a.y + b.y,
    a.z + b.z,
    a.w + b.w
  };
  return s;
}

graphene_simd4f_t
(graphene_simd4f_sub) (const graphene_simd4f_t a,
                       const graphene_simd4f_t b)
{
  graphene_simd4f_t s = {
    a.x - b.x,
    a.y - b.y,
    a.z - b.z,
    a.w - b.w
  };
  return s;
}

graphene_simd4f_t
(graphene_simd4f_mul) (const graphene_simd4f_t a,
                       const graphene_simd4f_t b)
{
  graphene_simd4f_t s = {
    a.x * b.x,
    a.y * b.y,
    a.z * b.z,
    a.w * b.w
  };
  return s;
}

graphene_simd4f_t
(graphene_simd4f_cross3) (const graphene_simd4f_t a,
                          const graphene_simd4f_t b)
{
  return graphene_simd4f_init (a.y * b.z - a.z * b.y,
                               a.z * b.x - a.x * b.z,
                               a.x * b.y - a.y * b.x,
                               0.f);
}

graphene_simd4f_t
(graphene_simd4f_dot3) (const graphene_simd4f_t a,
                        const graphene_simd4f_t b)
{
  return graphene_simd4f_splat (graphene_simd4f_dot3_scalar (a, b));
}

float
(graphene_simd4f_dot3_scalar) (const graphene_simd4f_t a,
                               const graphene_simd4f_t b)
{
  return a.x * b.x + a.y * b.y + a.z * b.z;
}

graphene_simd4f_t
(graphene_simd4f_min) (const graphene_simd4f_t a,
                       const graphene_simd4f_t b)
{
  return graphene_simd4f_init (a.x < b.x ? a.x : b.x,
                               a.y < b.y ? a.y : b.y,
                               a.z < b.z ? a.z : b.z,
                               a.w < b.w ? a.w : b.w);
}

graphene_simd4f_t
(graphene_simd4f_max) (const graphene_simd4f_t a,
                       const graphene_simd4f_t b)
{
  return graphene_simd4f_init (a.x > b.x ? a.x : b.x,
                               a.y > b.y ? a.y : b.y,
                               a.z > b.z ? a.z : b.z,
                               a.w > b.w ? a.w : b.w);
}

graphene_simd4f_t
(graphene_simd4f_shuffle_wxyz) (const graphene_simd4f_t s)
{
  return graphene_simd4f_init (s.w, s.x, s.y, s.z);
}

graphene_simd4f_t
(graphene_simd4f_shuffle_zwxy) (const graphene_simd4f_t s)
{
  return graphene_simd4f_init (s.z, s.w, s.x, s.y);
}

bool
(graphene_simd4f_cmp_eq) (const graphene_simd4f_t a,
                          const graphene_simd4f_t b)
{
  return a.x == b.x &&
         a.y == b.y &&
         a.z == b.z &&
         a.w == b.w;
}


graphene_simd4f_t
(graphene_simd4f_neg) (const graphene_simd4f_t s)
{
  return graphene_simd4f_init (-s.x, -s.y, -s.z, -s.w);
}

