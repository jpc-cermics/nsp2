/* graphene-simd4f.h: SIMD wrappers and operations
 *
 * Copyright 2014  Emmanuele Bassi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SH1_0 THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#ifndef __GRAPHENE_SIMD4F_H__
#define __GRAPHENE_SIMD4F_H__

#include <string.h>
#include <math.h>

static void                    graphene_simd4x4f_transpose_in_place (graphene_simd4x4f_t *s);


static graphene_simd4f_t       graphene_simd4f_init            (float                   x,
								float                   y,
								float                   z,
								float                   w);
static graphene_simd4f_t       graphene_simd4f_init_zero       (void);

static void                    graphene_simd4f_dup_4f          (const graphene_simd4f_t s,
                                                         float                  *v);

static float                   graphene_simd4f_get_x           (const graphene_simd4f_t s);

static float                   graphene_simd4f_get_y           (const graphene_simd4f_t s);

static float                   graphene_simd4f_get_z           (const graphene_simd4f_t s);

static graphene_simd4f_t       graphene_simd4f_splat           (float                   v);

static graphene_simd4f_t       graphene_simd4f_splat_x         (const graphene_simd4f_t s);

static graphene_simd4f_t       graphene_simd4f_splat_y         (const graphene_simd4f_t s);

static graphene_simd4f_t       graphene_simd4f_splat_z         (const graphene_simd4f_t s);

static graphene_simd4f_t       graphene_simd4f_splat_w         (const graphene_simd4f_t s);


static graphene_simd4f_t       graphene_simd4f_add             (const graphene_simd4f_t a,
                                                         const graphene_simd4f_t b);

static graphene_simd4f_t       graphene_simd4f_sub             (const graphene_simd4f_t a,
                                                         const graphene_simd4f_t b);

static graphene_simd4f_t       graphene_simd4f_mul             (const graphene_simd4f_t a,
                                                         const graphene_simd4f_t b);

static graphene_simd4f_t       graphene_simd4f_sqrt            (const graphene_simd4f_t s);

static graphene_simd4f_t       graphene_simd4f_rsqrt           (const graphene_simd4f_t s);


static graphene_simd4f_t       graphene_simd4f_cross3          (const graphene_simd4f_t a,
                                                         const graphene_simd4f_t b);

static graphene_simd4f_t       graphene_simd4f_dot3            (const graphene_simd4f_t a,
                                                         const graphene_simd4f_t b);

static float                   graphene_simd4f_dot3_scalar     (const graphene_simd4f_t a,
                                                         const graphene_simd4f_t b);

static graphene_simd4f_t       graphene_simd4f_min             (const graphene_simd4f_t a,
                                                         const graphene_simd4f_t b);

static graphene_simd4f_t       graphene_simd4f_max             (const graphene_simd4f_t a,
                                                         const graphene_simd4f_t b);


static graphene_simd4f_t       graphene_simd4f_shuffle_wxyz    (const graphene_simd4f_t s);

static graphene_simd4f_t       graphene_simd4f_shuffle_zwxy    (const graphene_simd4f_t s);

static bool                    graphene_simd4f_cmp_eq          (const graphene_simd4f_t a,
                                                         const graphene_simd4f_t b);

static graphene_simd4f_t       graphene_simd4f_neg             (const graphene_simd4f_t s);


/* Fallback implementation using scalar types */

#define graphene_simd4f_init(x,y,z,w) \
  (graphene_simd4f_init ((x), (y), (z), (w)))
#define graphene_simd4f_init_zero() \
  (graphene_simd4f_init_zero ())
#define graphene_simd4f_init_4f(v) \
  (graphene_simd4f_init_4f ((const float *) (v)))
#define graphene_simd4f_init_3f(v) \
  (graphene_simd4f_init_3f ((const float *) (v)))
#define graphene_simd4f_init_2f(v) \
  (graphene_simd4f_init_2f ((const float *) (v)))
#define graphene_simd4f_dup_4f(s,v) \
  (graphene_simd4f_dup_4f ((s), (float *) (v)))
#define graphene_simd4f_dup_3f(s,v) \
  (graphene_simd4f_dup_3f ((s), (float *) (v)))
#define graphene_simd4f_dup_2f(s,v) \
  (graphene_simd4f_dup_2f ((s), (float *) (v)))
#define graphene_simd4f_get(s,i) \
  (graphene_simd4f_get ((s), (i)))
#define graphene_simd4f_get_x(s) \
  (graphene_simd4f_get_x ((s)))
#define graphene_simd4f_get_y(s) \
  (graphene_simd4f_get_y ((s)))
#define graphene_simd4f_get_z(s) \
  (graphene_simd4f_get_z ((s)))
#define graphene_simd4f_get_w(s) \
  (graphene_simd4f_get_w ((s)))
#define graphene_simd4f_splat(v) \
  (graphene_simd4f_splat ((v)))
#define graphene_simd4f_splat_x(s) \
  (graphene_simd4f_splat_x ((s)))
#define graphene_simd4f_splat_y(s) \
  (graphene_simd4f_splat_y ((s)))
#define graphene_simd4f_splat_z(s) \
  (graphene_simd4f_splat_z ((s)))
#define graphene_simd4f_splat_w(s) \
  (graphene_simd4f_splat_w ((s)))
#define graphene_simd4f_add(a,b) \
  (graphene_simd4f_add ((a), (b)))
#define graphene_simd4f_sub(a,b) \
  (graphene_simd4f_sub ((a), (b)))
#define graphene_simd4f_mul(a,b) \
  (graphene_simd4f_mul ((a), (b)))
#define graphene_simd4f_div(a,b) \
  (graphene_simd4f_div ((a), (b)))
#define graphene_simd4f_sqrt(s) \
  (graphene_simd4f_sqrt ((s)))
#define graphene_simd4f_rsqrt(s) \
  (graphene_simd4f_rsqrt ((s)))
#define graphene_simd4f_reciprocal(s) \
  (graphene_simd4f_reciprocal ((s)))
#define graphene_simd4f_cross3(a,b) \
  (graphene_simd4f_cross3 ((a), (b)))
#define graphene_simd4f_dot3(a,b) \
  (graphene_simd4f_dot3 ((a), (b)))
#define graphene_simd4f_dot3_scalar(a,b) \
  (graphene_simd4f_dot3_scalar ((a), (b)))
#define graphene_simd4f_min(a,b) \
  (graphene_simd4f_min ((a), (b)))
#define graphene_simd4f_max(a,b) \
  (graphene_simd4f_max ((a), (b)))
#define graphene_simd4f_shuffle_wxyz(s) \
  (graphene_simd4f_shuffle_wxyz ((s)))
#define graphene_simd4f_shuffle_zwxy(s) \
  (graphene_simd4f_shuffle_zwxy ((s)))
#define graphene_simd4f_shuffle_yzwx(s) \
  (graphene_simd4f_shuffle_yzwx ((s)))
#define graphene_simd4f_flip_sign_0101(s) \
  (graphene_simd4f_flip_sign_0101 ((s)))
#define graphene_simd4f_flip_sign_1010(s) \
  (graphene_simd4f_flip_sign_1010 ((s)))
#define graphene_simd4f_zero_w(v) \
  (graphene_simd4f_zero_w ((v)))
#define graphene_simd4f_zero_zw(v) \
  (graphene_simd4f_zero_zw ((v)))
#define graphene_simd4f_merge_w(s,v) \
  (graphene_simd4f_merge_w ((s), (v)))
#define graphene_simd4f_merge_high(a,b) \
  (graphene_simd4f_merge_high ((a), (b)))
#define graphene_simd4f_merge_low(a,b) \
  (graphene_simd4f_merge_low ((a), (b)))
#define graphene_simd4f_cmp_eq(a,b) \
  (graphene_simd4f_cmp_eq ((a), (b)))
#define graphene_simd4f_cmp_neq(a,b) \
  (graphene_simd4f_cmp_neq ((a), (b)))
#define graphene_simd4f_cmp_lt(a,b) \
  (graphene_simd4f_cmp_lt ((a), (b)))
#define graphene_simd4f_cmp_le(a,b) \
  (graphene_simd4f_cmp_le ((a), (b)))
#define graphene_simd4f_cmp_ge(a,b) \
  (graphene_simd4f_cmp_ge ((a), (b)))
#define graphene_simd4f_cmp_gt(a,b) \
  (graphene_simd4f_cmp_gt ((a), (b)))
#define graphene_simd4f_neg(s) \
  (graphene_simd4f_neg ((s)))

/* Generic operations, inlined */

/**
 * graphene_simd4f_madd:
 * @m1: a #graphene_simd4f_t
 * @m2: a #graphene_simd4f_t
 * @a: a #graphene_simd4f_t
 *
 * Adds @a to the product of @m1 and @m2.
 *
 * Returns: the result vector
 *
 * Since: 1.0
 */
static inline graphene_simd4f_t
graphene_simd4f_madd (const graphene_simd4f_t m1,
                      const graphene_simd4f_t m2,
                      const graphene_simd4f_t a)
{
  return graphene_simd4f_add (graphene_simd4f_mul (m1, m2), a);
}

/**
 * graphene_simd4f_sum:
 * @v: a #graphene_simd4f_t
 *
 * Sums all components of the given vector.
 *
 * Returns: a vector with all components set to be the
 *   sum of the passed #graphene_simd4f_t
 *
 * Since: 1.0
 */
static inline graphene_simd4f_t
graphene_simd4f_sum (const graphene_simd4f_t v)
{
  const graphene_simd4f_t x = graphene_simd4f_splat_x (v);
  const graphene_simd4f_t y = graphene_simd4f_splat_y (v);
  const graphene_simd4f_t z = graphene_simd4f_splat_z (v);
  const graphene_simd4f_t w = graphene_simd4f_splat_w (v);

  return graphene_simd4f_add (graphene_simd4f_add (x, y),
                              graphene_simd4f_add (z, w));
}

/**
 * graphene_simd4f_sum_scalar:
 * @v: a #graphene_simd4f_t
 *
 * Sums all the components of the given vector.
 *
 * Returns: a scalar value with the sum of the components
 *   of the given #graphene_simd4f_t
 *
 * Since: 1.0
 */
static inline float
graphene_simd4f_sum_scalar (const graphene_simd4f_t v)
{
  return graphene_simd4f_get_x (graphene_simd4f_sum (v));
}

/**
 * graphene_simd4f_dot4:
 * @a: a #graphene_simd4f_t
 * @b: a #graphene_simd4f_t
 *
 * Computes the dot product of all the components of the two
 * given #graphene_simd4f_t.
 *
 * Returns: a vector whose components are all set to be the
 *   dot product of the components of the two operands
 *
 * Since: 1.0
 */
static inline graphene_simd4f_t
graphene_simd4f_dot4 (const graphene_simd4f_t a,
                      const graphene_simd4f_t b)
{
  return graphene_simd4f_sum (graphene_simd4f_mul (a, b));
}

/**
 * graphene_simd4f_dot2:
 * @a: a #graphene_simd4f_t
 * @b: a #graphene_simd4f_t
 *
 * Computes the dot product of the first two components of the
 * two given #graphene_simd4f_t.
 *
 * Returns: a vector whose components are all set to the
 *   dot product of the components of the two operands
 *
 * Since: 1.0
 */
static inline graphene_simd4f_t
graphene_simd4f_dot2 (const graphene_simd4f_t a,
                      const graphene_simd4f_t b)
{
  const graphene_simd4f_t m = graphene_simd4f_mul (a, b);
  const graphene_simd4f_t x = graphene_simd4f_splat_x (m);
  const graphene_simd4f_t y = graphene_simd4f_splat_y (m);

  return graphene_simd4f_add (x, y);
}

/**
 * graphene_simd4f_length4:
 * @v: a #graphene_simd4f_t
 *
 * Computes the length of the given #graphene_simd4f_t vector,
 * using all four of its components.
 *
 * Returns: the length vector
 *
 * Since: 1.0
 */
static inline graphene_simd4f_t
graphene_simd4f_length4 (const graphene_simd4f_t v)
{
  return graphene_simd4f_sqrt (graphene_simd4f_dot4 (v, v));
}

/**
 * graphene_simd4f_length3:
 * @v: a #graphene_simd4f_t
 *
 * Computes the length of the given #graphene_simd4f_t vector,
 * using the first three of its components.
 *
 * Returns: the length vector
 *
 * Since: 1.0
 */
static inline graphene_simd4f_t
graphene_simd4f_length3 (const graphene_simd4f_t v)
{
  return graphene_simd4f_sqrt (graphene_simd4f_dot3 (v, v));
}

/**
 * graphene_simd4f_length2:
 * @v: a #graphene_simd4f_t
 *
 * Computes the length of the given #graphene_simd4f_t vector,
 * using the first two of its components.
 *
 * Returns: the length vector
 *
 * Since: 1.0
 */
static inline graphene_simd4f_t
graphene_simd4f_length2 (const graphene_simd4f_t v)
{
  return graphene_simd4f_sqrt (graphene_simd4f_dot2 (v, v));
}

/**
 * graphene_simd4f_normalize4:
 * @v: a #graphene_simd4f_t
 *
 * Computes the normalization of the given #graphene_simd4f_t vector,
 * using all of its components.
 *
 * Returns: the normalized vector
 *
 * Since: 1.0
 */
static inline graphene_simd4f_t
graphene_simd4f_normalize4 (const graphene_simd4f_t v)
{
  graphene_simd4f_t invlen = graphene_simd4f_rsqrt (graphene_simd4f_dot4 (v, v));
  return graphene_simd4f_mul (v, invlen);
}

/**
 * graphene_simd4f_normalize3:
 * @v: a #graphene_simd4f_t
 *
 * Computes the normalization of the given #graphene_simd4f_t vector,
 * using the first three of its components.
 *
 * Returns: the normalized vector
 *
 * Since: 1.0
 */
static inline graphene_simd4f_t
graphene_simd4f_normalize3 (const graphene_simd4f_t v)
{
  graphene_simd4f_t invlen = graphene_simd4f_rsqrt (graphene_simd4f_dot3 (v, v));
  return graphene_simd4f_mul (v, invlen);
}

/**
 * graphene_simd4f_normalize2:
 * @v: a #graphene_simd4f_t
 *
 * Computes the normalization of the given #graphene_simd4f_t vector,
 * using the first two of its components.
 *
 * Returns: the normalized vector
 *
 * Since: 1.0
 */
static inline graphene_simd4f_t
graphene_simd4f_normalize2 (const graphene_simd4f_t v)
{
  graphene_simd4f_t invlen = graphene_simd4f_rsqrt (graphene_simd4f_dot2 (v, v));
  return graphene_simd4f_mul (v, invlen);
}

/**
 * graphene_simd4f_is_zero4:
 * @v: a #graphene_simd4f_t
 *
 * Checks whether the given #graphene_simd4f_t has all its components
 * set to 0.
 *
 * Returns: `true` if all the vector components are zero
 *
 * Since: 1.0
 */
static inline bool
graphene_simd4f_is_zero4 (const graphene_simd4f_t v)
{
  graphene_simd4f_t zero = graphene_simd4f_init_zero ();
  return graphene_simd4f_cmp_eq (v, zero);
}

/**
 * graphene_simd4f_is_zero3:
 * @v: a #graphene_simd4f_t
 *
 * Checks whether the given #graphene_simd4f_t has the first three of
 * its components set to 0.
 *
 * Returns: `true` if the vector's components are zero
 *
 * Since: 1.0
 */
static inline bool
graphene_simd4f_is_zero3 (const graphene_simd4f_t v)
{
  return graphene_simd4f_get_x (v) == 0.f &&
         graphene_simd4f_get_y (v) == 0.f &&
         graphene_simd4f_get_z (v) == 0.f;
}

/**
 * graphene_simd4f_is_zero2:
 * @v: a #graphene_simd4f_t
 *
 * Checks whether the given #graphene_simd4f_t has the first two of
 * its components set to 0.
 *
 * Returns: `true` if the vector's components are zero
 *
 * Since: 1.0
 */
static inline bool
graphene_simd4f_is_zero2 (const graphene_simd4f_t v)
{
  return graphene_simd4f_get_x (v) == 0.f &&
         graphene_simd4f_get_y (v) == 0.f;
}

/**
 * graphene_simd4f_interpolate:
 * @a: a #graphene_simd4f_t
 * @b: a #graphene_simd4f_t
 * @f: the interpolation factor
 *
 * Linearly interpolates all components of the two given
 * #graphene_simd4f_t vectors using the given factor @f.
 *
 * Returns: the intrerpolated vector
 *
 * Since: 1.0
 */
static inline graphene_simd4f_t
graphene_simd4f_interpolate (const graphene_simd4f_t a,
                             const graphene_simd4f_t b,
                             float                   f)
{
  const graphene_simd4f_t one_minus_f = graphene_simd4f_sub (graphene_simd4f_splat (1.f),
                                                             graphene_simd4f_splat (f));

  return graphene_simd4f_add (graphene_simd4f_mul (one_minus_f, a),
                              graphene_simd4f_mul (graphene_simd4f_splat (f), b));
}

/**
 * graphene_simd4f_clamp:
 * @v: a #graphene_simd4f_t
 * @min: the lower boundary
 * @max: the upper boundary
 *
 * Ensures that all components of the vector @v are within
 * the components of the @lower and @upper boundaries.
 *
 * Returns: the clamped vector
 *
 * Since: 1.2
 */
static inline graphene_simd4f_t
graphene_simd4f_clamp (const graphene_simd4f_t v,
                       const graphene_simd4f_t min,
                       const graphene_simd4f_t max)
{
  const graphene_simd4f_t tmp = graphene_simd4f_max (min, v);

  return graphene_simd4f_min (tmp, max);
}

/**
 * graphene_simd4f_clamp_scalar:
 * @v: a #graphene_simd4f_t
 * @min: the lower boundary
 * @max: the upper boundary
 *
 * Ensures that all components of the vector @v are within
 * the @lower and @upper boundary scalar values.
 *
 * Returns: the clamped vector
 *
 * Since: 1.2
 */
static inline graphene_simd4f_t
graphene_simd4f_clamp_scalar (const graphene_simd4f_t v,
                              float                   min,
                              float                   max)
{
  return graphene_simd4f_clamp (v,
                                graphene_simd4f_splat (min),
                                graphene_simd4f_splat (max));
}

/**
 * graphene_simd4f_min_val:
 * @v: a #graphene_simd4f_t
 *
 * Computes the minimum value of all the channels in the given vector.
 *
 * Returns: a vector whose components are all set to the
 *   minimum value in the original vector
 *
 * Since: 1.4
 */
static inline graphene_simd4f_t
graphene_simd4f_min_val (const graphene_simd4f_t v)
{
  graphene_simd4f_t s = v;

  s = graphene_simd4f_min (s, graphene_simd4f_shuffle_wxyz (s));
  s = graphene_simd4f_min (s, graphene_simd4f_shuffle_zwxy (s));

  return s;
}

/**
 * graphene_simd4f_max_val:
 * @v: a #graphene_simd4f_t
 *
 * Computes the maximum value of all the channels in the given vector.
 *
 * Returns: a vector whose components are all set to the
 *   maximum value in the original vector
 *
 * Since: 1.4
 */
static inline graphene_simd4f_t
graphene_simd4f_max_val (const graphene_simd4f_t v)
{
  graphene_simd4f_t s = v;

  s = graphene_simd4f_max (s, graphene_simd4f_shuffle_wxyz (s));
  s = graphene_simd4f_max (s, graphene_simd4f_shuffle_zwxy (s));

  return s;
}


#endif /* __GRAPHENE_SIMD4F_H__ */

