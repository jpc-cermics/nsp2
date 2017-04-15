#ifndef __GRAPHENE_MATRIX__H
#define __GRAPHENE_MATRIX__H

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

typedef struct {
  float x, y, z, w;
} graphene_simd4f_t;

typedef struct {
  graphene_simd4f_t x, y, z, w;
} graphene_simd4x4f_t;

#define GRAPHENE_PI_2           1.5707963267948966192313217f
#define GRAPHENE_PI M_PI
#define GRAPHENE_DEG_TO_RAD(x)          ((x) * (GRAPHENE_PI / 180.f))
#define GRAPHENE_RAD_TO_DEG(x)          ((x) * (180.f / GRAPHENE_PI))
#define GRAPHENE_FLOAT_EPSILON FLT_EPSILON
struct _graphene_matrix_t
{
  graphene_simd4x4f_t value;
};

struct _graphene_vec4_t
{
  graphene_simd4f_t  value;
};

struct _graphene_vec3_t
{
  graphene_simd4f_t value;
};

struct _graphene_point3d_t
{
  float x;
  float y;
  float z;
};

graphene_point3d_t *
graphene_point3d_init (graphene_point3d_t *p,
                       float               x,
                       float               y,
                       float               z);

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
                    float            w);


void
graphene_matrix_to_float (const graphene_matrix_t *m,
                          float                   *v);

graphene_matrix_t *
graphene_matrix_init_identity (graphene_matrix_t *m);
graphene_matrix_t *
graphene_matrix_init_from_float (graphene_matrix_t *m,
                                 const float       *v);

graphene_matrix_t *
graphene_matrix_init_from_vec4 (graphene_matrix_t     *m,
                                const graphene_vec4_t *v0,
                                const graphene_vec4_t *v1,
                                const graphene_vec4_t *v2,
                                const graphene_vec4_t *v3);

graphene_matrix_t *
graphene_matrix_init_from_matrix (graphene_matrix_t       *m,
                                  const graphene_matrix_t *src);

graphene_matrix_t *
graphene_matrix_init_perspective (graphene_matrix_t *m,
                                  float              fovy,
                                  float              aspect,
                                  float              z_near,
                                  float              z_far);

graphene_matrix_t *
graphene_matrix_init_ortho (graphene_matrix_t *m,
                            float              left,
                            float              right,
                            float              top,
                            float              bottom,
                            float              z_near,
                            float              z_far);
graphene_matrix_t *
graphene_matrix_init_look_at (graphene_matrix_t     *m,
                              const graphene_vec3_t *eye,
                              const graphene_vec3_t *center,
                              const graphene_vec3_t *up);

graphene_matrix_t *
graphene_matrix_init_frustum (graphene_matrix_t *m,
                              float              left,
                              float              right,
                              float              bottom,
                              float              top,
                              float              z_near,
                              float              z_far);

graphene_matrix_t *
graphene_matrix_init_scale (graphene_matrix_t *m,
                            float              x,
                            float              y,
                            float              z);

graphene_matrix_t *
graphene_matrix_init_translate (graphene_matrix_t        *m,
                                const graphene_point3d_t *p);

graphene_matrix_t *
graphene_matrix_init_skew (graphene_matrix_t *m,
                           float              x_skew,
                           float              y_skew);
graphene_matrix_t *
graphene_matrix_init_rotate (graphene_matrix_t     *m,
                             float                  angle,
                             const graphene_vec3_t *axis);
bool
graphene_matrix_is_identity (const graphene_matrix_t *m);

bool
graphene_matrix_is_2d (const graphene_matrix_t *m);

bool
graphene_matrix_is_backface_visible (const graphene_matrix_t *m);

bool
graphene_matrix_is_singular (const graphene_matrix_t *m);

graphene_matrix_t *
graphene_matrix_init_from_2d (graphene_matrix_t *m,
                              double             xx,
                              double             yx,
                              double             xy,
                              double             yy,
                              double             x_0,
                              double             y_0);
bool
graphene_matrix_to_2d (const graphene_matrix_t *m,
                       double                  *xx,
                       double                  *yx,
                       double                  *xy,
                       double                  *yy,
                       double                  *x_0,
                       double                  *y_0);

void
graphene_matrix_get_row (const graphene_matrix_t *m,
                         unsigned int             index_,
                         graphene_vec4_t         *res);
float
graphene_matrix_get_value (const graphene_matrix_t *m,
                           unsigned int             row,
                           unsigned int             col);

void
  graphene_matrix_print (const graphene_matrix_t *m);

void
graphene_matrix_multiply (const graphene_matrix_t *a,
                          const graphene_matrix_t *b,
                          graphene_matrix_t       *res);

float
  graphene_matrix_determinant (const graphene_matrix_t *m);

void
graphene_matrix_translate (graphene_matrix_t        *m,
                           const graphene_point3d_t *pos);

void
graphene_matrix_rotate (graphene_matrix_t     *m,
                        float                  angle,
                        const graphene_vec3_t *axis);
void
graphene_matrix_rotate_x (graphene_matrix_t *m,
                          float              angle);

void
graphene_matrix_rotate_y (graphene_matrix_t *m,
                          float              angle);

void
graphene_matrix_rotate_z (graphene_matrix_t *m,
                          float              angle);
void
graphene_matrix_scale (graphene_matrix_t *m,
                       float              factor_x,
                       float              factor_y,
                       float              factor_z);
void
graphene_matrix_skew_xy (graphene_matrix_t *m,
                         float              factor);
void
graphene_matrix_skew_xz (graphene_matrix_t *m,
                         float              factor);
void
graphene_matrix_skew_yz (graphene_matrix_t *m,
                         float              factor);

void
graphene_matrix_transpose (const graphene_matrix_t *m,
                           graphene_matrix_t       *res);

bool
graphene_matrix_inverse (const graphene_matrix_t *m,
                         graphene_matrix_t       *res);
void
graphene_matrix_perspective (const graphene_matrix_t *m,
                             float                    depth,
                             graphene_matrix_t       *res);
void
graphene_matrix_normalize (const graphene_matrix_t *m,
                           graphene_matrix_t       *res);
float
graphene_matrix_get_x_scale (const graphene_matrix_t *m);

float
graphene_matrix_get_y_scale (const graphene_matrix_t *m);
float
graphene_matrix_get_z_scale (const graphene_matrix_t *m);
graphene_vec3_t *
graphene_vec3_init (graphene_vec3_t *v,
                    float            x,
                    float            y,
                    float            z);

void show(float *v,const char *str);

#endif
