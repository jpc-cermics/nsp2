/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <nsp/interf.h> 
#include <nsp/lapack-c.h>

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
static int _wrap_dgbsv(Stack stack, int rhs, int opt, int lhs) /* dgbsv */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int, realmat, mat_int, mat_int, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *kl, *ku, *nrhs, *ab, *ldab, *ipiv, *b, *ldb, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &kl, &ku, &nrhs, &ab, &ldab, &ipiv, &b, &ldb, &info) == FAIL) return RET_BUG;
    ret = C2F(dgbsv)(n->I, kl->I, ku->I, nrhs->I, ab->R, ldab->I, ipiv->I, b->R, ldb->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgbsv(Stack stack, int rhs, int opt, int lhs) /* zgbsv */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int, mat, mat_int, mat_int, mat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *kl, *ku, *nrhs, *ab, *ldab, *ipiv, *b, *ldb, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &kl, &ku, &nrhs, &ab, &ldab, &ipiv, &b, &ldb, &info) == FAIL) return RET_BUG;
    ret = C2F(zgbsv)(n->I, kl->I, ku->I, nrhs->I, ab->C, ldab->I, ipiv->I, b->C, ldb->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dbdsqr(Stack stack, int rhs, int opt, int lhs) /* dbdsqr */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, mat_int, realmat, realmat, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *ncvt, *nru, *ncc, *d, *e, *vt, *ldvt, *u, *ldu, *c, *ldc, *work, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &ncvt, &nru, &ncc, &d, &e, &vt, &ldvt, &u, &ldu, &c, &ldc, &work, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dbdsqr)(uplo, n->I, ncvt->I, nru->I, ncc->I, d->R, e->R, vt->R, ldvt->I, u->R, ldu->I, c->R, ldc->I, work->R, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgebak(Stack stack, int rhs, int opt, int lhs) /* dgebak */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *job, *side;
  NspMatrix *n, *ilo, *ihi, *scale, *m, *v, *ldv, *info;
  int job_len, side_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &side, &n, &ilo, &ihi, &scale, &m, &v, &ldv, &info, &job_len, &side_len) == FAIL) return RET_BUG;
    ret = C2F(dgebak)(job, side, n->I, ilo->I, ihi->I, scale->R, m->I, v->R, ldv->I, info->I, job_len, side_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgebal(Stack stack, int rhs, int opt, int lhs) /* dgebal */
{
  int_types T[] = {string, mat_int, realmat, mat_int, mat_int, mat_int, realmat, mat_int, s_int,t_end};
  char *job;
  NspMatrix *n, *a, *lda, *ilo, *ihi, *scale, *info;
  int job_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &n, &a, &lda, &ilo, &ihi, &scale, &info, &job_len) == FAIL) return RET_BUG;
    ret = C2F(dgebal)(job, n->I, a->R, lda->I, ilo->I, ihi->I, scale->R, info->I, job_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgebd2(Stack stack, int rhs, int opt, int lhs) /* dgebd2 */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *d, *e, *tauq, *taup, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &d, &e, &tauq, &taup, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dgebd2)(m->I, n->I, a->R, lda->I, d->R, e->R, tauq->R, taup->R, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgebrd(Stack stack, int rhs, int opt, int lhs) /* dgebrd */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *d, *e, *tauq, *taup, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &d, &e, &tauq, &taup, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dgebrd)(m->I, n->I, a->R, lda->I, d->R, e->R, tauq->R, taup->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgecon(Stack stack, int rhs, int opt, int lhs) /* dgecon */
{
  int_types T[] = {string, mat_int, realmat, mat_int, realmat, realmat, realmat, mat_int, mat_int, s_int,t_end};
  char *norm;
  NspMatrix *n, *a, *lda, *anorm, *rcond, *work, *iwork, *info;
  int norm_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &n, &a, &lda, &anorm, &rcond, &work, &iwork, &info, &norm_len) == FAIL) return RET_BUG;
    ret = C2F(dgecon)(norm, n->I, a->R, lda->I, anorm->R, rcond->R, work->R, iwork->I, info->I, norm_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgbcon(Stack stack, int rhs, int opt, int lhs) /* dgbcon */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, realmat, mat_int, mat_int, realmat, realmat, realmat, mat_int, mat_int, s_int,t_end};
  char *norm;
  NspMatrix *n, *kl, *ku, *a, *lda, *ipiv, *anorm, *rcond, *work, *iwork, *info;
  int norm_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &n, &kl, &ku, &a, &lda, &ipiv, &anorm, &rcond, &work, &iwork, &info, &norm_len) == FAIL) return RET_BUG;
    ret = C2F(dgbcon)(norm, n->I, kl->I, ku->I, a->R, lda->I, ipiv->I, anorm->R, rcond->R, work->R, iwork->I, info->I, norm_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgbcon(Stack stack, int rhs, int opt, int lhs) /* zgbcon */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, mat, mat_int, mat_int, realmat, realmat, mat, realmat, mat_int, s_int,t_end};
  char *norm;
  NspMatrix *n, *kl, *ku, *a, *lda, *ipiv, *anorm, *rcond, *cwork, *work, *info;
  int norm_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &n, &kl, &ku, &a, &lda, &ipiv, &anorm, &rcond, &cwork, &work, &info, &norm_len) == FAIL) return RET_BUG;
    ret = C2F(zgbcon)(norm, n->I, kl->I, ku->I, a->C, lda->I, ipiv->I, anorm->R, rcond->R, cwork->C, work->R, info->I, norm_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgeequ(Stack stack, int rhs, int opt, int lhs) /* dgeequ */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *r, *c, *rowcnd, *colcnd, *amax, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &r, &c, &rowcnd, &colcnd, &amax, &info) == FAIL) return RET_BUG;
    ret = C2F(dgeequ)(m->I, n->I, a->R, lda->I, r->R, c->R, rowcnd->R, colcnd->R, amax->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}



static int _wrap_dgeev(Stack stack, int rhs, int opt, int lhs) /* dgeev */
{
  int_types T[] = {string, string, mat_int, realmat, mat_int, realmat, realmat, realmat, mat_int, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *jobvl, *jobvr;
  NspMatrix *n, *a, *lda, *wr, *wi, *vl, *ldvl, *vr, *ldvr, *work, *lwork, *info;
  int jobvl_len, jobvr_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobvl, &jobvr, &n, &a, &lda, &wr, &wi, &vl, &ldvl, &vr, &ldvr, &work, &lwork, &info, &jobvl_len, &jobvr_len) == FAIL) return RET_BUG;
    ret = C2F(dgeev)(jobvl, jobvr, n->I, a->R, lda->I, wr->R, wi->R, vl->R, ldvl->I, vr->R, ldvr->I, work->R, lwork->I, info->I, jobvl_len, jobvr_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgegs(Stack stack, int rhs, int opt, int lhs) /* dgegs */
{
  int_types T[] = {string, string, mat_int, realmat, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, mat_int, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *jobvsl, *jobvsr;
  NspMatrix *n, *a, *lda, *b, *ldb, *alphar, *alphai, *beta, *vsl, *ldvsl, *vsr, *ldvsr, *work, *lwork, *info;
  int jobvsl_len, jobvsr_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobvsl, &jobvsr, &n, &a, &lda, &b, &ldb, &alphar, &alphai, &beta, &vsl, &ldvsl, &vsr, &ldvsr, &work, &lwork, &info, &jobvsl_len, &jobvsr_len) == FAIL) return RET_BUG;
    ret = C2F(dgegs)(jobvsl, jobvsr, n->I, a->R, lda->I, b->R, ldb->I, alphar->R, alphai->R, beta->R, vsl->R, ldvsl->I, vsr->R, ldvsr->I, work->R, lwork->I, info->I, jobvsl_len, jobvsr_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgehd2(Stack stack, int rhs, int opt, int lhs) /* dgehd2 */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *n, *ilo, *ihi, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &ilo, &ihi, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dgehd2)(n->I, ilo->I, ihi->I, a->R, lda->I, tau->R, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgehrd(Stack stack, int rhs, int opt, int lhs) /* dgehrd */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *ilo, *ihi, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &ilo, &ihi, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dgehrd)(n->I, ilo->I, ihi->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgelq2(Stack stack, int rhs, int opt, int lhs) /* dgelq2 */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dgelq2)(m->I, n->I, a->R, lda->I, tau->R, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgelqf(Stack stack, int rhs, int opt, int lhs) /* dgelqf */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dgelqf)(m->I, n->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgels(Stack stack, int rhs, int opt, int lhs) /* dgels */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *trans;
  NspMatrix *m, *n, *nrhs, *a, *lda, *b, *ldb, *work, *lwork, *info;
  int trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&trans, &m, &n, &nrhs, &a, &lda, &b, &ldb, &work, &lwork, &info, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dgels)(trans, m->I, n->I, nrhs->I, a->R, lda->I, b->R, ldb->I, work->R, lwork->I, info->I, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgelss(Stack stack, int rhs, int opt, int lhs) /* dgelss */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *nrhs, *a, *lda, *b, *ldb, *s, *rcond, *rank, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &nrhs, &a, &lda, &b, &ldb, &s, &rcond, &rank, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dgelss)(m->I, n->I, nrhs->I, a->R, lda->I, b->R, ldb->I, s->R, rcond->R, rank->I, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgelsd(Stack stack, int rhs, int opt, int lhs) /* dgelsd */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *nrhs, *a, *lda, *b, *ldb, *s, *rcond, *rank, *work, *lwork, *iwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &nrhs, &a, &lda, &b, &ldb, &s, &rcond, &rank, &work, &lwork, &iwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dgelsd)(m->I, n->I, nrhs->I, a->R, lda->I, b->R, ldb->I, s->R, rcond->R, rank->I, work->R, lwork->I, iwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgelsx(Stack stack, int rhs, int opt, int lhs) /* dgelsx */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, mat_int, realmat, mat_int, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *nrhs, *a, *lda, *b, *ldb, *jpvt, *rcond, *rank, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &nrhs, &a, &lda, &b, &ldb, &jpvt, &rcond, &rank, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dgelsx)(m->I, n->I, nrhs->I, a->R, lda->I, b->R, ldb->I, jpvt->I, rcond->R, rank->I, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgelsy(Stack stack, int rhs, int opt, int lhs) /* dgelsy */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, mat_int, realmat, mat_int, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *nrhs, *a, *lda, *b, *ldb, *jpvt, *rcond, *rank, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &nrhs, &a, &lda, &b, &ldb, &jpvt, &rcond, &rank, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dgelsy)(m->I, n->I, nrhs->I, a->R, lda->I, b->R, ldb->I, jpvt->I, rcond->R, rank->I, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgeqlf(Stack stack, int rhs, int opt, int lhs) /* dgeqlf */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dgeqlf)(m->I, n->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgeql2(Stack stack, int rhs, int opt, int lhs) /* dgeql2 */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dgeql2)(m->I, n->I, a->R, lda->I, tau->R, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgeqp3(Stack stack, int rhs, int opt, int lhs) /* dgeqp3 */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, mat_int, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *jpvt, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &jpvt, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dgeqp3)(m->I, n->I, a->R, lda->I, jpvt->I, tau->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgeqpf(Stack stack, int rhs, int opt, int lhs) /* dgeqpf */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *jpvt, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &jpvt, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dgeqpf)(m->I, n->I, a->R, lda->I, jpvt->I, tau->R, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgeqr2(Stack stack, int rhs, int opt, int lhs) /* dgeqr2 */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dgeqr2)(m->I, n->I, a->R, lda->I, tau->R, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgeqrf(Stack stack, int rhs, int opt, int lhs) /* dgeqrf */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dgeqrf)(m->I, n->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgerfs(Stack stack, int rhs, int opt, int lhs) /* dgerfs */
{
  int_types T[] = {string, mat_int, mat_int, realmat, mat_int, realmat, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, realmat, realmat, mat_int, mat_int, s_int,t_end};
  char *trans;
  NspMatrix *n, *nrhs, *a, *lda, *af, *ldaf, *ipiv, *b, *ldb, *x, *ldx, *ferr, *berr, *work, *iwork, *info;
  int trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&trans, &n, &nrhs, &a, &lda, &af, &ldaf, &ipiv, &b, &ldb, &x, &ldx, &ferr, &berr, &work, &iwork, &info, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dgerfs)(trans, n->I, nrhs->I, a->R, lda->I, af->R, ldaf->I, ipiv->I, b->R, ldb->I, x->R, ldx->I, ferr->R, berr->R, work->R, iwork->I, info->I, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgerq2(Stack stack, int rhs, int opt, int lhs) /* dgerq2 */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dgerq2)(m->I, n->I, a->R, lda->I, tau->R, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgerqf(Stack stack, int rhs, int opt, int lhs) /* dgerqf */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dgerqf)(m->I, n->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgesvd(Stack stack, int rhs, int opt, int lhs) /* dgesvd */
{
  int_types T[] = {string, string, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *jobu, *jobvt;
  NspMatrix *m, *n, *a, *lda, *s, *u, *ldu, *vt, *ldvt, *work, *lwork, *info;
  int jobu_len, jobvt_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobu, &jobvt, &m, &n, &a, &lda, &s, &u, &ldu, &vt, &ldvt, &work, &lwork, &info, &jobu_len, &jobvt_len) == FAIL) return RET_BUG;
    ret = C2F(dgesvd)(jobu, jobvt, m->I, n->I, a->R, lda->I, s->R, u->R, ldu->I, vt->R, ldvt->I, work->R, lwork->I, info->I, jobu_len, jobvt_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgesdd(Stack stack, int rhs, int opt, int lhs) /* dgesdd */
{
  int_types T[] = {string, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, realmat, mat_int, mat_int, mat_int, s_int,t_end};
  char *jobz;
  NspMatrix *m, *n, *a, *lda, *s, *u, *ldu, *vt, *ldvt, *work, *lwork, *iwork, *info;
  int jobz_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobz, &m, &n, &a, &lda, &s, &u, &ldu, &vt, &ldvt, &work, &lwork, &iwork, &info, &jobz_len) == FAIL) return RET_BUG;
    ret = C2F(dgesdd)(jobz, m->I, n->I, a->R, lda->I, s->R, u->R, ldu->I, vt->R, ldvt->I, work->R, lwork->I, iwork->I, info->I, jobz_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgesv(Stack stack, int rhs, int opt, int lhs) /* dgesv */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, mat_int, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *nrhs, *a, *lda, *ipiv, *b, *ldb, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &nrhs, &a, &lda, &ipiv, &b, &ldb, &info) == FAIL) return RET_BUG;
    ret = C2F(dgesv)(n->I, nrhs->I, a->R, lda->I, ipiv->I, b->R, ldb->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgesvx(Stack stack, int rhs, int opt, int lhs) /* dgesvx */
{
  int_types T[] = {string, string, mat_int, mat_int, realmat, mat_int, realmat, mat_int, mat_int, string, realmat, realmat, realmat, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, mat_int, mat_int, s_int, s_int, s_int,t_end};
  char *fact, *trans, *equed;
  NspMatrix *n, *nrhs, *a, *lda, *af, *ldaf, *ipiv, *r, *c, *b, *ldb, *x, *ldx, *rcond, *ferr, *berr, *work, *iwork, *info;
  int fact_len, trans_len, equed_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&fact, &trans, &n, &nrhs, &a, &lda, &af, &ldaf, &ipiv, &equed, &r, &c, &b, &ldb, &x, &ldx, &rcond, &ferr, &berr, &work, &iwork, &info, &fact_len, &trans_len, &equed_len) == FAIL) return RET_BUG;
    ret = C2F(dgesvx)(fact, trans, n->I, nrhs->I, a->R, lda->I, af->R, ldaf->I, ipiv->I, equed, r->R, c->R, b->R, ldb->I, x->R, ldx->I, rcond->R, ferr->R, berr->R, work->R, iwork->I, info->I, fact_len, trans_len, equed_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgetf2(Stack stack, int rhs, int opt, int lhs) /* dgetf2 */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *ipiv, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &ipiv, &info) == FAIL) return RET_BUG;
    ret = C2F(dgetf2)(m->I, n->I, a->R, lda->I, ipiv->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgetrf(Stack stack, int rhs, int opt, int lhs) /* dgetrf */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *ipiv, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &ipiv, &info) == FAIL) return RET_BUG;
    ret = C2F(dgetrf)(m->I, n->I, a->R, lda->I, ipiv->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgbtrf(Stack stack, int rhs, int opt, int lhs) /* dgbtrf */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int, realmat, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *kl, *ku, *a, *lda, *ipiv, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &kl, &ku, &a, &lda, &ipiv, &info) == FAIL) return RET_BUG;
    ret = C2F(dgbtrf)(m->I, n->I, kl->I, ku->I, a->R, lda->I, ipiv->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgetri(Stack stack, int rhs, int opt, int lhs) /* dgetri */
{
  int_types T[] = {mat_int, realmat, mat_int, mat_int, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *a, *lda, *ipiv, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &a, &lda, &ipiv, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dgetri)(n->I, a->R, lda->I, ipiv->I, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgetrs(Stack stack, int rhs, int opt, int lhs) /* dgetrs */
{
  int_types T[] = {string, mat_int, mat_int, realmat, mat_int, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *trans;
  NspMatrix *n, *nrhs, *a, *lda, *ipiv, *b, *ldb, *info;
  int trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&trans, &n, &nrhs, &a, &lda, &ipiv, &b, &ldb, &info, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dgetrs)(trans, n->I, nrhs->I, a->R, lda->I, ipiv->I, b->R, ldb->I, info->I, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgbtrs(Stack stack, int rhs, int opt, int lhs) /* dgbtrs */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, mat_int, realmat, mat_int, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *trans;
  NspMatrix *n, *kl, *ku, *nrhs, *a, *lda, *ipiv, *b, *ldb, *info;
  int trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&trans, &n, &kl, &ku, &nrhs, &a, &lda, &ipiv, &b, &ldb, &info, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dgbtrs)(trans, n->I, kl->I, ku->I, nrhs->I, a->R, lda->I, ipiv->I, b->R, ldb->I, info->I, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dggbak(Stack stack, int rhs, int opt, int lhs) /* dggbak */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *job, *side;
  NspMatrix *n, *ilo, *ihi, *lscale, *rscale, *m, *v, *ldv, *info;
  int job_len, side_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &side, &n, &ilo, &ihi, &lscale, &rscale, &m, &v, &ldv, &info, &job_len, &side_len) == FAIL) return RET_BUG;
    ret = C2F(dggbak)(job, side, n->I, ilo->I, ihi->I, lscale->R, rscale->R, m->I, v->R, ldv->I, info->I, job_len, side_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dggbal(Stack stack, int rhs, int opt, int lhs) /* dggbal */
{
  int_types T[] = {string, mat_int, realmat, mat_int, realmat, mat_int, mat_int, mat_int, realmat, realmat, realmat, mat_int, s_int,t_end};
  char *job;
  NspMatrix *n, *a, *lda, *b, *ldb, *ilo, *ihi, *lscale, *rscale, *work, *info;
  int job_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &n, &a, &lda, &b, &ldb, &ilo, &ihi, &lscale, &rscale, &work, &info, &job_len) == FAIL) return RET_BUG;
    ret = C2F(dggbal)(job, n->I, a->R, lda->I, b->R, ldb->I, ilo->I, ihi->I, lscale->R, rscale->R, work->R, info->I, job_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}


static int _wrap_dlags2(Stack stack, int rhs, int opt, int lhs) /* dlags2 */
{
  int_types T[] = {mat_int, realmat, realmat, realmat, realmat, realmat, realmat, realmat, realmat, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *upper, *a1, *a2, *a3, *b1, *b2, *b3, *csu, *snu, *csv, *snv, *csq, *snq;

  if ( GetArgs(stack,rhs,opt,T,&upper, &a1, &a2, &a3, &b1, &b2, &b3, &csu, &snu, &csv, &snv, &csq, &snq) == FAIL) return RET_BUG;
    ret = C2F(dlags2)(upper->I, a1->R, a2->R, a3->R, b1->R, b2->R, b3->R, csu->R, snu->R, csv->R, snv->R, csq->R, snq->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlagv2(Stack stack, int rhs, int opt, int lhs) /* dlagv2 */
{
  int_types T[] = {realmat, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *a, *lda, *b, *ldb, *alphar, *alphai, *beta, *csl, *snl, *csr, *snr;

  if ( GetArgs(stack,rhs,opt,T,&a, &lda, &b, &ldb, &alphar, &alphai, &beta, &csl, &snl, &csr, &snr) == FAIL) return RET_BUG;
    ret = C2F(dlagv2)(a->R, lda->I, b->R, ldb->I, alphar->R, alphai->R, beta->R, csl->R, snl->R, csr->R, snr->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlatdf(Stack stack, int rhs, int opt, int lhs) /* dlatdf */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *ijob, *n, *z, *ldz, *rrhs, *rdsum, *rdscal, *ipiv, *jpiv;

  if ( GetArgs(stack,rhs,opt,T,&ijob, &n, &z, &ldz, &rrhs, &rdsum, &rdscal, &ipiv, &jpiv) == FAIL) return RET_BUG;
    ret = C2F(dlatdf)(ijob->I, n->I, z->R, ldz->I, rrhs->R, rdsum->R, rdscal->R, ipiv->I, jpiv->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtgex2(Stack stack, int rhs, int opt, int lhs) /* dtgex2 */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, mat_int, mat_int, mat_int, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *wantq, *wantz, *n, *a, *lda, *b, *ldb, *q, *ldq, *z, *ldz, *j1, *n1, *n2, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&wantq, &wantz, &n, &a, &lda, &b, &ldb, &q, &ldq, &z, &ldz, &j1, &n1, &n2, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dtgex2)(wantq->I, wantz->I, n->I, a->R, lda->I, b->R, ldb->I, q->R, ldq->I, z->R, ldz->I, j1->I, n1->I, n2->I, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtgexc(Stack stack, int rhs, int opt, int lhs) /* dtgexc */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, mat_int, mat_int, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *wantq, *wantz, *n, *a, *lda, *b, *ldb, *q, *ldq, *z, *ldz, *ifst, *ilst, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&wantq, &wantz, &n, &a, &lda, &b, &ldb, &q, &ldq, &z, &ldz, &ifst, &ilst, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dtgexc)(wantq->I, wantz->I, n->I, a->R, lda->I, b->R, ldb->I, q->R, ldq->I, z->R, ldz->I, ifst->I, ilst->I, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtgsy2(Stack stack, int rhs, int opt, int lhs) /* dtgsy2 */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, realmat, realmat, mat_int, mat_int, mat_int, s_int,t_end};
  char *trans;
  NspMatrix *ijob, *m, *n, *a, *lda, *b, *ldb, *c, *ldc, *d, *ldd, *e, *lde, *f, *ldf, *scale, *rdsum, *rdscal, *iwork, *pq, *info;
  int trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&trans, &ijob, &m, &n, &a, &lda, &b, &ldb, &c, &ldc, &d, &ldd, &e, &lde, &f, &ldf, &scale, &rdsum, &rdscal, &iwork, &pq, &info, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dtgsy2)(trans, ijob->I, m->I, n->I, a->R, lda->I, b->R, ldb->I, c->R, ldc->I, d->R, ldd->I, e->R, lde->I, f->R, ldf->I, scale->R, rdsum->R, rdscal->R, iwork->I, pq->I, info->I, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtgsyl(Stack stack, int rhs, int opt, int lhs) /* dtgsyl */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, realmat, realmat, mat_int, mat_int, mat_int, s_int,t_end};
  char *trans;
  NspMatrix *ijob, *m, *n, *a, *lda, *b, *ldb, *c, *ldc, *d, *ldd, *e, *lde, *f, *ldf, *scale, *dif, *work, *lwork, *iwork, *info;
  int trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&trans, &ijob, &m, &n, &a, &lda, &b, &ldb, &c, &ldc, &d, &ldd, &e, &lde, &f, &ldf, &scale, &dif, &work, &lwork, &iwork, &info, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dtgsyl)(trans, ijob->I, m->I, n->I, a->R, lda->I, b->R, ldb->I, c->R, ldc->I, d->R, ldd->I, e->R, lde->I, f->R, ldf->I, scale->R, dif->R, work->R, lwork->I, iwork->I, info->I, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgesc2(Stack stack, int rhs, int opt, int lhs) /* dgesc2 */
{
  int_types T[] = {mat_int, realmat, mat_int, realmat, mat_int, mat_int, realmat,t_end};
  int ret;
  NspMatrix *n, *a, *lda, *rrhs, *ipiv, *jpiv, *scale;

  if ( GetArgs(stack,rhs,opt,T,&n, &a, &lda, &rrhs, &ipiv, &jpiv, &scale) == FAIL) return RET_BUG;
    ret = C2F(dgesc2)(n->I, a->R, lda->I, rrhs->R, ipiv->I, jpiv->I, scale->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgetc2(Stack stack, int rhs, int opt, int lhs) /* dgetc2 */
{
  int_types T[] = {mat_int, realmat, mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *a, *lda, *ipiv, *jpiv, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &a, &lda, &ipiv, &jpiv, &info) == FAIL) return RET_BUG;
    ret = C2F(dgetc2)(n->I, a->R, lda->I, ipiv->I, jpiv->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtgsen(Stack stack, int rhs, int opt, int lhs) /* dtgsen */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, mat_int, realmat, mat_int, mat_int, realmat, realmat, realmat, realmat, mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *ijob, *wantq, *wantz, *select, *n, *a, *lda, *b, *ldb, *alphar, *alphai, *beta, *q, *ldq, *z, *ldz, *m, *pl, *pr, *dif, *work, *lwork, *iwork, *liwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&ijob, &wantq, &wantz, &select, &n, &a, &lda, &b, &ldb, &alphar, &alphai, &beta, &q, &ldq, &z, &ldz, &m, &pl, &pr, &dif, &work, &lwork, &iwork, &liwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dtgsen)(ijob->I, wantq->I, wantz->I, select->I, n->I, a->R, lda->I, b->R, ldb->I, alphar->R, alphai->R, beta->R, q->R, ldq->I, z->R, ldz->I, m->I, pl->R, pr->R, dif->R, work->R, lwork->I, iwork->I, liwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dggev(Stack stack, int rhs, int opt, int lhs) /* dggev */
{
  int_types T[] = {string, string, mat_int, realmat, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, mat_int, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *jobvl, *jobvr;
  NspMatrix *n, *a, *lda, *b, *ldb, *alphar, *alphai, *beta, *vl, *ldvl, *vr, *ldvr, *work, *lwork, *info;
  int jobvl_len, jobvr_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobvl, &jobvr, &n, &a, &lda, &b, &ldb, &alphar, &alphai, &beta, &vl, &ldvl, &vr, &ldvr, &work, &lwork, &info, &jobvl_len, &jobvr_len) == FAIL) return RET_BUG;
    ret = C2F(dggev)(jobvl, jobvr, n->I, a->R, lda->I, b->R, ldb->I, alphar->R, alphai->R, beta->R, vl->R, ldvl->I, vr->R, ldvr->I, work->R, lwork->I, info->I, jobvl_len, jobvr_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgghrd(Stack stack, int rhs, int opt, int lhs) /* dgghrd */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *compq, *compz;
  NspMatrix *n, *ilo, *ihi, *a, *lda, *b, *ldb, *q, *ldq, *z, *ldz, *info;
  int compq_len, compz_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&compq, &compz, &n, &ilo, &ihi, &a, &lda, &b, &ldb, &q, &ldq, &z, &ldz, &info, &compq_len, &compz_len) == FAIL) return RET_BUG;
    ret = C2F(dgghrd)(compq, compz, n->I, ilo->I, ihi->I, a->R, lda->I, b->R, ldb->I, q->R, ldq->I, z->R, ldz->I, info->I, compq_len, compz_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dhgeqz(Stack stack, int rhs, int opt, int lhs) /* dhgeqz */
{
  int_types T[] = {string, string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, mat_int, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int, s_int,t_end};
  char *job, *compq, *compz;
  NspMatrix *n, *ilo, *ihi, *a, *lda, *b, *ldb, *alphar, *alphai, *beta, *q, *ldq, *z, *ldz, *work, *lwork, *info;
  int job_len, compq_len, compz_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &compq, &compz, &n, &ilo, &ihi, &a, &lda, &b, &ldb, &alphar, &alphai, &beta, &q, &ldq, &z, &ldz, &work, &lwork, &info, &job_len, &compq_len, &compz_len) == FAIL) return RET_BUG;
    ret = C2F(dhgeqz)(job, compq, compz, n->I, ilo->I, ihi->I, a->R, lda->I, b->R, ldb->I, alphar->R, alphai->R, beta->R, q->R, ldq->I, z->R, ldz->I, work->R, lwork->I, info->I, job_len, compq_len, compz_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dhseqr(Stack stack, int rhs, int opt, int lhs) /* dhseqr */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *job, *compz;
  NspMatrix *n, *ilo, *ihi, *h, *ldh, *wr, *wi, *z, *ldz, *work, *lwork, *info;
  int job_len, compz_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &compz, &n, &ilo, &ihi, &h, &ldh, &wr, &wi, &z, &ldz, &work, &lwork, &info, &job_len, &compz_len) == FAIL) return RET_BUG;
    ret = C2F(dhseqr)(job, compz, n->I, ilo->I, ihi->I, h->R, ldh->I, wr->R, wi->R, z->R, ldz->I, work->R, lwork->I, info->I, job_len, compz_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlabad(Stack stack, int rhs, int opt, int lhs) /* dlabad */
{
  int_types T[] = {realmat, realmat,t_end};
  int ret;
  NspMatrix *dsmall, *large;

  if ( GetArgs(stack,rhs,opt,T,&dsmall, &large) == FAIL) return RET_BUG;
    ret = C2F(dlabad)(dsmall->R, large->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlabrd(Stack stack, int rhs, int opt, int lhs) /* dlabrd */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, realmat, mat_int, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *nb, *a, *lda, *d, *e, *tauq, *taup, *x, *ldx, *y, *ldy;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &nb, &a, &lda, &d, &e, &tauq, &taup, &x, &ldx, &y, &ldy) == FAIL) return RET_BUG;
    ret = C2F(dlabrd)(m->I, n->I, nb->I, a->R, lda->I, d->R, e->R, tauq->R, taup->R, x->R, ldx->I, y->R, ldy->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlacon(Stack stack, int rhs, int opt, int lhs) /* dlacon */
{
  int_types T[] = {mat_int, realmat, realmat, mat_int, realmat, mat_int,t_end};
  int ret;
  NspMatrix *n, *v, *x, *isgn, *est, *kase;

  if ( GetArgs(stack,rhs,opt,T,&n, &v, &x, &isgn, &est, &kase) == FAIL) return RET_BUG;
    ret = C2F(dlacon)(n->I, v->R, x->R, isgn->I, est->R, kase->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlacpy(Stack stack, int rhs, int opt, int lhs) /* dlacpy */
{
  int_types T[] = {string, mat_int, mat_int, realmat, mat_int, realmat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *m, *n, *a, *lda, *b, *ldb;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &m, &n, &a, &lda, &b, &ldb, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dlacpy)(uplo, m->I, n->I, a->R, lda->I, b->R, ldb->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dladiv(Stack stack, int rhs, int opt, int lhs) /* dladiv */
{
  int_types T[] = {realmat, realmat, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *a, *b, *c, *d, *p, *q;

  if ( GetArgs(stack,rhs,opt,T,&a, &b, &c, &d, &p, &q) == FAIL) return RET_BUG;
    ret = C2F(dladiv)(a->R, b->R, c->R, d->R, p->R, q->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlae2(Stack stack, int rhs, int opt, int lhs) /* dlae2 */
{
  int_types T[] = {realmat, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *a, *b, *c, *rt1, *rt2;

  if ( GetArgs(stack,rhs,opt,T,&a, &b, &c, &rt1, &rt2) == FAIL) return RET_BUG;
    ret = C2F(dlae2)(a->R, b->R, c->R, rt1->R, rt2->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlaev2(Stack stack, int rhs, int opt, int lhs) /* dlaev2 */
{
  int_types T[] = {realmat, realmat, realmat, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *a, *b, *c, *rt1, *rt2, *cs1, *sn1;

  if ( GetArgs(stack,rhs,opt,T,&a, &b, &c, &rt1, &rt2, &cs1, &sn1) == FAIL) return RET_BUG;
    ret = C2F(dlaev2)(a->R, b->R, c->R, rt1->R, rt2->R, cs1->R, sn1->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlaexc(Stack stack, int rhs, int opt, int lhs) /* dlaexc */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, mat_int, mat_int, mat_int, mat_int, realmat, mat_int,t_end};
  int ret;
  NspMatrix *wantq, *n, *t, *ldt, *q, *ldq, *j1, *n1, *n2, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&wantq, &n, &t, &ldt, &q, &ldq, &j1, &n1, &n2, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dlaexc)(wantq->I, n->I, t->R, ldt->I, q->R, ldq->I, j1->I, n1->I, n2->I, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlag2(Stack stack, int rhs, int opt, int lhs) /* dlag2 */
{
  int_types T[] = {realmat, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *a, *lda, *b, *ldb, *safmin, *scale1, *scale2, *wr1, *wr2, *wi;

  if ( GetArgs(stack,rhs,opt,T,&a, &lda, &b, &ldb, &safmin, &scale1, &scale2, &wr1, &wr2, &wi) == FAIL) return RET_BUG;
    ret = C2F(dlag2)(a->R, lda->I, b->R, ldb->I, safmin->R, scale1->R, scale2->R, wr1->R, wr2->R, wi->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlahqr(Stack stack, int rhs, int opt, int lhs) /* dlahqr */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *wantt, *wantz, *n, *ilo, *ihi, *h, *ldh, *wr, *wi, *iloz, *ihiz, *z, *ldz, *info;

  if ( GetArgs(stack,rhs,opt,T,&wantt, &wantz, &n, &ilo, &ihi, &h, &ldh, &wr, &wi, &iloz, &ihiz, &z, &ldz, &info) == FAIL) return RET_BUG;
    ret = C2F(dlahqr)(wantt->I, wantz->I, n->I, ilo->I, ihi->I, h->R, ldh->I, wr->R, wi->R, iloz->I, ihiz->I, z->R, ldz->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlahrd(Stack stack, int rhs, int opt, int lhs) /* dlahrd */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int,t_end};
  int ret;
  NspMatrix *n, *k, *nb, *a, *lda, *tau, *t, *ldt, *y, *ldy;

  if ( GetArgs(stack,rhs,opt,T,&n, &k, &nb, &a, &lda, &tau, &t, &ldt, &y, &ldy) == FAIL) return RET_BUG;
    ret = C2F(dlahrd)(n->I, k->I, nb->I, a->R, lda->I, tau->R, t->R, ldt->I, y->R, ldy->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlaic1(Stack stack, int rhs, int opt, int lhs) /* dlaic1 */
{
  int_types T[] = {mat_int, mat_int, realmat, realmat, realmat, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *job, *j, *x, *sest, *w, *gamma, *sestpr, *s, *c;

  if ( GetArgs(stack,rhs,opt,T,&job, &j, &x, &sest, &w, &gamma, &sestpr, &s, &c) == FAIL) return RET_BUG;
    ret = C2F(dlaic1)(job->I, j->I, x->R, sest->R, w->R, gamma->R, sestpr->R, s->R, c->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlaln2(Stack stack, int rhs, int opt, int lhs) /* dlaln2 */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, realmat, realmat, mat_int, realmat, realmat, realmat, mat_int, realmat, realmat, realmat, mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *ltrans, *na, *nw, *smin, *ca, *a, *lda, *d1, *d2, *b, *ldb, *wr, *wi, *x, *ldx, *scale, *xnorm, *info;

  if ( GetArgs(stack,rhs,opt,T,&ltrans, &na, &nw, &smin, &ca, &a, &lda, &d1, &d2, &b, &ldb, &wr, &wi, &x, &ldx, &scale, &xnorm, &info) == FAIL) return RET_BUG;
    ret = C2F(dlaln2)(ltrans->I, na->I, nw->I, smin->R, ca->R, a->R, lda->I, d1->R, d2->R, b->R, ldb->I, wr->R, wi->R, x->R, ldx->I, scale->R, xnorm->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlamch(Stack stack, int rhs, int opt, int lhs) /* dlamch */
{
  int_types T[] = {string, s_int,t_end};
  char *cmach;
  int cmach_len;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&cmach, &cmach_len) == FAIL) return RET_BUG;
    ret = C2F(dlamch)(cmach, cmach_len);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlamc1(Stack stack, int rhs, int opt, int lhs) /* dlamc1 */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *beta, *t, *rnd, *ieee1;

  if ( GetArgs(stack,rhs,opt,T,&beta, &t, &rnd, &ieee1) == FAIL) return RET_BUG;
    ret = C2F(dlamc1)(beta->I, t->I, rnd->I, ieee1->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlamc2(Stack stack, int rhs, int opt, int lhs) /* dlamc2 */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat,t_end};
  int ret;
  NspMatrix *beta, *t, *rnd, *eps, *emin, *rmin, *emax, *rmax;

  if ( GetArgs(stack,rhs,opt,T,&beta, &t, &rnd, &eps, &emin, &rmin, &emax, &rmax) == FAIL) return RET_BUG;
    ret = C2F(dlamc2)(beta->I, t->I, rnd->I, eps->R, emin->I, rmin->R, emax->I, rmax->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlamc3(Stack stack, int rhs, int opt, int lhs) /* dlamc3 */
{
  int_types T[] = {realmat, realmat,t_end};
  NspMatrix *a, *b;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&a, &b) == FAIL) return RET_BUG;
    ret = C2F(dlamc3)(a->R, b->R);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlamc4(Stack stack, int rhs, int opt, int lhs) /* dlamc4 */
{
  int_types T[] = {mat_int, realmat, mat_int,t_end};
  int ret;
  NspMatrix *emin, *start, *base;

  if ( GetArgs(stack,rhs,opt,T,&emin, &start, &base) == FAIL) return RET_BUG;
    ret = C2F(dlamc4)(emin->I, start->R, base->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlamc5(Stack stack, int rhs, int opt, int lhs) /* dlamc5 */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int, mat_int, realmat,t_end};
  int ret;
  NspMatrix *beta, *p, *emin, *ieee, *emax, *rmax;

  if ( GetArgs(stack,rhs,opt,T,&beta, &p, &emin, &ieee, &emax, &rmax) == FAIL) return RET_BUG;
    ret = C2F(dlamc5)(beta->I, p->I, emin->I, ieee->I, emax->I, rmax->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlange(Stack stack, int rhs, int opt, int lhs) /* dlange */
{
  int_types T[] = {string, mat_int, mat_int, realmat, mat_int, realmat, s_int,t_end};
  char *norm;
  NspMatrix *m, *n, *a, *lda, *work;
  int norm_len;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &m, &n, &a, &lda, &work, &norm_len) == FAIL) return RET_BUG;
    ret = C2F(dlange)(norm, m->I, n->I, a->R, lda->I, work->R, norm_len);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlangb(Stack stack, int rhs, int opt, int lhs) /* dlangb */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, s_int,t_end};
  char *norm;
  NspMatrix *m, *kl, *ku, *a, *lda, *work;
  int norm_len;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &m, &kl, &ku, &a, &lda, &work, &norm_len) == FAIL) return RET_BUG;
    ret = C2F(dlangb)(norm, m->I, kl->I, ku->I, a->R, lda->I, work->R, norm_len);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlangb(Stack stack, int rhs, int opt, int lhs) /* zlangb */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, mat, mat_int, realmat, s_int,t_end};
  char *norm;
  NspMatrix *m, *kl, *ku, *a, *lda, *work;
  int norm_len;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &m, &kl, &ku, &a, &lda, &work, &norm_len) == FAIL) return RET_BUG;
    ret = C2F(zlangb)(norm, m->I, kl->I, ku->I, a->C, lda->I, work->R, norm_len);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlanhs(Stack stack, int rhs, int opt, int lhs) /* dlanhs */
{
  int_types T[] = {string, mat_int, realmat, mat_int, realmat, s_int,t_end};
  char *norm;
  NspMatrix *n, *a, *lda, *work;
  int norm_len;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &n, &a, &lda, &work, &norm_len) == FAIL) return RET_BUG;
    ret = C2F(dlanhs)(norm, n->I, a->R, lda->I, work->R, norm_len);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlansp(Stack stack, int rhs, int opt, int lhs) /* dlansp */
{
  int_types T[] = {string, string, mat_int, realmat, realmat, s_int, s_int,t_end};
  char *norm, *uplo;
  NspMatrix *n, *ap, *work;
  int norm_len, uplo_len;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &uplo, &n, &ap, &work, &norm_len, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dlansp)(norm, uplo, n->I, ap->R, work->R, norm_len, uplo_len);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlanst(Stack stack, int rhs, int opt, int lhs) /* dlanst */
{
  int_types T[] = {string, mat_int, realmat, realmat, s_int,t_end};
  char *norm;
  NspMatrix *n, *d, *e;
  int norm_len;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &n, &d, &e, &norm_len) == FAIL) return RET_BUG;
    ret = C2F(dlanst)(norm, n->I, d->R, e->R, norm_len);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlansy(Stack stack, int rhs, int opt, int lhs) /* dlansy */
{
  int_types T[] = {string, string, mat_int, realmat, mat_int, realmat, s_int, s_int,t_end};
  char *norm, *uplo;
  NspMatrix *n, *a, *lda, *work;
  int norm_len, uplo_len;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &uplo, &n, &a, &lda, &work, &norm_len, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dlansy)(norm, uplo, n->I, a->R, lda->I, work->R, norm_len, uplo_len);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlantr(Stack stack, int rhs, int opt, int lhs) /* dlantr */
{
  int_types T[] = {string, string, string, mat_int, mat_int, realmat, mat_int, realmat, s_int, s_int, s_int,t_end};
  char *norm, *uplo, *diag;
  NspMatrix *m, *n, *a, *lda, *work;
  int norm_len, uplo_len, diag_len;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &uplo, &diag, &m, &n, &a, &lda, &work, &norm_len, &uplo_len, &diag_len) == FAIL) return RET_BUG;
    ret = C2F(dlantr)(norm, uplo, diag, m->I, n->I, a->R, lda->I, work->R, norm_len, uplo_len, diag_len);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlanv2(Stack stack, int rhs, int opt, int lhs) /* dlanv2 */
{
  int_types T[] = {realmat, realmat, realmat, realmat, realmat, realmat, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *a, *b, *c, *d, *rt1r, *rt1i, *rt2r, *rt2i, *cs, *sn;

  if ( GetArgs(stack,rhs,opt,T,&a, &b, &c, &d, &rt1r, &rt1i, &rt2r, &rt2i, &cs, &sn) == FAIL) return RET_BUG;
    ret = C2F(dlanv2)(a->R, b->R, c->R, d->R, rt1r->R, rt1i->R, rt2r->R, rt2i->R, cs->R, sn->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlapmt(Stack stack, int rhs, int opt, int lhs) /* dlapmt */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *forwrd, *m, *n, *x, *ldx, *k;

  if ( GetArgs(stack,rhs,opt,T,&forwrd, &m, &n, &x, &ldx, &k) == FAIL) return RET_BUG;
    ret = C2F(dlapmt)(forwrd->I, m->I, n->I, x->R, ldx->I, k->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlapy2(Stack stack, int rhs, int opt, int lhs) /* dlapy2 */
{
  int_types T[] = {realmat, realmat,t_end};
  NspMatrix *x, *y;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&x, &y) == FAIL) return RET_BUG;
    ret = C2F(dlapy2)(x->R, y->R);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlapy3(Stack stack, int rhs, int opt, int lhs) /* dlapy3 */
{
  int_types T[] = {realmat, realmat, realmat,t_end};
  NspMatrix *x, *y, *z;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&x, &y, &z) == FAIL) return RET_BUG;
    ret = C2F(dlapy3)(x->R, y->R, z->R);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlaqge(Stack stack, int rhs, int opt, int lhs) /* dlaqge */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, realmat, string, s_int,t_end};
  char *equed;
  NspMatrix *m, *n, *a, *lda, *r, *c, *rowcnd, *colcnd, *amax;
  int equed_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &r, &c, &rowcnd, &colcnd, &amax, &equed, &equed_len) == FAIL) return RET_BUG;
    ret = C2F(dlaqge)(m->I, n->I, a->R, lda->I, r->R, c->R, rowcnd->R, colcnd->R, amax->R, equed, equed_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlaqp2(Stack stack, int rhs, int opt, int lhs) /* dlaqp2 */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, mat_int, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *m, *n, *offset, *a, *lda, *jpvt, *tau, *vn1, *vn2, *work;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &offset, &a, &lda, &jpvt, &tau, &vn1, &vn2, &work) == FAIL) return RET_BUG;
    ret = C2F(dlaqp2)(m->I, n->I, offset->I, a->R, lda->I, jpvt->I, tau->R, vn1->R, vn2->R, work->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlaqps(Stack stack, int rhs, int opt, int lhs) /* dlaqps */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int, mat_int, realmat, mat_int, mat_int, realmat, realmat, realmat, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *offset, *nb, *kb, *a, *lda, *jpvt, *tau, *vn1, *vn2, *auxv, *f, *ldf;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &offset, &nb, &kb, &a, &lda, &jpvt, &tau, &vn1, &vn2, &auxv, &f, &ldf) == FAIL) return RET_BUG;
    ret = C2F(dlaqps)(m->I, n->I, offset->I, nb->I, kb->I, a->R, lda->I, jpvt->I, tau->R, vn1->R, vn2->R, auxv->R, f->R, ldf->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlarfb(Stack stack, int rhs, int opt, int lhs) /* dlarfb */
{
  int_types T[] = {string, string, string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, s_int, s_int, s_int, s_int,t_end};
  char *side, *trans, *direct, *storev;
  NspMatrix *m, *n, *k, *v, *ldv, *t, *ldt, *c, *ldc, *work, *ldwork;
  int side_len, trans_len, direct_len, storev_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &direct, &storev, &m, &n, &k, &v, &ldv, &t, &ldt, &c, &ldc, &work, &ldwork, &side_len, &trans_len, &direct_len, &storev_len) == FAIL) return RET_BUG;
    ret = C2F(dlarfb)(side, trans, direct, storev, m->I, n->I, k->I, v->R, ldv->I, t->R, ldt->I, c->R, ldc->I, work->R, ldwork->I, side_len, trans_len, direct_len, storev_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlarf(Stack stack, int rhs, int opt, int lhs) /* dlarf */
{
  int_types T[] = {string, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, s_int,t_end};
  char *side;
  NspMatrix *m, *n, *v, *incv, *tau, *c, *ldc, *work;
  int side_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &m, &n, &v, &incv, &tau, &c, &ldc, &work, &side_len) == FAIL) return RET_BUG;
    ret = C2F(dlarf)(side, m->I, n->I, v->R, incv->I, tau->R, c->R, ldc->I, work->R, side_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlarfg(Stack stack, int rhs, int opt, int lhs) /* dlarfg */
{
  int_types T[] = {mat_int, realmat, realmat, mat_int, realmat,t_end};
  int ret;
  NspMatrix *n, *alpha, *x, *incx, *tau;

  if ( GetArgs(stack,rhs,opt,T,&n, &alpha, &x, &incx, &tau) == FAIL) return RET_BUG;
    ret = C2F(dlarfg)(n->I, alpha->R, x->R, incx->I, tau->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlarft(Stack stack, int rhs, int opt, int lhs) /* dlarft */
{
  int_types T[] = {string, string, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, s_int, s_int,t_end};
  char *direct, *storev;
  NspMatrix *n, *k, *v, *ldv, *tau, *t, *ldt;
  int direct_len, storev_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&direct, &storev, &n, &k, &v, &ldv, &tau, &t, &ldt, &direct_len, &storev_len) == FAIL) return RET_BUG;
    ret = C2F(dlarft)(direct, storev, n->I, k->I, v->R, ldv->I, tau->R, t->R, ldt->I, direct_len, storev_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlarfx(Stack stack, int rhs, int opt, int lhs) /* dlarfx */
{
  int_types T[] = {string, mat_int, mat_int, realmat, realmat, realmat, mat_int, realmat, s_int,t_end};
  char *side;
  NspMatrix *m, *n, *v, *tau, *c, *ldc, *work;
  int side_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &m, &n, &v, &tau, &c, &ldc, &work, &side_len) == FAIL) return RET_BUG;
    ret = C2F(dlarfx)(side, m->I, n->I, v->R, tau->R, c->R, ldc->I, work->R, side_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlartg(Stack stack, int rhs, int opt, int lhs) /* dlartg */
{
  int_types T[] = {realmat, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *f, *g, *cs, *sn, *r;

  if ( GetArgs(stack,rhs,opt,T,&f, &g, &cs, &sn, &r) == FAIL) return RET_BUG;
    ret = C2F(dlartg)(f->R, g->R, cs->R, sn->R, r->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlarzb(Stack stack, int rhs, int opt, int lhs) /* dlarzb */
{
  int_types T[] = {string, string, string, string, mat_int, mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, s_int, s_int, s_int, s_int,t_end};
  char *side, *trans, *direct, *storev;
  NspMatrix *m, *n, *k, *l, *v, *ldv, *t, *ldt, *c, *ldc, *work, *ldwork;
  int side_len, trans_len, direct_len, storev_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &direct, &storev, &m, &n, &k, &l, &v, &ldv, &t, &ldt, &c, &ldc, &work, &ldwork, &side_len, &trans_len, &direct_len, &storev_len) == FAIL) return RET_BUG;
    ret = C2F(dlarzb)(side, trans, direct, storev, m->I, n->I, k->I, l->I, v->R, ldv->I, t->R, ldt->I, c->R, ldc->I, work->R, ldwork->I, side_len, trans_len, direct_len, storev_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlarz(Stack stack, int rhs, int opt, int lhs) /* dlarz */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, s_int,t_end};
  char *side;
  NspMatrix *m, *n, *l, *v, *incv, *tau, *c, *ldc, *work;
  int side_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &m, &n, &l, &v, &incv, &tau, &c, &ldc, &work, &side_len) == FAIL) return RET_BUG;
    ret = C2F(dlarz)(side, m->I, n->I, l->I, v->R, incv->I, tau->R, c->R, ldc->I, work->R, side_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlarzt(Stack stack, int rhs, int opt, int lhs) /* dlarzt */
{
  int_types T[] = {string, string, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, s_int, s_int,t_end};
  char *direct, *storev;
  NspMatrix *n, *k, *v, *ldv, *tau, *t, *ldt;
  int direct_len, storev_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&direct, &storev, &n, &k, &v, &ldv, &tau, &t, &ldt, &direct_len, &storev_len) == FAIL) return RET_BUG;
    ret = C2F(dlarzt)(direct, storev, n->I, k->I, v->R, ldv->I, tau->R, t->R, ldt->I, direct_len, storev_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlas2(Stack stack, int rhs, int opt, int lhs) /* dlas2 */
{
  int_types T[] = {realmat, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *f, *g, *h, *ssmin, *ssmax;

  if ( GetArgs(stack,rhs,opt,T,&f, &g, &h, &ssmin, &ssmax) == FAIL) return RET_BUG;
    ret = C2F(dlas2)(f->R, g->R, h->R, ssmin->R, ssmax->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlascl(Stack stack, int rhs, int opt, int lhs) /* dlascl */
{
  int_types T[] = {string, mat_int, mat_int, realmat, realmat, mat_int, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *type;
  NspMatrix *kl, *ku, *cfrom, *cto, *m, *n, *a, *lda, *info;
  int type_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&type, &kl, &ku, &cfrom, &cto, &m, &n, &a, &lda, &info, &type_len) == FAIL) return RET_BUG;
    ret = C2F(dlascl)(type, kl->I, ku->I, cfrom->R, cto->R, m->I, n->I, a->R, lda->I, info->I, type_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlaset(Stack stack, int rhs, int opt, int lhs) /* dlaset */
{
  int_types T[] = {string, mat_int, mat_int, realmat, realmat, realmat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *m, *n, *alpha, *beta, *a, *lda;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &m, &n, &alpha, &beta, &a, &lda, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dlaset)(uplo, m->I, n->I, alpha->R, beta->R, a->R, lda->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlasq1(Stack stack, int rhs, int opt, int lhs) /* dlasq1 */
{
  int_types T[] = {mat_int, realmat, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *n, *d, *e, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &d, &e, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dlasq1)(n->I, d->R, e->R, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlasq2(Stack stack, int rhs, int opt, int lhs) /* dlasq2 */
{
  int_types T[] = {mat_int, realmat, realmat, realmat, realmat, realmat, realmat, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *q, *e, *qq, *ee, *eps, *tol2, *small2, *sup, *kend, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &q, &e, &qq, &ee, &eps, &tol2, &small2, &sup, &kend, &info) == FAIL) return RET_BUG;
    ret = C2F(dlasq2)(m->I, q->R, e->R, qq->R, ee->R, eps->R, tol2->R, small2->R, sup->R, kend->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlasq3(Stack stack, int rhs, int opt, int lhs) /* dlasq3 */
{
  int_types T[] = {mat_int, realmat, realmat, realmat, realmat, realmat, realmat, mat_int, mat_int, mat_int, mat_int, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *n, *q, *e, *qq, *ee, *sup, *sigma, *kend, *off, *iphase, *iconv, *eps, *tol2, *small2;

  if ( GetArgs(stack,rhs,opt,T,&n, &q, &e, &qq, &ee, &sup, &sigma, &kend, &off, &iphase, &iconv, &eps, &tol2, &small2) == FAIL) return RET_BUG;
    ret = C2F(dlasq3)(n->I, q->R, e->R, qq->R, ee->R, sup->R, sigma->R, kend->I, off->I, iphase->I, iconv->I, eps->R, tol2->R, small2->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlasq4(Stack stack, int rhs, int opt, int lhs) /* dlasq4 */
{
  int_types T[] = {mat_int, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *n, *q, *e, *tau, *sup;

  if ( GetArgs(stack,rhs,opt,T,&n, &q, &e, &tau, &sup) == FAIL) return RET_BUG;
    ret = C2F(dlasq4)(n->I, q->R, e->R, tau->R, sup->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlasr(Stack stack, int rhs, int opt, int lhs) /* dlasr */
{
  int_types T[] = {string, string, string, mat_int, mat_int, realmat, realmat, realmat, mat_int, s_int, s_int, s_int,t_end};
  char *side, *pivot, *direct;
  NspMatrix *m, *n, *c, *s, *a, *lda;
  int side_len, pivot_len, direct_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &pivot, &direct, &m, &n, &c, &s, &a, &lda, &side_len, &pivot_len, &direct_len) == FAIL) return RET_BUG;
    ret = C2F(dlasr)(side, pivot, direct, m->I, n->I, c->R, s->R, a->R, lda->I, side_len, pivot_len, direct_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlasrt(Stack stack, int rhs, int opt, int lhs) /* dlasrt */
{
  int_types T[] = {string, mat_int, realmat, mat_int, s_int,t_end};
  char *id;
  NspMatrix *n, *d, *info;
  int id_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&id, &n, &d, &info, &id_len) == FAIL) return RET_BUG;
    ret = C2F(dlasrt)(id, n->I, d->R, info->I, id_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlassq(Stack stack, int rhs, int opt, int lhs) /* dlassq */
{
  int_types T[] = {mat_int, realmat, mat_int, realmat, realmat,t_end};
  int ret;
  NspMatrix *n, *x, *incx, *scale, *sumsq;

  if ( GetArgs(stack,rhs,opt,T,&n, &x, &incx, &scale, &sumsq) == FAIL) return RET_BUG;
    ret = C2F(dlassq)(n->I, x->R, incx->I, scale->R, sumsq->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlasv2(Stack stack, int rhs, int opt, int lhs) /* dlasv2 */
{
  int_types T[] = {realmat, realmat, realmat, realmat, realmat, realmat, realmat, realmat, realmat,t_end};
  int ret;
  NspMatrix *f, *g, *h, *ssmin, *ssmax, *snr, *csr, *snl, *csl;

  if ( GetArgs(stack,rhs,opt,T,&f, &g, &h, &ssmin, &ssmax, &snr, &csr, &snl, &csl) == FAIL) return RET_BUG;
    ret = C2F(dlasv2)(f->R, g->R, h->R, ssmin->R, ssmax->R, snr->R, csr->R, snl->R, csl->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlaswp(Stack stack, int rhs, int opt, int lhs) /* dlaswp */
{
  int_types T[] = {mat_int, realmat, mat_int, mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *a, *lda, *k1, *k2, *ipiv, *incx;

  if ( GetArgs(stack,rhs,opt,T,&n, &a, &lda, &k1, &k2, &ipiv, &incx) == FAIL) return RET_BUG;
    ret = C2F(dlaswp)(n->I, a->R, lda->I, k1->I, k2->I, ipiv->I, incx->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlasy2(Stack stack, int rhs, int opt, int lhs) /* dlasy2 */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int,t_end};
  int ret;
  NspMatrix *ltranl, *ltranr, *isgn, *n1, *n2, *tl, *ldtl, *tr, *ldtr, *b, *ldb, *scale, *x, *ldx, *xnorm, *info;

  if ( GetArgs(stack,rhs,opt,T,&ltranl, &ltranr, &isgn, &n1, &n2, &tl, &ldtl, &tr, &ldtr, &b, &ldb, &scale, &x, &ldx, &xnorm, &info) == FAIL) return RET_BUG;
    ret = C2F(dlasy2)(ltranl->I, ltranr->I, isgn->I, n1->I, n2->I, tl->R, ldtl->I, tr->R, ldtr->I, b->R, ldb->I, scale->R, x->R, ldx->I, xnorm->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlasyf(Stack stack, int rhs, int opt, int lhs) /* dlasyf */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, realmat, mat_int, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *nb, *kb, *a, *lda, *ipiv, *w, *ldw, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &nb, &kb, &a, &lda, &ipiv, &w, &ldw, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dlasyf)(uplo, n->I, nb->I, kb->I, a->R, lda->I, ipiv->I, w->R, ldw->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlatrs(Stack stack, int rhs, int opt, int lhs) /* dlatrs */
{
  int_types T[] = {string, string, string, string, mat_int, realmat, mat_int, realmat, realmat, realmat, mat_int, s_int, s_int, s_int, s_int,t_end};
  char *uplo, *trans, *diag, *normin;
  NspMatrix *n, *a, *lda, *x, *scale, *cnorm, *info;
  int uplo_len, trans_len, diag_len, normin_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &trans, &diag, &normin, &n, &a, &lda, &x, &scale, &cnorm, &info, &uplo_len, &trans_len, &diag_len, &normin_len) == FAIL) return RET_BUG;
    ret = C2F(dlatrs)(uplo, trans, diag, normin, n->I, a->R, lda->I, x->R, scale->R, cnorm->R, info->I, uplo_len, trans_len, diag_len, normin_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlatrz(Stack stack, int rhs, int opt, int lhs) /* dlatrz */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat,t_end};
  int ret;
  NspMatrix *m, *n, *l, *a, *lda, *tau, *work;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &l, &a, &lda, &tau, &work) == FAIL) return RET_BUG;
    ret = C2F(dlatrz)(m->I, n->I, l->I, a->R, lda->I, tau->R, work->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlatzm(Stack stack, int rhs, int opt, int lhs) /* dlatzm */
{
  int_types T[] = {string, mat_int, mat_int, realmat, mat_int, realmat, realmat, realmat, mat_int, realmat, s_int,t_end};
  char *side;
  NspMatrix *m, *n, *v, *incv, *tau, *c1, *c2, *ldc, *work;
  int side_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &m, &n, &v, &incv, &tau, &c1, &c2, &ldc, &work, &side_len) == FAIL) return RET_BUG;
    ret = C2F(dlatzm)(side, m->I, n->I, v->R, incv->I, tau->R, c1->R, c2->R, ldc->I, work->R, side_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dopgtr(Stack stack, int rhs, int opt, int lhs) /* dopgtr */
{
  int_types T[] = {string, mat_int, realmat, realmat, realmat, mat_int, realmat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *ap, *tau, *q, *ldq, *work, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &ap, &tau, &q, &ldq, &work, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dopgtr)(uplo, n->I, ap->R, tau->R, q->R, ldq->I, work->R, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorg2l(Stack stack, int rhs, int opt, int lhs) /* dorg2l */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dorg2l)(m->I, n->I, k->I, a->R, lda->I, tau->R, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorg2r(Stack stack, int rhs, int opt, int lhs) /* dorg2r */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dorg2r)(m->I, n->I, k->I, a->R, lda->I, tau->R, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorgbr(Stack stack, int rhs, int opt, int lhs) /* dorgbr */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int, s_int,t_end};
  char *vect;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *lwork, *info;
  int vect_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&vect, &m, &n, &k, &a, &lda, &tau, &work, &lwork, &info, &vect_len) == FAIL) return RET_BUG;
    ret = C2F(dorgbr)(vect, m->I, n->I, k->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I, vect_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorghr(Stack stack, int rhs, int opt, int lhs) /* dorghr */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *ilo, *ihi, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &ilo, &ihi, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dorghr)(n->I, ilo->I, ihi->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorgl2(Stack stack, int rhs, int opt, int lhs) /* dorgl2 */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dorgl2)(m->I, n->I, k->I, a->R, lda->I, tau->R, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorglq(Stack stack, int rhs, int opt, int lhs) /* dorglq */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dorglq)(m->I, n->I, k->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorgql(Stack stack, int rhs, int opt, int lhs) /* dorgql */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dorgql)(m->I, n->I, k->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorgqr(Stack stack, int rhs, int opt, int lhs) /* dorgqr */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dorgqr)(m->I, n->I, k->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorgr2(Stack stack, int rhs, int opt, int lhs) /* dorgr2 */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(dorgr2)(m->I, n->I, k->I, a->R, lda->I, tau->R, work->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorgrq(Stack stack, int rhs, int opt, int lhs) /* dorgrq */
{
  int_types T[] = {mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dorgrq)(m->I, n->I, k->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorgtr(Stack stack, int rhs, int opt, int lhs) /* dorgtr */
{
  int_types T[] = {string, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *tau, *work, *lwork, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &tau, &work, &lwork, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dorgtr)(uplo, n->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorm2r(Stack stack, int rhs, int opt, int lhs) /* dorm2r */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dorm2r)(side, trans, m->I, n->I, k->I, a->R, lda->I, tau->R, c->R, ldc->I, work->R, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dormbr(Stack stack, int rhs, int opt, int lhs) /* dormbr */
{
  int_types T[] = {string, string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int, s_int,t_end};
  char *vect, *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *lwork, *info;
  int vect_len, side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&vect, &side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &lwork, &info, &vect_len, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dormbr)(vect, side, trans, m->I, n->I, k->I, a->R, lda->I, tau->R, c->R, ldc->I, work->R, lwork->I, info->I, vect_len, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dormhr(Stack stack, int rhs, int opt, int lhs) /* dormhr */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *ilo, *ihi, *a, *lda, *tau, *c, *ldc, *work, *lwork, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &ilo, &ihi, &a, &lda, &tau, &c, &ldc, &work, &lwork, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dormhr)(side, trans, m->I, n->I, ilo->I, ihi->I, a->R, lda->I, tau->R, c->R, ldc->I, work->R, lwork->I, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorml2(Stack stack, int rhs, int opt, int lhs) /* dorml2 */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dorml2)(side, trans, m->I, n->I, k->I, a->R, lda->I, tau->R, c->R, ldc->I, work->R, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dormlq(Stack stack, int rhs, int opt, int lhs) /* dormlq */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *lwork, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &lwork, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dormlq)(side, trans, m->I, n->I, k->I, a->R, lda->I, tau->R, c->R, ldc->I, work->R, lwork->I, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dormql(Stack stack, int rhs, int opt, int lhs) /* dormql */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *lwork, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &lwork, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dormql)(side, trans, m->I, n->I, k->I, a->R, lda->I, tau->R, c->R, ldc->I, work->R, lwork->I, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dorm2l(Stack stack, int rhs, int opt, int lhs) /* dorm2l */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dorm2l)(side, trans, m->I, n->I, k->I, a->R, lda->I, tau->R, c->R, ldc->I, work->R, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dormqr(Stack stack, int rhs, int opt, int lhs) /* dormqr */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *lwork, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &lwork, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dormqr)(side, trans, m->I, n->I, k->I, a->R, lda->I, tau->R, c->R, ldc->I, work->R, lwork->I, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dormr2(Stack stack, int rhs, int opt, int lhs) /* dormr2 */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dormr2)(side, trans, m->I, n->I, k->I, a->R, lda->I, tau->R, c->R, ldc->I, work->R, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dormr3(Stack stack, int rhs, int opt, int lhs) /* dormr3 */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *l, *a, *lda, *tau, *c, *ldc, *work, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &l, &a, &lda, &tau, &c, &ldc, &work, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dormr3)(side, trans, m->I, n->I, k->I, l->I, a->R, lda->I, tau->R, c->R, ldc->I, work->R, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dormrq(Stack stack, int rhs, int opt, int lhs) /* dormrq */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *lwork, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &lwork, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dormrq)(side, trans, m->I, n->I, k->I, a->R, lda->I, tau->R, c->R, ldc->I, work->R, lwork->I, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dormrz(Stack stack, int rhs, int opt, int lhs) /* dormrz */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *l, *a, *lda, *tau, *c, *ldc, *work, *lwork, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &l, &a, &lda, &tau, &c, &ldc, &work, &lwork, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(dormrz)(side, trans, m->I, n->I, k->I, l->I, a->R, lda->I, tau->R, c->R, ldc->I, work->R, lwork->I, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dpocon(Stack stack, int rhs, int opt, int lhs) /* dpocon */
{
  int_types T[] = {string, mat_int, realmat, mat_int, realmat, realmat, realmat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *anorm, *rcond, *work, *iwork, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &anorm, &rcond, &work, &iwork, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dpocon)(uplo, n->I, a->R, lda->I, anorm->R, rcond->R, work->R, iwork->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dpotrf(Stack stack, int rhs, int opt, int lhs) /* dpotrf */
{
  int_types T[] = {string, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dpotrf)(uplo, n->I, a->R, lda->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dpotf2(Stack stack, int rhs, int opt, int lhs) /* dpotf2 */
{
  int_types T[] = {string, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dpotf2)(uplo, n->I, a->R, lda->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dpotrs(Stack stack, int rhs, int opt, int lhs) /* dpotrs */
{
  int_types T[] = {string, mat_int, mat_int, realmat, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *nrhs, *a, *lda, *b, *ldb, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &nrhs, &a, &lda, &b, &ldb, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dpotrs)(uplo, n->I, nrhs->I, a->R, lda->I, b->R, ldb->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dpptrf(Stack stack, int rhs, int opt, int lhs) /* dpptrf */
{
  int_types T[] = {string, mat_int, realmat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *ap, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &ap, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dpptrf)(uplo, n->I, ap->R, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_drscl(Stack stack, int rhs, int opt, int lhs) /* drscl */
{
  int_types T[] = {mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *n, *sa, *sx, *incx;

  if ( GetArgs(stack,rhs,opt,T,&n, &sa, &sx, &incx) == FAIL) return RET_BUG;
    ret = C2F(drscl)(n->I, sa->R, sx->R, incx->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dspev(Stack stack, int rhs, int opt, int lhs) /* dspev */
{
  int_types T[] = {string, string, mat_int, realmat, realmat, realmat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *jobz, *uplo;
  NspMatrix *n, *ap, *w, *z, *ldz, *work, *info;
  int jobz_len, uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobz, &uplo, &n, &ap, &w, &z, &ldz, &work, &info, &jobz_len, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dspev)(jobz, uplo, n->I, ap->R, w->R, z->R, ldz->I, work->R, info->I, jobz_len, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dspgst(Stack stack, int rhs, int opt, int lhs) /* dspgst */
{
  int_types T[] = {mat_int, string, mat_int, realmat, realmat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *itype, *n, *ap, *bp, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&itype, &uplo, &n, &ap, &bp, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dspgst)(itype->I, uplo, n->I, ap->R, bp->R, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dspgv(Stack stack, int rhs, int opt, int lhs) /* dspgv */
{
  int_types T[] = {mat_int, string, string, mat_int, realmat, realmat, realmat, realmat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *jobz, *uplo;
  NspMatrix *itype, *n, *ap, *bp, *w, *z, *ldz, *work, *info;
  int jobz_len, uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&itype, &jobz, &uplo, &n, &ap, &bp, &w, &z, &ldz, &work, &info, &jobz_len, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dspgv)(itype->I, jobz, uplo, n->I, ap->R, bp->R, w->R, z->R, ldz->I, work->R, info->I, jobz_len, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsptrd(Stack stack, int rhs, int opt, int lhs) /* dsptrd */
{
  int_types T[] = {string, mat_int, realmat, realmat, realmat, realmat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *ap, *d, *e, *tau, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &ap, &d, &e, &tau, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dsptrd)(uplo, n->I, ap->R, d->R, e->R, tau->R, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsptrf(Stack stack, int rhs, int opt, int lhs) /* dsptrf */
{
  int_types T[] = {string, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *ap, *ipiv, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &ap, &ipiv, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dsptrf)(uplo, n->I, ap->R, ipiv->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsteqr(Stack stack, int rhs, int opt, int lhs) /* dsteqr */
{
  int_types T[] = {string, mat_int, realmat, realmat, realmat, mat_int, realmat, mat_int, s_int,t_end};
  char *compz;
  NspMatrix *n, *d, *e, *z, *ldz, *work, *info;
  int compz_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&compz, &n, &d, &e, &z, &ldz, &work, &info, &compz_len) == FAIL) return RET_BUG;
    ret = C2F(dsteqr)(compz, n->I, d->R, e->R, z->R, ldz->I, work->R, info->I, compz_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsterf(Stack stack, int rhs, int opt, int lhs) /* dsterf */
{
  int_types T[] = {mat_int, realmat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *n, *d, *e, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &d, &e, &info) == FAIL) return RET_BUG;
    ret = C2F(dsterf)(n->I, d->R, e->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsycon(Stack stack, int rhs, int opt, int lhs) /* dsycon */
{
  int_types T[] = {string, mat_int, realmat, mat_int, mat_int, realmat, realmat, realmat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *ipiv, *anorm, *rcond, *work, *iwork, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &ipiv, &anorm, &rcond, &work, &iwork, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dsycon)(uplo, n->I, a->R, lda->I, ipiv->I, anorm->R, rcond->R, work->R, iwork->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsyev(Stack stack, int rhs, int opt, int lhs) /* dsyev */
{
  int_types T[] = {string, string, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *jobz, *uplo;
  NspMatrix *n, *a, *lda, *w, *work, *lwork, *info;
  int jobz_len, uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobz, &uplo, &n, &a, &lda, &w, &work, &lwork, &info, &jobz_len, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dsyev)(jobz, uplo, n->I, a->R, lda->I, w->R, work->R, lwork->I, info->I, jobz_len, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsyevr(Stack stack, int rhs, int opt, int lhs) /* dsyevr */
{
  int_types T[] = {string, string, string, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int, realmat, mat_int, mat_int, mat_int, mat_int, s_int, s_int, s_int,t_end};
  char *jobz, *range, *uplo;
  NspMatrix *n, *a, *lda, *vl, *vu, *il, *iu, *abstol, *m, *w, *z, *ldz, *isuppz, *work, *lwork, *iwork, *liwork, *info;
  int jobz_len, range_len, uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobz, &range, &uplo, &n, &a, &lda, &vl, &vu, &il, &iu, &abstol, &m, &w, &z, &ldz, &isuppz, &work, &lwork, &iwork, &liwork, &info, &jobz_len, &range_len, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dsyevr)(jobz, range, uplo, n->I, a->R, lda->I, vl->R, vu->R, il->I, iu->I, abstol->R, m->I, w->R, z->R, ldz->I, isuppz->I, work->R, lwork->I, iwork->I, liwork->I, info->I, jobz_len, range_len, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsysv(Stack stack, int rhs, int opt, int lhs) /* dsysv */
{
  int_types T[] = {string, mat_int, mat_int, realmat, mat_int, mat_int, realmat, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *nrhs, *a, *lda, *ipiv, *b, *ldb, *work, *lwork, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &nrhs, &a, &lda, &ipiv, &b, &ldb, &work, &lwork, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dsysv)(uplo, n->I, nrhs->I, a->R, lda->I, ipiv->I, b->R, ldb->I, work->R, lwork->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsytf2(Stack stack, int rhs, int opt, int lhs) /* dsytf2 */
{
  int_types T[] = {string, mat_int, realmat, mat_int, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *ipiv, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &ipiv, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dsytf2)(uplo, n->I, a->R, lda->I, ipiv->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsytrd(Stack stack, int rhs, int opt, int lhs) /* dsytrd */
{
  int_types T[] = {string, mat_int, realmat, mat_int, realmat, realmat, realmat, realmat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *d, *e, *tau, *work, *lwork, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &d, &e, &tau, &work, &lwork, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dsytrd)(uplo, n->I, a->R, lda->I, d->R, e->R, tau->R, work->R, lwork->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dlatrd(Stack stack, int rhs, int opt, int lhs) /* dlatrd */
{
  int_types T[] = {string, mat_int, mat_int, realmat, mat_int, realmat, realmat, realmat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *nb, *a, *lda, *e, *tau, *w, *ldw;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &nb, &a, &lda, &e, &tau, &w, &ldw, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dlatrd)(uplo, n->I, nb->I, a->R, lda->I, e->R, tau->R, w->R, ldw->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsytd2(Stack stack, int rhs, int opt, int lhs) /* dsytd2 */
{
  int_types T[] = {string, mat_int, realmat, mat_int, realmat, realmat, realmat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *d, *e, *tau, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &d, &e, &tau, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dsytd2)(uplo, n->I, a->R, lda->I, d->R, e->R, tau->R, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsytrf(Stack stack, int rhs, int opt, int lhs) /* dsytrf */
{
  int_types T[] = {string, mat_int, realmat, mat_int, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *ipiv, *work, *lwork, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &ipiv, &work, &lwork, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dsytrf)(uplo, n->I, a->R, lda->I, ipiv->I, work->R, lwork->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsytri(Stack stack, int rhs, int opt, int lhs) /* dsytri */
{
  int_types T[] = {string, mat_int, realmat, mat_int, mat_int, realmat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *ipiv, *work, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &ipiv, &work, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dsytri)(uplo, n->I, a->R, lda->I, ipiv->I, work->R, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dsytrs(Stack stack, int rhs, int opt, int lhs) /* dsytrs */
{
  int_types T[] = {string, mat_int, mat_int, realmat, mat_int, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *nrhs, *a, *lda, *ipiv, *b, *ldb, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &nrhs, &a, &lda, &ipiv, &b, &ldb, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(dsytrs)(uplo, n->I, nrhs->I, a->R, lda->I, ipiv->I, b->R, ldb->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtgevc(Stack stack, int rhs, int opt, int lhs) /* dtgevc */
{
  int_types T[] = {string, string, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, mat_int, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *side, *howmny;
  NspMatrix *select, *n, *a, *lda, *b, *ldb, *vl, *ldvl, *vr, *ldvr, *mm, *m, *work, *info;
  int side_len, howmny_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &howmny, &select, &n, &a, &lda, &b, &ldb, &vl, &ldvl, &vr, &ldvr, &mm, &m, &work, &info, &side_len, &howmny_len) == FAIL) return RET_BUG;
    ret = C2F(dtgevc)(side, howmny, select->I, n->I, a->R, lda->I, b->R, ldb->I, vl->R, ldvl->I, vr->R, ldvr->I, mm->I, m->I, work->R, info->I, side_len, howmny_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtrcon(Stack stack, int rhs, int opt, int lhs) /* dtrcon */
{
  int_types T[] = {string, string, string, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int, s_int, s_int, s_int,t_end};
  char *norm, *uplo, *diag;
  NspMatrix *n, *a, *lda, *rcond, *work, *iwork, *info;
  int norm_len, uplo_len, diag_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &uplo, &diag, &n, &a, &lda, &rcond, &work, &iwork, &info, &norm_len, &uplo_len, &diag_len) == FAIL) return RET_BUG;
    ret = C2F(dtrcon)(norm, uplo, diag, n->I, a->R, lda->I, rcond->R, work->R, iwork->I, info->I, norm_len, uplo_len, diag_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtrevc(Stack stack, int rhs, int opt, int lhs) /* dtrevc */
{
  int_types T[] = {string, string, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, mat_int, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *side, *howmny;
  NspMatrix *select, *n, *t, *ldt, *vl, *ldvl, *vr, *ldvr, *mm, *m, *work, *info;
  int side_len, howmny_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &howmny, &select, &n, &t, &ldt, &vl, &ldvl, &vr, &ldvr, &mm, &m, &work, &info, &side_len, &howmny_len) == FAIL) return RET_BUG;
    ret = C2F(dtrevc)(side, howmny, select->I, n->I, t->R, ldt->I, vl->R, ldvl->I, vr->R, ldvr->I, mm->I, m->I, work->R, info->I, side_len, howmny_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtrexc(Stack stack, int rhs, int opt, int lhs) /* dtrexc */
{
  int_types T[] = {string, mat_int, realmat, mat_int, realmat, mat_int, mat_int, mat_int, realmat, mat_int, s_int,t_end};
  char *compq;
  NspMatrix *n, *t, *ldt, *q, *ldq, *ifst, *ilst, *work, *info;
  int compq_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&compq, &n, &t, &ldt, &q, &ldq, &ifst, &ilst, &work, &info, &compq_len) == FAIL) return RET_BUG;
    ret = C2F(dtrexc)(compq, n->I, t->R, ldt->I, q->R, ldq->I, ifst->I, ilst->I, work->R, info->I, compq_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtrsen(Stack stack, int rhs, int opt, int lhs) /* dtrsen */
{
  int_types T[] = {string, string, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, realmat, mat_int, realmat, realmat, realmat, mat_int, mat_int, mat_int, mat_int, s_int, s_int,t_end};
  char *job, *compq;
  NspMatrix *select, *n, *t, *ldt, *q, *ldq, *wr, *wi, *m, *s, *sep, *work, *lwork, *iwork, *liwork, *info;
  int job_len, compq_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &compq, &select, &n, &t, &ldt, &q, &ldq, &wr, &wi, &m, &s, &sep, &work, &lwork, &iwork, &liwork, &info, &job_len, &compq_len) == FAIL) return RET_BUG;
    ret = C2F(dtrsen)(job, compq, select->I, n->I, t->R, ldt->I, q->R, ldq->I, wr->R, wi->R, m->I, s->R, sep->R, work->R, lwork->I, iwork->I, liwork->I, info->I, job_len, compq_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtrsyl(Stack stack, int rhs, int opt, int lhs) /* dtrsyl */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *trana, *tranb;
  NspMatrix *isgn, *m, *n, *a, *lda, *b, *ldb, *c, *ldc, *scale, *info;
  int trana_len, tranb_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&trana, &tranb, &isgn, &m, &n, &a, &lda, &b, &ldb, &c, &ldc, &scale, &info, &trana_len, &tranb_len) == FAIL) return RET_BUG;
    ret = C2F(dtrsyl)(trana, tranb, isgn->I, m->I, n->I, a->R, lda->I, b->R, ldb->I, c->R, ldc->I, scale->R, info->I, trana_len, tranb_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtrti2(Stack stack, int rhs, int opt, int lhs) /* dtrti2 */
{
  int_types T[] = {string, string, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *uplo, *diag;
  NspMatrix *n, *a, *lda, *info;
  int uplo_len, diag_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &diag, &n, &a, &lda, &info, &uplo_len, &diag_len) == FAIL) return RET_BUG;
    ret = C2F(dtrti2)(uplo, diag, n->I, a->R, lda->I, info->I, uplo_len, diag_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtrtri(Stack stack, int rhs, int opt, int lhs) /* dtrtri */
{
  int_types T[] = {string, string, mat_int, realmat, mat_int, mat_int, s_int, s_int,t_end};
  char *uplo, *diag;
  NspMatrix *n, *a, *lda, *info;
  int uplo_len, diag_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &diag, &n, &a, &lda, &info, &uplo_len, &diag_len) == FAIL) return RET_BUG;
    ret = C2F(dtrtri)(uplo, diag, n->I, a->R, lda->I, info->I, uplo_len, diag_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtrtrs(Stack stack, int rhs, int opt, int lhs) /* dtrtrs */
{
  int_types T[] = {string, string, string, mat_int, mat_int, realmat, mat_int, realmat, mat_int, mat_int, s_int, s_int, s_int,t_end};
  char *uplo, *trans, *diag;
  NspMatrix *n, *nrhs, *a, *lda, *b, *ldb, *info;
  int uplo_len, trans_len, diag_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &trans, &diag, &n, &nrhs, &a, &lda, &b, &ldb, &info, &uplo_len, &trans_len, &diag_len) == FAIL) return RET_BUG;
    ret = C2F(dtrtrs)(uplo, trans, diag, n->I, nrhs->I, a->R, lda->I, b->R, ldb->I, info->I, uplo_len, trans_len, diag_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtzrqf(Stack stack, int rhs, int opt, int lhs) /* dtzrqf */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &info) == FAIL) return RET_BUG;
    ret = C2F(dtzrqf)(m->I, n->I, a->R, lda->I, tau->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dtzrzf(Stack stack, int rhs, int opt, int lhs) /* dtzrzf */
{
  int_types T[] = {mat_int, mat_int, realmat, mat_int, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(dtzrzf)(m->I, n->I, a->R, lda->I, tau->R, work->R, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dzsum1(Stack stack, int rhs, int opt, int lhs) /* dzsum1 */
{
  int_types T[] = {mat_int, mat, mat_int,t_end};
  NspMatrix *n, *cx, *incx;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&n, &cx, &incx) == FAIL) return RET_BUG;
    ret = C2F(dzsum1)(n->I, cx->C, incx->I);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ilaenv(Stack stack, int rhs, int opt, int lhs) /* ilaenv */
{
  int_types T[] = {mat_int, string, string, mat_int, mat_int, mat_int, mat_int, s_int, s_int,t_end};
  char *name, *opts;
  NspMatrix *ispec, *n1, *n2, *n3, *n4;
  int name_len, opts_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&ispec, &name, &opts, &n1, &n2, &n3, &n4, &name_len, &opts_len) == FAIL) return RET_BUG;
    ret = C2F(ilaenv)(ispec->I, name, opts, n1->I, n2->I, n3->I, n4->I, name_len, opts_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_izmax1(Stack stack, int rhs, int opt, int lhs) /* izmax1 */
{
  int_types T[] = {mat_int, mat, mat_int,t_end};
  int ret;
  NspMatrix *n, *cx, *incx;

  if ( GetArgs(stack,rhs,opt,T,&n, &cx, &incx) == FAIL) return RET_BUG;
    ret = C2F(izmax1)(n->I, cx->C, incx->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_lsame(Stack stack, int rhs, int opt, int lhs) /* lsame */
{
  int_types T[] = {string, string, s_int, s_int,t_end};
  char *ca, *cb;
  int ca_len, cb_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&ca, &cb, &ca_len, &cb_len) == FAIL) return RET_BUG;
    ret = C2F(lsame)(ca, cb, ca_len, cb_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_slamc1(Stack stack, int rhs, int opt, int lhs) /* slamc1 */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *beta, *t, *rnd, *ieee1;

  if ( GetArgs(stack,rhs,opt,T,&beta, &t, &rnd, &ieee1) == FAIL) return RET_BUG;
    ret = C2F(slamc1)(beta->I, t->I, rnd->I, ieee1->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_xerbla(Stack stack, int rhs, int opt, int lhs) /* xerbla */
{
  int_types T[] = {string, mat_int, s_int,t_end};
  char *srname;
  NspMatrix *info;
  int srname_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&srname, &info, &srname_len) == FAIL) return RET_BUG;
    ret = C2F(xerbla)(srname, info->I, srname_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zbdsqr(Stack stack, int rhs, int opt, int lhs) /* zbdsqr */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, mat_int, realmat, realmat, mat, mat_int, mat, mat_int, mat, mat_int, realmat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *ncvt, *nru, *ncc, *d, *e, *vt, *ldvt, *u, *ldu, *c, *ldc, *rwork, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &ncvt, &nru, &ncc, &d, &e, &vt, &ldvt, &u, &ldu, &c, &ldc, &rwork, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zbdsqr)(uplo, n->I, ncvt->I, nru->I, ncc->I, d->R, e->R, vt->C, ldvt->I, u->C, ldu->I, c->C, ldc->I, rwork->R, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zdrot(Stack stack, int rhs, int opt, int lhs) /* zdrot */
{
  int_types T[] = {mat_int, mat, mat_int, mat, mat_int, realmat, realmat,t_end};
  int ret;
  NspMatrix *n, *cx, *incx, *cy, *incy, *c, *s;

  if ( GetArgs(stack,rhs,opt,T,&n, &cx, &incx, &cy, &incy, &c, &s) == FAIL) return RET_BUG;
    ret = C2F(zdrot)(n->I, cx->C, incx->I, cy->C, incy->I, c->R, s->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zdrscl(Stack stack, int rhs, int opt, int lhs) /* zdrscl */
{
  int_types T[] = {mat_int, realmat, mat, mat_int,t_end};
  int ret;
  NspMatrix *n, *sa, *sx, *incx;

  if ( GetArgs(stack,rhs,opt,T,&n, &sa, &sx, &incx) == FAIL) return RET_BUG;
    ret = C2F(zdrscl)(n->I, sa->R, sx->C, incx->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgebak(Stack stack, int rhs, int opt, int lhs) /* zgebak */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, mat_int, mat, mat_int, mat_int, s_int, s_int,t_end};
  char *job, *side;
  NspMatrix *n, *ilo, *ihi, *scale, *m, *v, *ldv, *info;
  int job_len, side_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &side, &n, &ilo, &ihi, &scale, &m, &v, &ldv, &info, &job_len, &side_len) == FAIL) return RET_BUG;
    ret = C2F(zgebak)(job, side, n->I, ilo->I, ihi->I, scale->R, m->I, v->C, ldv->I, info->I, job_len, side_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgebal(Stack stack, int rhs, int opt, int lhs) /* zgebal */
{
  int_types T[] = {string, mat_int, mat, mat_int, mat_int, mat_int, realmat, mat_int, s_int,t_end};
  char *job;
  NspMatrix *n, *a, *lda, *ilo, *ihi, *scale, *info;
  int job_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &n, &a, &lda, &ilo, &ihi, &scale, &info, &job_len) == FAIL) return RET_BUG;
    ret = C2F(zgebal)(job, n->I, a->C, lda->I, ilo->I, ihi->I, scale->R, info->I, job_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgebd2(Stack stack, int rhs, int opt, int lhs) /* zgebd2 */
{
  int_types T[] = {mat_int, mat_int, mat, mat_int, realmat, realmat, mat, mat, mat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *d, *e, *tauq, *taup, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &d, &e, &tauq, &taup, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(zgebd2)(m->I, n->I, a->C, lda->I, d->R, e->R, tauq->C, taup->C, work->C, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgebrd(Stack stack, int rhs, int opt, int lhs) /* zgebrd */
{
  int_types T[] = {mat_int, mat_int, mat, mat_int, realmat, realmat, mat, mat, mat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *d, *e, *tauq, *taup, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &d, &e, &tauq, &taup, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zgebrd)(m->I, n->I, a->C, lda->I, d->R, e->R, tauq->C, taup->C, work->C, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgecon(Stack stack, int rhs, int opt, int lhs) /* zgecon */
{
  int_types T[] = {string, mat_int, mat, mat_int, realmat, realmat, mat, realmat, mat_int, s_int,t_end};
  char *norm;
  NspMatrix *n, *a, *lda, *anorm, *rcond, *work, *rwork, *info;
  int norm_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &n, &a, &lda, &anorm, &rcond, &work, &rwork, &info, &norm_len) == FAIL) return RET_BUG;
    ret = C2F(zgecon)(norm, n->I, a->C, lda->I, anorm->R, rcond->R, work->C, rwork->R, info->I, norm_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}


static int _wrap_zgeev(Stack stack, int rhs, int opt, int lhs) /* zgeev */
{
  int_types T[] = {string, string, mat_int, mat, mat_int, mat, mat, mat_int, mat, mat_int, mat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *jobvl, *jobvr;
  NspMatrix *n, *a, *lda, *w, *vl, *ldvl, *vr, *ldvr, *work, *lwork, *rwork, *info;
  int jobvl_len, jobvr_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobvl, &jobvr, &n, &a, &lda, &w, &vl, &ldvl, &vr, &ldvr, &work, &lwork, &rwork, &info, &jobvl_len, &jobvr_len) == FAIL) return RET_BUG;
    ret = C2F(zgeev)(jobvl, jobvr, n->I, a->C, lda->I, w->C, vl->C, ldvl->I, vr->C, ldvr->I, work->C, lwork->I, rwork->R, info->I, jobvl_len, jobvr_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgehd2(Stack stack, int rhs, int opt, int lhs) /* zgehd2 */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int,t_end};
  int ret;
  NspMatrix *n, *ilo, *ihi, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &ilo, &ihi, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(zgehd2)(n->I, ilo->I, ihi->I, a->C, lda->I, tau->C, work->C, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgehrd(Stack stack, int rhs, int opt, int lhs) /* zgehrd */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *ilo, *ihi, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &ilo, &ihi, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zgehrd)(n->I, ilo->I, ihi->I, a->C, lda->I, tau->C, work->C, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgelq2(Stack stack, int rhs, int opt, int lhs) /* zgelq2 */
{
  int_types T[] = {mat_int, mat_int, mat, mat_int, mat, mat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(zgelq2)(m->I, n->I, a->C, lda->I, tau->C, work->C, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgelqf(Stack stack, int rhs, int opt, int lhs) /* zgelqf */
{
  int_types T[] = {mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zgelqf)(m->I, n->I, a->C, lda->I, tau->C, work->C, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgelsd(Stack stack, int rhs, int opt, int lhs) /* zgelsd */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, realmat, realmat, mat_int, mat, mat_int, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *nrhs, *a, *lda, *b, *ldb, *s, *rcond, *rank, *work, *lwork, *rwork, *iwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &nrhs, &a, &lda, &b, &ldb, &s, &rcond, &rank, &work, &lwork, &rwork, &iwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zgelsd)(m->I, n->I, nrhs->I, a->C, lda->I, b->C, ldb->I, s->R, rcond->R, rank->I, work->C, lwork->I, rwork->R, iwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgelsy(Stack stack, int rhs, int opt, int lhs) /* zgelsy */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, mat_int, realmat, mat_int, mat, mat_int, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *nrhs, *a, *lda, *b, *ldb, *jpvt, *rcond, *rank, *work, *lwork, *rwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &nrhs, &a, &lda, &b, &ldb, &jpvt, &rcond, &rank, &work, &lwork, &rwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zgelsy)(m->I, n->I, nrhs->I, a->C, lda->I, b->C, ldb->I, jpvt->I, rcond->R, rank->I, work->C, lwork->I, rwork->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgeqp3(Stack stack, int rhs, int opt, int lhs) /* zgeqp3 */
{
  int_types T[] = {mat_int, mat_int, mat, mat_int, mat_int, mat, mat, mat_int, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *jpvt, *tau, *work, *lwork, *rwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &jpvt, &tau, &work, &lwork, &rwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zgeqp3)(m->I, n->I, a->C, lda->I, jpvt->I, tau->C, work->C, lwork->I, rwork->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgeqpf(Stack stack, int rhs, int opt, int lhs) /* zgeqpf */
{
  int_types T[] = {mat_int, mat_int, mat, mat_int, mat_int, mat, mat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *jpvt, *tau, *work, *rwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &jpvt, &tau, &work, &rwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zgeqpf)(m->I, n->I, a->C, lda->I, jpvt->I, tau->C, work->C, rwork->R, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgeqr2(Stack stack, int rhs, int opt, int lhs) /* zgeqr2 */
{
  int_types T[] = {mat_int, mat_int, mat, mat_int, mat, mat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(zgeqr2)(m->I, n->I, a->C, lda->I, tau->C, work->C, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgeqrf(Stack stack, int rhs, int opt, int lhs) /* zgeqrf */
{
  int_types T[] = {mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zgeqrf)(m->I, n->I, a->C, lda->I, tau->C, work->C, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgesc2(Stack stack, int rhs, int opt, int lhs) /* zgesc2 */
{
  int_types T[] = {mat_int, mat, mat_int, mat, mat_int, mat_int, realmat,t_end};
  int ret;
  NspMatrix *n, *a, *lda, *rrhs, *ipiv, *jpiv, *scale;

  if ( GetArgs(stack,rhs,opt,T,&n, &a, &lda, &rrhs, &ipiv, &jpiv, &scale) == FAIL) return RET_BUG;
    ret = C2F(zgesc2)(n->I, a->C, lda->I, rrhs->C, ipiv->I, jpiv->I, scale->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgesvd(Stack stack, int rhs, int opt, int lhs) /* zgesvd */
{
  int_types T[] = {string, string, mat_int, mat_int, mat, mat_int, realmat, mat, mat_int, mat, mat_int, mat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *jobu, *jobvt;
  NspMatrix *m, *n, *a, *lda, *s, *u, *ldu, *vt, *ldvt, *work, *lwork, *rwork, *info;
  int jobu_len, jobvt_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobu, &jobvt, &m, &n, &a, &lda, &s, &u, &ldu, &vt, &ldvt, &work, &lwork, &rwork, &info, &jobu_len, &jobvt_len) == FAIL) return RET_BUG;
    ret = C2F(zgesvd)(jobu, jobvt, m->I, n->I, a->C, lda->I, s->R, u->C, ldu->I, vt->C, ldvt->I, work->C, lwork->I, rwork->R, info->I, jobu_len, jobvt_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgesdd(Stack stack, int rhs, int opt, int lhs) /* zgesdd */
{
  int_types T[] = {string, mat_int, mat_int, mat, mat_int, realmat, mat, mat_int, mat, mat_int, mat, mat_int, realmat, mat_int, mat_int, s_int,t_end};
  char *jobz;
  NspMatrix *m, *n, *a, *lda, *s, *u, *ldu, *vt, *ldvt, *work, *lwork, *rwork, *iwork, *info;
  int jobz_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobz, &m, &n, &a, &lda, &s, &u, &ldu, &vt, &ldvt, &work, &lwork, &rwork, &iwork, &info, &jobz_len) == FAIL) return RET_BUG;
    ret = C2F(zgesdd)(jobz, m->I, n->I, a->C, lda->I, s->R, u->C, ldu->I, vt->C, ldvt->I, work->C, lwork->I, rwork->R, iwork->I, info->I, jobz_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgetc2(Stack stack, int rhs, int opt, int lhs) /* zgetc2 */
{
  int_types T[] = {mat_int, mat, mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *a, *lda, *ipiv, *jpiv, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &a, &lda, &ipiv, &jpiv, &info) == FAIL) return RET_BUG;
    ret = C2F(zgetc2)(n->I, a->C, lda->I, ipiv->I, jpiv->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgetf2(Stack stack, int rhs, int opt, int lhs) /* zgetf2 */
{
  int_types T[] = {mat_int, mat_int, mat, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *ipiv, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &ipiv, &info) == FAIL) return RET_BUG;
    ret = C2F(zgetf2)(m->I, n->I, a->C, lda->I, ipiv->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgetrf(Stack stack, int rhs, int opt, int lhs) /* zgetrf */
{
  int_types T[] = {mat_int, mat_int, mat, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *ipiv, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &ipiv, &info) == FAIL) return RET_BUG;
    ret = C2F(zgetrf)(m->I, n->I, a->C, lda->I, ipiv->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgbtrf(Stack stack, int rhs, int opt, int lhs) /* zgbtrf */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int, mat, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *kl, *ku, *a, *lda, *ipiv, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &kl, &ku, &a, &lda, &ipiv, &info) == FAIL) return RET_BUG;
    ret = C2F(zgbtrf)(m->I, n->I, kl->I, ku->I, a->C, lda->I, ipiv->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgetri(Stack stack, int rhs, int opt, int lhs) /* zgetri */
{
  int_types T[] = {mat_int, mat, mat_int, mat_int, mat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *a, *lda, *ipiv, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &a, &lda, &ipiv, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zgetri)(n->I, a->C, lda->I, ipiv->I, work->C, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgetrs(Stack stack, int rhs, int opt, int lhs) /* zgetrs */
{
  int_types T[] = {string, mat_int, mat_int, mat, mat_int, mat_int, mat, mat_int, mat_int, s_int,t_end};
  char *trans;
  NspMatrix *n, *nrhs, *a, *lda, *ipiv, *b, *ldb, *info;
  int trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&trans, &n, &nrhs, &a, &lda, &ipiv, &b, &ldb, &info, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(zgetrs)(trans, n->I, nrhs->I, a->C, lda->I, ipiv->I, b->C, ldb->I, info->I, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgbtrs(Stack stack, int rhs, int opt, int lhs) /* zgbtrs */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, mat_int, mat, mat_int, mat_int, mat, mat_int, mat_int, s_int,t_end};
  char *trans;
  NspMatrix *n, *kl, *ku, *nrhs, *a, *lda, *ipiv, *b, *ldb, *info;
  int trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&trans, &n, &kl, &ku, &nrhs, &a, &lda, &ipiv, &b, &ldb, &info, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(zgbtrs)(trans, n->I, kl->I, ku->I, nrhs->I, a->C, lda->I, ipiv->I, b->C, ldb->I, info->I, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zggbak(Stack stack, int rhs, int opt, int lhs) /* zggbak */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, realmat, realmat, mat_int, mat, mat_int, mat_int, s_int, s_int,t_end};
  char *job, *side;
  NspMatrix *n, *ilo, *ihi, *lscale, *rscale, *m, *v, *ldv, *info;
  int job_len, side_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &side, &n, &ilo, &ihi, &lscale, &rscale, &m, &v, &ldv, &info, &job_len, &side_len) == FAIL) return RET_BUG;
    ret = C2F(zggbak)(job, side, n->I, ilo->I, ihi->I, lscale->R, rscale->R, m->I, v->C, ldv->I, info->I, job_len, side_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zggbal(Stack stack, int rhs, int opt, int lhs) /* zggbal */
{
  int_types T[] = {string, mat_int, mat, mat_int, mat, mat_int, mat_int, mat_int, realmat, realmat, realmat, mat_int, s_int,t_end};
  char *job;
  NspMatrix *n, *a, *lda, *b, *ldb, *ilo, *ihi, *lscale, *rscale, *work, *info;
  int job_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &n, &a, &lda, &b, &ldb, &ilo, &ihi, &lscale, &rscale, &work, &info, &job_len) == FAIL) return RET_BUG;
    ret = C2F(zggbal)(job, n->I, a->C, lda->I, b->C, ldb->I, ilo->I, ihi->I, lscale->R, rscale->R, work->R, info->I, job_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}


static int _wrap_zggev(Stack stack, int rhs, int opt, int lhs) /* zggev */
{
  int_types T[] = {string, string, mat_int, mat, mat_int, mat, mat_int, mat, mat, mat, mat_int, mat, mat_int, mat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *jobvl, *jobvr;
  NspMatrix *n, *a, *lda, *b, *ldb, *alpha, *beta, *vl, *ldvl, *vr, *ldvr, *work, *lwork, *rwork, *info;
  int jobvl_len, jobvr_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobvl, &jobvr, &n, &a, &lda, &b, &ldb, &alpha, &beta, &vl, &ldvl, &vr, &ldvr, &work, &lwork, &rwork, &info, &jobvl_len, &jobvr_len) == FAIL) return RET_BUG;
    ret = C2F(zggev)(jobvl, jobvr, n->I, a->C, lda->I, b->C, ldb->I, alpha->C, beta->C, vl->C, ldvl->I, vr->C, ldvr->I, work->C, lwork->I, rwork->R, info->I, jobvl_len, jobvr_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgghrd(Stack stack, int rhs, int opt, int lhs) /* zgghrd */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat_int, s_int, s_int,t_end};
  char *compq, *compz;
  NspMatrix *n, *ilo, *ihi, *a, *lda, *b, *ldb, *q, *ldq, *z, *ldz, *info;
  int compq_len, compz_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&compq, &compz, &n, &ilo, &ihi, &a, &lda, &b, &ldb, &q, &ldq, &z, &ldz, &info, &compq_len, &compz_len) == FAIL) return RET_BUG;
    ret = C2F(zgghrd)(compq, compz, n->I, ilo->I, ihi->I, a->C, lda->I, b->C, ldb->I, q->C, ldq->I, z->C, ldz->I, info->I, compq_len, compz_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zheev(Stack stack, int rhs, int opt, int lhs) /* zheev */
{
  int_types T[] = {string, string, mat_int, mat, mat_int, realmat, mat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *jobz, *uplo;
  NspMatrix *n, *a, *lda, *w, *work, *lwork, *rwork, *info;
  int jobz_len, uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobz, &uplo, &n, &a, &lda, &w, &work, &lwork, &rwork, &info, &jobz_len, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zheev)(jobz, uplo, n->I, a->C, lda->I, w->R, work->C, lwork->I, rwork->R, info->I, jobz_len, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zheevr(Stack stack, int rhs, int opt, int lhs) /* zheevr */
{
  int_types T[] = {string, string, string, mat_int, mat, mat_int, realmat, realmat, mat_int, mat_int, realmat, mat_int, realmat, mat, mat_int, mat_int, mat, mat_int, realmat, mat_int, mat_int, mat_int, mat_int, s_int, s_int, s_int,t_end};
  char *jobz, *range, *uplo;
  NspMatrix *n, *a, *lda, *vl, *vu, *il, *iu, *abstol, *m, *w, *z, *ldz, *isuppz, *work, *lwork, *rwork, *lrwork, *iwork, *liwork, *info;
  int jobz_len, range_len, uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&jobz, &range, &uplo, &n, &a, &lda, &vl, &vu, &il, &iu, &abstol, &m, &w, &z, &ldz, &isuppz, &work, &lwork, &rwork, &lrwork, &iwork, &liwork, &info, &jobz_len, &range_len, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zheevr)(jobz, range, uplo, n->I, a->C, lda->I, vl->R, vu->R, il->I, iu->I, abstol->R, m->I, w->R, z->C, ldz->I, isuppz->I, work->C, lwork->I, rwork->R, lrwork->I, iwork->I, liwork->I, info->I, jobz_len, range_len, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zhetd2(Stack stack, int rhs, int opt, int lhs) /* zhetd2 */
{
  int_types T[] = {string, mat_int, mat, mat_int, realmat, realmat, mat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *d, *e, *tau, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &d, &e, &tau, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zhetd2)(uplo, n->I, a->C, lda->I, d->R, e->R, tau->C, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zhetrd(Stack stack, int rhs, int opt, int lhs) /* zhetrd */
{
  int_types T[] = {string, mat_int, mat, mat_int, realmat, realmat, mat, mat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *d, *e, *tau, *work, *lwork, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &d, &e, &tau, &work, &lwork, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zhetrd)(uplo, n->I, a->C, lda->I, d->R, e->R, tau->C, work->C, lwork->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zhgeqz(Stack stack, int rhs, int opt, int lhs) /* zhgeqz */
{
  int_types T[] = {string, string, string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat, mat, mat_int, mat, mat_int, mat, mat_int, realmat, mat_int, s_int, s_int, s_int,t_end};
  char *job, *compq, *compz;
  NspMatrix *n, *ilo, *ihi, *a, *lda, *b, *ldb, *alpha, *beta, *q, *ldq, *z, *ldz, *work, *lwork, *rwork, *info;
  int job_len, compq_len, compz_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &compq, &compz, &n, &ilo, &ihi, &a, &lda, &b, &ldb, &alpha, &beta, &q, &ldq, &z, &ldz, &work, &lwork, &rwork, &info, &job_len, &compq_len, &compz_len) == FAIL) return RET_BUG;
    ret = C2F(zhgeqz)(job, compq, compz, n->I, ilo->I, ihi->I, a->C, lda->I, b->C, ldb->I, alpha->C, beta->C, q->C, ldq->I, z->C, ldz->I, work->C, lwork->I, rwork->R, info->I, job_len, compq_len, compz_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zhseqr(Stack stack, int rhs, int opt, int lhs) /* zhseqr */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat, mat_int, mat_int, s_int, s_int,t_end};
  char *job, *compz;
  NspMatrix *n, *ilo, *ihi, *h, *ldh, *w, *z, *ldz, *work, *lwork, *info;
  int job_len, compz_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &compz, &n, &ilo, &ihi, &h, &ldh, &w, &z, &ldz, &work, &lwork, &info, &job_len, &compz_len) == FAIL) return RET_BUG;
    ret = C2F(zhseqr)(job, compz, n->I, ilo->I, ihi->I, h->C, ldh->I, w->C, z->C, ldz->I, work->C, lwork->I, info->I, job_len, compz_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlabrd(Stack stack, int rhs, int opt, int lhs) /* zlabrd */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, realmat, realmat, mat, mat, mat, mat_int, mat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *nb, *a, *lda, *d, *e, *tauq, *taup, *x, *ldx, *y, *ldy;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &nb, &a, &lda, &d, &e, &tauq, &taup, &x, &ldx, &y, &ldy) == FAIL) return RET_BUG;
    ret = C2F(zlabrd)(m->I, n->I, nb->I, a->C, lda->I, d->R, e->R, tauq->C, taup->C, x->C, ldx->I, y->C, ldy->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlacgv(Stack stack, int rhs, int opt, int lhs) /* zlacgv */
{
  int_types T[] = {mat_int, mat, mat_int,t_end};
  int ret;
  NspMatrix *n, *x, *incx;

  if ( GetArgs(stack,rhs,opt,T,&n, &x, &incx) == FAIL) return RET_BUG;
    ret = C2F(zlacgv)(n->I, x->C, incx->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlacon(Stack stack, int rhs, int opt, int lhs) /* zlacon */
{
  int_types T[] = {mat_int, mat, mat, realmat, mat_int,t_end};
  int ret;
  NspMatrix *n, *v, *x, *est, *kase;

  if ( GetArgs(stack,rhs,opt,T,&n, &v, &x, &est, &kase) == FAIL) return RET_BUG;
    ret = C2F(zlacon)(n->I, v->C, x->C, est->R, kase->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlacpy(Stack stack, int rhs, int opt, int lhs) /* zlacpy */
{
  int_types T[] = {string, mat_int, mat_int, mat, mat_int, mat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *m, *n, *a, *lda, *b, *ldb;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &m, &n, &a, &lda, &b, &ldb, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zlacpy)(uplo, m->I, n->I, a->C, lda->I, b->C, ldb->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zladiv(Stack stack, int rhs, int opt, int lhs) /* zladiv */
{
  int_types T[] = {mat, mat, mat,t_end};
  NspMatrix *ret_val, *x, *y;

  if ( GetArgs(stack,rhs,opt,T,&ret_val, &x, &y) == FAIL) return RET_BUG;
    C2F(zladiv)(ret_val->C, x->C, y->C);
  return 0;
}

static int _wrap_zlahqr(Stack stack, int rhs, int opt, int lhs) /* zlahqr */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, mat_int, mat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *wantt, *wantz, *n, *ilo, *ihi, *h, *ldh, *w, *iloz, *ihiz, *z, *ldz, *info;

  if ( GetArgs(stack,rhs,opt,T,&wantt, &wantz, &n, &ilo, &ihi, &h, &ldh, &w, &iloz, &ihiz, &z, &ldz, &info) == FAIL) return RET_BUG;
    ret = C2F(zlahqr)(wantt->I, wantz->I, n->I, ilo->I, ihi->I, h->C, ldh->I, w->C, iloz->I, ihiz->I, z->C, ldz->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlahrd(Stack stack, int rhs, int opt, int lhs) /* zlahrd */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat, mat_int,t_end};
  int ret;
  NspMatrix *n, *k, *nb, *a, *lda, *tau, *t, *ldt, *y, *ldy;

  if ( GetArgs(stack,rhs,opt,T,&n, &k, &nb, &a, &lda, &tau, &t, &ldt, &y, &ldy) == FAIL) return RET_BUG;
    ret = C2F(zlahrd)(n->I, k->I, nb->I, a->C, lda->I, tau->C, t->C, ldt->I, y->C, ldy->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlaic1(Stack stack, int rhs, int opt, int lhs) /* zlaic1 */
{
  int_types T[] = {mat_int, mat_int, mat, realmat, mat, mat, realmat, mat, mat,t_end};
  int ret;
  NspMatrix *job, *j, *x, *sest, *w, *gamma, *sestpr, *s, *c;

  if ( GetArgs(stack,rhs,opt,T,&job, &j, &x, &sest, &w, &gamma, &sestpr, &s, &c) == FAIL) return RET_BUG;
    ret = C2F(zlaic1)(job->I, j->I, x->C, sest->R, w->C, gamma->C, sestpr->R, s->C, c->C);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlange(Stack stack, int rhs, int opt, int lhs) /* zlange */
{
  int_types T[] = {string, mat_int, mat_int, mat, mat_int, realmat, s_int,t_end};
  char *norm;
  NspMatrix *m, *n, *a, *lda, *work;
  int norm_len;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &m, &n, &a, &lda, &work, &norm_len) == FAIL) return RET_BUG;
    ret = C2F(zlange)(norm, m->I, n->I, a->C, lda->I, work->R, norm_len);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlanhe(Stack stack, int rhs, int opt, int lhs) /* zlanhe */
{
  int_types T[] = {string, string, mat_int, mat, mat_int, realmat, s_int, s_int,t_end};
  char *norm, *uplo;
  NspMatrix *n, *a, *lda, *work;
  int norm_len, uplo_len;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &uplo, &n, &a, &lda, &work, &norm_len, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zlanhe)(norm, uplo, n->I, a->C, lda->I, work->R, norm_len, uplo_len);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlanhs(Stack stack, int rhs, int opt, int lhs) /* zlanhs */
{
  int_types T[] = {string, mat_int, mat, mat_int, realmat, s_int,t_end};
  char *norm;
  NspMatrix *n, *a, *lda, *work;
  int norm_len;
  double ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &n, &a, &lda, &work, &norm_len) == FAIL) return RET_BUG;
    ret = C2F(zlanhs)(norm, n->I, a->C, lda->I, work->R, norm_len);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlaqp2(Stack stack, int rhs, int opt, int lhs) /* zlaqp2 */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat_int, mat, realmat, realmat, mat,t_end};
  int ret;
  NspMatrix *m, *n, *offset, *a, *lda, *jpvt, *tau, *vn1, *vn2, *work;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &offset, &a, &lda, &jpvt, &tau, &vn1, &vn2, &work) == FAIL) return RET_BUG;
    ret = C2F(zlaqp2)(m->I, n->I, offset->I, a->C, lda->I, jpvt->I, tau->C, vn1->R, vn2->R, work->C);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlaqps(Stack stack, int rhs, int opt, int lhs) /* zlaqps */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int, mat_int, mat, mat_int, mat_int, mat, realmat, realmat, mat, mat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *offset, *nb, *kb, *a, *lda, *jpvt, *tau, *vn1, *vn2, *auxv, *f, *ldf;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &offset, &nb, &kb, &a, &lda, &jpvt, &tau, &vn1, &vn2, &auxv, &f, &ldf) == FAIL) return RET_BUG;
    ret = C2F(zlaqps)(m->I, n->I, offset->I, nb->I, kb->I, a->C, lda->I, jpvt->I, tau->C, vn1->R, vn2->R, auxv->C, f->C, ldf->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlarfb(Stack stack, int rhs, int opt, int lhs) /* zlarfb */
{
  int_types T[] = {string, string, string, string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, s_int, s_int, s_int, s_int,t_end};
  char *side, *trans, *direct, *storev;
  NspMatrix *m, *n, *k, *v, *ldv, *t, *ldt, *c, *ldc, *work, *ldwork;
  int side_len, trans_len, direct_len, storev_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &direct, &storev, &m, &n, &k, &v, &ldv, &t, &ldt, &c, &ldc, &work, &ldwork, &side_len, &trans_len, &direct_len, &storev_len) == FAIL) return RET_BUG;
    ret = C2F(zlarfb)(side, trans, direct, storev, m->I, n->I, k->I, v->C, ldv->I, t->C, ldt->I, c->C, ldc->I, work->C, ldwork->I, side_len, trans_len, direct_len, storev_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlarf(Stack stack, int rhs, int opt, int lhs) /* zlarf */
{
  int_types T[] = {string, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat, s_int,t_end};
  char *side;
  NspMatrix *m, *n, *v, *incv, *tau, *c, *ldc, *work;
  int side_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &m, &n, &v, &incv, &tau, &c, &ldc, &work, &side_len) == FAIL) return RET_BUG;
    ret = C2F(zlarf)(side, m->I, n->I, v->C, incv->I, tau->C, c->C, ldc->I, work->C, side_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlarfg(Stack stack, int rhs, int opt, int lhs) /* zlarfg */
{
  int_types T[] = {mat_int, mat, mat, mat_int, mat,t_end};
  int ret;
  NspMatrix *n, *alpha, *x, *incx, *tau;

  if ( GetArgs(stack,rhs,opt,T,&n, &alpha, &x, &incx, &tau) == FAIL) return RET_BUG;
    ret = C2F(zlarfg)(n->I, alpha->C, x->C, incx->I, tau->C);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlarft(Stack stack, int rhs, int opt, int lhs) /* zlarft */
{
  int_types T[] = {string, string, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, s_int, s_int,t_end};
  char *direct, *storev;
  NspMatrix *n, *k, *v, *ldv, *tau, *t, *ldt;
  int direct_len, storev_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&direct, &storev, &n, &k, &v, &ldv, &tau, &t, &ldt, &direct_len, &storev_len) == FAIL) return RET_BUG;
    ret = C2F(zlarft)(direct, storev, n->I, k->I, v->C, ldv->I, tau->C, t->C, ldt->I, direct_len, storev_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlarfx(Stack stack, int rhs, int opt, int lhs) /* zlarfx */
{
  int_types T[] = {string, mat_int, mat_int, mat, mat, mat, mat_int, mat, s_int,t_end};
  char *side;
  NspMatrix *m, *n, *v, *tau, *c, *ldc, *work;
  int side_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &m, &n, &v, &tau, &c, &ldc, &work, &side_len) == FAIL) return RET_BUG;
    ret = C2F(zlarfx)(side, m->I, n->I, v->C, tau->C, c->C, ldc->I, work->C, side_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlartg(Stack stack, int rhs, int opt, int lhs) /* zlartg */
{
  int_types T[] = {mat, mat, realmat, mat, mat,t_end};
  int ret;
  NspMatrix *f, *g, *cs, *sn, *r;

  if ( GetArgs(stack,rhs,opt,T,&f, &g, &cs, &sn, &r) == FAIL) return RET_BUG;
    ret = C2F(zlartg)(f->C, g->C, cs->R, sn->C, r->C);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlarzb(Stack stack, int rhs, int opt, int lhs) /* zlarzb */
{
  int_types T[] = {string, string, string, string, mat_int, mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, s_int, s_int, s_int, s_int,t_end};
  char *side, *trans, *direct, *storev;
  NspMatrix *m, *n, *k, *l, *v, *ldv, *t, *ldt, *c, *ldc, *work, *ldwork;
  int side_len, trans_len, direct_len, storev_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &direct, &storev, &m, &n, &k, &l, &v, &ldv, &t, &ldt, &c, &ldc, &work, &ldwork, &side_len, &trans_len, &direct_len, &storev_len) == FAIL) return RET_BUG;
    ret = C2F(zlarzb)(side, trans, direct, storev, m->I, n->I, k->I, l->I, v->C, ldv->I, t->C, ldt->I, c->C, ldc->I, work->C, ldwork->I, side_len, trans_len, direct_len, storev_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlarz(Stack stack, int rhs, int opt, int lhs) /* zlarz */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat, s_int,t_end};
  char *side;
  NspMatrix *m, *n, *l, *v, *incv, *tau, *c, *ldc, *work;
  int side_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &m, &n, &l, &v, &incv, &tau, &c, &ldc, &work, &side_len) == FAIL) return RET_BUG;
    ret = C2F(zlarz)(side, m->I, n->I, l->I, v->C, incv->I, tau->C, c->C, ldc->I, work->C, side_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlarzt(Stack stack, int rhs, int opt, int lhs) /* zlarzt */
{
  int_types T[] = {string, string, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, s_int, s_int,t_end};
  char *direct, *storev;
  NspMatrix *n, *k, *v, *ldv, *tau, *t, *ldt;
  int direct_len, storev_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&direct, &storev, &n, &k, &v, &ldv, &tau, &t, &ldt, &direct_len, &storev_len) == FAIL) return RET_BUG;
    ret = C2F(zlarzt)(direct, storev, n->I, k->I, v->C, ldv->I, tau->C, t->C, ldt->I, direct_len, storev_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlascl(Stack stack, int rhs, int opt, int lhs) /* zlascl */
{
  int_types T[] = {string, mat_int, mat_int, realmat, realmat, mat_int, mat_int, mat, mat_int, mat_int, s_int,t_end};
  char *type;
  NspMatrix *kl, *ku, *cfrom, *cto, *m, *n, *a, *lda, *info;
  int type_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&type, &kl, &ku, &cfrom, &cto, &m, &n, &a, &lda, &info, &type_len) == FAIL) return RET_BUG;
    ret = C2F(zlascl)(type, kl->I, ku->I, cfrom->R, cto->R, m->I, n->I, a->C, lda->I, info->I, type_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlaset(Stack stack, int rhs, int opt, int lhs) /* zlaset */
{
  int_types T[] = {string, mat_int, mat_int, mat, mat, mat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *m, *n, *alpha, *beta, *a, *lda;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &m, &n, &alpha, &beta, &a, &lda, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zlaset)(uplo, m->I, n->I, alpha->C, beta->C, a->C, lda->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlasr(Stack stack, int rhs, int opt, int lhs) /* zlasr */
{
  int_types T[] = {string, string, string, mat_int, mat_int, realmat, realmat, mat, mat_int, s_int, s_int, s_int,t_end};
  char *side, *pivot, *direct;
  NspMatrix *m, *n, *c, *s, *a, *lda;
  int side_len, pivot_len, direct_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &pivot, &direct, &m, &n, &c, &s, &a, &lda, &side_len, &pivot_len, &direct_len) == FAIL) return RET_BUG;
    ret = C2F(zlasr)(side, pivot, direct, m->I, n->I, c->R, s->R, a->C, lda->I, side_len, pivot_len, direct_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlassq(Stack stack, int rhs, int opt, int lhs) /* zlassq */
{
  int_types T[] = {mat_int, mat, mat_int, realmat, realmat,t_end};
  int ret;
  NspMatrix *n, *x, *incx, *scale, *sumsq;

  if ( GetArgs(stack,rhs,opt,T,&n, &x, &incx, &scale, &sumsq) == FAIL) return RET_BUG;
    ret = C2F(zlassq)(n->I, x->C, incx->I, scale->R, sumsq->R);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlaswp(Stack stack, int rhs, int opt, int lhs) /* zlaswp */
{
  int_types T[] = {mat_int, mat, mat_int, mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *a, *lda, *k1, *k2, *ipiv, *incx;

  if ( GetArgs(stack,rhs,opt,T,&n, &a, &lda, &k1, &k2, &ipiv, &incx) == FAIL) return RET_BUG;
    ret = C2F(zlaswp)(n->I, a->C, lda->I, k1->I, k2->I, ipiv->I, incx->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlatdf(Stack stack, int rhs, int opt, int lhs) /* zlatdf */
{
  int_types T[] = {mat_int, mat_int, mat, mat_int, mat, realmat, realmat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *ijob, *n, *z, *ldz, *rrhs, *rdsum, *rdscal, *ipiv, *jpiv;

  if ( GetArgs(stack,rhs,opt,T,&ijob, &n, &z, &ldz, &rrhs, &rdsum, &rdscal, &ipiv, &jpiv) == FAIL) return RET_BUG;
    ret = C2F(zlatdf)(ijob->I, n->I, z->C, ldz->I, rrhs->C, rdsum->R, rdscal->R, ipiv->I, jpiv->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlatrd(Stack stack, int rhs, int opt, int lhs) /* zlatrd */
{
  int_types T[] = {string, mat_int, mat_int, mat, mat_int, realmat, mat, mat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *nb, *a, *lda, *e, *tau, *w, *ldw;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &nb, &a, &lda, &e, &tau, &w, &ldw, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zlatrd)(uplo, n->I, nb->I, a->C, lda->I, e->R, tau->C, w->C, ldw->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlatrs(Stack stack, int rhs, int opt, int lhs) /* zlatrs */
{
  int_types T[] = {string, string, string, string, mat_int, mat, mat_int, mat, realmat, realmat, mat_int, s_int, s_int, s_int, s_int,t_end};
  char *uplo, *trans, *diag, *normin;
  NspMatrix *n, *a, *lda, *x, *scale, *cnorm, *info;
  int uplo_len, trans_len, diag_len, normin_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &trans, &diag, &normin, &n, &a, &lda, &x, &scale, &cnorm, &info, &uplo_len, &trans_len, &diag_len, &normin_len) == FAIL) return RET_BUG;
    ret = C2F(zlatrs)(uplo, trans, diag, normin, n->I, a->C, lda->I, x->C, scale->R, cnorm->R, info->I, uplo_len, trans_len, diag_len, normin_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zlatrz(Stack stack, int rhs, int opt, int lhs) /* zlatrz */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat,t_end};
  int ret;
  NspMatrix *m, *n, *l, *a, *lda, *tau, *work;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &l, &a, &lda, &tau, &work) == FAIL) return RET_BUG;
    ret = C2F(zlatrz)(m->I, n->I, l->I, a->C, lda->I, tau->C, work->C);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zpotf2(Stack stack, int rhs, int opt, int lhs) /* zpotf2 */
{
  int_types T[] = {string, mat_int, mat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zpotf2)(uplo, n->I, a->C, lda->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zpotrf(Stack stack, int rhs, int opt, int lhs) /* zpotrf */
{
  int_types T[] = {string, mat_int, mat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zpotrf)(uplo, n->I, a->C, lda->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zpotrs(Stack stack, int rhs, int opt, int lhs) /* zpotrs */
{
  int_types T[] = {string, mat_int, mat_int, mat, mat_int, mat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *nrhs, *a, *lda, *b, *ldb, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &nrhs, &a, &lda, &b, &ldb, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zpotrs)(uplo, n->I, nrhs->I, a->C, lda->I, b->C, ldb->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zrot(Stack stack, int rhs, int opt, int lhs) /* zrot */
{
  int_types T[] = {mat_int, mat, mat_int, mat, mat_int, realmat, mat,t_end};
  int ret;
  NspMatrix *n, *cx, *incx, *cy, *incy, *c, *s;

  if ( GetArgs(stack,rhs,opt,T,&n, &cx, &incx, &cy, &incy, &c, &s) == FAIL) return RET_BUG;
    ret = C2F(zrot)(n->I, cx->C, incx->I, cy->C, incy->I, c->R, s->C);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zsteqr(Stack stack, int rhs, int opt, int lhs) /* zsteqr */
{
  int_types T[] = {string, mat_int, realmat, realmat, mat, mat_int, realmat, mat_int, s_int,t_end};
  char *compz;
  NspMatrix *n, *d, *e, *z, *ldz, *work, *info;
  int compz_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&compz, &n, &d, &e, &z, &ldz, &work, &info, &compz_len) == FAIL) return RET_BUG;
    ret = C2F(zsteqr)(compz, n->I, d->R, e->R, z->C, ldz->I, work->R, info->I, compz_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztgevc(Stack stack, int rhs, int opt, int lhs) /* ztgevc */
{
  int_types T[] = {string, string, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat_int, mat_int, mat, realmat, mat_int, s_int, s_int,t_end};
  char *side, *howmny;
  NspMatrix *select, *n, *a, *lda, *b, *ldb, *vl, *ldvl, *vr, *ldvr, *mm, *m, *work, *rwork, *info;
  int side_len, howmny_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &howmny, &select, &n, &a, &lda, &b, &ldb, &vl, &ldvl, &vr, &ldvr, &mm, &m, &work, &rwork, &info, &side_len, &howmny_len) == FAIL) return RET_BUG;
    ret = C2F(ztgevc)(side, howmny, select->I, n->I, a->C, lda->I, b->C, ldb->I, vl->C, ldvl->I, vr->C, ldvr->I, mm->I, m->I, work->C, rwork->R, info->I, side_len, howmny_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztgex2(Stack stack, int rhs, int opt, int lhs) /* ztgex2 */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *wantq, *wantz, *n, *a, *lda, *b, *ldb, *q, *ldq, *z, *ldz, *j1, *info;

  if ( GetArgs(stack,rhs,opt,T,&wantq, &wantz, &n, &a, &lda, &b, &ldb, &q, &ldq, &z, &ldz, &j1, &info) == FAIL) return RET_BUG;
    ret = C2F(ztgex2)(wantq->I, wantz->I, n->I, a->C, lda->I, b->C, ldb->I, q->C, ldq->I, z->C, ldz->I, j1->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztgexc(Stack stack, int rhs, int opt, int lhs) /* ztgexc */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *wantq, *wantz, *n, *a, *lda, *b, *ldb, *q, *ldq, *z, *ldz, *ifst, *ilst, *info;

  if ( GetArgs(stack,rhs,opt,T,&wantq, &wantz, &n, &a, &lda, &b, &ldb, &q, &ldq, &z, &ldz, &ifst, &ilst, &info) == FAIL) return RET_BUG;
    ret = C2F(ztgexc)(wantq->I, wantz->I, n->I, a->C, lda->I, b->C, ldb->I, q->C, ldq->I, z->C, ldz->I, ifst->I, ilst->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztgsen(Stack stack, int rhs, int opt, int lhs) /* ztgsen */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat, mat, mat_int, mat, mat_int, mat_int, realmat, realmat, realmat, mat, mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *ijob, *wantq, *wantz, *select, *n, *a, *lda, *b, *ldb, *alpha, *beta, *q, *ldq, *z, *ldz, *m, *pl, *pr, *dif, *work, *lwork, *iwork, *liwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&ijob, &wantq, &wantz, &select, &n, &a, &lda, &b, &ldb, &alpha, &beta, &q, &ldq, &z, &ldz, &m, &pl, &pr, &dif, &work, &lwork, &iwork, &liwork, &info) == FAIL) return RET_BUG;
    ret = C2F(ztgsen)(ijob->I, wantq->I, wantz->I, select->I, n->I, a->C, lda->I, b->C, ldb->I, alpha->C, beta->C, q->C, ldq->I, z->C, ldz->I, m->I, pl->R, pr->R, dif->R, work->C, lwork->I, iwork->I, liwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztgsy2(Stack stack, int rhs, int opt, int lhs) /* ztgsy2 */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, realmat, realmat, realmat, mat_int, s_int,t_end};
  char *trans;
  NspMatrix *ijob, *m, *n, *a, *lda, *b, *ldb, *c, *ldc, *d, *ldd, *e, *lde, *f, *ldf, *scale, *rdsum, *rdscal, *info;
  int trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&trans, &ijob, &m, &n, &a, &lda, &b, &ldb, &c, &ldc, &d, &ldd, &e, &lde, &f, &ldf, &scale, &rdsum, &rdscal, &info, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(ztgsy2)(trans, ijob->I, m->I, n->I, a->C, lda->I, b->C, ldb->I, c->C, ldc->I, d->C, ldd->I, e->C, lde->I, f->C, ldf->I, scale->R, rdsum->R, rdscal->R, info->I, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztgsyl(Stack stack, int rhs, int opt, int lhs) /* ztgsyl */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, realmat, realmat, mat, mat_int, mat_int, mat_int, s_int,t_end};
  char *trans;
  NspMatrix *ijob, *m, *n, *a, *lda, *b, *ldb, *c, *ldc, *d, *ldd, *e, *lde, *f, *ldf, *scale, *dif, *work, *lwork, *iwork, *info;
  int trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&trans, &ijob, &m, &n, &a, &lda, &b, &ldb, &c, &ldc, &d, &ldd, &e, &lde, &f, &ldf, &scale, &dif, &work, &lwork, &iwork, &info, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(ztgsyl)(trans, ijob->I, m->I, n->I, a->C, lda->I, b->C, ldb->I, c->C, ldc->I, d->C, ldd->I, e->C, lde->I, f->C, ldf->I, scale->R, dif->R, work->C, lwork->I, iwork->I, info->I, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztrcon(Stack stack, int rhs, int opt, int lhs) /* ztrcon */
{
  int_types T[] = {string, string, string, mat_int, mat, mat_int, realmat, mat, realmat, mat_int, s_int, s_int, s_int,t_end};
  char *norm, *uplo, *diag;
  NspMatrix *n, *a, *lda, *rcond, *workC, *work, *info;
  int norm_len, uplo_len, diag_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&norm, &uplo, &diag, &n, &a, &lda, &rcond, &workC, &work, &info, &norm_len, &uplo_len, &diag_len) == FAIL) return RET_BUG;
    ret = C2F(ztrcon)(norm, uplo, diag, n->I, a->C, lda->I, rcond->R, workC->C, work->R, info->I, norm_len, uplo_len, diag_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztrevc(Stack stack, int rhs, int opt, int lhs) /* ztrevc */
{
  int_types T[] = {string, string, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, mat_int, mat_int, mat, realmat, mat_int, s_int, s_int,t_end};
  char *side, *howmny;
  NspMatrix *select, *n, *t, *ldt, *vl, *ldvl, *vr, *ldvr, *mm, *m, *work, *rwork, *info;
  int side_len, howmny_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &howmny, &select, &n, &t, &ldt, &vl, &ldvl, &vr, &ldvr, &mm, &m, &work, &rwork, &info, &side_len, &howmny_len) == FAIL) return RET_BUG;
    ret = C2F(ztrevc)(side, howmny, select->I, n->I, t->C, ldt->I, vl->C, ldvl->I, vr->C, ldvr->I, mm->I, m->I, work->C, rwork->R, info->I, side_len, howmny_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztrexc(Stack stack, int rhs, int opt, int lhs) /* ztrexc */
{
  int_types T[] = {string, mat_int, mat, mat_int, mat, mat_int, mat_int, mat_int, mat_int, s_int,t_end};
  char *compq;
  NspMatrix *n, *t, *ldt, *q, *ldq, *ifst, *ilst, *info;
  int compq_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&compq, &n, &t, &ldt, &q, &ldq, &ifst, &ilst, &info, &compq_len) == FAIL) return RET_BUG;
    ret = C2F(ztrexc)(compq, n->I, t->C, ldt->I, q->C, ldq->I, ifst->I, ilst->I, info->I, compq_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztrsen(Stack stack, int rhs, int opt, int lhs) /* ztrsen */
{
  int_types T[] = {string, string, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, realmat, realmat, mat, mat_int, mat_int, s_int, s_int,t_end};
  char *job, *compq;
  NspMatrix *select, *n, *t, *ldt, *q, *ldq, *w, *m, *s, *sep, *work, *lwork, *info;
  int job_len, compq_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&job, &compq, &select, &n, &t, &ldt, &q, &ldq, &w, &m, &s, &sep, &work, &lwork, &info, &job_len, &compq_len) == FAIL) return RET_BUG;
    ret = C2F(ztrsen)(job, compq, select->I, n->I, t->C, ldt->I, q->C, ldq->I, w->C, m->I, s->R, sep->R, work->C, lwork->I, info->I, job_len, compq_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztrsyl(Stack stack, int rhs, int opt, int lhs) /* ztrsyl */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat_int, mat, mat_int, realmat, mat_int, s_int, s_int,t_end};
  char *trana, *tranb;
  NspMatrix *isgn, *m, *n, *a, *lda, *b, *ldb, *c, *ldc, *scale, *info;
  int trana_len, tranb_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&trana, &tranb, &isgn, &m, &n, &a, &lda, &b, &ldb, &c, &ldc, &scale, &info, &trana_len, &tranb_len) == FAIL) return RET_BUG;
    ret = C2F(ztrsyl)(trana, tranb, isgn->I, m->I, n->I, a->C, lda->I, b->C, ldb->I, c->C, ldc->I, scale->R, info->I, trana_len, tranb_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztrti2(Stack stack, int rhs, int opt, int lhs) /* ztrti2 */
{
  int_types T[] = {string, string, mat_int, mat, mat_int, mat_int, s_int, s_int,t_end};
  char *uplo, *diag;
  NspMatrix *n, *a, *lda, *info;
  int uplo_len, diag_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &diag, &n, &a, &lda, &info, &uplo_len, &diag_len) == FAIL) return RET_BUG;
    ret = C2F(ztrti2)(uplo, diag, n->I, a->C, lda->I, info->I, uplo_len, diag_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztrtri(Stack stack, int rhs, int opt, int lhs) /* ztrtri */
{
  int_types T[] = {string, string, mat_int, mat, mat_int, mat_int, s_int, s_int,t_end};
  char *uplo, *diag;
  NspMatrix *n, *a, *lda, *info;
  int uplo_len, diag_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &diag, &n, &a, &lda, &info, &uplo_len, &diag_len) == FAIL) return RET_BUG;
    ret = C2F(ztrtri)(uplo, diag, n->I, a->C, lda->I, info->I, uplo_len, diag_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztzrzf(Stack stack, int rhs, int opt, int lhs) /* ztzrzf */
{
  int_types T[] = {mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(ztzrzf)(m->I, n->I, a->C, lda->I, tau->C, work->C, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_ztrtrs(Stack stack, int rhs, int opt, int lhs) /* ztrtrs */
{
  int_types T[] = {string, string, string, mat_int, mat_int, mat, mat_int, mat, mat_int, mat_int, s_int, s_int, s_int,t_end};
  char *uplo, *trans, *diag;
  NspMatrix *n, *nrhs, *a, *lda, *b, *ldb, *info;
  int uplo_len, trans_len, diag_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &trans, &diag, &n, &nrhs, &a, &lda, &b, &ldb, &info, &uplo_len, &trans_len, &diag_len) == FAIL) return RET_BUG;
    ret = C2F(ztrtrs)(uplo, trans, diag, n->I, nrhs->I, a->C, lda->I, b->C, ldb->I, info->I, uplo_len, trans_len, diag_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zung2l(Stack stack, int rhs, int opt, int lhs) /* zung2l */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(zung2l)(m->I, n->I, k->I, a->C, lda->I, tau->C, work->C, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zung2r(Stack stack, int rhs, int opt, int lhs) /* zung2r */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(zung2r)(m->I, n->I, k->I, a->C, lda->I, tau->C, work->C, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zungbr(Stack stack, int rhs, int opt, int lhs) /* zungbr */
{
  int_types T[] = {string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat_int, s_int,t_end};
  char *vect;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *lwork, *info;
  int vect_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&vect, &m, &n, &k, &a, &lda, &tau, &work, &lwork, &info, &vect_len) == FAIL) return RET_BUG;
    ret = C2F(zungbr)(vect, m->I, n->I, k->I, a->C, lda->I, tau->C, work->C, lwork->I, info->I, vect_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zunghr(Stack stack, int rhs, int opt, int lhs) /* zunghr */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *n, *ilo, *ihi, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&n, &ilo, &ihi, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zunghr)(n->I, ilo->I, ihi->I, a->C, lda->I, tau->C, work->C, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zungl2(Stack stack, int rhs, int opt, int lhs) /* zungl2 */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &info) == FAIL) return RET_BUG;
    ret = C2F(zungl2)(m->I, n->I, k->I, a->C, lda->I, tau->C, work->C, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zunglq(Stack stack, int rhs, int opt, int lhs) /* zunglq */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zunglq)(m->I, n->I, k->I, a->C, lda->I, tau->C, work->C, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zungql(Stack stack, int rhs, int opt, int lhs) /* zungql */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zungql)(m->I, n->I, k->I, a->C, lda->I, tau->C, work->C, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zungqr(Stack stack, int rhs, int opt, int lhs) /* zungqr */
{
  int_types T[] = {mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *work, *lwork, *info;

  if ( GetArgs(stack,rhs,opt,T,&m, &n, &k, &a, &lda, &tau, &work, &lwork, &info) == FAIL) return RET_BUG;
    ret = C2F(zungqr)(m->I, n->I, k->I, a->C, lda->I, tau->C, work->C, lwork->I, info->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zungtr(Stack stack, int rhs, int opt, int lhs) /* zungtr */
{
  int_types T[] = {string, mat_int, mat, mat_int, mat, mat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *tau, *work, *lwork, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &tau, &work, &lwork, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zungtr)(uplo, n->I, a->C, lda->I, tau->C, work->C, lwork->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zunm2r(Stack stack, int rhs, int opt, int lhs) /* zunm2r */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(zunm2r)(side, trans, m->I, n->I, k->I, a->C, lda->I, tau->C, c->C, ldc->I, work->C, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zunmbr(Stack stack, int rhs, int opt, int lhs) /* zunmbr */
{
  int_types T[] = {string, string, string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat, mat_int, mat_int, s_int, s_int, s_int,t_end};
  char *vect, *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *lwork, *info;
  int vect_len, side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&vect, &side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &lwork, &info, &vect_len, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(zunmbr)(vect, side, trans, m->I, n->I, k->I, a->C, lda->I, tau->C, c->C, ldc->I, work->C, lwork->I, info->I, vect_len, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zunml2(Stack stack, int rhs, int opt, int lhs) /* zunml2 */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(zunml2)(side, trans, m->I, n->I, k->I, a->C, lda->I, tau->C, c->C, ldc->I, work->C, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zunmlq(Stack stack, int rhs, int opt, int lhs) /* zunmlq */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat, mat_int, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *lwork, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &lwork, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(zunmlq)(side, trans, m->I, n->I, k->I, a->C, lda->I, tau->C, c->C, ldc->I, work->C, lwork->I, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zunmqr(Stack stack, int rhs, int opt, int lhs) /* zunmqr */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat, mat_int, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *a, *lda, *tau, *c, *ldc, *work, *lwork, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &a, &lda, &tau, &c, &ldc, &work, &lwork, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(zunmqr)(side, trans, m->I, n->I, k->I, a->C, lda->I, tau->C, c->C, ldc->I, work->C, lwork->I, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zunmr3(Stack stack, int rhs, int opt, int lhs) /* zunmr3 */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *l, *a, *lda, *tau, *c, *ldc, *work, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &l, &a, &lda, &tau, &c, &ldc, &work, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(zunmr3)(side, trans, m->I, n->I, k->I, l->I, a->C, lda->I, tau->C, c->C, ldc->I, work->C, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zunmrz(Stack stack, int rhs, int opt, int lhs) /* zunmrz */
{
  int_types T[] = {string, string, mat_int, mat_int, mat_int, mat_int, mat, mat_int, mat, mat, mat_int, mat, mat_int, mat_int, s_int, s_int,t_end};
  char *side, *trans;
  NspMatrix *m, *n, *k, *l, *a, *lda, *tau, *c, *ldc, *work, *lwork, *info;
  int side_len, trans_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&side, &trans, &m, &n, &k, &l, &a, &lda, &tau, &c, &ldc, &work, &lwork, &info, &side_len, &trans_len) == FAIL) return RET_BUG;
    ret = C2F(zunmrz)(side, trans, m->I, n->I, k->I, l->I, a->C, lda->I, tau->C, c->C, ldc->I, work->C, lwork->I, info->I, side_len, trans_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dgpadm(Stack stack, int rhs, int opt, int lhs) /* dgpadm */
{
  int_types T[] = {mat_int, mat_int, realmat, realmat, mat_int, realmat, mat_int, mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *ideg, *m, *t, *H, *ldh, *wsp, *lwsp, *ipiv, *iexph, *ns, *iflag;

  if ( GetArgs(stack,rhs,opt,T,&ideg, &m, &t, &H, &ldh, &wsp, &lwsp, &ipiv, &iexph, &ns, &iflag) == FAIL) return RET_BUG;
    ret = C2F(dgpadm)(ideg->I, m->I, t->R, H->R, ldh->I, wsp->R, lwsp->I, ipiv->I, iexph->I, ns->I, iflag->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_dspadm(Stack stack, int rhs, int opt, int lhs) /* dspadm */
{
  int_types T[] = {mat_int, mat_int, realmat, realmat, mat_int, realmat, mat_int, mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *ideg, *m, *t, *H, *ldh, *wsp, *lwsp, *ipiv, *iexph, *ns, *iflag;

  if ( GetArgs(stack,rhs,opt,T,&ideg, &m, &t, &H, &ldh, &wsp, &lwsp, &ipiv, &iexph, &ns, &iflag) == FAIL) return RET_BUG;
    ret = C2F(dspadm)(ideg->I, m->I, t->R, H->R, ldh->I, wsp->R, lwsp->I, ipiv->I, iexph->I, ns->I, iflag->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zgpadm(Stack stack, int rhs, int opt, int lhs) /* zgpadm */
{
  int_types T[] = {mat_int, mat_int, realmat, mat, mat_int, mat, mat_int, mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *ideg, *m, *t, *H, *ldh, *wsp, *lwsp, *ipiv, *iexph, *ns, *iflag;

  if ( GetArgs(stack,rhs,opt,T,&ideg, &m, &t, &H, &ldh, &wsp, &lwsp, &ipiv, &iexph, &ns, &iflag) == FAIL) return RET_BUG;
    ret = C2F(zgpadm)(ideg->I, m->I, t->R, H->C, ldh->I, wsp->C, lwsp->I, ipiv->I, iexph->I, ns->I, iflag->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zhpadm(Stack stack, int rhs, int opt, int lhs) /* zhpadm */
{
  int_types T[] = {mat_int, mat_int, realmat, mat, mat_int, mat, mat_int, mat_int, mat_int, mat_int, mat_int,t_end};
  int ret;
  NspMatrix *ideg, *m, *t, *H, *ldh, *wsp, *lwsp, *ipiv, *iexph, *ns, *iflag;

  if ( GetArgs(stack,rhs,opt,T,&ideg, &m, &t, &H, &ldh, &wsp, &lwsp, &ipiv, &iexph, &ns, &iflag) == FAIL) return RET_BUG;
    ret = C2F(zhpadm)(ideg->I, m->I, t->R, H->C, ldh->I, wsp->C, lwsp->I, ipiv->I, iexph->I, ns->I, iflag->I);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zsytrf(Stack stack, int rhs, int opt, int lhs) /* zsytrf */
{
  int_types T[] = {string, mat_int, mat, mat_int, mat_int, mat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *ipiv, *work, *lwork, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &ipiv, &work, &lwork, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zsytrf)(uplo, n->I, a->C, lda->I, ipiv->I, work->C, lwork->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zsycon(Stack stack, int rhs, int opt, int lhs) /* zsycon */
{
  int_types T[] = {string, mat_int, mat, mat_int, mat_int, realmat, realmat, mat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *ipiv, *anorm, *rcond, *work, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &ipiv, &anorm, &rcond, &work, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zsycon)(uplo, n->I, a->C, lda->I, ipiv->I, anorm->R, rcond->R, work->C, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zsytrs(Stack stack, int rhs, int opt, int lhs) /* zsytrs */
{
  int_types T[] = {string, mat_int, mat_int, mat, mat_int, mat_int, mat, mat_int, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *nrhs, *a, *lda, *ipiv, *b, *ldb, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &nrhs, &a, &lda, &ipiv, &b, &ldb, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zsytrs)(uplo, n->I, nrhs->I, a->C, lda->I, ipiv->I, b->C, ldb->I, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_zpocon(Stack stack, int rhs, int opt, int lhs) /* zpocon */
{
  int_types T[] = {string, mat_int, mat, mat_int, realmat, realmat, mat, realmat, mat_int, s_int,t_end};
  char *uplo;
  NspMatrix *n, *a, *lda, *anorm, *rcond, *work, *rwork, *info;
  int uplo_len, ret;

  if ( GetArgs(stack,rhs,opt,T,&uplo, &n, &a, &lda, &anorm, &rcond, &work, &rwork, &info, &uplo_len) == FAIL) return RET_BUG;
    ret = C2F(zpocon)(uplo, n->I, a->C, lda->I, anorm->R, rcond->R, work->C, rwork->R, info->I, uplo_len);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab lapack_all_func[]={
  {"lapack_dgbsv", _wrap_dgbsv},
  {"lapack_zgbsv", _wrap_zgbsv},
  {"lapack_dbdsqr", _wrap_dbdsqr},
  {"lapack_dgebak", _wrap_dgebak},
  {"lapack_dgebal", _wrap_dgebal},
  {"lapack_dgebd2", _wrap_dgebd2},
  {"lapack_dgebrd", _wrap_dgebrd},
  {"lapack_dgecon", _wrap_dgecon},
  {"lapack_dgbcon", _wrap_dgbcon},
  {"lapack_zgbcon", _wrap_zgbcon},
  {"lapack_dgeequ", _wrap_dgeequ},
  /* {"lapack_dgees", _wrap_dgees}, */
  {"lapack_dgeev", _wrap_dgeev},
  {"lapack_dgegs", _wrap_dgegs},
  {"lapack_dgehd2", _wrap_dgehd2},
  {"lapack_dgehrd", _wrap_dgehrd},
  {"lapack_dgelq2", _wrap_dgelq2},
  {"lapack_dgelqf", _wrap_dgelqf},
  {"lapack_dgels", _wrap_dgels},
  {"lapack_dgelss", _wrap_dgelss},
  {"lapack_dgelsd", _wrap_dgelsd},
  {"lapack_dgelsx", _wrap_dgelsx},
  {"lapack_dgelsy", _wrap_dgelsy},
  {"lapack_dgeqlf", _wrap_dgeqlf},
  {"lapack_dgeql2", _wrap_dgeql2},
  {"lapack_dgeqp3", _wrap_dgeqp3},
  {"lapack_dgeqpf", _wrap_dgeqpf},
  {"lapack_dgeqr2", _wrap_dgeqr2},
  {"lapack_dgeqrf", _wrap_dgeqrf},
  {"lapack_dgerfs", _wrap_dgerfs},
  {"lapack_dgerq2", _wrap_dgerq2},
  {"lapack_dgerqf", _wrap_dgerqf},
  {"lapack_dgesvd", _wrap_dgesvd},
  {"lapack_dgesdd", _wrap_dgesdd},
  {"lapack_dgesv", _wrap_dgesv},
  {"lapack_dgesvx", _wrap_dgesvx},
  {"lapack_dgetf2", _wrap_dgetf2},
  {"lapack_dgetrf", _wrap_dgetrf},
  {"lapack_dgbtrf", _wrap_dgbtrf},
  {"lapack_dgetri", _wrap_dgetri},
  {"lapack_dgetrs", _wrap_dgetrs},
  {"lapack_dgbtrs", _wrap_dgbtrs},
  {"lapack_dggbak", _wrap_dggbak},
  {"lapack_dggbal", _wrap_dggbal},
  /* {"lapack_dgges", _wrap_dgges}, */
  {"lapack_dlags2", _wrap_dlags2},
  {"lapack_dlagv2", _wrap_dlagv2},
  {"lapack_dlatdf", _wrap_dlatdf},
  {"lapack_dtgex2", _wrap_dtgex2},
  {"lapack_dtgexc", _wrap_dtgexc},
  {"lapack_dtgsy2", _wrap_dtgsy2},
  {"lapack_dtgsyl", _wrap_dtgsyl},
  {"lapack_dgesc2", _wrap_dgesc2},
  {"lapack_dgetc2", _wrap_dgetc2},
  {"lapack_dtgsen", _wrap_dtgsen},
  {"lapack_dggev", _wrap_dggev},
  {"lapack_dgghrd", _wrap_dgghrd},
  {"lapack_dhgeqz", _wrap_dhgeqz},
  {"lapack_dhseqr", _wrap_dhseqr},
  {"lapack_dlabad", _wrap_dlabad},
  {"lapack_dlabrd", _wrap_dlabrd},
  {"lapack_dlacon", _wrap_dlacon},
  {"lapack_dlacpy", _wrap_dlacpy},
  {"lapack_dladiv", _wrap_dladiv},
  {"lapack_dlae2", _wrap_dlae2},
  {"lapack_dlaev2", _wrap_dlaev2},
  {"lapack_dlaexc", _wrap_dlaexc},
  {"lapack_dlag2", _wrap_dlag2},
  {"lapack_dlahqr", _wrap_dlahqr},
  {"lapack_dlahrd", _wrap_dlahrd},
  {"lapack_dlaic1", _wrap_dlaic1},
  {"lapack_dlaln2", _wrap_dlaln2},
  {"lapack_dlamch", _wrap_dlamch},
  {"lapack_dlamc1", _wrap_dlamc1},
  {"lapack_dlamc2", _wrap_dlamc2},
  {"lapack_dlamc3", _wrap_dlamc3},
  {"lapack_dlamc4", _wrap_dlamc4},
  {"lapack_dlamc5", _wrap_dlamc5},
  {"lapack_dlange", _wrap_dlange},
  {"lapack_dlangb", _wrap_dlangb},
  {"lapack_zlangb", _wrap_zlangb},
  {"lapack_dlanhs", _wrap_dlanhs},
  {"lapack_dlansp", _wrap_dlansp},
  {"lapack_dlanst", _wrap_dlanst},
  {"lapack_dlansy", _wrap_dlansy},
  {"lapack_dlantr", _wrap_dlantr},
  {"lapack_dlanv2", _wrap_dlanv2},
  {"lapack_dlapmt", _wrap_dlapmt},
  {"lapack_dlapy2", _wrap_dlapy2},
  {"lapack_dlapy3", _wrap_dlapy3},
  {"lapack_dlaqge", _wrap_dlaqge},
  {"lapack_dlaqp2", _wrap_dlaqp2},
  {"lapack_dlaqps", _wrap_dlaqps},
  {"lapack_dlarfb", _wrap_dlarfb},
  {"lapack_dlarf", _wrap_dlarf},
  {"lapack_dlarfg", _wrap_dlarfg},
  {"lapack_dlarft", _wrap_dlarft},
  {"lapack_dlarfx", _wrap_dlarfx},
  {"lapack_dlartg", _wrap_dlartg},
  {"lapack_dlarzb", _wrap_dlarzb},
  {"lapack_dlarz", _wrap_dlarz},
  {"lapack_dlarzt", _wrap_dlarzt},
  {"lapack_dlas2", _wrap_dlas2},
  {"lapack_dlascl", _wrap_dlascl},
  {"lapack_dlaset", _wrap_dlaset},
  {"lapack_dlasq1", _wrap_dlasq1},
  {"lapack_dlasq2", _wrap_dlasq2},
  {"lapack_dlasq3", _wrap_dlasq3},
  {"lapack_dlasq4", _wrap_dlasq4},
  {"lapack_dlasr", _wrap_dlasr},
  {"lapack_dlasrt", _wrap_dlasrt},
  {"lapack_dlassq", _wrap_dlassq},
  {"lapack_dlasv2", _wrap_dlasv2},
  {"lapack_dlaswp", _wrap_dlaswp},
  {"lapack_dlasy2", _wrap_dlasy2},
  {"lapack_dlasyf", _wrap_dlasyf},
  {"lapack_dlatrs", _wrap_dlatrs},
  {"lapack_dlatrz", _wrap_dlatrz},
  {"lapack_dlatzm", _wrap_dlatzm},
  {"lapack_dopgtr", _wrap_dopgtr},
  {"lapack_dorg2l", _wrap_dorg2l},
  {"lapack_dorg2r", _wrap_dorg2r},
  {"lapack_dorgbr", _wrap_dorgbr},
  {"lapack_dorghr", _wrap_dorghr},
  {"lapack_dorgl2", _wrap_dorgl2},
  {"lapack_dorglq", _wrap_dorglq},
  {"lapack_dorgql", _wrap_dorgql},
  {"lapack_dorgqr", _wrap_dorgqr},
  {"lapack_dorgr2", _wrap_dorgr2},
  {"lapack_dorgrq", _wrap_dorgrq},
  {"lapack_dorgtr", _wrap_dorgtr},
  {"lapack_dorm2r", _wrap_dorm2r},
  {"lapack_dormbr", _wrap_dormbr},
  {"lapack_dormhr", _wrap_dormhr},
  {"lapack_dorml2", _wrap_dorml2},
  {"lapack_dormlq", _wrap_dormlq},
  {"lapack_dormql", _wrap_dormql},
  {"lapack_dorm2l", _wrap_dorm2l},
  {"lapack_dormqr", _wrap_dormqr},
  {"lapack_dormr2", _wrap_dormr2},
  {"lapack_dormr3", _wrap_dormr3},
  {"lapack_dormrq", _wrap_dormrq},
  {"lapack_dormrz", _wrap_dormrz},
  {"lapack_dpocon", _wrap_dpocon},
  {"lapack_dpotrf", _wrap_dpotrf},
  {"lapack_dpotf2", _wrap_dpotf2},
  {"lapack_dpotrs", _wrap_dpotrs},
  {"lapack_dpptrf", _wrap_dpptrf},
  {"lapack_drscl", _wrap_drscl},
  {"lapack_dspev", _wrap_dspev},
  {"lapack_dspgst", _wrap_dspgst},
  {"lapack_dspgv", _wrap_dspgv},
  {"lapack_dsptrd", _wrap_dsptrd},
  {"lapack_dsptrf", _wrap_dsptrf},
  {"lapack_dsteqr", _wrap_dsteqr},
  {"lapack_dsterf", _wrap_dsterf},
  {"lapack_dsycon", _wrap_dsycon},
  {"lapack_dsyev", _wrap_dsyev},
  {"lapack_dsyevr", _wrap_dsyevr},
  {"lapack_dsysv", _wrap_dsysv},
  {"lapack_dsytf2", _wrap_dsytf2},
  {"lapack_dsytrd", _wrap_dsytrd},
  {"lapack_dlatrd", _wrap_dlatrd},
  {"lapack_dsytd2", _wrap_dsytd2},
  {"lapack_dsytrf", _wrap_dsytrf},
  {"lapack_dsytri", _wrap_dsytri},
  {"lapack_dsytrs", _wrap_dsytrs},
  {"lapack_dtgevc", _wrap_dtgevc},
  {"lapack_dtrcon", _wrap_dtrcon},
  {"lapack_dtrevc", _wrap_dtrevc},
  {"lapack_dtrexc", _wrap_dtrexc},
  {"lapack_dtrsen", _wrap_dtrsen},
  {"lapack_dtrsyl", _wrap_dtrsyl},
  {"lapack_dtrti2", _wrap_dtrti2},
  {"lapack_dtrtri", _wrap_dtrtri},
  {"lapack_dtrtrs", _wrap_dtrtrs},
  {"lapack_dtzrqf", _wrap_dtzrqf},
  {"lapack_dtzrzf", _wrap_dtzrzf},
  {"lapack_dzsum1", _wrap_dzsum1},
  {"lapack_ilaenv", _wrap_ilaenv},
  {"lapack_izmax1", _wrap_izmax1},
  {"lapack_lsame", _wrap_lsame},
  {"lapack_slamc1", _wrap_slamc1},
  {"lapack_xerbla", _wrap_xerbla},
  {"lapack_zbdsqr", _wrap_zbdsqr},
  {"lapack_zdrot", _wrap_zdrot},
  {"lapack_zdrscl", _wrap_zdrscl},
  {"lapack_zgebak", _wrap_zgebak},
  {"lapack_zgebal", _wrap_zgebal},
  {"lapack_zgebd2", _wrap_zgebd2},
  {"lapack_zgebrd", _wrap_zgebrd},
  {"lapack_zgecon", _wrap_zgecon},
  /* {"lapack_zgees", _wrap_zgees}, */
  {"lapack_zgeev", _wrap_zgeev},
  {"lapack_zgehd2", _wrap_zgehd2},
  {"lapack_zgehrd", _wrap_zgehrd},
  {"lapack_zgelq2", _wrap_zgelq2},
  {"lapack_zgelqf", _wrap_zgelqf},
  {"lapack_zgelsd", _wrap_zgelsd},
  {"lapack_zgelsy", _wrap_zgelsy},
  {"lapack_zgeqp3", _wrap_zgeqp3},
  {"lapack_zgeqpf", _wrap_zgeqpf},
  {"lapack_zgeqr2", _wrap_zgeqr2},
  {"lapack_zgeqrf", _wrap_zgeqrf},
  {"lapack_zgesc2", _wrap_zgesc2},
  {"lapack_zgesvd", _wrap_zgesvd},
  {"lapack_zgesdd", _wrap_zgesdd},
  {"lapack_zgetc2", _wrap_zgetc2},
  {"lapack_zgetf2", _wrap_zgetf2},
  {"lapack_zgetrf", _wrap_zgetrf},
  {"lapack_zgbtrf", _wrap_zgbtrf},
  {"lapack_zgetri", _wrap_zgetri},
  {"lapack_zgetrs", _wrap_zgetrs},
  {"lapack_zgbtrs", _wrap_zgbtrs},
  {"lapack_zggbak", _wrap_zggbak},
  {"lapack_zggbal", _wrap_zggbal},
  /* {"lapack_zgges", _wrap_zgges}, */
  {"lapack_zggev", _wrap_zggev},
  {"lapack_zgghrd", _wrap_zgghrd},
  {"lapack_zheev", _wrap_zheev},
  {"lapack_zheevr", _wrap_zheevr},
  {"lapack_zhetd2", _wrap_zhetd2},
  {"lapack_zhetrd", _wrap_zhetrd},
  {"lapack_zhgeqz", _wrap_zhgeqz},
  {"lapack_zhseqr", _wrap_zhseqr},
  {"lapack_zlabrd", _wrap_zlabrd},
  {"lapack_zlacgv", _wrap_zlacgv},
  {"lapack_zlacon", _wrap_zlacon},
  {"lapack_zlacpy", _wrap_zlacpy},
  {"lapack_zladiv", _wrap_zladiv},
  {"lapack_zlahqr", _wrap_zlahqr},
  {"lapack_zlahrd", _wrap_zlahrd},
  {"lapack_zlaic1", _wrap_zlaic1},
  {"lapack_zlange", _wrap_zlange},
  {"lapack_zlanhe", _wrap_zlanhe},
  {"lapack_zlanhs", _wrap_zlanhs},
  {"lapack_zlaqp2", _wrap_zlaqp2},
  {"lapack_zlaqps", _wrap_zlaqps},
  {"lapack_zlarfb", _wrap_zlarfb},
  {"lapack_zlarf", _wrap_zlarf},
  {"lapack_zlarfg", _wrap_zlarfg},
  {"lapack_zlarft", _wrap_zlarft},
  {"lapack_zlarfx", _wrap_zlarfx},
  {"lapack_zlartg", _wrap_zlartg},
  {"lapack_zlarzb", _wrap_zlarzb},
  {"lapack_zlarz", _wrap_zlarz},
  {"lapack_zlarzt", _wrap_zlarzt},
  {"lapack_zlascl", _wrap_zlascl},
  {"lapack_zlaset", _wrap_zlaset},
  {"lapack_zlasr", _wrap_zlasr},
  {"lapack_zlassq", _wrap_zlassq},
  {"lapack_zlaswp", _wrap_zlaswp},
  {"lapack_zlatdf", _wrap_zlatdf},
  {"lapack_zlatrd", _wrap_zlatrd},
  {"lapack_zlatrs", _wrap_zlatrs},
  {"lapack_zlatrz", _wrap_zlatrz},
  {"lapack_zpotf2", _wrap_zpotf2},
  {"lapack_zpotrf", _wrap_zpotrf},
  {"lapack_zpotrs", _wrap_zpotrs},
  {"lapack_zrot", _wrap_zrot},
  {"lapack_zsteqr", _wrap_zsteqr},
  {"lapack_ztgevc", _wrap_ztgevc},
  {"lapack_ztgex2", _wrap_ztgex2},
  {"lapack_ztgexc", _wrap_ztgexc},
  {"lapack_ztgsen", _wrap_ztgsen},
  {"lapack_ztgsy2", _wrap_ztgsy2},
  {"lapack_ztgsyl", _wrap_ztgsyl},
  {"lapack_ztrcon", _wrap_ztrcon},
  {"lapack_ztrevc", _wrap_ztrevc},
  {"lapack_ztrexc", _wrap_ztrexc},
  {"lapack_ztrsen", _wrap_ztrsen},
  {"lapack_ztrsyl", _wrap_ztrsyl},
  {"lapack_ztrti2", _wrap_ztrti2},
  {"lapack_ztrtri", _wrap_ztrtri},
  {"lapack_ztzrzf", _wrap_ztzrzf},
  {"lapack_ztrtrs", _wrap_ztrtrs},
  {"lapack_zung2l", _wrap_zung2l},
  {"lapack_zung2r", _wrap_zung2r},
  {"lapack_zungbr", _wrap_zungbr},
  {"lapack_zunghr", _wrap_zunghr},
  {"lapack_zungl2", _wrap_zungl2},
  {"lapack_zunglq", _wrap_zunglq},
  {"lapack_zungql", _wrap_zungql},
  {"lapack_zungqr", _wrap_zungqr},
  {"lapack_zungtr", _wrap_zungtr},
  {"lapack_zunm2r", _wrap_zunm2r},
  {"lapack_zunmbr", _wrap_zunmbr},
  {"lapack_zunml2", _wrap_zunml2},
  {"lapack_zunmlq", _wrap_zunmlq},
  {"lapack_zunmqr", _wrap_zunmqr},
  {"lapack_zunmr3", _wrap_zunmr3},
  {"lapack_zunmrz", _wrap_zunmrz},
  {"lapack_dgpadm", _wrap_dgpadm},
  {"lapack_dspadm", _wrap_dspadm},
  {"lapack_zgpadm", _wrap_zgpadm},
  {"lapack_zhpadm", _wrap_zhpadm},
  {"lapack_zsytrf", _wrap_zsytrf},
  {"lapack_zsycon", _wrap_zsycon},
  {"lapack_zsytrs", _wrap_zsytrs},
  {"lapack_zpocon", _wrap_zpocon},
  { NULL, NULL}
};

/* call ith function in the lapack_all interface */

int lapack_all_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(lapack_all_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void lapack_all_Interf_Info(int i, char **fname, function (**f))
{
  *fname = lapack_all_func[i].name;
  *f = lapack_all_func[i].fonc;
}

