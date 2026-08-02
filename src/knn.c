/* knn.f -- translated by f2c (version 20090411).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"
#include <R.h>   /* R_alloc */

/* Subroutine */ int knn1_(integer *ntr, integer *nte, integer *p, doublereal 
	*train, integer *class__, doublereal *test, integer *res, doublereal *
	d__)
{
    /* System generated locals */
    integer train_dim1, train_offset, test_dim1, test_offset, i__1, i__2, 
	    i__3;
    doublereal d__1;

    /* Local variables */
    static integer j, k;
    static real dm;
    static integer npat;
    static real dist;
    static integer index;

    /* Parameter adjustments */
    --d__;
    --res;
    --class__;
    test_dim1 = *nte;
    test_offset = 1 + test_dim1;
    test -= test_offset;
    train_dim1 = *ntr;
    train_offset = 1 + train_dim1;
    train -= train_offset;

    /* Function Body */
    i__1 = *nte;
    for (npat = 1; npat <= i__1; ++npat) {
	dm = 1e35f;
	i__2 = *ntr;
	for (j = 1; j <= i__2; ++j) {
	    dist = 0.f;
	    i__3 = *p;
	    for (k = 1; k <= i__3; ++k) {
/* L10: */
/* Computing 2nd power */
		d__1 = test[npat + k * test_dim1] - train[j + k * train_dim1];
		dist += d__1 * d__1;
	    }
	    if (dist < dm) {
		dm = dist;
		index = j;
	    }
/* L50: */
	}
	d__[npat] = dm;
/* L100: */
	res[npat] = class__[index];
    }
    return 0;
} /* knn1_ */

/* Subroutine */ int knn2_(integer *ntr, integer *nte, integer *p, doublereal 
	*train, integer *class__, doublereal *test, integer *res, doublereal *
	d__)
{
    /* System generated locals */
    integer train_dim1, train_offset, test_dim1, test_offset, i__1, i__2, 
	    i__3;
    doublereal d__1;

    /* Local variables */
    static integer j, k;
    static real dm, dm2;
    static integer npat;
    static real dist;
    static integer index, index2;

    /* Parameter adjustments */
    --d__;
    --res;
    --class__;
    test_dim1 = *nte;
    test_offset = 1 + test_dim1;
    test -= test_offset;
    train_dim1 = *ntr;
    train_offset = 1 + train_dim1;
    train -= train_offset;

    /* Function Body */
    i__1 = *nte;
    for (npat = 1; npat <= i__1; ++npat) {
	dm = 1e35f;
	dm2 = dm;
	index = 0;
	index2 = 0;
	i__2 = *ntr;
	for (j = 1; j <= i__2; ++j) {
	    dist = 0.f;
	    i__3 = *p;
	    for (k = 1; k <= i__3; ++k) {
/* L10: */
/* Computing 2nd power */
		d__1 = test[npat + k * test_dim1] - train[j + k * train_dim1];
		dist += d__1 * d__1;
	    }
	    if (dist < dm) {
		dm2 = dm;
		index2 = index;
		dm = dist;
		index = j;
	    } else if (dist < dm2) {
		dm2 = dist;
		index2 = j;
	    }
/* L50: */
	}
	d__[npat] = dm;
	if (class__[index] == class__[index2]) {
	    res[npat] = class__[index];
	} else {
	    res[npat] = 0;
	}
/* L100: */
    }
    return 0;
} /* knn2_ */

/* Subroutine */ int knn_(integer *kn, integer *ntr, integer *nte, integer *p,
	 doublereal *train, integer *class__, doublereal *test, integer *res, 
	real *u, doublereal *d__)
{
    /* System generated locals */
    integer train_dim1, train_offset, test_dim1, test_offset, i__1, i__2, 
	    i__3, i__4;
    doublereal d__1;

    /* Local variables */
    static integer j, k, l, k1, mm, ntie, npat;
    static doublereal dist;
    /* These were fixed-size stack arrays -- pos[50], votes[51], nndist[100] --
       while k was validated only against the number of training rows and the
       class codes only by convention. k > 50 wrote past pos, giving wrong
       neighbours for k >= 101 and a crash from k = 128. Sized from the actual
       arguments instead. R_alloc memory is released when knn_ returns to R. */
    integer *pos, *votes, nclass;
    real *nndist;

    /* Parameter adjustments */
    --u;
    --d__;
    --res;
    --class__;
    test_dim1 = *nte;
    test_offset = 1 + test_dim1;
    test -= test_offset;
    train_dim1 = *ntr;
    train_offset = 1 + train_dim1;
    train -= train_offset;

    /* Function Body */
    nclass = 0;
    for (j = 1; j <= *ntr; ++j) {
	if (class__[j] > nclass) {
	    nclass = class__[j];
	}
    }
    if (nclass < 1) {
	nclass = 1;
    }
    pos = (integer *) R_alloc((size_t) *kn, sizeof(integer));
    nndist = (real *) R_alloc((size_t) *kn, sizeof(real));
    votes = (integer *) R_alloc((size_t) (nclass + 1), sizeof(integer));

    i__1 = *nte;
    for (npat = 1; npat <= i__1; ++npat) {
	i__2 = *ntr;
	for (j = 1; j <= i__2; ++j) {
	    dist = 0.f;
	    i__3 = *p;
	    for (k = 1; k <= i__3; ++k) {
/* L10: */
/* Computing 2nd power */
		d__1 = test[npat + k * test_dim1] - train[j + k * train_dim1];
		dist += d__1 * d__1;
	    }
	    if (j <= *kn) {
		i__3 = j - 1;
		for (k = 1; k <= i__3; ++k) {
		    if (dist < nndist[k - 1]) {
			i__4 = k;
			for (k1 = j - 1; k1 >= i__4; --k1) {
			    nndist[k1] = nndist[k1 - 1];
/* L20: */
			    pos[k1] = pos[k1 - 1];
			}
			nndist[k - 1] = dist;
			pos[k - 1] = j;
			goto L50;
		    }
/* L30: */
		}
		nndist[j - 1] = dist;
		pos[j - 1] = j;
	    } else {
		if (dist >= nndist[*kn - 1]) {
		    goto L50;
		}
		i__3 = *kn;
		for (k = 1; k <= i__3; ++k) {
		    if (dist < nndist[k - 1]) {
			i__4 = k;
			for (k1 = *kn - 1; k1 >= i__4; --k1) {
			    nndist[k1] = nndist[k1 - 1];
/* L35: */
			    pos[k1] = pos[k1 - 1];
			}
			nndist[k - 1] = dist;
			pos[k - 1] = j;
			goto L50;
		    }
/* L40: */
		}
	    }
L50:
	    ;
	}
	for (j = 0; j <= nclass; ++j) {
/* L60: */
	    votes[j] = 0;
	}
	ntie = 1;
	i__2 = *kn;
	for (j = 1; j <= i__2; ++j) {
/* L70: */
	    ++votes[class__[pos[j - 1]]];
	}
	mm = votes[0];
	l = 0;
	for (j = 1; j <= nclass; ++j) {
	    if (votes[j] > mm) {
		mm = votes[j];
		l = j;
	    } else if (votes[j] == mm) {
		++ntie;
		if (ntie * u[npat] < 1.f) {
		    mm = votes[j];
		    l = j;
		    u[npat] = ntie * u[npat];
		} else {
		    u[npat] = (ntie * u[npat] - 1) / (ntie - 1);
		}
	    }
/* L80: */
	}
	d__[npat] = nndist[*kn - 1];
/* L100: */
	res[npat] = l;
    }
    return 0;
} /* knn_ */

/* Subroutine */ int olvq_(real *alpha, integer *n, integer *p, doublereal *x,
	 integer *class__, integer *nc, doublereal *xc, integer *clc, integer 
	*niter, integer *iters)
{
    /* System generated locals */
    integer x_dim1, x_offset, xc_dim1, xc_offset, i__1, i__2, i__3;
    real r__1, r__2;
    doublereal d__1;

    /* Local variables */
    static integer i__, j, k;
    static real s, al[500], dm;
    static integer npat, iter;
    static real dist;
    static integer index;

    /* Parameter adjustments */
    --class__;
    x_dim1 = *n;
    x_offset = 1 + x_dim1;
    x -= x_offset;
    --clc;
    xc_dim1 = *nc;
    xc_offset = 1 + xc_dim1;
    xc -= xc_offset;
    --iters;

    /* Function Body */
    i__1 = *nc;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L10: */
	al[i__ - 1] = *alpha;
    }
    i__1 = *niter;
    for (iter = 1; iter <= i__1; ++iter) {
	npat = iters[iter];
	dm = 1e35f;
	i__2 = *nc;
	for (j = 1; j <= i__2; ++j) {
	    dist = 0.f;
	    i__3 = *p;
	    for (k = 1; k <= i__3; ++k) {
/* L20: */
/* Computing 2nd power */
		d__1 = x[npat + k * x_dim1] - xc[j + k * xc_dim1];
		dist += d__1 * d__1;
	    }
	    if (dist < dm) {
		dm = dist;
		index = j;
	    }
/* L30: */
	}
	if (clc[index] == class__[npat]) {
	    s = 1.f;
	} else {
	    s = -1.f;
	}
	i__2 = *p;
	for (k = 1; k <= i__2; ++k) {
/* L40: */
	    xc[index + k * xc_dim1] += s * al[index - 1] * (x[npat + k * 
		    x_dim1] - xc[index + k * xc_dim1]);
	}
/* Computing MIN */
	r__1 = *alpha, r__2 = al[index - 1] / (s * al[index - 1] + 1);
	al[index - 1] = dmin(r__1,r__2);
/* L100: */
    }
    return 0;
} /* olvq_ */

/* Subroutine */ int lvq1_(real *alpha, integer *n, integer *p, doublereal *x,
	 integer *class__, integer *nc, doublereal *xc, integer *clc, integer 
	*niter, integer *iters)
{
    /* System generated locals */
    integer x_dim1, x_offset, xc_dim1, xc_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Local variables */
    static integer j, k;
    static real s, dm;
    static integer npat, iter;
    static real dist;
    static integer index;
    static real talpha;

    /* Parameter adjustments */
    --class__;
    x_dim1 = *n;
    x_offset = 1 + x_dim1;
    x -= x_offset;
    --clc;
    xc_dim1 = *nc;
    xc_offset = 1 + xc_dim1;
    xc -= xc_offset;
    --iters;

    /* Function Body */
    i__1 = *niter;
    for (iter = 1; iter <= i__1; ++iter) {
	npat = iters[iter];
	talpha = *alpha * iter / (real) (*niter);
	dm = 1e35f;
	i__2 = *nc;
	for (j = 1; j <= i__2; ++j) {
	    dist = 0.f;
	    i__3 = *p;
	    for (k = 1; k <= i__3; ++k) {
/* L20: */
/* Computing 2nd power */
		d__1 = x[npat + k * x_dim1] - xc[j + k * xc_dim1];
		dist += d__1 * d__1;
	    }
	    if (dist < dm) {
		dm = dist;
		index = j;
	    }
/* L30: */
	}
	if (clc[index] == class__[npat]) {
	    s = 1.f;
	} else {
	    s = -1.f;
	}
	i__2 = *p;
	for (k = 1; k <= i__2; ++k) {
/* L40: */
	    xc[index + k * xc_dim1] += s * talpha * (x[npat + k * x_dim1] - 
		    xc[index + k * xc_dim1]);
	}
/* L100: */
    }
    return 0;
} /* lvq1_ */

/* Subroutine */ int lvq2_(real *alpha, real *win, integer *n, integer *p, 
	doublereal *x, integer *class__, integer *nc, doublereal *xc, integer 
	*clc, integer *niter, integer *iters)
{
    /* System generated locals */
    integer x_dim1, x_offset, xc_dim1, xc_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Local variables */
    static integer j, k;
    static real dm;
    static integer ndm, npat, iter;
    static real dist;
    static integer ntmp, index;
    static real talpha;
    static integer nindex;

    /* Parameter adjustments */
    --class__;
    x_dim1 = *n;
    x_offset = 1 + x_dim1;
    x -= x_offset;
    --clc;
    xc_dim1 = *nc;
    xc_offset = 1 + xc_dim1;
    xc -= xc_offset;
    --iters;

    /* Function Body */
    i__1 = *niter;
    for (iter = 1; iter <= i__1; ++iter) {
	npat = iters[iter];
	talpha = *alpha * iter / (real) (*niter);
	dm = 1e35f;
	ndm = dm;

/*  find two nearest codebook vectors */

	i__2 = *nc;
	for (j = 1; j <= i__2; ++j) {
	    dist = 0.f;
	    i__3 = *p;
	    for (k = 1; k <= i__3; ++k) {
/* L20: */
/* Computing 2nd power */
		d__1 = x[npat + k * x_dim1] - xc[j + k * xc_dim1];
		dist += d__1 * d__1;
	    }
	    if (dist < dm) {
		ndm = dm;
		nindex = index;
		dm = dist;
		index = j;
	    } else if (dist < (real) ndm) {
		ndm = dist;
		nindex = j;
	    }
/* L30: */
	}
	if (clc[index] != clc[nindex] && dm / ndm > (1 - *win) / (*win + 1)) {
	    if (clc[index] == class__[npat] || clc[nindex] == class__[npat]) {
		if (clc[nindex] == class__[npat]) {
		    ntmp = index;
		    index = nindex;
		    nindex = ntmp;
		}
		i__2 = *p;
		for (k = 1; k <= i__2; ++k) {
		    xc[index + k * xc_dim1] += talpha * (x[npat + k * x_dim1] 
			    - xc[index + k * xc_dim1]);
/* L40: */
		    xc[nindex + k * xc_dim1] -= talpha * (x[npat + k * x_dim1]
			     - xc[nindex + k * xc_dim1]);
		}
	    }
	}
/* L100: */
    }
    return 0;
} /* lvq2_ */

/* Subroutine */ int lvq3_(real *alpha, real *win, real *epsilon, integer *n, 
	integer *p, doublereal *x, integer *class__, integer *nc, doublereal *
	xc, integer *clc, integer *niter, integer *iters)
{
    /* System generated locals */
    integer x_dim1, x_offset, xc_dim1, xc_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Local variables */
    static integer j, k;
    static real dm;
    static integer ndm, npat, iter;
    static real dist;
    static integer ntmp, index;
    static real talpha;
    static integer nindex;

    /* Parameter adjustments */
    --class__;
    x_dim1 = *n;
    x_offset = 1 + x_dim1;
    x -= x_offset;
    --clc;
    xc_dim1 = *nc;
    xc_offset = 1 + xc_dim1;
    xc -= xc_offset;
    --iters;

    /* Function Body */
    i__1 = *niter;
    for (iter = 1; iter <= i__1; ++iter) {
	npat = iters[iter];
	talpha = *alpha * iter / (real) (*niter);
	dm = 1e35f;
	ndm = dm;

/*  find two nearest codebook vectors */

	i__2 = *nc;
	for (j = 1; j <= i__2; ++j) {
	    dist = 0.f;
	    i__3 = *p;
	    for (k = 1; k <= i__3; ++k) {
/* L20: */
/* Computing 2nd power */
		d__1 = x[npat + k * x_dim1] - xc[j + k * xc_dim1];
		dist += d__1 * d__1;
	    }
	    if (dist < dm) {
		ndm = dm;
		nindex = index;
		dm = dist;
		index = j;
	    } else if (dist < (real) ndm) {
		ndm = dist;
		nindex = j;
	    }
/* L30: */
	}
	if (clc[index] != clc[nindex]) {
	    if (dm / ndm > (1 - *win) / (*win + 1)) {
		if (clc[index] == class__[npat] || clc[nindex] == class__[
			npat]) {
		    if (clc[nindex] == class__[npat]) {
			ntmp = index;
			index = nindex;
			nindex = ntmp;
		    }
		    i__2 = *p;
		    for (k = 1; k <= i__2; ++k) {
			xc[index + k * xc_dim1] += talpha * (x[npat + k * 
				x_dim1] - xc[index + k * xc_dim1]);
/* L40: */
			xc[nindex + k * xc_dim1] -= talpha * (x[npat + k * 
				x_dim1] - xc[nindex + k * xc_dim1]);
		    }
		}
	    }
	} else {
	    if (clc[nindex] == class__[npat]) {
		i__2 = *p;
		for (k = 1; k <= i__2; ++k) {
		    xc[index + k * xc_dim1] += *epsilon * talpha * (x[npat + 
			    k * x_dim1] - xc[index + k * xc_dim1]);
/* L50: */
		    xc[nindex + k * xc_dim1] += *epsilon * talpha * (x[npat + 
			    k * x_dim1] - xc[nindex + k * xc_dim1]);
		}
	    }
	}
/* L100: */
    }
    return 0;
} /* lvq3_ */

