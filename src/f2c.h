/* Minimal f2c.h for R package compatibility */

#ifndef F2C_INCLUDE
#define F2C_INCLUDE

typedef int integer;
typedef double doublereal;
typedef float real;
typedef int logical;
typedef int ftnlen;

/* Macros */
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define dmin(a,b) ((a) <= (b) ? (a) : (b))
#define dmax(a,b) ((a) >= (b) ? (a) : (b))

#endif
