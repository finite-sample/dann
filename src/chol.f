      subroutine chol(a,n,scratch,i1,i2,irank)
c
c     Upper triangular Cholesky factorization: A = R'R
c     On output, upper triangle of A contains R
c
c     on input
c
c        a      contains the symmetric positive definite matrix
c               only upper triangle is used
c
c        n      is the order of the matrix
c
c        scratch  workspace array (not used, kept for interface compatibility)
c
c        i1, i2   integer flags (not used, kept for interface compatibility)
c
c     on output
c
c        a      upper triangle contains the Cholesky factor R
c
c        irank  the rank of the matrix (n if successful, j-1 if
c               the matrix is not positive definite at column j)
c
      implicit double precision (a-h,o-z)
      integer n,i1,i2,irank
      double precision a(n,n),scratch(*)

      irank = n
      do 30 j = 1, n
         sum = a(j,j)
         if (j .gt. 1) then
            do 10 k = 1, j-1
               sum = sum - a(k,j)**2
   10       continue
         endif
         if (sum .le. 0.0d0) then
            irank = j - 1
            return
         endif
         a(j,j) = dsqrt(sum)
         if (j .lt. n) then
            do 25 i = j+1, n
               sum = a(j,i)
               if (j .gt. 1) then
                  do 20 k = 1, j-1
                     sum = sum - a(k,j)*a(k,i)
   20             continue
               endif
               a(j,i) = sum / a(j,j)
   25       continue
         endif
   30 continue
      return
      end
