      module timctr 
! ______________________________________________________________________
!     timctr : control time during execution
!
!     author  : Michel Olagnon 
!     date    : 5/4/93 
! ______________________________________________________________________
!
      integer, save              :: jhhh0, jmmm0 ! start time
      real, save                 :: xsss0
      integer, save              :: jhhh, jmmm   ! current time
      real, save                 :: xsss
      character (len = 10), save :: ztim
      character (len = 10), parameter :: zfmt = '(2i2,f6.3)'
!
      contains
         subroutine initim
         call date_and_time (time = ztim)
         read (ztim, zfmt) jhhh0, jmmm0, xsss0
         return
         end subroutine initim
!
         subroutine gettim (jheu, jmin, xsec)
         integer, intent (out) :: jheu, jmin     ! elapsed time
         real, intent (out)    :: xsec
         call date_and_time (time = ztim)
         read (ztim, zfmt) jhhh, jmmm, xsss
         if (xsss < xsss0) then
            xsss = xsss + 60.0
            jmmm = jmmm - 1
         endif
         do
            if (jmmm >= jmmm0 ) exit
            jmmm = jmmm + 60
            jhhh = jhhh - 1
         enddo
         do
            if (jhhh >= jhhh0) exit
            jhhh = jhhh + 24
         enddo
         jheu = jhhh - jhhh0
         jmin = jmmm - jmmm0
         xsec = xsss - xsss0
         return
         end subroutine gettim
! ______________________________________________________________________
      end module timctr
      subroutine cmpcpt (xxtrt, ixtr1t, ixtr2t, nxtr, kdif)
! ______________________________________________________________________
!     cmpcpt  : compare deux resultats de comptage.
!
!     auteur  : Michel Olagnon
!     mouture : 1.2 
!     date    : 2/12/93 
! ______________________________________________________________________
      real, dimension (1:nxtr), intent (in)    :: xxtrt ! extrema
      integer, dimension (1:nxtr), intent (in) :: ixtr1t, ixtr2t 
!                                                         indices de fin
      integer, intent (in)                  :: nxtr     ! leur nombre
      integer, intent (out)                 :: kdif     ! code resultat
! ______________________________________________________________________
!
      integer, dimension (:), allocatable :: ideb1t, ideb2t 
!                                                         indices debut
      kdif = 0
      allocate (ideb1t (nxtr))
      allocate (ideb2t (nxtr))
!
      do ixtr = 1, nxtr
         ixtr1 = ixtr1t (ixtr)
         ixtr2 = ixtr2t (ixtr)
         if (ixtr1 /= 0) then
            ideb1t (ixtr1) = ixtr
         endif
         if (ixtr2 /= 0) then
            ideb2t (ixtr2) = ixtr
         endif
      enddo
!
! .... pour chaque pic, on va chercher la vallee correspondante
!
      do ixtr = 1, nxtr
!
         ixtr1 = ixtr1t (ixtr)
         ixtr2 = ixtr2t (ixtr)
         if (ixtr1 == ixtr2) cycle
         if (ixtr1 == 0 .or. ixtr2 == 0) then
            kdif = -ixtr
            exit
         endif
         if (xxtrt (ixtr1) /= xxtrt (ixtr2)) then
            if (xxtrt (ideb2t (ixtr1)) /= xxtrt (ideb1t (ixtr2))) then
               kdif = ixtr
               exit
            endif
         endif
      enddo
!
 9000 continue
      deallocate (ideb1t)
      deallocate (ideb2t)
      return
      end subroutine cmpcpt
      SUBROUTINE DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,&
     &                   BETA, C, LDC )
!     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
!     ..
!
!  Purpose
!  =======
!
!  DGEMM  performs one of the matrix-matrix operations
!
!     C := alpha*op( A )*op( B ) + beta*C,
!
!  where  op( X ) is one of
!
!     op( X ) = X   or   op( X ) = X',
!
!  alpha and beta are scalars, and A, B and C are matrices, with op( A )
!  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
!
!  Parameters
!  ==========
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n',  op( A ) = A.
!
!              TRANSA = 'T' or 't',  op( A ) = A'.
!
!              TRANSA = 'C' or 'c',  op( A ) = A'.
!
!           Unchanged on exit.
!
!  TRANSB - CHARACTER*1.
!           On entry, TRANSB specifies the form of op( B ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSB = 'N' or 'n',  op( B ) = B.
!
!              TRANSB = 'T' or 't',  op( B ) = B'.
!
!              TRANSB = 'C' or 'c',  op( B ) = B'.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry,  M  specifies  the number  of rows  of the  matrix
!           op( A )  and of the  matrix  C.  M  must  be at least  zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry,  N  specifies the number  of columns of the matrix
!           op( B ) and the number of columns of the matrix C. N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - INTEGER.
!           On entry,  K  specifies  the number of columns of the matrix
!           op( A ) and the number of rows of the matrix op( B ). K must
!           be at least  zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
!           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by m  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
!           LDA must be at least  max( 1, m ), otherwise  LDA must be at
!           least  max( 1, k ).
!           Unchanged on exit.
!
!  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
!           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
!           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  n by k  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
!           LDB must be at least  max( 1, k ), otherwise  LDB must be at
!           least  max( 1, n ).
!           Unchanged on exit.
!
!  BETA   - DOUBLE PRECISION.
!           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.
!           Unchanged on exit.
!
!  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
!           Before entry, the leading  m by n  part of the array  C must
!           contain the matrix  C,  except when  beta  is zero, in which
!           case C need not be set on entry.
!           On exit, the array  C  is overwritten by the  m by n  matrix
!           ( alpha*op( A )*op( B ) + beta*C ).
!
!  LDC    - INTEGER.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     .. External Subroutines ..
      EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      DOUBLE PRECISION   TEMP
!     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Executable Statements ..
!
!     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
!     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
!     and  columns of  A  and the  number of  rows  of  B  respectively.
!
      NOTA  = LSAME( TRANSA, 'N' )
      NOTB  = LSAME( TRANSB, 'N' )
      IF( NOTA )THEN
         NROWA = M
         NCOLA = K
      ELSE
         NROWA = K
         NCOLA = M
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
!
!     Test the input parameters.
!
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.                       &
     &         ( .NOT.LSAME( TRANSA, 'C' ) ).AND.                       &
     &         ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.                       &
     &         ( .NOT.LSAME( TRANSB, 'C' ) ).AND.                       &
     &         ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMM ', INFO )
         RETURN
      END IF
!
!     Quick return if possible.
!
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.                                  &
     &    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) ) &
     &   RETURN
!
!     And if  alpha.eq.zero.
!
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
!
!     Start the operations.
!
      IF( NOTB )THEN
         IF( NOTA )THEN
!
!           Form  C := alpha*A*B + beta*C.
!
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE
!
!           Form  C := alpha*A'*B + beta*C
!
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         END IF
      ELSE
         IF( NOTA )THEN
!
!           Form  C := alpha*A*B' + beta*C
!
            DO 170, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 130, I = 1, M
                     C( I, J ) = ZERO
  130             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 140, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  140             CONTINUE
               END IF
               DO 160, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 150, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  150                CONTINUE
                  END IF
  160          CONTINUE
  170       CONTINUE
         ELSE
!
!           Form  C := alpha*A'*B' + beta*C
!
            DO 200, J = 1, N
               DO 190, I = 1, M
                  TEMP = ZERO
                  DO 180, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  180             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  190          CONTINUE
  200       CONTINUE
         END IF
      END IF
!
      RETURN
!
!     End of DGEMM .
!
      END
      SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
!
!  -- LAPACK routine (version 1.1) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     March 31, 1993
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DGETRF computes an LU factorization of a general M-by-N matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 3 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the M-by-N matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <= i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
!                has been completed, but the factor U is exactly
!                singular, and division by zero will occur if it is used
!                to solve a system of equations.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, IINFO, J, JB, NB
!     ..
!     .. External Subroutines ..
      EXTERNAL           DGEMM, DGETF2, DLASWP, DTRSM, XERBLA
!     ..
!     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETRF', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 )                                          &
     &   RETURN
!
!     Determine the block size for this environment.
!
      NB = ILAENV( 1, 'DGETRF', ' ', M, N, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.MIN( M, N ) ) THEN
!
!        Use unblocked code.
!
         CALL DGETF2( M, N, A, LDA, IPIV, INFO )
      ELSE
!
!        Use blocked code.
!
         DO 20 J = 1, MIN( M, N ), NB
            JB = MIN( MIN( M, N )-J+1, NB )
!
!           Factor diagonal and subdiagonal blocks and test for exact
!           singularity.
!
            CALL DGETF2( M-J+1, JB, A( J, J ), LDA, IPIV( J ), IINFO )
!
!           Adjust INFO and the pivot indices.
!
            IF( INFO.EQ.0 .AND. IINFO.GT.0 )                            &
     &         INFO = IINFO + J - 1
            DO 10 I = J, MIN( M, J+JB-1 )
               IPIV( I ) = J - 1 + IPIV( I )
   10       CONTINUE
!
!           Apply interchanges to columns 1:J-1.
!
            CALL DLASWP( J-1, A, LDA, J, J+JB-1, IPIV, 1 )
!
            IF( J+JB.LE.N ) THEN
!
!              Apply interchanges to columns J+JB:N.
!
               CALL DLASWP( N-J-JB+1, A( 1, J+JB ), LDA, J, J+JB-1,     &
     &                      IPIV, 1 )
!
!              Compute block row of U.
!
               CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Unit', JB, &
     &                     N-J-JB+1, ONE, A( J, J ), LDA, A( J, J+JB ), &
     &                     LDA )
               IF( J+JB.LE.M ) THEN
!
!                 Update trailing submatrix.
!
                  CALL DGEMM( 'No transpose', 'No transpose', M-J-JB+1, &
     &                        N-J-JB+1, JB, -ONE, A( J+JB, J ), LDA,    &
     &                        A( J, J+JB ), LDA, ONE, A( J+JB, J+JB ),  &
     &                        LDA )
               END IF
            END IF
   20    CONTINUE
      END IF
      RETURN
!
!     End of DGETRF
!
      END
      subroutine  dswap (n,dx,incx,dy,incy)
!
!     interchanges two vectors.
!     uses unrolled loops for increments equal one.
!     jack dongarra, linpack, 3/11/78.
!
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
!
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!       code for unequal increments or equal increments not equal
!         to 1
!
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
!
!       code for both increments equal to 1
!
!
!       clean-up loop
!
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
   50 continue
      return
      end
      SUBROUTINE DTRTI2( UPLO, DIAG, N, A, LDA, INFO )
!
!  -- LAPACK routine (version 1.1) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992
!
!     .. Scalar Arguments ..
      CHARACTER          DIAG, UPLO
      INTEGER            INFO, LDA, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DTRTI2 computes the inverse of a real upper or lower triangular
!  matrix.
!
!  This is the Level 2 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          Specifies whether the matrix A is upper or lower triangular.
!          = 'U':  Upper triangular
!          = 'L':  Lower triangular
!
!  DIAG    (input) CHARACTER*1
!          Specifies whether or not the matrix A is unit triangular.
!          = 'N':  Non-unit triangular
!          = 'U':  Unit triangular
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the triangular matrix A.  If UPLO = 'U', the
!          leading n by n upper triangular part of the array A contains
!          the upper triangular matrix, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading n by n lower triangular part of the array A contains
!          the lower triangular matrix, and the strictly upper
!          triangular part of A is not referenced.  If DIAG = 'U', the
!          diagonal elements of A are also not referenced and are
!          assumed to be 1.
!
!          On exit, the (triangular) inverse of the original matrix, in
!          the same storage format.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            NOUNIT, UPPER
      INTEGER            J
      DOUBLE PRECISION   AJJ
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           DSCAL, DTRMV, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOUNIT = LSAME( DIAG, 'N' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DTRTI2', -INFO )
         RETURN
      END IF
!
      IF( UPPER ) THEN
!
!        Compute inverse of upper triangular matrix.
!
         DO 10 J = 1, N
            IF( NOUNIT ) THEN
               A( J, J ) = ONE / A( J, J )
               AJJ = -A( J, J )
            ELSE
               AJJ = -ONE
            END IF
!
!           Compute elements 1:j-1 of j-th column.
!
            CALL DTRMV( 'Upper', 'No transpose', DIAG, J-1, A, LDA,     &
     &                  A( 1, J ), 1 )
            CALL DSCAL( J-1, AJJ, A( 1, J ), 1 )
   10    CONTINUE
      ELSE
!
!        Compute inverse of lower triangular matrix.
!
         DO 20 J = N, 1, -1
            IF( NOUNIT ) THEN
               A( J, J ) = ONE / A( J, J )
               AJJ = -A( J, J )
            ELSE
               AJJ = -ONE
            END IF
            IF( J.LT.N ) THEN
!
!              Compute elements j+1:n of j-th column.
!
               CALL DTRMV( 'Lower', 'No transpose', DIAG, N-J,          &
     &                     A( J+1, J+1 ), LDA, A( J+1, J ), 1 )
               CALL DSCAL( N-J, AJJ, A( J+1, J ), 1 )
            END IF
   20    CONTINUE
      END IF
!
      RETURN
!
!     End of DTRTI2
!
      END
!CRAY - The following compiler directive assures that the multiply
!       in the internal function is done with 64 bits on the CRAY
!       See below for more details
!DIR$ INTEGER=64
      subroutine gentrs (ptrst, ncls, xmin, dcls, xdont, ndon) 
! ______________________________________________________________________
!     gentrs  : Genere des donnees a partir d'une matrice de proba
!               de transition.
!
!     auteur  : Michel Olagnon
!     mouture : 1.1 
!     date    : 4/30/93 
! ______________________________________________________________________
      real, dimension (1:ncls,1:ncls), intent (in) :: ptrst
!                                                probas transition
      integer, intent (in)            :: ncls  ! nombre de classes
      real, intent (in)               :: xmin  ! valeur min
      real, intent (in)               :: dcls  ! largeur classes
      real, dimension (1:ndon), intent (out)       :: xdont
!                                                donnees generees
      integer, intent (in)            :: ndon  ! nombre de donnees
! ______________________________________________________________________
!
      integer, save :: jgrm = 1993 ! germe, impair, a laisser evoluer.
      real, allocatable, dimension (:,:) :: rtrst ! repartition
      real, allocatable, dimension (:)   :: rpict ! repartition pics
!
! ... construction de la matrice de repartition
!
      allocate (rtrst (1:ncls,1:ncls))
      rtrst = 1.0
      do icls1 = 1, ncls
         do icls2 = icls1 - 1, 1, -1
            rtrst (icls1, icls2) = rtrst (icls1, icls2 + 1) -           &
     &                             ptrst (icls1, icls2)
         enddo
         do icls2 = icls1 + 1, ncls
            rtrst (icls1, icls2) = rtrst (icls1, icls2 - 1) -           &
     &                             ptrst (icls1, icls2)
         enddo
      enddo
!
! ... construction de la repartition des pics
!
      allocate (rpict (1:ncls))
      do icls = 2, ncls
         rpict (icls) = ptrst (icls, icls)
      enddo
      rpict (1) = 0.0
      spic = sum (rpict (1:ncls))
      rpic = 1.0
      do icls = ncls, 1, -1
         rpic = rpic - rpict (icls) / spic
         rpict (icls) = rpic
      enddo
!
! ... determination de la valeur de depart (un pic)
!
      dcls2 = dcls * 0.5
      xale = genuni (jgrm)
      icls1 = 1
      do icls = ncls, 1, -1
         if (xale > rpict (icls)) then
            icls1 = icls
            exit
         endif
      enddo
      xdont (1) = xmin + real (2 * icls1 - 1) * dcls2
!
! ... avance markovienne
!
      isns = -1
      do idon = 2, ndon
!
! ...... boucle redondante pour les pb de precision
!
prec:    do
            xale = genuni (jgrm)
            select case (isns)
            case (-1)
               do icls = icls1, 1, -1
                  if (xale > rtrst (icls1, icls)) then
                     icls1 = icls
                     exit prec
                  endif
               enddo
            case (+1)
               do icls = icls1, ncls
                  if (xale > rtrst (icls1, icls)) then
                     icls1 = icls
                     exit prec
                  endif
               enddo
            end select
         enddo prec
         xdont (idon) = xmin + real (2 * icls1 - 1) * dcls2
         isns = - isns
      enddo
!
      deallocate (rtrst)
      deallocate (rpict)
      return
! ______________________________________________________________________
      contains
      real function genuni (jsee)
!     generation maison d'une valeur pseudo-aleatoire uniforme sur (0,1)
!
      integer, intent (inout) :: jsee               ! germe courant
      integer, parameter   :: jmul =  843314861     ! multiplicateur
      integer, parameter   :: jadd =  453816693     ! constante additive
      integer, parameter   :: j230 = 1073741824     ! 2 puissance 30
      real, parameter      :: us231 = 0.46566129e-9 ! 1 / 2**31
!
! ...... on genere la variable uniformement repartie sur (0,1)
! ...... par un algorithme lineaire congruentiel modulo 2 ** 31
! ...... (suppose que le debordement entier perd simplement les
! ......  bits au dela du 32ieme, et on ajoute deux fois 2 ** 30
! ......  s'il faut effacer le bit de signe, car on ne peut ajouter
! ......  2 ** 31, non representable)
!
!CRAY - The following multiply must be done with 64 bits (not 46 bits)
!       The algoritm depends on the overflow characteristics of 
!       a 32 or 64 bit multiply.
      jsee = jsee * jmul + jadd
!CRAY - Change to avoid 32 bit integer dependency
      !
      !  The original line is needlessly dependent on the
      !  bit_size being 32.  Using ibits instead will work
      !  for sizes 32 and larger.
      !
      ! if (jsee < 0) jsee = (jsee + j230) + j230 ! Original
      !
      jsee = ibits(jsee, 0, 31)                   ! Replacement
      genuni = us231 * real (jsee)
      end function genuni
! ______________________________________________________________________
      end subroutine gentrs
      subroutine reaseq (xseqt, nseqm, nseq) 
! ______________________________________________________________________
!     Read Load sequence (from standard input)
!
!     author : Michel Olagnon
!     date   : 4 May 93
! ______________________________________________________________________
      include 'rnfprm.h'
! ______________________________________________________________________
      real, dimension (1:nseqm), intent (out) :: xseqt
      integer, intent (in)  :: nseqm
      integer, intent (out) :: nseq
!
      nseq = 0
      do iseq = 1, nseqm
         read (luinp, 99999, iostat = ksta) jseq
         if (ksta == 0) then
            nseq = nseq + 1
            xseqt (nseq) = real (jseq) - 0.5
         else
            exit
         endif
      enddo
!
      return
! ______________________________________________________________________
99999 format (bz, i2)
      end subroutine reaseq
      subroutine cmpmat (mrnf1t, mrnf2t)
! ______________________________________________________________________
!     cmpmat  : Comparaison des matrices rainflow (64x64)
!
!     auteur  : Michel Olagnon
!     date    : 3/6/93 
! ______________________________________________________________________
      include 'rnfprm.h'
! ______________________________________________________________________
!
      integer, dimension (1:ncls,1:ncls), intent (in) :: mrnf1t, mrnf2t
!
      do icls1 = 1, ncls
         do icls2 = icls1, ncls
            mrnf1 = mrnf1t (icls1, icls2)
            mrnf2 = mrnf2t (icls1, icls2)
            jdif  = abs (mrnf1 - mrnf2)
            if (jdif < nsim) cycle
            pdif = (100.0 * real (jdif)) / real (max (mrnf1, mrnf2))
            pdif = real (nint (100.0 * pdif)) / 100.0
            write (luout, *) icls1, ',', icls2, ' ', mrnf1, mrnf2, pdif
         end do
      end do
!
      return
! ______________________________________________________________________
      end subroutine cmpmat
      SUBROUTINE DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,           &
     &                   BETA, Y, INCY )
!     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
!     ..
!
!  Purpose
!  =======
!
!  DGEMV  performs one of the matrix-vector operations
!
!     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
!
!  where alpha and beta are scalars, x and y are vectors and A is an
!  m by n matrix.
!
!  Parameters
!  ==========
!
!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
!
!              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
!
!              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!  X      - DOUBLE PRECISION array of DIMENSION at least
!           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
!           Before entry, the incremented array X must contain the
!           vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  BETA   - DOUBLE PRECISION.
!           On entry, BETA specifies the scalar beta. When BETA is
!           supplied as zero then Y need not be set on input.
!           Unchanged on exit.
!
!  Y      - DOUBLE PRECISION array of DIMENSION at least
!           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
!           Before entry with BETA non-zero, the incremented array Y
!           must contain the vector y. On exit, Y is overwritten by the
!           updated vector y.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     .. External Subroutines ..
      EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.                            &
     &         .NOT.LSAME( TRANS, 'T' ).AND.                            &
     &         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMV ', INFO )
         RETURN
      END IF
!
!     Quick return if possible.
!
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.                                  &
     &    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )                   &
     &   RETURN
!
!     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
!     up the start points in  X  and  Y.
!
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
!     First form  y := beta*y.
!
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )                                               &
     &   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
!
!        Form  y := alpha*A*x + y.
!
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
!
!        Form  y := alpha*A'*x + y.
!
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
!
      RETURN
!
!     End of DGEMV .
!
      END
      SUBROUTINE DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
!
!  -- LAPACK routine (version 1.1) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     March 31, 1993
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, N
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), WORK( LWORK )
!     ..
!
!  Purpose
!  =======
!
!  DGETRI computes the inverse of a matrix using the LU factorization
!  computed by DGETRF.
!
!  This method inverts U and then computes inv(A) by solving the system
!  inv(A)*L = inv(U) for inv(A).
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the factors L and U from the factorization
!          A = P*L*U as computed by DGETRF.
!          On exit, if INFO = 0, the inverse of the original matrix A.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  IPIV    (input) INTEGER array, dimension (N)
!          The pivot indices from DGETRF; for 1<=i<=N, row i of the
!          matrix was interchanged with row IPIV(i).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
!          On exit, if INFO=0, then WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK.  LWORK >= max(1,N).
!          For optimal performance LWORK >= N*NB, where NB is
!          the optimal blocksize returned by ILAENV.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  if INFO = i, U(i,i) is exactly zero; the matrix is
!                singular and its inverse could not be computed.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, IWS, J, JB, JJ, JP, LDWORK, NB, NBMIN, NN
!     ..
!     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
!     ..
!     .. External Subroutines ..
      EXTERNAL           DGEMM, DGEMV, DSWAP, DTRSM, DTRTRI, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      WORK( 1 ) = MAX( N, 1 )
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -3
      ELSE IF( LWORK.LT.MAX( 1, N ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETRI', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.EQ.0 )                                                      &
     &   RETURN
!
!     Form inv(U).  If INFO > 0 from DTRTRI, then U is singular,
!     and the inverse is not computed.
!
      CALL DTRTRI( 'Upper', 'Non-unit', N, A, LDA, INFO )
      IF( INFO.GT.0 )                                                   &
     &   RETURN
!
!     Determine the block size for this environment.
!
      NB = ILAENV( 1, 'DGETRI', ' ', N, -1, -1, -1 )
      NBMIN = 2
      LDWORK = N
      IF( NB.GT.1 .AND. NB.LT.N ) THEN
         IWS = MAX( LDWORK*NB, 1 )
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'DGETRI', ' ', N, -1, -1, -1 ) )
         END IF
      ELSE
         IWS = N
      END IF
!
!     Solve the equation inv(A)*L = inv(U) for inv(A).
!
      IF( NB.LT.NBMIN .OR. NB.GE.N ) THEN
!
!        Use unblocked code.
!
         DO 20 J = N, 1, -1
!
!           Copy current column of L to WORK and replace with zeros.
!
            DO 10 I = J + 1, N
               WORK( I ) = A( I, J )
               A( I, J ) = ZERO
   10       CONTINUE
!
!           Compute current column of inv(A).
!
            IF( J.LT.N )                                                &
     &         CALL DGEMV( 'No transpose', N, N-J, -ONE, A( 1, J+1 ),   &
     &                     LDA, WORK( J+1 ), 1, ONE, A( 1, J ), 1 )
   20    CONTINUE
      ELSE
!
!        Use blocked code.
!
         NN = ( ( N-1 ) / NB )*NB + 1
         DO 50 J = NN, 1, -NB
            JB = MIN( NB, N-J+1 )
!
!           Copy current block column of L to WORK and replace with
!           zeros.
!
            DO 40 JJ = J, J + JB - 1
               DO 30 I = JJ + 1, N
                  WORK( I+( JJ-J )*LDWORK ) = A( I, JJ )
                  A( I, JJ ) = ZERO
   30          CONTINUE
   40       CONTINUE
!
!           Compute current block column of inv(A).
!
            IF( J+JB.LE.N )                                             &
     &         CALL DGEMM( 'No transpose', 'No transpose', N, JB,       &
     &                     N-J-JB+1, -ONE, A( 1, J+JB ), LDA,           &
     &                     WORK( J+JB ), LDWORK, ONE, A( 1, J ), LDA )
            CALL DTRSM( 'Right', 'Lower', 'No transpose', 'Unit', N, JB,&
     &                  ONE, WORK( J ), LDWORK, A( 1, J ), LDA )
   50    CONTINUE
      END IF
!
!     Apply column interchanges.
!
      DO 60 J = N - 1, 1, -1
         JP = IPIV( J )
         IF( JP.NE.J )                                                  &
     &      CALL DSWAP( N, A( 1, J ), 1, A( 1, JP ), 1 )
   60 CONTINUE
!
      WORK( 1 ) = IWS
      RETURN
!
!     End of DGETRI
!
      END
      SUBROUTINE DTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA, &
     &                   B, LDB )
!     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
!     ..
!
!  Purpose
!  =======
!
!  DTRMM  performs one of the matrix-matrix operations
!
!     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
!
!  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!
!     op( A ) = A   or   op( A ) = A'.
!
!  Parameters
!  ==========
!
!  SIDE   - CHARACTER*1.
!           On entry,  SIDE specifies whether  op( A ) multiplies B from
!           the left or right as follows:
!
!              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
!
!              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
!
!           Unchanged on exit.
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n'   op( A ) = A.
!
!              TRANSA = 'T' or 't'   op( A ) = A'.
!
!              TRANSA = 'C' or 'c'   op( A ) = A'.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of B. M must be at
!           least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           A is not referenced.
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).
!           Unchanged on exit.
!
!  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
!           Before entry,  the leading  m by n part of the array  B must
!           contain the matrix  B,  and  on exit  is overwritten  by the
!           transformed matrix.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     .. External Subroutines ..
      EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
!     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
!
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.                       &
     &         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.                       &
     &         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.                       &
     &         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.                       &
     &         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.                       &
     &         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRMM ', INFO )
         RETURN
      END IF
!
!     Quick return if possible.
!
      IF( N.EQ.0 )                                                      &
     &   RETURN
!
!     And when  alpha.eq.zero.
!
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
!
!     Start the operations.
!
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
!
!           Form  B := alpha*A*B.
!
            IF( UPPER )THEN
               DO 50, J = 1, N
                  DO 40, K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*B( K, J )
                        DO 30, I = 1, K - 1
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   30                   CONTINUE
                        IF( NOUNIT )                                    &
     &                     TEMP = TEMP*A( K, K )
                        B( K, J ) = TEMP
                     END IF
   40             CONTINUE
   50          CONTINUE
            ELSE
               DO 80, J = 1, N
                  DO 70 K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP      = ALPHA*B( K, J )
                        B( K, J ) = TEMP
                        IF( NOUNIT )                                    &
     &                     B( K, J ) = B( K, J )*A( K, K )
                        DO 60, I = K + 1, M
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   60                   CONTINUE
                     END IF
   70             CONTINUE
   80          CONTINUE
            END IF
         ELSE
!
!           Form  B := alpha*B*A'.
!
            IF( UPPER )THEN
               DO 110, J = 1, N
                  DO 100, I = M, 1, -1
                     TEMP = B( I, J )
                     IF( NOUNIT )                                       &
     &                  TEMP = TEMP*A( I, I )
                     DO 90, K = 1, I - 1
                        TEMP = TEMP + A( K, I )*B( K, J )
   90                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  100             CONTINUE
  110          CONTINUE
            ELSE
               DO 140, J = 1, N
                  DO 130, I = 1, M
                     TEMP = B( I, J )
                     IF( NOUNIT )                                       &
     &                  TEMP = TEMP*A( I, I )
                     DO 120, K = I + 1, M
                        TEMP = TEMP + A( K, I )*B( K, J )
  120                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  130             CONTINUE
  140          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
!
!           Form  B := alpha*B*A.
!
            IF( UPPER )THEN
               DO 180, J = N, 1, -1
                  TEMP = ALPHA
                  IF( NOUNIT )                                          &
     &               TEMP = TEMP*A( J, J )
                  DO 150, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  150             CONTINUE
                  DO 170, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 160, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  160                   CONTINUE
                     END IF
  170             CONTINUE
  180          CONTINUE
            ELSE
               DO 220, J = 1, N
                  TEMP = ALPHA
                  IF( NOUNIT )                                          &
     &               TEMP = TEMP*A( J, J )
                  DO 190, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  190             CONTINUE
                  DO 210, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 200, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  200                   CONTINUE
                     END IF
  210             CONTINUE
  220          CONTINUE
            END IF
         ELSE
!
!           Form  B := alpha*B*A'.
!
            IF( UPPER )THEN
               DO 260, K = 1, N
                  DO 240, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )                                          &
     &               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 250, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  250                CONTINUE
                  END IF
  260          CONTINUE
            ELSE
               DO 300, K = N, 1, -1
                  DO 280, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 270, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  270                   CONTINUE
                     END IF
  280             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )                                          &
     &               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 290, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  290                CONTINUE
                  END IF
  300          CONTINUE
            END IF
         END IF
      END IF
!
      RETURN
!
!     End of DTRMM .
!
      END
      SUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )
!
!  -- LAPACK routine (version 1.1) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     March 31, 1993
!
!     .. Scalar Arguments ..
      CHARACTER          DIAG, UPLO
      INTEGER            INFO, LDA, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DTRTRI computes the inverse of a real upper or lower triangular
!  matrix A.
!
!  This is the Level 3 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          = 'U':  A is upper triangular;
!          = 'L':  A is lower triangular.
!
!  DIAG    (input) CHARACTER*1
!          = 'N':  A is non-unit triangular;
!          = 'U':  A is unit triangular.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the triangular matrix A.  If UPLO = 'U', the
!          leading N-by-N upper triangular part of the array A contains
!          the upper triangular matrix, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading N-by-N lower triangular part of the array A contains
!          the lower triangular matrix, and the strictly upper
!          triangular part of A is not referenced.  If DIAG = 'U', the
!          diagonal elements of A are also not referenced and are
!          assumed to be 1.
!          On exit, the (triangular) inverse of the original matrix, in
!          the same storage format.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value
!          > 0: if INFO = i, A(i,i) is exactly zero.  The triangular
!               matrix is singular and its inverse can not be computed.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            NOUNIT, UPPER
      INTEGER            J, JB, NB, NN
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
!     ..
!     .. External Subroutines ..
      EXTERNAL           DTRMM, DTRSM, DTRTI2, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOUNIT = LSAME( DIAG, 'N' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DTRTRI', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.EQ.0 )                                                      &
     &   RETURN
!
!     Check for singularity if non-unit.
!
      IF( NOUNIT ) THEN
         DO 10 INFO = 1, N
            IF( A( INFO, INFO ).EQ.ZERO )                               &
     &         RETURN
   10    CONTINUE
         INFO = 0
      END IF
!
!     Determine the block size for this environment.
!
      NB = ILAENV( 1, 'DTRTRI', UPLO // DIAG, N, -1, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.N ) THEN
!
!        Use unblocked code
!
         CALL DTRTI2( UPLO, DIAG, N, A, LDA, INFO )
      ELSE
!
!        Use blocked code
!
         IF( UPPER ) THEN
!
!           Compute inverse of upper triangular matrix
!
            DO 20 J = 1, N, NB
               JB = MIN( NB, N-J+1 )
!
!              Compute rows 1:j-1 of current block column
!
               CALL DTRMM( 'Left', 'Upper', 'No transpose', DIAG, J-1,  &
     &                     JB, ONE, A, LDA, A( 1, J ), LDA )
               CALL DTRSM( 'Right', 'Upper', 'No transpose', DIAG, J-1, &
     &                     JB, -ONE, A( J, J ), LDA, A( 1, J ), LDA )
!
!              Compute inverse of current diagonal block
!
               CALL DTRTI2( 'Upper', DIAG, JB, A( J, J ), LDA, INFO )
   20       CONTINUE
         ELSE
!
!           Compute inverse of lower triangular matrix
!
            NN = ( ( N-1 ) / NB )*NB + 1
            DO 30 J = NN, 1, -NB
               JB = MIN( NB, N-J+1 )
               IF( J+JB.LE.N ) THEN
!
!                 Compute rows j+jb:n of current block column
!
                  CALL DTRMM( 'Left', 'Lower', 'No transpose', DIAG,    &
     &                        N-J-JB+1, JB, ONE, A( J+JB, J+JB ), LDA,  &
     &                        A( J+JB, J ), LDA )
                  CALL DTRSM( 'Right', 'Lower', 'No transpose', DIAG,   &
     &                        N-J-JB+1, JB, -ONE, A( J, J ), LDA,       &
     &                        A( J+JB, J ), LDA )
               END IF
!
!              Compute inverse of current diagonal block
!
               CALL DTRTI2( 'Lower', DIAG, JB, A( J, J ), LDA, INFO )
   30       CONTINUE
         END IF
      END IF
!
      RETURN
!
!     End of DTRTRI
!
      END
      integer function idamax(n,dx,incx)
!
!     finds the index of element having max. absolute value.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!
      double precision dx(*),dmax
      integer i,incx,ix,n
!
      idamax = 0
      if( n.lt.1 .or. incx.le.0 ) return
      idamax = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
      ix = 1
      dmax = dabs(dx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(dabs(dx(ix)).le.dmax) go to 5
         idamax = i
         dmax = dabs(dx(ix))
    5    ix = ix + incx
   10 continue
      return
!
!        code for increment equal to 1
!
   20 dmax = dabs(dx(1))
      do 30 i = 2,n
         if(dabs(dx(i)).le.dmax) go to 30
         idamax = i
         dmax = dabs(dx(i))
   30 continue
      return
      end
      subroutine matcnt (xpict, npic, mtrsbt)
! ______________________________________________________________________
!     matcnt  : Calcul de la matrice de transition (64x64)
!
!     auteur  : Michel Olagnon
!     date    : 3/6/93 
! ______________________________________________________________________
      include 'rnfprm.h'
! ______________________________________________________________________
!
! .... declaration des variables
!
      integer, intent (in)                   :: npic
      real, dimension (1:npic), intent (in)  :: xpict  ! extrema
      integer, dimension (1:ncls,1:ncls), intent (out) :: mtrsbt
!                                                        transitions
!
! ... calcul des classes ...............................................
!
      xmin = xmin0
      xmax = xmax0
      xrng  = xmax - xmin
      dcls  = xrng / real (ncls)
!
! ... calcul de l'histogramme 2-D ......................................
!
      mtrsbt = 0
      do ipic = 1, npic - 1
         icls1 = (xpict (ipic) - xmin) / dcls + 1
         icls1 = max (min (ncls, icls1), 1)
         icls2 = (xpict (ipic + 1) - xmin) / dcls + 1
         icls2 = max (min (ncls, icls2), 1)
         mtrsbt (icls1, icls2) = mtrsbt (icls1, icls2) + 1
      end do
!
      return
! ______________________________________________________________________
      end subroutine matcnt
      subroutine cptrf1 (xxtrt, nxtr, ixtrt, kerr)
! ______________________________________________________________________
!     cptrf1  : execute le comptage rainflow suivant la recommandation
!               AFNOR A03-406
!               On fournit le tableau ixtrt des indices tels que 
!               ( xxtrt (i), xxtrt (ixtrt (i)) ) soit un demi-cycle
!               N.B. Quand i = ixtrt (ixtrt (i)), on a un cycle,
!                    sinon, c'est un demi-cycle du residu.
!
!     auteur  : Michel Olagnon
!     mouture : 1.1 
!     date    : 2/8/93 
! ______________________________________________________________________
      real, dimension (1:nxtr), intent (in)     :: xxtrt ! extrema
      integer, intent (in)                      :: nxtr  ! leur nombre
      integer, dimension (1:nxtr), intent (out) :: ixtrt ! indices
      integer, intent (out)                     :: kerr  ! code d'erreur
! ______________________________________________________________________
!
      real, dimension (:), allocatable      :: xwrkt
      integer, dimension (:), allocatable   :: iwrkt
!
      kerr = 0
      allocate (xwrkt (nxtr))
      allocate (iwrkt (nxtr))
!
      itst = 0
avancer: do ixtr = 1, nxtr
!
! ...... lecture des points
!
         itst = itst + 1
         xwrkt (itst) = xxtrt (ixtr)
         iwrkt (itst) = ixtr
!
! ...... test sur l'existence d'un cycle ferme
!
isoler:     do
            if (itst < 4) cycle avancer
!
            if (xwrkt (itst - 2) > xwrkt (itst - 3)) then
                if (xwrkt (itst - 1) < xwrkt (itst - 3) .or.            &
     &              xwrkt (itst)     < xwrkt (itst - 2)) cycle avancer
!
            elseif (xwrkt (itst - 2) < xwrkt (itst - 3)) then
                if (xwrkt (itst - 1) > xwrkt (itst - 3) .or.            &
     &              xwrkt (itst)     > xwrkt (itst - 2)) cycle avancer
!
            else
                kerr = 1
                goto 9000
            endif
!
! ...... stockage du cycle trouve
!
            iw1 = iwrkt (itst - 1)
            iw2 = iwrkt (itst - 2)
            ixtrt (iw2) = iw1
            ixtrt (iw1) = iw2
            xwrkt (itst - 2) = xwrkt (itst)
            iwrkt (itst - 2) = iwrkt (itst)
            itst = itst - 2
         enddo isoler
      enddo avancer
!
! .... traitement du residu 
!
      do ixtr = 1, itst - 1
         ixtrt (iwrkt (ixtr)) = iwrkt (ixtr + 1)
      enddo
      ixtrt (nxtr) = 0
!
!
      deallocate (xwrkt)
      deallocate (iwrkt)
!
 9000 continue
      return
      end subroutine cptrf1
      SUBROUTINE DGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
!     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, INCY, LDA, M, N
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
!     ..
!
!  Purpose
!  =======
!
!  DGER   performs the rank 1 operation
!
!     A := alpha*x*y' + A,
!
!  where alpha is a scalar, x is an m element vector, y is an n element
!  vector and A is an m by n matrix.
!
!  Parameters
!  ==========
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  X      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( m - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the m
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  Y      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y.
!           Unchanged on exit.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients. On exit, A is
!           overwritten by the updated matrix.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
!     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JY, KX
!     .. External Subroutines ..
      EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGER  ', INFO )
         RETURN
      END IF
!
!     Quick return if possible.
!
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )               &
     &   RETURN
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
!
      RETURN
!
!     End of DGER  .
!
      END
      SUBROUTINE DLASWP( N, A, LDA, K1, K2, IPIV, INCX )
!
!  -- LAPACK auxiliary routine (version 1.1) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     October 31, 1992
!
!     .. Scalar Arguments ..
      INTEGER            INCX, K1, K2, LDA, N
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DLASWP performs a series of row interchanges on the matrix A.
!  One row interchange is initiated for each of rows K1 through K2 of A.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the matrix of column dimension N to which the row
!          interchanges will be applied.
!          On exit, the permuted matrix.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.
!
!  K1      (input) INTEGER
!          The first element of IPIV for which a row interchange will
!          be done.
!
!  K2      (input) INTEGER
!          The last element of IPIV for which a row interchange will
!          be done.
!
!  IPIV    (input) INTEGER array, dimension (M*abs(INCX))
!          The vector of pivot indices.  Only the elements in positions
!          K1 through K2 of IPIV are accessed.
!          IPIV(K) = L implies rows K and L are to be interchanged.
!
!  INCX    (input) INTEGER
!          The increment between successive values of IPIV.  If IPIV
!          is negative, the pivots are applied in reverse order.
!
! =====================================================================
!
!     .. Local Scalars ..
      INTEGER            I, IP, IX
!     ..
!     .. External Subroutines ..
      EXTERNAL           DSWAP
!     ..
!     .. Executable Statements ..
!
!     Interchange row I with row IPIV(I) for each of rows K1 through K2.
!
      IF( INCX.EQ.0 )                                                   &
     &   RETURN
      IF( INCX.GT.0 ) THEN
         IX = K1
      ELSE
         IX = 1 + ( 1-K2 )*INCX
      END IF
      IF( INCX.EQ.1 ) THEN
         DO 10 I = K1, K2
            IP = IPIV( I )
            IF( IP.NE.I )                                               &
     &         CALL DSWAP( N, A( I, 1 ), LDA, A( IP, 1 ), LDA )
   10    CONTINUE
      ELSE IF( INCX.GT.1 ) THEN
         DO 20 I = K1, K2
            IP = IPIV( IX )
            IF( IP.NE.I )                                               &
     &         CALL DSWAP( N, A( I, 1 ), LDA, A( IP, 1 ), LDA )
            IX = IX + INCX
   20    CONTINUE
      ELSE IF( INCX.LT.0 ) THEN
         DO 30 I = K2, K1, -1
            IP = IPIV( IX )
            IF( IP.NE.I )                                               &
     &         CALL DSWAP( N, A( I, 1 ), LDA, A( IP, 1 ), LDA )
            IX = IX + INCX
   30    CONTINUE
      END IF
!
      RETURN
!
!     End of DLASWP
!
      END
      SUBROUTINE DTRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
!     .. Scalar Arguments ..
      INTEGER            INCX, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * )
!     ..
!
!  Purpose
!  =======
!
!  DTRMV  performs one of the matrix-vector operations
!
!     x := A*x,   or   x := A'*x,
!
!  where x is an n element vector and  A is an n by n unit, or non-unit,
!  upper or lower triangular matrix.
!
!  Parameters
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   x := A*x.
!
!              TRANS = 'T' or 't'   x := A'*x.
!
!              TRANS = 'C' or 'c'   x := A'*x.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit
!           triangular as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry with  UPLO = 'U' or 'u', the leading n by n
!           upper triangular part of the array A must contain the upper
!           triangular matrix and the strictly lower triangular part of
!           A is not referenced.
!           Before entry with UPLO = 'L' or 'l', the leading n by n
!           lower triangular part of the array A must contain the lower
!           triangular matrix and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u', the diagonal elements of
!           A are not referenced either, but are assumed to be unity.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, n ).
!           Unchanged on exit.
!
!  X      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x. On exit, X is overwritten with the
!           tranformed vector x.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
!     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JX, KX
      LOGICAL            NOUNIT
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     .. External Subroutines ..
      EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.                            &
     &         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.                            &
     &         .NOT.LSAME( TRANS, 'T' ).AND.                            &
     &         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.                            &
     &         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRMV ', INFO )
         RETURN
      END IF
!
!     Quick return if possible.
!
      IF( N.EQ.0 )                                                      &
     &   RETURN
!
      NOUNIT = LSAME( DIAG, 'N' )
!
!     Set up the start point in X if the increment is not unity. This
!     will be  ( N - 1 )*INCX  too small for descending loops.
!
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
      IF( LSAME( TRANS, 'N' ) )THEN
!
!        Form  x := A*x.
!
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 20, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 10, I = 1, J - 1
                        X( I ) = X( I ) + TEMP*A( I, J )
   10                CONTINUE
                     IF( NOUNIT )                                       &
     &                  X( J ) = X( J )*A( J, J )
                  END IF
   20          CONTINUE
            ELSE
               JX = KX
               DO 40, J = 1, N
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 30, I = 1, J - 1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      + INCX
   30                CONTINUE
                     IF( NOUNIT )                                       &
     &                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX + INCX
   40          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 60, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 50, I = N, J + 1, -1
                        X( I ) = X( I ) + TEMP*A( I, J )
   50                CONTINUE
                     IF( NOUNIT )                                       &
     &                  X( J ) = X( J )*A( J, J )
                  END IF
   60          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 80, J = N, 1, -1
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 70, I = N, J + 1, -1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      - INCX
   70                CONTINUE
                     IF( NOUNIT )                                       &
     &                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX - INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
!
!        Form  x := A'*x.
!
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 100, J = N, 1, -1
                  TEMP = X( J )
                  IF( NOUNIT )                                          &
     &               TEMP = TEMP*A( J, J )
                  DO 90, I = J - 1, 1, -1
                     TEMP = TEMP + A( I, J )*X( I )
   90             CONTINUE
                  X( J ) = TEMP
  100          CONTINUE
            ELSE
               JX = KX + ( N - 1 )*INCX
               DO 120, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )                                          &
     &               TEMP = TEMP*A( J, J )
                  DO 110, I = J - 1, 1, -1
                     IX   = IX   - INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  110             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   - INCX
  120          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 140, J = 1, N
                  TEMP = X( J )
                  IF( NOUNIT )                                          &
     &               TEMP = TEMP*A( J, J )
                  DO 130, I = J + 1, N
                     TEMP = TEMP + A( I, J )*X( I )
  130             CONTINUE
                  X( J ) = TEMP
  140          CONTINUE
            ELSE
               JX = KX
               DO 160, J = 1, N
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )                                          &
     &               TEMP = TEMP*A( J, J )
                  DO 150, I = J + 1, N
                     IX   = IX   + INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  150             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   + INCX
  160          CONTINUE
            END IF
         END IF
      END IF
!
      RETURN
!
!     End of DTRMV .
!
      END
      subroutine evlrnf (ptrs0t, nclsm, prnf0t) 
! ______________________________________________________________________
!     evlrnf  : Genere la matrice rainflow d'une matrice de proba
!               de transition.
!
!     auteur  : Michel Olagnon
!     mouture : 1.1 
!     date    : 4/30/93 
! ______________________________________________________________________
      real, dimension (1:nclsm,1:nclsm), intent (in) :: ptrs0t
!                                                probas transition
      integer, intent (in)            :: nclsm ! nombre de classes
      real, dimension (1:nclsm,1:nclsm), intent (out):: prnf0t
!                                                matrice rainflow
! ______________________________________________________________________
!
      real, allocatable, dimension (:,:) :: ptrst ! probas transition
      real, allocatable, dimension (:,:) :: ptrsbt! probas backwards
      real, allocatable, dimension (:,:) :: prnft ! probas transition
      real, allocatable, dimension (:,:) :: utrsft ! probas up
      real, allocatable, dimension (:,:) :: dtrsft ! probas down
      real, allocatable, dimension (:,:) :: utrsbt ! probas up
      real, allocatable, dimension (:,:) :: dtrsbt ! probas down
      real, allocatable, dimension (:,:) :: xwrkt ! matrice
      real, allocatable, dimension (:)   :: ppict ! probas pics
      real, allocatable, dimension (:)   :: pvalt ! probas vallees
      real, allocatable, dimension (:)   :: vwrkt,  vwrkft
      real, allocatable, dimension (:)   :: vwrk1t, vwrk2t
      real, allocatable, dimension (:)   :: vwrk3t, vwrk4t
!
! ... determination de la vraie gamme
!
      iclsd = nclsm
      iclsf = 1
      do icls = 1, nclsm
         if (iclsd > icls) then
            if (maxval (ptrs0t (icls+1:nclsm, icls)) > 0.0) then
               iclsd = icls
            endif
         endif
         icls1 = nclsm + 1 - icls
         if (iclsf < icls1) then
            if (ptrs0t (icls1, icls1) > 0.0) then
               iclsf = icls1
            endif
         endif
         if (iclsd < iclsf) exit
      enddo
!
      ncls = iclsf - iclsd + 1
      allocate (prnft (1:ncls,1:ncls))
      prnft = 0.0
      allocate (ptrst (1:ncls,1:ncls))
      ptrst (1:ncls, 1:ncls) = ptrs0t (iclsd:iclsf,iclsd:iclsf)
!
! ... construction de la probabilite des pics
!
      allocate (ppict (1:ncls))
      do icls = 2, ncls
         ppict (icls) = ptrst (icls, icls)
      enddo
      ppict (1) = 0.0
!
! ... construction des probas ascendantes et descendantes
!
      allocate (utrsft (1:ncls,1:ncls))
      utrsft = 0.0
      do icls = 2, ncls
         utrsft (1:icls-1, icls) = ptrst (1:icls-1, icls)
      enddo
!
      allocate (dtrsft (1:ncls,1:ncls))
      dtrsft = 0.0
      do icls = 1, ncls - 1
         dtrsft (icls+1:ncls, icls) = ptrst (icls+1:ncls, icls)
      enddo
!
! ... construction de la probabilite des vallees
!
      allocate (pvalt (1:ncls))
      pvalt = matmul (ppict, dtrsft)
!
! ... construction des probas back ascendantes et descendantes
!
      allocate (ptrsbt (1:ncls,1:ncls))
      ptrsbt = bcktrs (ptrst, ncls, ppict, pvalt)
      allocate (utrsbt (1:ncls,1:ncls))
      utrsbt = 0.0
      do icls = 2, ncls
         utrsbt (1:icls-1, icls) = ptrsbt (1:icls-1, icls)
      enddo
!
      allocate (dtrsbt (1:ncls,1:ncls))
      dtrsbt = 0.0
      do icls = 1, ncls - 1
         dtrsbt (icls+1:ncls, icls) = ptrsbt (icls+1:ncls, icls)
      enddo
!
! ... boucle sur les niveaux de pics ..................................
!
      allocate (xwrkt (1:ncls,1:ncls))
      xwrkt = matmul (dtrsbt, utrsft)
      allocate (vwrkft (1:ncls))
      allocate (vwrk1t (1:ncls))
      allocate (vwrk2t (1:ncls))
      allocate (vwrk3t (1:ncls))
      allocate (vwrk4t (1:ncls))
      allocate (vwrkt (1:ncls))
      do ipic = 2, ncls
         if (ppict (ipic) == 0.0) cycle
         vwrkft = 1.0
         vwrkft (1:ipic-1) = 0.0
         vwrk1t = dtrsft (ipic, 1:ncls)     ! EkT D
         vwrk2t = matmul (utrsft, vwrkft)   ! U Fk
         vwrkft (ipic) = 0.0
         vwrk3t = dtrsbt (ipic, 1:ncls)     ! EkT Db
         vwrk4t = matmul (utrsbt, vwrkft)   ! Ub Fk+1
!
! ...... Lkk et Rkk
!
         xrkj1 = 0.0
         xlkj1 = 0.0
!
! ...... calcul de Lkj et Rkj
!
         do ival = ipic - 1, 1, -1
            xwrkt = trs2a2 (ival, ipic, utrsft, dtrsft, ncls)
            xwrkt = invima (xwrkt, ival, ipic, ncls)
            vwrkt = 0.0
            vwrkt (ival:ncls) = vwrk1t (ival:ncls)
            vwrkt = matmul (vwrkt, xwrkt)
            vwrkt = vwrkt * vwrk2t
            xrkj  = sum (vwrkt)
            if (ipic == ncls) then
               if (ival == 1) then
                  xlkj = 1.0
               else
                  xlkj = 0.0
               endif
            else
               if (ival == 1) then
                  xlkj = 1.0
               else
                  xwrkt = trs2a2 (ival, ipic+1, utrsbt, dtrsbt, ncls)
                  xwrkt = invima (xwrkt, ival, ipic, ncls)
                  vwrkt = 0.0
                  vwrkt (ival:ncls) = vwrk3t (ival:ncls)
                  vwrkt = matmul (vwrkt, xwrkt)
                  vwrkt = vwrkt * vwrk4t
                  xlkj  = sum (vwrkt)
               endif
            endif
            xequr = xrkj - xrkj1
            xltr  = 1.0 - xrkj
            xlel  = 1.0 - xlkj1
            xequl = xlkj - xlkj1
            xfct  = 0.5 * (xequr * xlel + xequl * xltr)
            prnft (ival, ipic) = ppict (ipic) * xfct
            prnft (ipic, ival) = prnft (ival, ipic)
            xlkj1 = xlkj
            xrkj1 = xrkj
         enddo
      enddo
      prnf0t = 0.0
      prnf0t (iclsd:iclsf, iclsd:iclsf) = prnft (1:ncls, 1:ncls)
!
      deallocate (prnft)
      deallocate (ptrst)
      deallocate (ptrsbt)
      deallocate (ppict)
      deallocate (utrsft)
      deallocate (dtrsft)
      deallocate (utrsbt)
      deallocate (dtrsbt)
      deallocate (xwrkt)
      deallocate (vwrkt)
      deallocate (vwrk1t)
      deallocate (vwrk2t)
      deallocate (vwrk3t)
      deallocate (vwrk4t)
      deallocate (vwrkft)
      return
! ______________________________________________________________________
      contains
      function bcktrs (a, m, pp, pv)
!      matrice de transition retrograde 
!
      real, dimension (1:m,1:m) :: bcktrs  ! resultat
      real, dimension (1:m,1:m) :: a       ! matrice
      real, dimension (1:m)     :: pp, pv  ! probas pics et vallees
      integer, intent (in)      :: m       ! dimension
!
      bcktrs = 0.0
      do i = 1, m
         do j = 1, i - 1
            if (pp (i) == 0.0) cycle
            bcktrs (i, j) = a (j, i) * pv (j) / pp (i)
         enddo
         do j = i + 1, m
            if (pv (i) == 0.0) cycle
            bcktrs (i, j) = a (j, i) * pp (j) / pv (i)
         enddo
      enddo
      return
      end function bcktrs
! ______________________________________________________________________
      function trs2a2 (j, k, u, d, m)
!      matrice de transition intermediaire, partant de k sans descendre
!      sous j. R = IjU(I-Ik)DIj, avec Ii = deltajj, j >= i.
!      alternative: trs2a2 = 0
!                   trs2a2 (j:k-1, j:k-1) = matmul (utrsft (j:k-1,j:k-1),
!                                                   dtrsft (j:k-1,j:k-1))
!
      real, dimension (1:m,1:m) :: trs2a2  ! resultat
      real, dimension (1:m,1:m) :: u, d    ! matrices utrsft, dtrsft
      integer, intent (in)      :: j, k, m ! niveaux vallee pic
!
!##### following line replaced by Prentice to make less system dependent
!      real (kind = kind (1.0d0)) :: dtmp 
      real (kind = selected_real_kind (10,50)) :: dtmp
!
      trs2a2 = 0.0
      do iclw1 = j, k - 1
         do iclw2 = j, k - 1
            dtmp = 0.0d0
            do iclww = j, k - 1
               dtmp = dtmp + u (iclw1, iclww) * d (iclww, iclw2)
            enddo
            trs2a2 (iclw1, iclw2) = dtmp
         enddo
      enddo
      return
      end function trs2a2
! ______________________________________________________________________
      function invima (a, j, k, m)
!      matrice I / (I - A), A sous-matrice (j:k-1, j:k-1)
!
      real, dimension (1:m,1:m)              :: invima  ! resultat
      real, dimension (1:m,1:m), intent (in) :: a       ! matrice
      integer, intent (in)            :: j, k    ! niveaux vallee pic
!
!##### next two lines replaced by Prentice to make less system dependent
!      real (kind = kind (1.0d0)), allocatable, dimension (:,:) :: da
!      real (kind = kind (1.0d0)), allocatable, dimension (:)   :: dw
      real (kind = selected_real_kind(10,50)), allocatable, dimension (:,:) :: da
      real (kind = selected_real_kind(10,50)), allocatable, dimension(:) :: dw
      integer, allocatable, dimension (:)                      :: ipivt
!
      n = k - j
      if (n == 1) then
         invima = 0.0
         do iclw = 1, m
            invima (iclw, iclw) = 1.0
         enddo
         invima (j, j) = 1.0 / (1.0 - a (j, j))
      elseif (n > 1) then
         allocate (da (1:n,1:n))
         lw = n * m
         allocate (dw (1:lw))
         allocate (ipivt (1:n))
         da (1:n, 1:n) = - a (j:k-1, j:k-1)
         do i = 1, n
            da (i, i) = da (i, i) + 1.0d0
         enddo
         call dgetrf (n, n, da, n, ipivt, info)
         call dgetri (n, da, n, ipivt, dw, lw, info)
         invima = 0.0
         do iclw = 1, m
            invima (iclw, iclw) = 1.0
         enddo
         invima (j:k-1, j:k-1) = da (1:n, 1:n)
         deallocate (da)
         deallocate (dw)
         deallocate (ipivt)
      endif
      return
      end function invima
! ______________________________________________________________________
      end subroutine evlrnf
      INTEGER          FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3,  &
     &                 N4 )
!
!  -- LAPACK auxiliary routine (preliminary version) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 20, 1992
!
!     .. Scalar Arguments ..
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N3, N4
!     ..
!
!  Purpose
!  =======
!
!  ILAENV is called from the LAPACK routines to choose problem-dependent
!  parameters for the local environment.  See ISPEC for a description of
!  the parameters.
!
!  This version provides a set of parameters which should give good,
!  but not optimal, performance on many of the currently available
!  computers.  Users are encouraged to modify this subroutine to set
!  the tuning parameters for their particular machine using the option
!  and problem size information in the arguments.
!
!  This routine will not function correctly if it is converted to all
!  lower case.  Converting it to all upper case is allowed.
!
!  Arguments
!  =========
!
!  ISPEC   (input) INTEGER
!          Specifies the parameter to be returned as the value of
!          ILAENV.
!          = 1: the optimal blocksize; if this value is 1, an unblocked
!               algorithm will give the best performance.
!          = 2: the minimum block size for which the block routine
!               should be used; if the usable block size is less than
!               this value, an unblocked routine should be used.
!          = 3: the crossover point (in a block routine, for N less
!               than this value, an unblocked routine should be used)
!          = 4: the number of shifts, used in the nonsymmetric
!               eigenvalue routines
!          = 5: the minimum column dimension for blocking to be used;
!               rectangular blocks must have dimension at least k by m,
!               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
!          = 6: the crossover point for the SVD (when reducing an m by n
!               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
!               this value, a QR factorization is used first to reduce
!               the matrix to a triangular form.)
!          = 7: the number of processors
!          = 8: the crossover point for the multishift QR and QZ methods
!               for nonsymmetric eigenvalue problems.
!
!  NAME    (input) CHARACTER*(*)
!          The name of the calling subroutine, in either upper case or
!          lower case.
!
!  OPTS    (input) CHARACTER*(*)
!          The character options to the subroutine NAME, concatenated
!          into a single character string.  For example, UPLO = 'U',
!          TRANS = 'T', and DIAG = 'N' for a triangular routine would
!          be specified as OPTS = 'UTN'.
!
!  N1      (input) INTEGER
!  N2      (input) INTEGER
!  N3      (input) INTEGER
!  N4      (input) INTEGER
!          Problem dimensions for the subroutine NAME; these may not all
!          be required.
!
! (ILAENV) (output) INTEGER
!          >= 0: the value of the parameter specified by ISPEC
!          < 0:  if ILAENV = -k, the k-th argument had an illegal value.
!
!  Further Details
!  ===============
!
!  The following conventions have been used when calling ILAENV from the
!  LAPACK routines:
!  1)  OPTS is a concatenation of all of the character options to
!      subroutine NAME, in the same order that they appear in the
!      argument list for NAME, even if they are not used in determining
!      the value of the parameter specified by ISPEC.
!  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
!      that they appear in the argument list for NAME.  N1 is used
!      first, N2 second, and so on, and unused problem dimensions are
!      passed a value of -1.
!  3)  The parameter value returned by ILAENV is checked for validity in
!      the calling subroutine.  For example, ILAENV is used to retrieve
!      the optimal blocksize for STRTRI as follows:
!
!      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
!      IF( NB.LE.1 ) NB = MAX( 1, N )
!
!  =====================================================================
!
!     .. Local Scalars ..
      LOGICAL            CNAME, SNAME
      CHARACTER*1        C1
      CHARACTER*2        C2, C4
      CHARACTER*3        C3
      CHARACTER*6        SUBNAM
      INTEGER            I, IC, IZ, NB, NBMIN, NX
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          CHAR, ICHAR, INT, MIN, REAL
!     ..
!     .. Executable Statements ..
!
      GO TO ( 100, 100, 100, 400, 500, 600, 700, 800 ) ISPEC
!
!     Invalid value for ISPEC
!
      ILAENV = -1
      RETURN
!
  100 CONTINUE
!
!     Convert NAME to upper case if the first character is lower case.
!
      ILAENV = 1
      SUBNAM = NAME
      IC = ICHAR( SUBNAM( 1:1 ) )
      IZ = ICHAR( 'Z' )
      IF( IZ.EQ.90 .OR. IZ.EQ.122 ) THEN
!
!        ASCII character set
!
         IF( IC.GE.97 .AND. IC.LE.122 ) THEN
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 10 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( IC.GE.97 .AND. IC.LE.122 )                           &
     &            SUBNAM( I:I ) = CHAR( IC-32 )
   10       CONTINUE
         END IF
!
      ELSE IF( IZ.EQ.233 .OR. IZ.EQ.169 ) THEN
!
!        EBCDIC character set
!
         IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.                         &
     &       ( IC.GE.145 .AND. IC.LE.153 ) .OR.                         &
     &       ( IC.GE.162 .AND. IC.LE.169 ) ) THEN
            SUBNAM( 1:1 ) = CHAR( IC+64 )
            DO 20 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.                   &
     &             ( IC.GE.145 .AND. IC.LE.153 ) .OR.                   &
     &             ( IC.GE.162 .AND. IC.LE.169 ) )                      &
     &            SUBNAM( I:I ) = CHAR( IC+64 )
   20       CONTINUE
         END IF
!
      ELSE IF( IZ.EQ.218 .OR. IZ.EQ.250 ) THEN
!
!        Prime machines:  ASCII+128
!
         IF( IC.GE.225 .AND. IC.LE.250 ) THEN
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 30 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( IC.GE.225 .AND. IC.LE.250 )                          &
     &            SUBNAM( I:I ) = CHAR( IC-32 )
   30       CONTINUE
         END IF
      END IF
!
      C1 = SUBNAM( 1:1 )
      SNAME = C1.EQ.'S' .OR. C1.EQ.'D'
      CNAME = C1.EQ.'C' .OR. C1.EQ.'Z'
      IF( .NOT.( CNAME .OR. SNAME ) )                                   &
     &   RETURN
      C2 = SUBNAM( 2:3 )
      C3 = SUBNAM( 4:6 )
      C4 = C3( 2:3 )
!
      GO TO ( 110, 200, 300 ) ISPEC
!
  110 CONTINUE
!
!     ISPEC = 1:  block size
!
!     In these examples, separate code is provided for setting NB for
!     real and complex.  We assume that NB will take the same value in
!     single or double precision.
!
      NB = 1
!
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.    &
     &            C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'PO' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NB = 1
         ELSE IF( SNAME .AND. C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            NB = 64
         ELSE IF( C3.EQ.'TRD' ) THEN
            NB = 1
         ELSE IF( C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.         &
     &          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.         &
     &          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.         &
     &          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.         &
     &          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.         &
     &          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.         &
     &          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.         &
     &          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.         &
     &          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         END IF
      ELSE IF( C2.EQ.'GB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'PB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'TR' ) THEN
         IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'LA' ) THEN
         IF( C3.EQ.'UUM' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'ST' ) THEN
         IF( C3.EQ.'EBZ' ) THEN
            NB = 1
         END IF
      END IF
      ILAENV = NB
      RETURN
!
  200 CONTINUE
!
!     ISPEC = 2:  minimum block size
!
      NBMIN = 2
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.         &
     &       C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.         &
     &          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.         &
     &          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.         &
     &          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.         &
     &          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.         &
     &          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.         &
     &          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.         &
     &          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.         &
     &          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         END IF
      END IF
      ILAENV = NBMIN
      RETURN
!
  300 CONTINUE
!
!     ISPEC = 3:  crossover point
!
      NX = 0
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.         &
     &       C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NX = 1
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NX = 1
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.         &
     &          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.         &
     &          C4.EQ.'BR' ) THEN
               NX = 128
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.         &
     &          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.         &
     &          C4.EQ.'BR' ) THEN
               NX = 128
            END IF
         END IF
      END IF
      ILAENV = NX
      RETURN
!
  400 CONTINUE
!
!     ISPEC = 4:  number of shifts (used by xHSEQR)
!
      ILAENV = 6
      RETURN
!
  500 CONTINUE
!
!     ISPEC = 5:  minimum column dimension (not used)
!
      ILAENV = 2
      RETURN
!
  600 CONTINUE 
!
!     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
!
      ILAENV = INT( REAL( MIN( N1, N2 ) )*1.6E0 )
      RETURN
!
  700 CONTINUE
!
!     ISPEC = 7:  number of processors (not used)
!
      ILAENV = 1
      RETURN
!
  800 CONTINUE
!
!     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
!
      ILAENV = 50
      RETURN
!
!     End of ILAENV
!
      END
      subroutine matsim (ptrst, mtrsrt)
! ______________________________________________________________________
!     matsim  : Comptage en fatigue selon la methode "rainflow", sur
!               donnees generees a partir d'une matrice de proba.
!
!     auteur  : Michel Olagnon
!     date    : 5/1/93 
! ______________________________________________________________________
      use timctr
      include 'rnfprm.h'
! ______________________________________________________________________
!
      real, dimension (1:ncls,1:ncls), intent (in)     :: ptrst
      integer, dimension (1:ncls,1:ncls), intent (out) :: mtrsrt 
!
! .... declaration des variables
!
      real, dimension (1:npicm)    :: xpict  ! tableau des extrema
      integer, dimension (1:npicm) :: ifin1t ! indices de fin de cycles
      integer, dimension (1:npicm) :: ifin2t ! indices de fin de cycles
!
!
! ... calcul des classes ...............................................
!
      xmin = xmin0
      xmax = xmax0
      xrng  = xmax - xmin
      dcls  = xrng / real (ncls)
!
! .... Simulations .....................................................
!
      mtrsrt = 0
      do isim = 1, nsim
!
! .... generation du signal a analyser
!
         ngen = ndonm
         call gentrs (ptrst, ncls, xmin, dcls, xpict, ngen)
!
! .... extraction des extrema inutile
!
         npic = ngen
!
! .... comptage rainflow, premier algorithme
!
         call cptrf1 (xpict, npic, ifin1t, kerr)
!
! .... comptage rainflow, second algorithme
!
         call cptrf2 (xpict, npic, ifin2t, kerr)
!
! .... comparaison
!
         call cmpcpt (xpict, ifin1t, ifin2t, npic, kdif)
         call gettim (jheu, jmin, xsec)
         write (luout, 99999) jheu, jmin, xsec,                          &
     & ' Completed simulation # ', isim
         if (kdif /= 0) then
            write (luout,*) 'Comparison failed'
            stop
         endif
!
! ... calcul de l'histogramme 2-D ......................................
!
         do ipic = 1, npic
            ifin = ifin1t (ipic)
            if (ifin /= 0) then
               icls1 = (xpict (ipic) - xmin) / dcls + 1
               icls1 = max (min (ncls, icls1), 1)
               icls2 = (xpict (ifin) - xmin) / dcls + 1
               icls2 = max (min (ncls, icls2), 1)
               mtrsrt (icls1, icls2) = mtrsrt (icls1, icls2) + 1
            endif
         end do
      end do
!
 9000 continue
      return
! ______________________________________________________________________
99999 format (i3, ':', i2, ':', f6.3, ' ->', a, i4)
      end subroutine matsim
      subroutine cptrf2 (xxtrt, nxtr, ixtrt, kerr)
! ______________________________________________________________________
!     cptrf2  : execute le comptage rainflow suivant les proprietes
!               definies par M. Olagnon.
!               On fournit le tableau ixtrt des indices tels que 
!               ( xxtrt (i), xxtrt (ixtrt (i)) ) soit un demi-cycle
!               N.B. Quand xxtrt (i) = xxtrt (ixtrt (ixtrt (i))), on a
!                    un cycle, sinon, c'est un demi-cycle du residu.
!
!     auteur  : Michel Olagnon
!     mouture : 1.2 
!     date    : 2/9/93 
! ______________________________________________________________________
      real, dimension (1:nxtr), intent (in)     :: xxtrt ! extrema
      integer, intent (in)                      :: nxtr  ! leur nombre
      integer, dimension (1:nxtr), intent (out) :: ixtrt ! indices
      integer, intent (out)                     :: kerr  ! code d'erreur
! ______________________________________________________________________
!
      kerr = 0
      ixtrt = 0
!
! .... on va traiter les pics
!
! ...... cas ou on n'en a pas assez
!
      if (nxtr < 4) then
         kerr = 1
         do ixtr = 1, nxtr - 1
           ixtrt (ixtr) = ixtr + 1
         enddo
         goto 9000
      endif
!
! ...... trouver le premier et le dernier
!
      if (xxtrt (1) > xxtrt (2)) then
         ipicd = 1
      elseif (xxtrt (1) < xxtrt (2)) then
         ipicd = 2
      else
         kerr = 2
         goto 9000
      endif
!
      if (xxtrt (nxtr - 1) > xxtrt (nxtr)) then
         ipicf = nxtr - 1
      elseif (xxtrt (nxtr - 1) < xxtrt (nxtr)) then
         ipicf = nxtr
      else
         kerr = 2
         goto 9000
      endif
!
! .... pour chaque pic, on va chercher la vallee correspondante
!
      do ipic = ipicd, ipicf, 2
!
! ...... rechercher les pics plus eleves qui encadrent le pic courant
!
         xpic = xxtrt (ipic)
         ipic1 = ipicd
         ifdeb = 1
         do ipicw = ipic - 2, ipicd, -2
            if (xxtrt (ipicw) > xpic) then
               ifdeb = 0
               ipic1 = ipicw
               exit
            endif
         enddo
!
         ipic2 = ipicf
         iffin = 1
         do ipicw = ipic + 2, ipicf, 2
            if (xxtrt (ipicw) >= xpic) then
               iffin = 0
               ipic2 = ipicw
               exit
            endif
         enddo
!
! ...... rechercher les vallees les plus profondes de part et d'autre
!
         if (ipic1 < ipic) then
            if (ifdeb == 1) then
               ival1 = minlst (1, ipic-1)
            else
               ival1 = minlst (ipic1+1, ipic-1)
            endif
         else
            if (ipicd == 1) then
               ival1 = 0
            else
               ival1 = 1
            endif
         endif
         if (ipic2 > ipic) then
            if (iffin == 1) then
               ival2 = minlst (ipic+1, nxtr)
            else
               ival2 = minlst (ipic+1, ipic2-1)
            endif
         else
            if (ipicf == nxtr) then
               ival2 = 0
            else
               ival2 = nxtr
            endif
         endif
!
! ...... extremites
!
         if     (ival1 == 0) then
            ixtrt (ipic) = ival2
!
         elseif (ival2 == 0) then
            ixtrt (ival1) = ipic
!
         else
!
! ...... on va prendre la plus elevee des deux vallees
!
            if     (ifdeb == 1 .and. iffin == 0) then
!
! ...... demi-cycles du residu ?
! ........ partie en forme de <
!
               if (xxtrt (ival1) >= xxtrt (ival2)) then
                  ixtrt (ival1) = ipic
                  ixtrt (ipic)  = ival2
                  ixtrt (ival2) = ipic
!
! ........ cycle normal
!
               else
                  ixtrt (ipic) = ival2
                  ixtrt (ival2) = ipic
               endif
!
            elseif (ifdeb == 1 .and. iffin == 1) then
!
! ...... demi-cycles du residu 
! ........ point milieu
!
                  ixtrt (ival1) = ipic
                  ixtrt (ipic) = ival2
!
            elseif (ifdeb == 0 .and. iffin == 1) then
!
! ...... demi-cycles du residu ?
! ........ cycle normal
!
               if (xxtrt (ival1) >= xxtrt (ival2)) then
                  ixtrt (ival1) = ipic
                  ixtrt (ipic) = ival1
!
! ........ partie en forme de >
!
               else
                  ixtrt (ival1) = ipic
                  ixtrt (ipic) = ival2
               endif
!
! ...... cycles normaux
!
            else
               if (xxtrt (ival1) >= xxtrt (ival2)) then
                  ixtrt (ipic) = ival1
                  ixtrt (ival1) = ipic
               else
                  ixtrt (ipic) = ival2
                  ixtrt (ival2) = ipic
               endif
            endif ! sur les cas de residu
         endif ! sur les bords libres
      enddo
!
 9000 continue
      return
! ______________________________________________________________________
      contains
!
     integer function minlst (ipos1, ipos2)
!
! .. dernier minimum de xxtrt entre ipos1 et ipos2
!
      minlst = ipos2
      do ipos = ipos2 - 1, ipos1, -1
         if (xxtrt (ipos) < xxtrt (minlst)) then
            minlst = ipos
         endif
      enddo
      end function minlst
!
      end subroutine cptrf2
      SUBROUTINE DGETF2( M, N, A, LDA, IPIV, INFO )
!
!  -- LAPACK routine (version 1.1) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     June 30, 1992
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DGETF2 computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 2 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the m by n matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <= i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
!               has been completed, but the factor U is exactly
!               singular, and division by zero will occur if it is used
!               to solve a system of equations.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            J, JP
!     ..
!     .. External Functions ..
      INTEGER            IDAMAX
      EXTERNAL           IDAMAX
!     ..
!     .. External Subroutines ..
      EXTERNAL           DGER, DSCAL, DSWAP, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETF2', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 )                                          &
     &   RETURN
!
      DO 10 J = 1, MIN( M, N )
!
!        Find pivot and test for singularity.
!
         JP = J - 1 + IDAMAX( M-J+1, A( J, J ), 1 )
         IPIV( J ) = JP
         IF( A( JP, J ).NE.ZERO ) THEN
!
!           Apply the interchange to columns 1:N.
!
            IF( JP.NE.J )                                               &
     &         CALL DSWAP( N, A( J, 1 ), LDA, A( JP, 1 ), LDA )
!
!           Compute elements J+1:M of J-th column.
!
            IF( J.LT.M )                                                &
     &         CALL DSCAL( M-J, ONE / A( J, J ), A( J+1, J ), 1 )
!
         ELSE IF( INFO.EQ.0 ) THEN
!
            INFO = J
         END IF
!
         IF( J.LT.MIN( M, N ) ) THEN
!
!           Update trailing submatrix.
!
            CALL DGER( M-J, N-J, -ONE, A( J+1, J ), 1, A( J, J+1 ), LDA,&
     &                 A( J+1, J+1 ), LDA )
         END IF
   10 CONTINUE
      RETURN
!
!     End of DGETF2
!
      END
      subroutine  dscal(n,da,dx,incx)
!
!     scales a vector by a constant.
!     uses unrolled loops for increment equal to one.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!
      double precision da,dx(*)
      integer i,incx,m,mp1,n,nincx
!
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
!
!        code for increment equal to 1
!
!
!        clean-up loop
!
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end
      SUBROUTINE DTRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA, &
     &                   B, LDB )
!     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
!     ..
!
!  Purpose
!  =======
!
!  DTRSM  solves one of the matrix equations
!
!     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
!
!  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!
!     op( A ) = A   or   op( A ) = A'.
!
!  The matrix X is overwritten on B.
!
!  Parameters
!  ==========
!
!  SIDE   - CHARACTER*1.
!           On entry, SIDE specifies whether op( A ) appears on the left
!           or right of X as follows:
!
!              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
!
!              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
!
!           Unchanged on exit.
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n'   op( A ) = A.
!
!              TRANSA = 'T' or 't'   op( A ) = A'.
!
!              TRANSA = 'C' or 'c'   op( A ) = A'.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of B. M must be at
!           least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           A is not referenced.
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).
!           Unchanged on exit.
!
!  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
!           Before entry,  the leading  m by n part of the array  B must
!           contain  the  right-hand  side  matrix  B,  and  on exit  is
!           overwritten by the solution matrix  X.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     .. External Subroutines ..
      EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
!     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
!
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.                       &
     &         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.                       &
     &         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.                       &
     &         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.                       &
     &         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.                       &
     &         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRSM ', INFO )
         RETURN
      END IF
!
!     Quick return if possible.
!
      IF( N.EQ.0 )                                                      &
     &   RETURN
!
!     And when  alpha.eq.zero.
!
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
!
!     Start the operations.
!
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
!
!           Form  B := alpha*inv( A )*B.
!
            IF( UPPER )THEN
               DO 60, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 30, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   30                CONTINUE
                  END IF
                  DO 50, K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )                                    &
     &                     B( K, J ) = B( K, J )/A( K, K )
                        DO 40, I = 1, K - 1
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   40                   CONTINUE
                     END IF
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 100, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 70, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   70                CONTINUE
                  END IF
                  DO 90 K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )                                    &
     &                     B( K, J ) = B( K, J )/A( K, K )
                        DO 80, I = K + 1, M
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   80                   CONTINUE
                     END IF
   90             CONTINUE
  100          CONTINUE
            END IF
         ELSE
!
!           Form  B := alpha*inv( A' )*B.
!
            IF( UPPER )THEN
               DO 130, J = 1, N
                  DO 120, I = 1, M
                     TEMP = ALPHA*B( I, J )
                     DO 110, K = 1, I - 1
                        TEMP = TEMP - A( K, I )*B( K, J )
  110                CONTINUE
                     IF( NOUNIT )                                       &
     &                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  120             CONTINUE
  130          CONTINUE
            ELSE
               DO 160, J = 1, N
                  DO 150, I = M, 1, -1
                     TEMP = ALPHA*B( I, J )
                     DO 140, K = I + 1, M
                        TEMP = TEMP - A( K, I )*B( K, J )
  140                CONTINUE
                     IF( NOUNIT )                                       &
     &                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  150             CONTINUE
  160          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
!
!           Form  B := alpha*B*inv( A ).
!
            IF( UPPER )THEN
               DO 210, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 170, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  170                CONTINUE
                  END IF
                  DO 190, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 180, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  180                   CONTINUE
                     END IF
  190             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 200, I = 1, M
                        B( I, J ) = TEMP*B( I, J )
  200                CONTINUE
                  END IF
  210          CONTINUE
            ELSE
               DO 260, J = N, 1, -1
                  IF( ALPHA.NE.ONE )THEN
                     DO 220, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  220                CONTINUE
                  END IF
                  DO 240, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 250, I = 1, M
                       B( I, J ) = TEMP*B( I, J )
  250                CONTINUE
                  END IF
  260          CONTINUE
            END IF
         ELSE
!
!           Form  B := alpha*B*inv( A' ).
!
            IF( UPPER )THEN
               DO 310, K = N, 1, -1
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 270, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  270                CONTINUE
                  END IF
                  DO 290, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 280, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  280                   CONTINUE
                     END IF
  290             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 300, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  300                CONTINUE
                  END IF
  310          CONTINUE
            ELSE
               DO 360, K = 1, N
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 320, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  320                CONTINUE
                  END IF
                  DO 340, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 330, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  330                   CONTINUE
                     END IF
  340             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 350, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  350                CONTINUE
                  END IF
  360          CONTINUE
            END IF
         END IF
      END IF
!
      RETURN
!
!     End of DTRSM .
!
      END
      subroutine extpic (xsigt, nsig, xxtrt, nxtr, kerr)
! ______________________________________________________________________
!     extpic  : extrait les extrema relatifs d'un signal
!
!     auteur  : Michel Olagnon
!     mouture : 1.2 
!     date    : 2/12/93 
! ______________________________________________________________________
      real, dimension (1:nsig), intent (in)  :: xsigt ! signal
      integer, intent (in)                   :: nsig  ! sa longueur
      real, dimension (1:nsig), intent (out) :: xxtrt ! extrema
      integer, intent (out)                  :: nxtr  ! leur nombre
      integer, intent (out)                  :: kerr  ! code d'erreur
! ______________________________________________________________________
!
      kerr = 0
!
! .... initialisation : trouver le sens de variation
!
      kvar = 0
      nxtr = 0
!
      xprv = xsigt (1)
      do isig = 2, nsig
         if (xsigt (isig) > xprv) then
            kvar = +1
            isigd = isig
            exit
         elseif (xsigt (isig) < xprv) then
            kvar = -1
            isigd = isig
            exit
         endif
      enddo
!
! .... cas du signal constant
!
      if (kvar == 0) then
         kerr = 1
         goto 9000
      endif
!
! .... boucle sur le signal restant
!
      nxtr = nxtr + 1
      xxtrt (nxtr) = xprv
      xprv = xsigt (isigd)
!
      do isig = isigd, nsig
         select case (kvar)
!
! ...... montee
!
         case (+1)
            if (xsigt (isig) < xprv) then
               nxtr = nxtr + 1
               xxtrt (nxtr) = xprv
               kvar = -1
            endif
            xprv = xsigt (isig)
!
! ...... descente
!
         case (-1)
            if (xsigt (isig) > xprv) then
               nxtr = nxtr + 1
               xxtrt (nxtr) = xprv
               kvar = +1
            endif
         end select
!
         xprv = xsigt (isig)
      enddo
!
      nxtr = nxtr + 1
      xxtrt (nxtr) = xsigt (nsig)
!
 9000 continue
      return
      end subroutine extpic
      LOGICAL          FUNCTION LSAME( CA, CB )
!
!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992
!
!     .. Scalar Arguments ..
      CHARACTER          CA, CB
!     ..
!
!  Purpose
!  =======
!
!  LSAME returns .TRUE. if CA is the same letter as CB regardless of
!  case.
!
!  Arguments
!  =========
!
!  CA      (input) CHARACTER*1
!  CB      (input) CHARACTER*1
!          CA and CB specify the single characters to be compared.
!
!     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
!     ..
!     .. Local Scalars ..
      INTEGER            INTA, INTB, ZCODE
!     ..
!     .. Executable Statements ..
!
!     Test if the characters are equal
!
      LSAME = CA.EQ.CB
      IF( LSAME )                                                       &
     &   RETURN
!
!     Now test for equivalence if both characters are alphabetic.
!
      ZCODE = ICHAR( 'Z' )
!
!     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
!     machines, on which ICHAR returns a value with bit 8 set.
!     ICHAR('A') on Prime machines returns 193 which is the same as
!     ICHAR('A') on an EBCDIC machine.
!
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
!
      IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 ) THEN
!
!        ASCII is assumed - ZCODE is the ASCII code of either lower or
!        upper case 'Z'.
!
         IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
         IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
!
      ELSE IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 ) THEN
!
!        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
!        upper case 'Z'.
!
         IF( INTA.GE.129 .AND. INTA.LE.137 .OR.                         &
     &       INTA.GE.145 .AND. INTA.LE.153 .OR.                         &
     &       INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
         IF( INTB.GE.129 .AND. INTB.LE.137 .OR.                         &
     &       INTB.GE.145 .AND. INTB.LE.153 .OR.                         &
     &       INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64
!
      ELSE IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 ) THEN
!
!        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
!        plus 128 of either lower or upper case 'Z'.
!
         IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
         IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
      END IF
      LSAME = INTA.EQ.INTB
!
!     RETURN
!
!     End of LSAME
!
      END
      subroutine mattrs (mtrst, ptrst)
! ______________________________________________________________________
!     mattrs  : Calcul de la matrice des probabilites de transition 
!               a partir de celle des nombres de transitions.
!
!     auteur  : Michel Olagnon
!     date    : 4/30/93 
! ______________________________________________________________________
      include 'rnfprm.h'
! ______________________________________________________________________
!
      integer, dimension (1:ncls,1:ncls), intent (in) :: mtrst  ! transitions
      real, dimension (1:ncls,1:ncls), intent (out)   :: ptrst  ! probas
!
! .... declaration des variables
!
      integer, dimension (1:ncls)  :: jpict ! histogramme pics
      integer, dimension (1:ncls)  :: jvalt ! histogramme vallees
      integer, dimension (1:ncls)  :: npict ! histogramme pics
!
! ... generation de la matrice aleatoire
!
      do icls = 2, ncls
         jvalt (icls) = sum (mtrst (icls, 1:icls-1))
      enddo
      jvalt (1) = 0
      do icls = 1, ncls - 1
         jpict (icls) = sum (mtrst (icls, icls+1:ncls))
      enddo
      jpict (ncls) = 0
      do icls = 2, ncls
         npict (icls) = sum (mtrst (1:icls-1, icls))
      enddo
      npict (1) = 0
      npic = sum (npict)
      ptrst = 0.0
      do icls1 = 1, ncls
         do icls2 = icls1 + 1, ncls
            if (jpict (icls1) /= 0) then
               ptrst (icls1, icls2) = real (mtrst (icls1, icls2))  /    &
     &                                real (jpict (icls1))
            endif
            if (jvalt (icls2) /= 0) then
               ptrst (icls2, icls1) = real (mtrst (icls2, icls1))  /    &
     &                                real (jvalt (icls2))
            endif
         enddo
         ptrst (icls1, icls1) = real (npict (icls1)) / real (npic)
      enddo
!
      return
! ______________________________________________________________________
      end subroutine mattrs
      program rnflow 
! ______________________________________________________________________
!     rnflow  : Benchmarking program in Fortran 90
!
!               Reads LBF's gaussian sequence Ir = 0.3
!               Generates raw transitions counts matrix
!               Computes Markov transition probabilities matrix
!               Calculates theoretical rainflow counts matrix
!               Simulates 100 sequences using Markov matrix
!               Verifies identity of 2 rainflow counting methods
!               Averages the corresponding rainflow counts matrices
!               Compares simulations average with theoretical matrix
!
!     author  : Michel Olagnon (Michel.Olagnon@ifremer.fr)
!     refer.  : Statistical Properties of Rainflow Counts
!               (submitted to Int. J. Fatigue)
!     version : 1.0 
!     date    : 5/4/93 
! ______________________________________________________________________
      use timctr
      include 'rnfprm.h'
! ______________________________________________________________________
!
      real, dimension (1:ndonm)    :: xdont  ! tableau des donnees
      real, dimension (1:npicm)    :: xpict  ! tableau des extrema
      integer, dimension (1:ncls,1:ncls) :: mtrsbt ! matrice transition
      integer, dimension (1:ncls,1:ncls) :: mrnftt ! matrice theorique
      integer, dimension (1:ncls,1:ncls) :: mrnfst ! matrice simulee
      real, dimension (1:ncls,1:ncls)    :: ptrst  ! matrice Markov
      real, dimension (1:ncls,1:ncls)    :: prnft  ! matrice Rainflow
!
!JRA      open (unit=luout,file="rnflow.out",status="unknown",form="formatted")
	  open (unit=luinp,file="rnflow.in",status="unknown",form="formatted")

!
      call initim
      call gettim (jheu, jmin, xsec)
      write (luout, 99999) jheu, jmin, xsec,                             &
     & ' Read sequence'
!
! .... Read sequence
!
      call reaseq (xdont, ndonm, ndon)
      call gettim (jheu, jmin, xsec)
      write (luout, 99999) jheu, jmin, xsec,                             &
     & ' extract extrema'
!
! .... extract extrema
!
      call extpic (xdont, ndon, xpict, npic, kerr)
      if (kerr /= 0) then
         goto 9000
      endif
      call gettim (jheu, jmin, xsec)
      write (luout, 99999) jheu, jmin, xsec,                             &
     & ' Generate raw transitions counts'
!
! .... Generate raw transitions counts matrix
!
      call matcnt (xpict, npic, mtrsbt)
      call gettim (jheu, jmin, xsec)
      write (luout, 99999) jheu, jmin, xsec,                             &
     & ' Compute Markov matrix'
!
! .... Compute Markov transition probabilities matrix
!
      call mattrs (mtrsbt, ptrst)
      call gettim (jheu, jmin, xsec)
      write (luout, 99999) jheu, jmin, xsec,                             &
     & ' Calculate theoretical rainflow'
!
! .... Calculate theoretical rainflow counts matrix
!
      call evlrnf (ptrst, ncls, prnft)
      mrnftt = nint (real (nsim) * real (npic) * prnft)
      call gettim (jheu, jmin, xsec)
      write (luout, 99999) jheu, jmin, xsec,                             &
     & ' Simulate random markov sequences'
!
! .... Simulate random markov sequences
!
      call matsim (ptrst, mrnfst)
      call gettim (jheu, jmin, xsec)
      write (luout, 99999) jheu, jmin, xsec,                             &
     & ' Compare results'
!
! .... Compare results
!
      call cmpmat (mrnftt, mrnfst)
!
 9000 continue
      call gettim (jheu, jmin, xsec)
      write (luout, 99999) jheu, jmin, xsec,                             &
     & ' Completed program execution'
!JRA	  close (unit = luout)
	  close (unit = luinp)
! ______________________________________________________________________
99999 format (i3, ':', i2, ':', f6.3, ' ->', a)
      end program rnflow
      SUBROUTINE XERBLA( SRNAME, INFO )
!
!  -- LAPACK auxiliary routine (version 1.1) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992
!
!     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      INTEGER            INFO
!     ..
!
!  Purpose
!  =======
!
!  XERBLA  is an error handler for the LAPACK routines.
!  It is called by an LAPACK routine if an input parameter has an
!  invalid value.  A message is printed and execution stops.
!
!  Installers may consider modifying the STOP statement in order to
!  call system-specific exception-handling facilities.
!
!  Arguments
!  =========
!
!  SRNAME  (input) CHARACTER*6
!          The name of the routine which called XERBLA.
!
!  INFO    (input) INTEGER
!          The position of the invalid parameter in the parameter list
!          of the calling routine.
!
!     .. Executable Statements ..
!
      WRITE( *, FMT = 9999 )SRNAME, INFO
!
      STOP
!
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',&
     &      'an illegal value' )
!
!     End of XERBLA
!
      END
