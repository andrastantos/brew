!*==LINPK.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!   SPAG options set to convert source form only
! *** Double-precision Linpack benchmark
      PROGRAM LINPK
      PARAMETER (N=2500,LDA=N+1)
      DOUBLE PRECISION a(LDA,N) , b(N) , x(N)
      DOUBLE PRECISION norma , normx
      DOUBLE PRECISION resid , residn , eps
      REAL*8 EPSLON
      INTEGER ipvt(N)
 
 
      CALL MATGEN(a,LDA,N,b,norma)
      CALL DGEFA(a,LDA,N,ipvt,info)
      CALL DGESL(a,LDA,N,ipvt,b,0)
 
!     compute a residual to verify results.
 
      DO i = 1 , N
         x(i) = b(i)
      ENDDO
      CALL MATGEN(a,LDA,N,b,norma)
      DO i = 1 , N
         b(i) = -b(i)
      ENDDO
      CALL DMXPY(N,b,N,LDA,x,a)
      resid = 0.0D00
      normx = 0.0D00
      DO i = 1 , N
         resid = DMAX1(resid,DABS(b(i)))
         normx = DMAX1(normx,DABS(x(i)))
      ENDDO
      eps = EPSLON(1.0D0)
      residn = resid/(N*norma*normx*eps)
      WRITE (*,200)
 200  FORMAT ('     norm. resid      resid           machep',           &
     &        '         x(1)          x(n)')
      WRITE (*,300) residn , resid , eps , x(1) , x(N)
 300  FORMAT (1P5d16.8)
 
      END
!*==MATGEN.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
      SUBROUTINE MATGEN(A,Lda,N,B,Norma)
      DOUBLE PRECISION A(Lda,*) , B(*) , Norma
 
      init = 1325
      Norma = 0.0
      DO j = 1 , N
         DO i = 1 , N
            init = MOD(3125*init,65536)
            A(i,j) = (init-32768.0)/16384.0
            Norma = DMAX1(A(i,j),Norma)
         ENDDO
      ENDDO
      DO i = 1 , N
         B(i) = 0.0
      ENDDO
      DO j = 1 , N
         DO i = 1 , N
            B(i) = B(i) + A(i,j)
         ENDDO
      ENDDO
      CONTINUE
      END
!*==DGEFA.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
      SUBROUTINE DGEFA(A,Lda,N,Ipvt,Info)
      INTEGER Lda , N , Ipvt(*) , Info
      DOUBLE PRECISION A(Lda,*)
 
!     dgefa factors a double precision matrix by gaussian elimination.
 
!     dgefa is usually called by dgeco, but it can be called
!     directly with a saving in time if  rcond  is not needed.
!     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
 
!     on entry
 
!     a       double precision(lda, n)
!     the matrix to be factored.
 
!     lda     integer
!     the leading dimension of the array  a .
 
!     n       integer
!     the order of the matrix  a .
 
!     on return
 
!     a       an upper triangular matrix and the multipliers
!     which were used to obtain it.
!     the factorization can be written  a = l*u  where
!     l  is a product of permutation and unit lower
!     triangular matrices and  u  is upper triangular.
 
!     ipvt    integer(n)
!     an integer vector of pivot indices.
 
!     info    integer
!     = 0  normal value.
!     = k  if  u(k,k) .eq. 0.0 .  this is not an error
!     condition for this subroutine, but it does
!     indicate that dgesl or dgedi will divide by zero
!     if called.  use  rcond  in dgeco for a reliable
!     indication of singularity.
 
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
 
!     subroutines and functions
 
!     blas daxpy,dscal,idamax
 
!     internal variables
 
      DOUBLE PRECISION t
      INTEGER IDAMAX , j , k , kp1 , l , nm1
 
 
!     gaussian elimination with partial pivoting
 
      Info = 0
      nm1 = N - 1
      IF ( nm1.GE.1 ) THEN
         DO k = 1 , nm1
            kp1 = k + 1
 
!             find l = pivot index
 
            l = IDAMAX(N-k+1,A(k,k),1) + k - 1
            Ipvt(k) = l
 
!             zero pivot implies this column already triangularized
 
            IF ( A(l,k).EQ.0.0D0 ) THEN
               Info = k
            ELSE
 
!                 interchange if necessary
 
               IF ( l.NE.k ) THEN
                  t = A(l,k)
                  A(l,k) = A(k,k)
                  A(k,k) = t
               ENDIF
 
!                 compute multipliers
 
               t = -1.0D0/A(k,k)
               CALL DSCAL(N-k,t,A(k+1,k),1)
 
!                 row elimination with column indexing
 
               DO j = kp1 , N
                  t = A(l,j)
                  IF ( l.NE.k ) THEN
                     A(l,j) = A(k,j)
                     A(k,j) = t
                  ENDIF
                  CALL DAXPY(N-k,t,A(k+1,k),1,A(k+1,j),1)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      Ipvt(N) = N
      IF ( A(N,N).EQ.0.0D0 ) Info = N
      CONTINUE
      END
!*==DGESL.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
      SUBROUTINE DGESL(A,Lda,N,Ipvt,B,Job)
      INTEGER Lda , N , Ipvt(*) , Job
      DOUBLE PRECISION A(Lda,*) , B(*)
 
!     dgesl solves the double precision system
!     a * x = b  or  trans(a) * x = b
!     using the factors computed by dgeco or dgefa.
 
!     on entry
 
!     a       double precision(lda, n)
!     the output from dgeco or dgefa.
 
!     lda     integer
!     the leading dimension of the array  a .
 
!     n       integer
!     the order of the matrix  a .
 
!     ipvt    integer(n)
!     the pivot vector from dgeco or dgefa.
 
!     b       double precision(n)
!     the right hand side vector.
 
!     job     integer
!     = 0         to solve  a*x = b ,
!     = nonzero   to solve  trans(a)*x = b  where
!     trans(a)  is the transpose.
 
!     on return
 
!     b       the solution vector  x .
 
!     error condition
 
!     a division by zero will occur if the input factor contains a
!     zero on the diagonal.  technically this indicates singularity
!     but it is often caused by improper arguments or improper
!     setting of lda .  it will not occur if the subroutines are
!     called correctly and if dgeco has set rcond .gt. 0.0
!     or dgefa has set info .eq. 0 .
 
!     to compute  inverse(a) * c  where  c  is a matrix
!     with  p  columns
!     call dgeco(a,lda,n,ipvt,rcond,z)
!     if (rcond is too small) go to ...
!     do 10 j = 1, p
!     call dgesl(a,lda,n,ipvt,c(1,j),0)
!     10 continue
 
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
 
!     subroutines and functions
 
!     blas daxpy,ddot
 
!     internal variables
 
      DOUBLE PRECISION DDOT , t
      INTEGER k , kb , l , nm1
 
      nm1 = N - 1
      IF ( Job.EQ.0 ) THEN
 
!         job = 0 , solve  a * x = b
!         first solve  l*y = b
 
         IF ( nm1.GE.1 ) THEN
            DO k = 1 , nm1
               l = Ipvt(k)
               t = B(l)
               IF ( l.NE.k ) THEN
                  B(l) = B(k)
                  B(k) = t
               ENDIF
               CALL DAXPY(N-k,t,A(k+1,k),1,B(k+1),1)
            ENDDO
         ENDIF
 
!         now solve  u*x = y
 
         DO kb = 1 , N
            k = N + 1 - kb
            B(k) = B(k)/A(k,k)
            t = -B(k)
            CALL DAXPY(k-1,t,A(1,k),1,B(1),1)
         ENDDO
      ELSE
 
!         job = nonzero, solve  trans(a) * x = b
!         first solve  trans(u)*y = b
 
         DO k = 1 , N
            t = DDOT(k-1,A(1,k),1,B(1),1)
            B(k) = (B(k)-t)/A(k,k)
         ENDDO
 
!         now solve trans(l)*x = y
 
         IF ( nm1.GE.1 ) THEN
            DO kb = 1 , nm1
               k = N - kb
               B(k) = B(k) + DDOT(N-k,A(k+1,k),1,B(k+1),1)
               l = Ipvt(k)
               IF ( l.NE.k ) THEN
                  t = B(l)
                  B(l) = B(k)
                  B(k) = t
               ENDIF
            ENDDO
         ENDIF
      ENDIF
      CONTINUE
      END
!*==DAXPY.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
      SUBROUTINE DAXPY(N,Da,Dx,Incx,Dy,Incy)
 
!     constant times a vector plus a vector.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
 
      DOUBLE PRECISION Dx(*) , Dy(*) , Da
      INTEGER i , Incx , Incy , ix , iy , m , mp1 , N
 
      IF ( N.GT.0 ) THEN
         IF ( Da.NE.0.0D0 ) THEN
            IF ( Incx.NE.1 .OR. Incy.NE.1 ) THEN
 
!                 code for unequal increments or equal increments
!                 not equal to 1
 
               ix = 1
               iy = 1
               IF ( Incx.LT.0 ) ix = (-N+1)*Incx + 1
               IF ( Incy.LT.0 ) iy = (-N+1)*Incy + 1
               DO i = 1 , N
                  Dy(iy) = Dy(iy) + Da*Dx(ix)
                  ix = ix + Incx
                  iy = iy + Incy
               ENDDO
            ELSE
 
!                 code for both increments equal to 1
 
 
!                 clean-up loop
 
               m = MOD(N,4)
               IF ( m.NE.0 ) THEN
                  DO i = 1 , m
                     Dy(i) = Dy(i) + Da*Dx(i)
                  ENDDO
                  IF ( N.LT.4 ) RETURN
               ENDIF
               mp1 = m + 1
               DO i = mp1 , N , 4
                  Dy(i) = Dy(i) + Da*Dx(i)
                  Dy(i+1) = Dy(i+1) + Da*Dx(i+1)
                  Dy(i+2) = Dy(i+2) + Da*Dx(i+2)
                  Dy(i+3) = Dy(i+3) + Da*Dx(i+3)
               ENDDO
            ENDIF
         ENDIF
      ENDIF
      CONTINUE
      END
!*==DDOT.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      DOUBLE PRECISION FUNCTION DDOT(N,Dx,Incx,Dy,Incy)
 
!     forms the dot product of two vectors.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
 
      DOUBLE PRECISION Dx(*) , Dy(*) , dtemp
      INTEGER i , Incx , Incy , ix , iy , m , mp1 , N
 
      DDOT = 0.0D0
      dtemp = 0.0D0
      IF ( N.GT.0 ) THEN
         IF ( Incx.NE.1 .OR. Incy.NE.1 ) THEN
 
!             code for unequal increments or equal increments
!             not equal to 1
 
            ix = 1
            iy = 1
            IF ( Incx.LT.0 ) ix = (-N+1)*Incx + 1
            IF ( Incy.LT.0 ) iy = (-N+1)*Incy + 1
            DO i = 1 , N
               dtemp = dtemp + Dx(ix)*Dy(iy)
               ix = ix + Incx
               iy = iy + Incy
            ENDDO
            DDOT = dtemp
         ELSE
 
!             code for both increments equal to 1
 
 
!             clean-up loop
 
            m = MOD(N,5)
            IF ( m.NE.0 ) THEN
               DO i = 1 , m
                  dtemp = dtemp + Dx(i)*Dy(i)
               ENDDO
               IF ( N.LT.5 ) GOTO 1
            ENDIF
            mp1 = m + 1
            DO i = mp1 , N , 5
               dtemp = dtemp + Dx(i)*Dy(i) + Dx(i+1)*Dy(i+1) + Dx(i+2)  &
     &                 *Dy(i+2) + Dx(i+3)*Dy(i+3) + Dx(i+4)*Dy(i+4)
            ENDDO
 1          DDOT = dtemp
         ENDIF
      ENDIF
      CONTINUE
      END
!*==DSCAL.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
      SUBROUTINE DSCAL(N,Da,Dx,Incx)
 
!     scales a vector by a constant.
!     uses unrolled loops for increment equal to one.
!     jack dongarra, linpack, 3/11/78.
 
      DOUBLE PRECISION Da , Dx(*)
      INTEGER i , Incx , m , mp1 , N , nincx
 
      IF ( N.GT.0 ) THEN
         IF ( Incx.EQ.1 ) THEN
 
!             code for increment equal to 1
 
 
!             clean-up loop
 
            m = MOD(N,5)
            IF ( m.NE.0 ) THEN
               DO i = 1 , m
                  Dx(i) = Da*Dx(i)
               ENDDO
               IF ( N.LT.5 ) RETURN
            ENDIF
            mp1 = m + 1
            DO i = mp1 , N , 5
               Dx(i) = Da*Dx(i)
               Dx(i+1) = Da*Dx(i+1)
               Dx(i+2) = Da*Dx(i+2)
               Dx(i+3) = Da*Dx(i+3)
               Dx(i+4) = Da*Dx(i+4)
            ENDDO
         ELSE
 
!             code for increment not equal to 1
 
            nincx = N*Incx
            DO i = 1 , nincx , Incx
               Dx(i) = Da*Dx(i)
            ENDDO
         ENDIF
      ENDIF
      CONTINUE
      END
!*==IDAMAX.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      INTEGER FUNCTION IDAMAX(N,Dx,Incx)
 
!     finds the index of element having max. dabsolute value.
!     jack dongarra, linpack, 3/11/78.
 
      DOUBLE PRECISION Dx(*) , dmax
      INTEGER i , Incx , ix , N
 
      IDAMAX = 0
      IF ( N.GE.1 ) THEN
         IDAMAX = 1
         IF ( N.NE.1 ) THEN
            IF ( Incx.EQ.1 ) THEN
 
!                 code for increment equal to 1
 
               dmax = DABS(Dx(1))
               DO i = 2 , N
                  IF ( DABS(Dx(i)).GT.dmax ) THEN
                     IDAMAX = i
                     dmax = DABS(Dx(i))
                  ENDIF
               ENDDO
            ELSE
 
!                 code for increment not equal to 1
 
               ix = 1
               dmax = DABS(Dx(1))
               ix = ix + Incx
               DO i = 2 , N
                  IF ( DABS(Dx(ix)).GT.dmax ) THEN
                     IDAMAX = i
                     dmax = DABS(Dx(ix))
                  ENDIF
                  ix = ix + Incx
               ENDDO
            ENDIF
         ENDIF
      ENDIF
      CONTINUE
      END
!*==EPSLON.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
      REAL*8 FUNCTION EPSLON(X)
      REAL*8 X
 
!     estimate unit roundoff in quantities of size x.
 
      REAL*8 a , b , c , eps
 
!     this program should function properly on all systems
!     satisfying the following two assumptions,
!     1.  the base used in representing dfloating point
!     numbers is not a power of three.
!     2.  the quantity  a  in statement 10 is represented to
!     the accuracy used in dfloating point variables
!     that are stored in memory.
!     the statement number 10 and the go to 10 are intended to
!     force optimizing compilers to generate code satisfying
!     assumption 2.
!     under these assumptions, it should be true that,
!     a  is not exactly equal to four-thirds,
!     b  has a zero for its last bit or digit,
!     c  is not exactly equal to one,
!     eps  measures the separation of 1.0 from
!     the next larger dfloating point number.
!     the developers of eispack would appreciate being informed
!     about any systems where these assumptions do not hold.
 
!     *****************************************************************
!     this routine is one of the auxiliary routines used by eispack iii
!     to avoid machine dependencies.
!     *****************************************************************
 
!     this version dated 4/6/83.
 
      a = 4.0D00/3.0D00
!     a = 4.0d00  * 0.33333333d00
      DO WHILE ( .TRUE. )
         b = a - 1.0D00
         c = b + b + b
         eps = DABS(c-1.0D00)
         IF ( eps.NE.0.0 ) GOTO 1
      ENDDO
 1    EPSLON = eps*DABS(X)
      CONTINUE
      END
!*==MM.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
 
!      DOUBLE PRECISION FUNCTION epslon(x)
!      DOUBLE PRECISION x
 
!     estimate unit roundoff in quantities of size x.
 
!      DOUBLE PRECISION a,b,c,eps
 
!     this program should function properly on all systems
!     satisfying the following two assumptions,
!     1.  the base used in representing dfloating point
!     numbers is not a power of three.
!     2.  the quantity  a  in statement 10 is represented to
!     the accuracy used in dfloating point variables
!     that are stored in memory.
!     the statement number 10 and the go to 10 are intended to
!     force optimizing compilers to generate code satisfying
!     assumption 2.
!     under these assumptions, it should be true that,
!     a  is not exactly equal to four-thirds,
!     b  has a zero for its last bit or digit,
!     c  is not exactly equal to one,
!     eps  measures the separation of 1.0 from
!     the next larger dfloating point number.
!     the developers of eispack would appreciate being informed
!     about any systems where these assumptions do not hold.
 
!     *****************************************************************
!     this routine is one of the auxiliary routines used by eispack iii
!     to avoid machine dependencies.
!     *****************************************************************
 
!     this version dated 4/6/83.
 
!      a = 4.0D0/3.0D0
!     a = 4.0d0 * 0.33333333d0
!	print*, 'In EPSLON'
!      DO WHILE ( .TRUE. )
!          b = a-1.0D0
!          c = b+b+b
!          eps = dabs(c-1.0D0)
!          IF ( eps .NE. 0.0D0 ) go to 1
!    1 END DO
!      print*, 'after loop'
!      epslon = eps*dabs(x)
!      RETURN
!      END
 
      SUBROUTINE MM(A,Lda,N1,N3,B,Ldb,N2,C,Ldc)
      DOUBLE PRECISION A(Lda,*) , B(Ldb,*) , C(Ldc,*)
 
!     purpose:
!     multiply matrix b times matrix c and store the result in matrix a.
 
!     parameters:
 
!     a double precision(lda,n3), matrix of n1 rows and n3 columns
 
!     lda integer, leading dimension of array a
 
!     n1 integer, number of rows in matrices a and b
 
!     n3 integer, number of columns in matrices a and c
 
!     b double precision(ldb,n2), matrix of n1 rows and n2 columns
 
!     ldb integer, leading dimension of array b
 
!     n2 integer, number of columns in matrix b, and number of rows in
!     matrix c
 
!     c double precision(ldc,n3), matrix of n2 rows and n3 columns
 
!     ldc integer, leading dimension of array c
 
!     ----------------------------------------------------------------------
 
      DO j = 1 , N3
         DO i = 1 , N1
            A(i,j) = 0.0
         ENDDO
         CALL DMXPY(N2,A(1,j),N1,Ldb,C(1,j),B)
 
      ENDDO
      CONTINUE
      END
!*==DMXPY.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
      SUBROUTINE DMXPY(N1,Y,N2,Ldm,X,M)
      DOUBLE PRECISION Y(*) , X(*) , M(Ldm,*)
 
!     purpose:
!     multiply matrix m times vector x and add the result to vector y.
 
!     parameters:
 
!     n1 integer, number of elements in vector y, and number of rows in
!     matrix m
 
!     y double precision(n1), vector of length n1 to which is added
!     the product m*x
 
!     n2 integer, number of elements in vector x, and number of columns
!     in matrix m
 
!     ldm integer, leading dimension of array m
 
!     x double precision(n2), vector of length n2
 
!     m double precision(ldm,n2), matrix of n1 rows and n2 columns
 
!     ----------------------------------------------------------------------
 
!     cleanup odd vector
 
      j = MOD(N2,2)
      IF ( j.GE.1 ) THEN
         DO i = 1 , N1
            Y(i) = (Y(i)) + X(j)*M(i,j)
         ENDDO
      ENDIF
 
!     cleanup odd group of two vectors
 
      j = MOD(N2,4)
      IF ( j.GE.2 ) THEN
         DO i = 1 , N1
            Y(i) = ((Y(i))+X(j-1)*M(i,j-1)) + X(j)*M(i,j)
         ENDDO
      ENDIF
 
!     cleanup odd group of four vectors
 
      j = MOD(N2,8)
      IF ( j.GE.4 ) THEN
         DO i = 1 , N1
            Y(i) = ((((Y(i))+X(j-3)*M(i,j-3))+X(j-2)*M(i,j-2))+X(j-1)   &
     &             *M(i,j-1)) + X(j)*M(i,j)
         ENDDO
      ENDIF
 
!     cleanup odd group of eight vectors
 
      j = MOD(N2,16)
      IF ( j.GE.8 ) THEN
         DO i = 1 , N1
            Y(i) = ((((((((Y(i))+X(j-7)*M(i,j-7))+X(j-6)*M(i,j-6))+X(j-5&
     &             )*M(i,j-5))+X(j-4)*M(i,j-4))+X(j-3)*M(i,j-3))+X(j-2) &
     &             *M(i,j-2))+X(j-1)*M(i,j-1)) + X(j)*M(i,j)
         ENDDO
      ENDIF
 
!     main loop - groups of sixteen vectors
 
      jmin = j + 16
      DO j = jmin , N2 , 16
         DO i = 1 , N1
            Y(i) = ((((((((((((((((Y(i))+X(j-15)*M(i,j-15))+X(j-14)*M(i,&
     &             j-14))+X(j-13)*M(i,j-13))+X(j-12)*M(i,j-12))+X(j-11) &
     &             *M(i,j-11))+X(j-10)*M(i,j-10))+X(j-9)*M(i,j-9))      &
     &             +X(j-8)*M(i,j-8))+X(j-7)*M(i,j-7))+X(j-6)*M(i,j-6))  &
     &             +X(j-5)*M(i,j-5))+X(j-4)*M(i,j-4))+X(j-3)*M(i,j-3))  &
     &             +X(j-2)*M(i,j-2))+X(j-1)*M(i,j-1)) + X(j)*M(i,j)
         ENDDO
      ENDDO
      CONTINUE
      END
