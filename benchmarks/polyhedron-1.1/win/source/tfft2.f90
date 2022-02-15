!*==TFFT.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!   SPAG options set to convert source form only
      PROGRAM TFFT
      PARAMETER (NN=33554432)           ! RJA was 1048576 (2^20), changed to 33554432 (2^25)
      REAL*4 x(NN) , trg(NN/4)
      INTEGER*4 ibi(NN/2)
      INTEGER*4 i0 , i1 , ia , ib
      LOGICAL pri
 
      n = NN                            ! Fake these answers for testing
      pri = .FALSE.
      ntrial = 10                     ! RJA - Change this to increase iterations. Originally 10.
      PRINT * , 'N = ' , n
!
      IF ( pri ) OPEN (UNIT=7,FILE='T.OUT',FORM='FORMATTED')
!
      DO i = 1 , n
         x(i) = 0
      ENDDO
      DO i = 1 , 10
         x(i) = 1
      ENDDO
      DO i = n - 8 , n
         x(i) = 1
      ENDDO
 
      DO itrial = 1 , ntrial
!
         IF ( pri ) THEN
            PRINT * , 'Writing Original Time Series'
            CALL OUT1(x,n,'ORIGINAL TIME SERIES','X')
         ENDIF
!
         PRINT * , 'FFTfwd, trial ' , itrial
!      IF (NTRIAL.EQ.1) CALL TIMER(I0)
         CALL RFFT(x,trg,ibi,n,+1)
         IF ( pri ) THEN
            PRINT * , 'Writing Transform'
            CALL OUT1C(x,n/2,'TRANSFORM','XT')
         ENDIF
!
         PRINT * , 'FFTinv, trial ' , itrial
!      IF (NTRIAL.EQ.1) CALL TIMER(I0)
         CALL RFFT(x,trg,ibi,n,-1)
         IF ( pri ) THEN
            PRINT * , 'Writing Inverse'
            CALL OUT1(x,n,'TEST INVERSION','X')
         ENDIF
!
      ENDDO
      errtot = 0
      DO i = 1 , 10
         errtot = errtot + (x(i)-1)**2
      ENDDO
      DO i = 11 , n - 9
         errtot = errtot + x(i)**2
      ENDDO
      DO i = n - 8 , n
         errtot = errtot + (x(i)-1)**2
      ENDDO
      errrms = SQRT(errtot/FLOAT(n))
      PRINT 640 , errrms
 640  FORMAT (' RMS Error = ',1PE15.7)
!
      CONTINUE
      END
!*==OUT1.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE OUT1(X,N,Title,Colhed)
      DIMENSION X(*)
      CHARACTER Title*(*) , Colhed*(*) , fmt1*11 , fmt2*17
!
      la = MIN(LEN_TRIM(Title),70)
      ls = (72-la)/2
      l10 = ls/10
      l1 = MOD(ls,10)
      fmt1 = '(1H1,10X,A)'
!      FMT1(6:7)=CHAR(48+L10)//CHAR(48+L1)
      fmt1(6:6) = CHAR(48+l10)
      fmt1(7:7) = CHAR(48+l1)
!
      lb = MIN(LEN_TRIM(Colhed),8)
      ls = 9 - lb
      l10 = ls/10
      l1 = MOD(ls,10)
      fmt2 = '(1X,6(01X,A8,3X))'
!      FMT2(7:8)=CHAR(48+L10)//CHAR(48+L1)
      fmt2(7:7) = CHAR(48+l10)
      fmt2(8:8) = CHAR(48+l1)
      fmt2(12:12) = CHAR(48+lb)
!
      i1 = 1
 10   i2 = MIN(i1+49,N)
      WRITE (7,fmt1) Title(1:la)
!
      i0 = MIN(1+((N-i1)/50),6)
      WRITE (7,fmt2) (Colhed(1:lb),i=1,i0)
!
      DO i = i1 , i2
         j2 = 50*MIN((N-i)/50,5)
         WRITE (7,60) (X(i+j),j=0,j2,50)
 60      FORMAT (1X,1P,6E12.4)
      ENDDO
      i1 = i1 + 300
      IF ( i1.LT.N ) GOTO 10
      CONTINUE
      END
!*==OUT1C.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
      SUBROUTINE OUT1C(X,N,Title,Colhed)
      REAL X(2,*)
      CHARACTER Title*(*) , Colhed*(*) , fmt1*11 , fmt2*33
!
      la = MIN(LEN_TRIM(Title),70)
      ls = (72-la)/2
      l10 = ls/10
      l1 = MOD(ls,10)
      fmt1 = '(1H1,10X,A)'
!      FMT1(6:7)=CHAR(48+L10)//CHAR(48+L1)
      fmt1(6:6) = CHAR(48+l10)
      fmt1(7:7) = CHAR(48+l1)
!
      lb = MIN(LEN_TRIM(Colhed),8)
      ls = 9 - lb
      l10 = ls/10
      l1 = MOD(ls,10)
      fmt2 = '(1X,3(01X,3HRE ,A8,01X,3HIM ,A8))'
!      FMT2(7:8)=CHAR(48+L10)//CHAR(48+L1)
      fmt2(7:7) = CHAR(48+l10)
      fmt2(8:8) = CHAR(48+l1)
      fmt2(20:21) = fmt2(7:8)
      fmt2(18:18) = CHAR(48+lb)
      fmt2(31:31) = fmt2(18:18)
!
      i1 = 1
 10   i2 = MIN(i1+49,N)
      WRITE (7,fmt1) Title(1:la)
!
      i0 = MIN(1+((N-i1)/50),6)
      WRITE (7,fmt2) (Colhed(1:lb),i=1,i0)
!
      DO i = i1 , i2
         j2 = 50*MIN((N-i)/50,2)
         WRITE (7,60) (X(1,i+j),X(2,i+j),j=0,j2,50)
 60      FORMAT (1X,1P,6E12.4)
      ENDDO
      i1 = i1 + 150
      IF ( i1.LT.N ) GOTO 10
      CONTINUE
      END
!*==RFFT.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
      SUBROUTINE RFFT(X,Trg,Ibi,Npts,Idir)
      DIMENSION X(*) , Trg(*) , Ibi(*)
      SAVE npsold , mpts
      DATA npsold/0/ , twopi/6.28318531/
!
!     Fourier Transform of Real-Valued Time Series
!     If IDIR > 0, forward transform
!       Input:  X is real-values time series
!       Output: X is transform with hermetian symmetry.
!               X(0) is zero frequency component
!               X(1) is Nyquist frequency component
!               X(i) is real part of i-frequency component
!               X(i+1) is imaginary part of i-th frequency component
!     If IDIR < 0, inverse transform
!     IBI is bit-reversal table;  must be dimensionned at least NPTS/2
!     TRG is trigonometric table; must be dimensionned at least NPTS/4
!     Note that time and frequency spacing are related by:
!       (delta t) (delta omega) = 2 pi.
!
      IF ( Npts.LE.0 ) THEN
         PRINT 10 , Npts
         STOP
      ENDIF
      IF ( Npts.EQ.1 ) RETURN
      DO nu = 1 , 30                ! RJA Loop endpoint is the largest power of two we can use for NN. Increased from 20 to 30.
         m = 2**nu
         IF ( m.EQ.Npts ) GOTO 40
      ENDDO
      PRINT 10 , Npts
      STOP
!
 40   xpts = 1./FLOAT(Npts)
      IF ( Idir.LT.0 ) THEN
         DO i = 1 , Npts
            X(i) = X(i)*xpts
         ENDDO
      ENDIF
      IF ( Npts.EQ.2 ) GOTO 430
      lpts = Npts/4
      const = twopi*xpts
!
      IF ( Npts.NE.npsold .OR. Idir.EQ.0 ) THEN
!                                      Form Bit-Reversal Table
         npsold = Npts
         i = 1
         Ibi(1) = 0
         ihi = 1
         mu = 2*lpts
         mpts = mu
         new = nu - 1
         DO j = 1 , new
            mu = mu/2
            DO k = 1 , ihi
               i = i + 1
               Ibi(i) = Ibi(k) + mu
            ENDDO
            ihi = ihi*2
         ENDDO
!                                      Form Trigonometric Table
         DO i = 1 , lpts
            Trg(i) = COS(const*FLOAT(i-1))
         ENDDO
      ENDIF
      IF ( Idir.EQ.0 ) RETURN
!
!     Reconstruct Transform of complex sequence Y(K)+i*Z(K)
!     (Inverse Transform Only)
!
      IF ( Idir.LT.0 ) THEN
         sr = X(1)
         X(1) = sr + X(2)
         X(2) = sr - X(2)
         X(mpts+1) = 2.*X(mpts+1)
         X(mpts+2) = -2.*X(mpts+2)
         IF ( Npts.EQ.4 ) GOTO 140
         ihi = mpts - 1
         ifold = Npts - 1
         icos = 1
         isin = lpts - 1
!
         DO i = 3 , ihi , 2
            yr = X(i) + X(ifold)
            yi = X(i+1) - X(ifold+1)
            zr = X(i) - X(ifold)
            zi = X(i+1) + X(ifold+1)
            sr = Trg(icos+1)*zr - Trg(isin+1)*zi
            si = Trg(icos+1)*zi + Trg(isin+1)*zr
            X(i) = yr - si
            X(i+1) = sr + yi
            X(ifold) = yr + si
            X(ifold+1) = sr - yi
            ifold = ifold - 2
            icos = icos + 1
            isin = isin - 1
         ENDDO
      ENDIF
!
!     Rearrange order of input points
!
      DO i = 1 , mpts
         j = Ibi(i)
         IF ( j.GT.(i-1) ) THEN
            m = 2*i - 1
            n = 2*j + 1
            sr = X(m)
            si = X(m+1)
            X(m) = X(n)
            X(m+1) = X(n+1)
            X(n) = sr
            X(n+1) = si
         ENDIF
      ENDDO
!
!     Find Discrete Fourier Transform of
!     <X(1)+iX(2)>,<X(3)+iX(4)>,...,<X(NPTS-1)+iX(NPTS)>
!
 140  DO i = 2 , nu
         igap = 2**i
         jgap = igap/2
         l = jgap + 1
         khi = Npts + 1 - igap
!
         DO k = 1 , khi , igap
            sr = X(k)
            si = X(k+1)
            X(k) = sr + X(l)
            X(k+1) = si + X(l+1)
            X(l) = sr - X(l)
            X(l+1) = si - X(l+1)
            l = l + igap
         ENDDO
!
         IF ( nu.EQ.2 .AND. Idir.GT.0 ) GOTO 420
         IF ( i.EQ.2 ) GOTO 400
!
         kgap = jgap/2
         klo = kgap + 1
         l = klo + jgap
         khi = Npts + 1 - igap + kgap
!
         DO k = klo , khi , igap
            sr = X(l)
            si = X(l+1)
            IF ( Idir.GT.0 ) THEN
               X(l) = X(k) - si
               X(l+1) = X(k+1) + sr
               X(k) = X(k) + si
               X(k+1) = X(k+1) - sr
            ELSE
               X(l) = X(k) + si
               X(l+1) = X(k+1) - sr
               X(k) = X(k) - si
               X(k+1) = X(k+1) + sr
            ENDIF
            l = l + igap
         ENDDO
!
         IF ( i.EQ.3 ) GOTO 400
         num = 2**(nu-i)
         int = 2*num
         ihi = lpts - int
         k = 3
!                                     Forward Transform
         IF ( Idir.GT.0 ) THEN
            DO j = 1 , num
               l = k + jgap
               m = k + kgap
               n = m + jgap
               isin = ihi
               DO icos = int , ihi , int
                  yr = Trg(icos+1)*X(l) + Trg(isin+1)*X(l+1)
                  yi = Trg(icos+1)*X(l+1) - Trg(isin+1)*X(l)
                  X(l) = X(k) - yr
                  X(l+1) = X(k+1) - yi
                  X(k) = X(k) + yr
                  X(k+1) = X(k+1) + yi
                  yr = Trg(icos+1)*X(n+1) - Trg(isin+1)*X(n)
                  yi = Trg(icos+1)*X(n) + Trg(isin+1)*X(n+1)
                  X(n) = X(m) - yr
                  X(n+1) = X(m+1) + yi
                  X(m) = X(m) + yr
                  X(m+1) = X(m+1) - yi
                  k = k + 2
                  l = l + 2
                  m = m + 2
                  n = n + 2
                  isin = isin - int
               ENDDO
               k = n + 2
            ENDDO
         ELSE
!                                     Inverse Transform
            DO j = 1 , num
               l = k + jgap
               m = k + kgap
               n = m + jgap
               isin = ihi
               DO icos = int , ihi , int
                  yr = Trg(icos+1)*X(l) - Trg(isin+1)*X(l+1)
                  yi = Trg(icos+1)*X(l+1) + Trg(isin+1)*X(l)
                  X(l) = X(k) - yr
                  X(l+1) = X(k+1) - yi
                  X(k) = X(k) + yr
                  X(k+1) = X(k+1) + yi
                  yr = Trg(icos+1)*X(n+1) + Trg(isin+1)*X(n)
                  yi = Trg(icos+1)*X(n) - Trg(isin+1)*X(n+1)
                  X(n) = X(m) + yr
                  X(n+1) = X(m+1) - yi
                  X(m) = X(m) - yr
                  X(m+1) = X(m+1) + yi
                  k = k + 2
                  l = l + 2
                  m = m + 2
                  n = n + 2
                  isin = isin - int
               ENDDO
               k = n + 2
            ENDDO
         ENDIF
 400  ENDDO
!
!     Decode Output (Forward Transform Only)
!
      IF ( Idir.LT.0 ) RETURN
      ihi = mpts - 1
      ifold = Npts - 1
      icos = 1
      isin = lpts - 1
!
      DO i = 3 , ihi , 2
         yr = .5*(X(i)+X(ifold))
         yi = .5*(X(i+1)-X(ifold+1))
         zr = .5*(X(i+1)+X(ifold+1))
         zi = .5*(X(ifold)-X(i))
         sr = Trg(icos+1)*zr + Trg(isin+1)*zi
         si = Trg(icos+1)*zi - Trg(isin+1)*zr
         X(i) = yr + sr
         X(i+1) = si + yi
         X(ifold) = yr - sr
         X(ifold+1) = si - yi
         ifold = ifold - 2
         icos = icos + 1
         isin = isin - 1
      ENDDO
!
 420  X(mpts) = -X(mpts)
 430  sr = X(1)
      X(1) = sr + X(2)
      X(2) = sr - X(2)
!
      CONTINUE
 10   FORMAT (' Number of points (',I5,') is not a power of two.')
      END
!*==LEN_TRIM.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      FUNCTION LEN_TRIM(A)
      CHARACTER*(*) A
      l = LEN(A)
      DO i = l , 1 , -1
         IF ( A(i:i).NE.' ' ) THEN
            LEN_TRIM = i
            RETURN
         ENDIF
      ENDDO
      CONTINUE
      END
