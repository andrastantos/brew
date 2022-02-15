!*******************************************************************************
!  Solve a series of large (~1 million variable) symmetric positive-definite 
!  finite difference matrix equations using the conjugate gradient method
!  preconditioned by the nested factorization algorithm described by 
!  "J. APPLEYARD AND I. CHESHIRE, Nested factorization, in Reservoir 
!  Simulation Symposium of the SPE, 1983. Paper 12264."  The method works 
!  well with very stiff equations which defeat other iterative methods, and 
!  is similar to that implemented in the widely used Eclipse oil reservoir
!  simulator.
!
!  For a matrix 
!
!     A = D + L1 + U1 + L2 + U2 + L3 + U3
!
!  where D is the matrix diagonal, 
!        L1 and U1 are the bands connecting adjacent cells within a line,  
!        L2 and U2 are the bands connecting adjacent lines within a plane, 
!    and L3 and U3 are the bands connecting adjacent planes of the FD grid,
!
!          D U1  U1    U3              Finite Difference Matrix for
!         L1 D U1  U2    U3            nx=3, ny=2, nz=2
!           L1 D     U2    U3
!         L2     D U1        U3
!           L2  L1 D U1        U3
!             L2  L1 D           U3
!         L3           D U1  U2
!           L3        L1 D U1  U2
!             L3        L1 D     U2
!               L3    L2     D U1
!                 L3    L2  L1 D U1
!                   L3    L2  L1 D
!          
!  the preconditioning matrix, B^-1, is defined by the recursive equations
! 
!     B = (P+L3)(I+P^-1.U3)   - B is block tridiagonal - block elements are planes
!     P = (T+L2)(I+T^-1.U2)   - P is block tridiagonal - block elements are lines
!     T = (G+L1)(I+G^-1.U1)   - T is tridiagonal
!     G = D - L1.G^-1.U1
!           - ColSum(E)       - G is diagonal
!     E = L2.T^-1.U2 - L3.P^-1.*U3
!
!  ColSum(E) is the diagonal matrix formed by summing the elements of E in 
!  columns.  Although, the definition of G is recursive, close examination 
!  shows that each element depends only on the previous cell, line or plane.
!
!  After expansion, we obtain
!
!     B = A + E - Colsum(E)
!
!  The fact that the column sum of the error matrix is zero is useful in 
!  some cases - for example it ensures zero material balance error for every
!  plane in some CFD problems.  It can also be used as a check on the
!  correctness of the implementation.
!
!  This preconditioning is usually most effective when the L1, and U1 bands are 
!  numerically largest, followed by L2, U2 and, finally, L3, U3.  This happens
!  naturally in some problem domains - for example if the vertical extent of 
!  finite difference grid blocks is smaller than the horizontal extent.  
!*******************************************************************************
program NF
implicit none

!  Theses tests require about 120MBytes
!            nx  ny nz bd1 bd2 bd3 stiff   it  rms method
call mattest(97,105,99,1D2,1D1,1D0,1000D0, 50,1D-7,'NFCG')  ! NFCG best case - band1>band2>band3
call mattest(97,105,99,1D2,1D2,1D0,1000D0, 50,1D-7,'NFCG')  ! NFCG band1=band2>band3
call mattest(97,105,99,1D1,1D2,1D0,100D0 , 50,1D-7,'NFCG')  ! NFCG bad ordering - band1<band2
                                                            !      but reduced stiffness
call mattest(97,105,99,1D1,1D1,1D1,1000D0, 50,1D-7,'NFCG')  ! NFCG all bands the same
!call mattest(97,105,99,1D2,1D1,1D0,10D0  ,150,1D-7,'ICCG')  ! ICCG takes too long with same
!                                                            !      stiffness as NFCG
!call mattest(97,105,99,1D2,1D1,1D0,10D0  ,200,1D-7,'SIP3D0.92')  
!                                                            ! SIP3D with alpha=0.92
!call mattest(97,105,99,1D2,1D1,1D0,1D0   ,150,1D-7,'CGSTAB')! CGSTAB (maybe not working?)

!  The following require an extra 240MBytes for the vector tridiagonal solver

!call mattest(97,105,99,1D2,1D1,1D0,1000D0, 50,1D-7,'VNFCG7')! VNFCG best case - band1>band2>band3
!call mattest(97,105,99,1D2,1D1,1D0,1000D0, 50,1D-7,'VNFCG6')! invwstigate effect of approximation
!call mattest(97,105,99,1D2,1D1,1D0,1000D0, 50,1D-7,'VNFCG5')! in tridiag solver
!call mattest(97,105,99,1D2,1D1,1D0,1000D0, 50,1D-7,'VNFCG4')! 
end program NF

!*******************************************************************************
!  Set up and solve matrix representing nx * ny * nz finite difference grid
!  Elements on 1st band (1     above and below diagonal) are set to band1
!  Elements on 2nd band (nx    above and below diagonal) are set to band2
!  Elements on 3rd band (nx*ny above and below diagonal) are set to band3
!  All bands have gaps appropriate for nx * ny * nz geometry
!  Elements on diagonal = -sum(band elements in same column) - 1.0/stiffness
!  Up to maxiter iterations are allowed to reduce the rms residual to targrms
!  method='NFCG', 'ICCG', 'SIP3D0.nn' (alpha = 0.nn), 'VNFCGn'
!*******************************************************************************

subroutine mattest(nx,ny,nz,band1,band2,band3,stiffness,maxiter,targrms,method)
implicit none
integer,parameter :: dpkind=kind(1.0D0)
integer :: nx , ny , nz , maxiter
character(*) :: method
real(dpkind) :: band1 , band2 , band3 , stiffness , targrms , alfa
real(dpkind),allocatable,dimension(:) :: ad,au1,au2,au3,x,b
integer :: nxyz , nxy , i , cycles

write(*,'(/A,I4,A,I4,A,I4,A,A)') &
            ' Solve ',nx,' by',ny,' by',nz,' FD problem - method=',method
write(*,'(A,F8.2,A,F8.2,A,F8.2,A,F10.2)') &
            ' Band elements are ',band1,', ',band2, &
            ', and ',band3,'. stiffness=',stiffness
write(*,'(A,G13.4,A,I4/)') ' Target RMS residual =',targrms,' Maximum Iterations =',maxiter
                           
                                     ! Set up matrix
nxy = nx*ny ; nxyz = nxy*nz
allocate(ad(nxyz),au1(nxyz),au2(nxyz),au3(nxyz),x(nxyz),b(nxyz))
au1 = band1 ; au2 = band2 ; au3 = band3
au1(nx:nxyz:nx) = 0.0
do i = nxy , nxyz , nxy  
   au2(i-nx+1:i) = 0.0d0   
enddo                           
au3(nxyz-nxy+1:) = 0.0d0
ad = -1.0/stiffness - au1 - au2 - au3
ad(2:nxyz)     = ad(2:nxyz)     - au1(1:nxyz-1)
ad(nx+1:nxyz)  = ad(nx+1:nxyz)  - au2(1:nxyz-nx)
ad(nxy+1:nxyz) = ad(nxy+1:nxyz) - au3(1:nxyz-nxy)

b=0.0 ; b(1) = 100.0                 ! set up rhs
x=0.0                                ! set up initial solution vector

                                     ! solve using requested method 
if ( method=='NFCG' ) then                                    
   call nfcg(nx,nxy,nxyz,ad,au1,au2,au3,x,b,maxiter,targrms)
!elseif ( method(1:5)=='VNFCG' ) then
!   read(method(6:6),'(I1)') cycles
!   call vnfcg(nx,nxy,nxyz,ad,au1,au2,au3,x,b,maxiter,targrms,cycles)
!elseif ( method=='ICCG' ) then                                        
!   call iccg(nx,nxy,nxyz,ad,au1,au2,au3,x,b,maxiter,targrms)
!elseif ( method(1:5)=='SIP3D' ) then                      
!   read(method(6:),*) alfa
!   call sip3d(nx,nxy,nxyz,ad,au1,au2,au3,x,b,maxiter,targrms,alfa)
!elseif ( method=='CGSTAB' ) then                    
!   call cgstab(nx,nxy,nxyz,ad,au1,au2,au3,x,b,maxiter,targrms)
endif

deallocate(ad,au1,au2,au3,x,b)
end subroutine mattest
!*******************************************************************************

!*******************************************************************************
!  Solve using conjugate gradients pre-conditioned by nested factorization
!*******************************************************************************
subroutine nfcg(nx,nxy,nxyz,ad,au1,au2,au3,x,b,maxiter,targrms)  
implicit none ; integer,parameter :: dpkind=kind(1.0D0)
integer :: nx , nxy , nxyz , maxiter
real(dpkind),dimension(nxyz):: ad,au1,au2,au3,x,b
real(dpkind)::targrms

real(dpkind),allocatable,dimension(:) :: r,q,p,z,g,gi
real(dpkind):: alpha,beta,qr,qrp,rmserr
integer :: iter , tbase , tgi , tcg , tickspersec , maxticks

allocate (gi(nxyz),g(nxyz))
call system_clock(tbase,tickspersec,maxticks)
call GetGI3D(1,nxyz)                 ! compute gi for preconditioning matrix    
call system_clock(tgi,tickspersec,maxticks)
deallocate(g)

allocate (r(nxyz),q(nxyz),p(nxyz),z(nxyz))
CALL SPMMULT(x,r) ; r = b - r        ! compute initial residual vector

write(*,'(A)') ' Iter      Alpha        Beta     RMS Residual   Sum of Residuals'
write(*,'(I4,24X,2G18.7)') 0,sqrt(DOT_PRODUCT(r,r)/nxyz),sum(r)

                                     !  Do a single iteration with alpha =1 
                                     !  to reduce sum of residuals to 0
p = r ; CALL NF3DPrecon(p,1,nxyz) ; CALL SPMMULT(p,z)
x = x + p ; r = r - z
write(*,'(I4,F12.5,12X,2G18.7)') 0,1.0,sqrt(DOT_PRODUCT(r,r)/nxyz),sum(r)

do iter = 1 , maxiter
   q = r ; CALL NF3DPrecon(q,1,nxyz)
   qr = DOT_PRODUCT(q,r)
   if ( iter==1 ) then
      beta = 0.0
      p = q
   else
      beta = qr/qrp
      p = q + beta*p
   endif
   qrp = qr
   CALL SPMMULT(p,z)
   alpha = qr/DOT_PRODUCT(p,z)
   x = x + alpha*p ; r = r - alpha*z
   rmserr = sqrt(DOT_PRODUCT(r,r)/nxyz)
   write(*,'(I4,2F12.5,3G18.7)') iter , alpha , beta , rmserr , sum(r)! , maxval(abs(r))
   if ( rmserr<targrms ) exit
enddo
call system_clock(tcg,tickspersec,maxticks)
write(*,'(/A,F10.3/A,F10.3/A,F10.3)') ' Time for setup     ',REAL(tgi-tbase)/REAL(tickspersec) , &
                                      ' Time per iteration ',REAL(tcg-tgi)/REAL(tickspersec*min(iter,maxiter)) , &
                                      ' Total Time         ',REAL(tcg-tbase)/REAL(tickspersec)
deallocate(r,q,p,z,gi)
contains
                                     !=========================================
                                     ! Banded matrix multiply b = A.x =========                                     
subroutine spmmult(x,b)
real(dpkind),dimension(nxyz):: x,b
b = ad*x
b(1:nxyz-1)  = b(1:nxyz-1)  + au1(1:nxyz-1) *x(2:nxyz)
b(2:nxyz)  = b(2:nxyz)  + au1(1:nxyz-1) *x(1:nxyz-1) ! symmetric - so use au for lower band too
b(1:nxyz-nx)  = b(1:nxyz-nx)  + au2(1:nxyz-nx) *x(nx+1:nxyz)
b(nx+1:nxyz)  = b(nx+1:nxyz)  + au2(1:nxyz-nx) *x(1:nxyz-nx)
b(1:nxyz-nxy) = b(1:nxyz-nxy) + au3(1:nxyz-nxy)*x(nxy+1:nxyz)
b(nxy+1:nxyz) = b(nxy+1:nxyz) + au3(1:nxyz-nxy)*x(1:nxyz-nxy)
end subroutine spmmult               !=========================================

                                     !=========================================
subroutine GetGI1D(i1,i2)            ! compute gi for a line of cells =========
integer :: i1 , i2
integer :: i
gi(i1) = 1.0D0/g(i1)
do i = i1+1,i2
   g(i) = g(i) - gi(i-1)*au1(i-1)*au1(i-1)
   gi(i) = 1.0D0/g(i)
enddo
end subroutine GetGI1D               !=========================================

                                     !=========================================
subroutine trisolve(x,i1,i2)         ! solve along a single line of cells =====
integer :: i1 , i2
real(dpkind),dimension(i2)::x
integer :: i
x(i1) = gi(i1)* x(i1)
do i = i1+1 , i2
   x(i) = gi(i)*(x(i)-au1(i-1)*x(i-1))
enddo
do i = i2-1 , i1 , -1
   x(i) = x(i) - gi(i)*au1(i)*x(i+1)
enddo
end subroutine trisolve              !=========================================

                                     !=========================================
subroutine GetGI2D(i1,i2)            ! compute gi for a plane of cells ========
integer :: i1 , i2
integer :: i
do i = i1 , i2 , nx                  ! advance one line at a time
   if ( i>i1 ) then                  ! get contribution from previous line
      g(i-nx:i-1) = au2(i-nx:i-1)
      call trisolve(g,i-nx,i-1)
      g(i:i+nx-1) = g(i:i+nx-1) - au2(i-nx:i-1)*g(i-nx:i-1)
   endif
   call GetGI1D(i,i+nx-1)            ! get contribution from this line
enddo
end subroutine GetGI2D               !=========================================

                                     !=========================================
                                     ! solve for a plane of cells using  ======
subroutine NF2DPrecon(x,i1,i2)       ! 2D NF Preconditioning matrix
integer :: i1 , i2
real(dpkind),dimension(i2)::x,t
integer :: i
do i = i1 , i2 , nx
   if ( i>i1 ) x(i:i+nx-1) = x(i:i+nx-1) - au2(i-nx:i-1)*x(i-nx:i-1)
   call trisolve(x,i,i+nx-1)
enddo 
do i = i2-2*nx+1 , i1 , -nx
   t(i:i+nx-1) = au2(i:i+nx-1)*x(i+nx:i+2*nx-1)
   call trisolve(t,i,i+nx-1)
   x(i:i+nx-1) = x(i:i+nx-1) - t(i:i+nx-1)
enddo
end subroutine NF2DPrecon            !=========================================

                                     !=========================================
subroutine GetGI3D(i1,i2)            ! compute gi for a 3D block of cells =====
integer :: i1 , i2
integer :: i
g = ad
do i = i1 , i2 , nxy                 ! advance one plane at a time
   if ( i>i1 ) then                  ! get contribution from previous plane 
      g(i-nxy:i-1) = au3(i-nxy:i-1)
      call NF2DPrecon(g,i-nxy,i-1)
      g(i:i+nxy-1) = g(i:i+nxy-1) - au3(i-nxy:i-1)*g(i-nxy:i-1)
   endif
   call GetGI2D(i,i+nxy-1)           ! get contribution from this plane
enddo
end subroutine GetGI3D               !=========================================

                                     !=========================================
                                     ! solve for a 3D block of cells using 
subroutine NF3DPrecon(x,i1,i2)       ! 3D Preconditioning matrix
integer :: i1 , i2
real(dpkind),dimension(i2)::x,t
integer :: i
do i = i1 , i2 , nxy
   if ( i>i1 ) x(i:i+nxy-1) = x(i:i+nxy-1) - au3(i-nxy:i-1)*x(i-nxy:i-1)
   call NF2DPrecon(x,i,i+nxy-1)
enddo   
do i = i2-2*nxy+1 , i1 , -nxy
   t(i:i+nxy-1) = au3(i:i+nxy-1)*x(i+nxy:i+2*nxy-1)
   call NF2DPrecon(t,i,i+nxy-1)
   x(i:i+nxy-1) = x(i:i+nxy-1) - t(i:i+nxy-1)
enddo
end subroutine NF3DPrecon            !=========================================

end subroutine nfcg                  
!******************************************************************************

!include '\nf\vnfcg.f90'
!include '\nf\cgstab.f90'
!include '\nf\iccg.f90'
!include '\nf\sip3d.f90'
