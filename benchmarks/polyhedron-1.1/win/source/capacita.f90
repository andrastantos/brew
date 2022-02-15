
module solv_cap
!
! Compute electric charge distribution on flat conductor
! Version 1.51, Jul 1998, J. Bergervoet

  implicit none

  public  :: solveC, solveP, init_solve

  private :: solve, mv_prod, prod0, prod1, Preco, FourirG, Ginteg, Vprim,  &
             fourir, Fourir2D, Fourir2DX, cgstab, div_care

  integer, parameter, public :: dp = selected_real_kind(5)
!   kind=dp is the default precision (select 5 for single, 15 for double)

  real(kind=dp), private :: Pi, Mu0, c0, eps0
  logical,       private :: UseFFT, UsePreco
  real(kind=dp), private :: D1, D2
  integer,       private, save :: Ng1=0, Ng2=0
  integer,       private, pointer,     dimension(:,:)  :: Grid
  real(kind=dp), private, allocatable, dimension(:,:)  :: G

contains


  subroutine init_solve(Grid_in, GrSize1, GrSize2, UseFFT_in, UsePreco_in)
    integer, intent(in), target, dimension(:,:) :: Grid_in
    real(kind=dp), intent(in)  :: GrSize1, GrSize2
    logical,       intent(in)  :: UseFFT_in, UsePreco_in
    integer                    :: i, j

    Pi = acos(-1.0_dp)
    Mu0 = 4e-7_dp * Pi
    c0 = 299792458             ! Speed of light in m/s (def)
    eps0 = 1 / (Mu0 * c0**2)
    
    UseFFT = UseFFT_in
    UsePreco = UsePreco_in

    if(Ng1 /= 0 .and. allocated(G) ) then
      deallocate( G )
    end if

    Grid => Grid_in
    Ng1 = size(Grid, 1)
    Ng2 = size(Grid, 2)
    D1 = GrSize1/Ng1
    D2 = GrSize2/Ng2           ! The elements have size D1 by D2, in meters.

    allocate( G(0:Ng1,0:Ng2) ) ! who does dealloc ???

    write(unit=*, fmt=*) "Calculating G"
    do i=0,Ng1
      do j=0,Ng2
        G(i,j) = Ginteg( -D1/2,-D2/2, D1/2,D2/2, i*D1,j*D2 )
      end do
    end do

    if(UseFFT) then
      write(unit=*, fmt=*) "Transforming G"
      call FourirG(G,1)
    end if

    return
  end subroutine init_solve


  subroutine solveC(C)
    real(kind=dp), intent(out) :: C

    real(kind=dp), allocatable, dimension(:,:)  :: Y0, X

    allocate( Y0(Ng1,Ng2), X(Ng1,Ng2) )

    Y0 = Grid                      ! RHS: V=1, where metal on grid

    call solve( X, Y0 )

    C = sum(X)*D1*D2 * 4*Pi*eps0   ! Capacitance of object

    deallocate( X, Y0 )
    return
  end subroutine solveC


  subroutine solveP(P)
    real(kind=dp), intent(out) :: P

    real(kind=dp), allocatable, dimension(:,:)  :: Y0, X
    integer :: i,j

    allocate( Y0(Ng1,Ng2), X(Ng1,Ng2) )

    do i=1,Ng1
      do j=1,Ng2
        Y0(i,j) = D1 * (i-(Ng1+1)/2.0_dp) * Grid(i,j)
      end do
    end do     ! RHS for in-field E_x=1.  V = -V_in = x, where metal on grid

    call solve( X, Y0 )

    X = X - sum(X)/size(X)         ! get rid of monopole term ..
    do i=1,Ng1                     !   .. and multiply charge with x
      do j=1,Ng2
        X(i,j) = X(i,j) * D1 * (i-(Ng1+1)/2.0_dp)
      end do
    end do
    P = sum(X)*D1*D2 * 4*Pi*eps0   ! E-dipole moment in 1 V/m field

    deallocate( X, Y0 )
    return
  end subroutine solveP


  subroutine solve(X, Y)
    !
    ! Solve : Mvprod * X = Y
    !
    real(kind=dp), intent(in),  dimension(:,:)  :: Y
    real(kind=dp), intent(out), dimension(:,:)  :: X
    real(kind=dp), allocatable, dimension(:)    :: Ar1
    integer  :: iter

    allocate( Ar1(Ng1*Ng2) )

    Ar1 = reshape( Y, (/Ng1*Ng2/) )        ! put RHS in 1-dim array

    write(unit=*, fmt=*) "Starting Bi-CGSTAB, for accuracy 1e-6,   N=",Ng1*Ng2
    call cgstab( Ar1, 1e-6_dp, Ng1+Ng2+20, iter )

    X = reshape( Ar1, (/Ng1,Ng2/) )        ! put sulution in 2-dim array

    call mv_prod( Ar1, Ar1 )               ! do last check

    write(unit=*, fmt=*) iter, " iterations,   RHS_error =",  &
      maxval(abs( reshape(Ar1,(/Ng1,Ng2/)) - Y ))

    if(UsePreco) then
      call Preco(X)
    end if

    deallocate( Ar1 )
    return
  end subroutine solve


  subroutine mv_prod(X, Yp)         ! Matrix-vector product (vector = grid)
    real(kind=dp), intent(in),  dimension(:)   :: X
    real(kind=dp), intent(out), dimension(:)   :: Yp
    real(kind=dp), allocatable, dimension(:,:) :: Y

    allocate( Y(Ng1,Ng2) )

    Y = reshape(X, (/Ng1,Ng2/))    ! Put charge-vector on Grid Y

    if(UsePreco) then
      call Preco(Y)
    end if
    
    where (Grid==0)                ! Restrict charges to conductor sub-space.
      Y = 0                        ! This projection reduces the rank of the
    end where                      ! operator. Iterative solvers allow that.

    if(UseFFT) then
      call prod1(G, Y)
    else
      call prod0(G, Y)
    end if

    where (Grid==0)                ! Restrict potential to conductor area.
      Y = 0                        ! The RHS was also contained in this
    end where                      ! subspace, that's why it works.

    Yp = reshape(Y,(/Ng1*Ng2/))

    deallocate( Y )
    return
  end subroutine mv_prod


  subroutine prod0( G, X )
  !   Apply Green function, no Fourier tfm.:  X = G o X
    real(kind=dp), intent(in),     dimension(0:,0:)  :: G
    real(kind=dp), intent(in out), dimension(:,:)    :: X
    integer   :: i, j, ip, jp
    real(kind=dp), dimension(size(X,1),size(X,2)) :: Y

    do i=1,Ng1
      do j=1,Ng2
        Y(i,j) = 0
        do ip=1,Ng1
          do jp=1,Ng2
            Y(i,j) = Y(i,j) + G(abs(i-ip),abs(j-jp)) * X(ip,jp)
          end do
        end do
      end do
    end do
    X = Y

    return
  end subroutine prod0


  subroutine prod1(G, X)
  !   Apply Green function with Fourier tfm.:  X = G o X
    real(kind=dp), intent(in),     dimension(0:,0:)  :: G
    real(kind=dp), intent(in out), dimension(0:,0:)  :: X
    complex(kind=dp), allocatable, dimension(:,:)    :: t
    integer :: j, jg

    allocate( t(0:Ng1,0:2*Ng2-1) )

    t = 0
    t(0:Ng1-1,0:Ng2-1) = X           ! Put X in corner of larger grid

    call Fourir2DX(t, 1)             ! 2D Fourier tfm. (to k-space)

    do j=0,2*Ng2-1
      jg = min (abs(j), abs(j-2*Ng2))
      t(:,j) = t(:,j) * G(:,jg)           ! Multiply with G in k-space
    end do

    call Fourir2DX(t, -1)            ! 2D inverse Fourier tfm.

    X = t(0:Ng1-1,0:Ng2-1)           ! Retain only this part of result

    deallocate( t )
    return
  end subroutine prod1


  subroutine Preco(X)
    !
    ! Preconditioner, based on estimated eigenvalues of the operator.
    !
    real(kind=dp), intent(in out), dimension(0:,0:)  :: X
    complex(kind=dp), allocatable, dimension(:,:)    :: t
    real(kind=dp)             :: K0, D
    integer                   :: i, j, is, js

    allocate( t(0:Ng1-1,0:Ng2-1) )
    t = X
    call Fourir2D(t, 1)
                           
    K0 = 0.15_dp       ! To give preconditioner a finite value for k=0

    do j=0,Ng2-1
      js = min(j, Ng2-j)
      do i=0,Ng1-1
        is = min(i, Ng1-i)
        D = sqrt( (K0+is**2)/(D1*Ng1)**2 + (K0+js**2)/(D2*Ng2)**2 )
        t(i,j) = t(i,j) * D
      end do
    end do
                           
    call Fourir2D(t, -1)
                           
    X = t(0:Ng1-1,0:Ng2-1)

    where (Grid==0)                ! Restrict charges to conductor.
      X = 0  
    end where

    deallocate( t )
    return
  end subroutine Preco


  subroutine FourirG(G, K)
    !
    ! Fourier transform 2D Green function

    real(kind=dp), intent(in out), dimension(0:,0:) :: G
    integer,       intent(in)                       :: K
    complex(kind=dp), allocatable, dimension(:,:)   :: t

    allocate( t(0:2*Ng1-1,0:2*Ng2-1) )

    t(0:Ng1,0:Ng2-1)    = G(:,0:Ng2-1)      ! Fill one quadrant (one extra row)
    t(0:Ng1,Ng2:2*Ng2-1) = G(:,Ng2:1:-1)    ! This quadrant using symmetry
    t(Ng1:2*Ng1-1,:)    = t(Ng1:1:-1,:)     ! Remaining half, using symmetry

    call Fourir2D(t, K)

    G = sqrt(4.0_dp*Ng1*Ng2) * t(0:Ng1,0:Ng2)   ! Use only this part

    deallocate( t )
    return  
  end subroutine FourirG


  subroutine Fourir2D(X, K)
    complex(kind=dp), intent(in out), dimension(0:,0:)  :: X
    integer,          intent(in)                        :: K
    complex(kind=dp), dimension(max(size(X,1),size(X,2))/4) :: E
    integer  :: i,j

    do i=0,size(X,1)-1
      call fourir(X(i,:), size(X,2), K, E, UseOld=min(i,1))
    end do
    do j=0,size(X,2)-1
      call fourir(X(:,j), size(X,1), K, E, UseOld=min(j,1))
    end do

    return  
  end subroutine Fourir2D


  subroutine Fourir2DX(X, K)
    !
    ! Double-sized C-Fourier transform 2-dim array (tricky)

    complex(kind=dp), intent(in out), dimension(0:,0:)  :: X
    integer,          intent(in)                        :: K
    complex(kind=dp), dimension( max(Ng1,Ng2)/2 )       :: E
    complex(kind=dp), dimension( 0 : 2*max(Ng1,Ng2)-1 ) :: temp
    integer  :: i,j

    if(K==1) then
      do i=0,Ng1-1
        temp(:Ng2-1) = X(i,:Ng2-1)
        temp(Ng2:2*Ng2-1) = 0
        call fourir(temp, 2*Ng2, K, E, UseOld=min(i,1))
        X(i,:) = temp(:2*Ng2-1)
      end do
      do j=0,2*Ng2-1
        temp(:Ng1-1) = X(:Ng1-1,j)
        temp(Ng1:2*Ng1-1) = 0
        call fourir(temp, 2*Ng1, K, E, UseOld=min(j,1))
        X(:,j) = temp(:Ng1)
      end do
    else
      do i=0,Ng1
        temp(:2*Ng2-1) = X(i,:)
        call fourir(temp, 2*Ng2, K, E, UseOld=min(i,1))
        X(i,:) = temp(:2*Ng2-1)
      end do
      do j=0,Ng2-1
        temp(:Ng1) = X(:Ng1,j)
        temp(Ng1:2*Ng1-1) = conjg( temp(Ng1:1:-1) )
        call fourir(temp, 2*Ng1, K, E, UseOld=min(j,1))
        X(:,j) = temp(:Ng1)
      end do
    end if

    return  
  end subroutine Fourir2DX


  subroutine cgstab(X, err,mxiter,iter, X_start)
    !
    ! Bi-conjugate gradient squared stabilized.
    ! Van der Vorst's Bi-CGSTAB, 1992 SIAM J. Sci. Stat. Comput.
    !
    ! Solves x, for A*x = y,  where A*x is computed by mv_prod()
    ! On entry, X contains Y, on exit X is the found solution.
    ! If X_start is not present, X_start=0 is used.
    !
    integer,       intent(in)     :: mxiter
    real(kind=dp), intent(in)     :: err
    real(kind=dp), intent(in out), dimension(:)       :: X
    integer,       intent(out)                        :: iter
    real(kind=dp), intent(in), optional, dimension(:) :: X_start

    real(kind=dp), allocatable, dimension(:)  :: P, R, R0, S, T, V
    real(kind=dp) :: alpha, beta, omega, rho0, rho1, rnorm, ynorm
    integer       :: i, n

    n = size(X)
    allocate( P(n), R(n), R0(n), S(n), T(n), V(n) )

    ynorm = dot_product(X,X)
    if (present(X_start)) then
      call mv_prod(X_start,R)
      R = X - R
    else
      R = X
      X = 0
    end if

    R0 = R
    P = 0
    V = 0
    rho0 = 1
    alpha = 1
    omega = 1

    iter = 0
    do i=1,mxiter
        iter = i
        rho1 = dot_product(R0,R)
        beta = (rho1/rho0)* (alpha/omega)
        P = R + beta*(P-omega*V)
        call mv_prod(P,V)
        alpha = rho1/dot_product(R0,V)
        S = R - alpha*V
        call mv_prod(S,T)
        omega = div_care( dot_product(T,S), dot_product(T,T) )
        X = X + alpha*P + omega*S
        R = S - omega*T
        rnorm = dot_product(R,R)
        write(unit=*, fmt=*) "|r|/|y| =", sqrt(rnorm/ynorm)
        if (sqrt(rnorm/ynorm) < err) then
          exit                          ! exit this loop
        end if
        rho0 = rho1
    end do

    deallocate( P, R, R0, S, T, V )
    return
  end subroutine cgstab


  function div_care(a, b)  result(q)     ! avoid divide check error !!!
    real(kind=dp), intent(in) :: a, b
    real(kind=dp)             :: q
    real(kind=dp), parameter  :: large = huge(a)

    if( abs(a)/large < abs(b)) then      ! The normal case
      q = a/b
    else if( a == 0 ) then               ! The 0/0 case, return 1
      q = 1
    else                                 ! The a/0 case, return +-large
      q = large
      if( a < 0 ) then
        q = -q
      end if
      if( b < 0 ) then
        q = -q
      end if
    end if

    return
  end function div_care


  function Ginteg(xq1,yq1, xq2,yq2, xp,yp)  result(G)
    !        /    /
    !       | dy | dx  1/|r-p| dx dy
    !      /    /
    ! Potential in point p, from rectangular charge between corners q1, q2
    !
    real(kind=dp), intent(in) :: xq1,yq1, xq2,yq2, xp,yp
    real(kind=dp)             :: G
    real(kind=dp)             :: x1,x2,y1,y2,t
    x1 = xq1-xp
    x2 = xq2-xp
    y1 = yq1-yp
    y2 = yq2-yp
 
    if (x1+x2 < 0) then    ! Mirror in x-axis (for accuracy of Vprim)
      t = -x1
      x1 = -x2
      x2 = t
    end if
    if (y1+y2 < 0) then    ! Mirror in y-axis
      t = -y1
      y1 = -y2
      y2 = t
    end if

    G = Vprim(x2,y2)-Vprim(x1,y2)-Vprim(x2,y1)+Vprim(x1,y1)

    return
  end function Ginteg


  function Vprim(x,y)  result(VP)
    !
    ! Twice primitivated function, for Integral_dx Integral_dy 1/r, for z=0
    !
    real(kind=dp), intent(in) :: x,y
    real(kind=dp)             :: VP
    real(kind=dp)             :: r

    r = sqrt(x**2+y**2)
    VP = y*log(x+r) + x*log(y+r)

    return
  end function Vprim


  subroutine fourir(A,ntot,kconjg, E,useold)
    !
    ! Discrete Fourier transform. A becomes fourier tfm. of A
    !   A(k) := 1/sqrt(ntot) *
    !           sum(m=0,ntot-1)  A(m) * exp(kconjg*2*pi*i*k*m/ntot)
    ! ntot should be power of 2.
    ! kconjg = +1 or -1 for four.trans. or inverse four. trans.
    ! if useold=1, E(1:ntot/4-1) will be reused. if not, it is first filled:
    !      E(m) = exp(2*pi/ntot*i*m)
    ! Derived from recursive fft; Loop is in fact recursion.
    !
    complex(kind=dp), intent(inout), dimension(0:)    :: A
!    complex(kind=dp), intent(out), dimension(0:)   :: B
    complex(kind=dp), intent(in out), dimension(:) :: E
    integer, intent(in)                            :: ntot,kconjg,useold
    integer          :: m,n,j,i,inc,j0,j1,j2,j3
    complex(kind=dp) :: h,eh,Icc
    real(kind=dp)    :: root

    Icc = kconjg*(0.0_dp,1.0_dp)
    h = 2*acos(-1.0_dp)/ntot*Icc

    if (useold==0) then
      do m=1,ntot/4-1
        E(m) = exp(m*h)
      end do
    end if

    root = sqrt(1.0_dp/ntot)
    j=0
    i=0
    inc=1

    loop : do
      if(i<inc) then          ! normal entry
        if(inc==ntot) then
          if(i>j) then
            h=A(i)*root
            A(i)=A(j)*root
            A(j)=h
          else if(i==j) then
            A(j)=A(i)*root
          end if
          j=j+1
        else
          inc=inc*2
          if(inc>ntot) then
            write(unit=*, fmt=*) "error in fourier: n=", ntot
            stop
          end if
          cycle loop             ! recursive invocation I, followed by II
        end if
      else                       ! returned from recursive invocation II
        i=i-inc
        inc=inc/2
        n=ntot/inc
        j0=j-n
        j2=j0+n/2
        h = A(j0) + A(j2)
        A(j2) = A(j0) - A(j2)
        A(j0) = h
        if(n>2) then
          j1=j0+n/4
          j3=j2+n/4
          h = A(j1) + A(j3)*Icc
          A(j3) = A(j1) - A(j3)*Icc
          A(j1) = h
          do m=1,n/4-1
            h = A(j0+m) + A(j2+m)*E(m*inc)
            A(j2+m) = A(j0+m) - A(j2+m)*E(m*inc)
            A(j0+m) = h
            eh = conjg(E(ntot/4-m*inc))
            h = A(j1+m) - A(j3+m)*eh
            A(j3+m) = A(j1+m) + A(j3+m)*eh
            A(j1+m) = h
          end do
        end if
      end if
      if(inc==1) then
        exit loop                    ! return from root-invocation
      end if
      i = i+inc/2
    end do loop                      ! return from recursive invocation

    return
  end subroutine fourir

end module solv_cap

program capacitance
!
! Disk or square, at constant potential or placed in uniform Ex field.
!
! Version 1.5, Aug 1998, J. Bergervoet
!
  use solv_cap
  implicit none
 
  real(kind=dp)     :: GrSize1, GrSize2, C, P
  logical           :: DoCircle, UseFFT, UsePreco
  integer           :: Ng1, Ng2, i, j , ios
  character(len=20) :: Name
  integer, allocatable, dimension(:,:)  :: Grid

  write(unit=*,fmt=*) "Give: N_x, N_y, GridSize_X, GridSize_Y, DoCircle, UseFFT, UsePreco"
  write(unit=*,fmt=*)
  write(unit=*,fmt=*) "  N_x and N_y give the grid division in x and y-directions and"
  write(unit=*,fmt=*) "  should be powers of 2, DoCircle and UseFFT are T or F (true"
  write(unit=*,fmt=*) "  or false). If DoCircle=T, the case of an ellipse is computed"
  write(unit=*,fmt=*) "  (circle: C = 8*eps0*R, P = 16/3*eps0*R^3), else that of the"
  write(unit=*,fmt=*) "  completely filled grid. UseFFT is for increased speed."
  ng1 = 512 ; ng2 = 512 ; GrSize1 = 1.0 ; GrSize2 = 1.0
  DoCircle = .TRUE. ; UseFFT = .TRUE. ; UsePreco = .TRUE.
  open(11,file='capacita.in',status='old',iostat=ios)
  if ( ios.eq.0 ) read(unit=11,fmt=*) Ng1,Ng2, GrSize1, GrSize2, DoCircle, UseFFT, UsePreco

  allocate( Grid(Ng1,Ng2) )
  Grid = 1                   ! fill entire grid with metal
  Name = "rectangle"

  if (DoCircle) then         ! leave only metal in inscribed circle
    Name = "disk"
    do j=1,Ng2
      do i=1,Ng1
        if( ((2*i-Ng1-1.0_dp)/Ng1)**2 + ((2*j-Ng2-1.0_dp)/Ng2)**2 > 1 ) then
          Grid(i,j)=0
        end if
      end do
    end do
  end if

  call init_solve(Grid, GrSize1, GrSize2, UseFFT, UsePreco)

  call solveC(C)
  write(unit=*,fmt=*) "C_", trim(Name), "[pF] =", C * 1e12_dp

  call solveP(P)
  write(unit=*,fmt=*) "P_", trim(Name), "[pF.m^2] =", P * 1e12_dp
  
  deallocate( Grid )
  stop

end program capacitance
