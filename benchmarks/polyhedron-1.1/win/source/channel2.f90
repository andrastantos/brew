!--------------------------------------------------------------
! A simple A-grid shallow water model for a meridional channel
! John D. McCalpin, mccalpin@perelandra.cms.udel.edu
! Thu Jul  1 23:07:57 EDT 1993
!--------------------------------------------------------------
program sw
implicit double precision (a-h,o-z)

double precision,parameter::            Pi = 3.1415926535d0
integer,parameter::             M=8000,N=120                     ! RJA - M and N appear to be problem sizes. Originally 500 and 60.
integer,parameter::             NSTEPS=6480,IPLOT=4320           ! RJA - NSTEPS is iteration count. Originally 6480.
integer,parameter::             JWID=10
logical,parameter::             NSIGHT=.false.
logical,parameter::             CONTOOL=.false.
logical,parameter::             F_PLANE=.false.
logical,parameter::             B_PLANE=.true.
logical,parameter::             BORDERS=.true.
logical,parameter::             SMOOTH=.true.
integer                         new,mid,old
double precision,dimension(M,N,3)::             u,v,h
double precision,dimension(M,N)::               f,dudx,dvdy,dhdx,dhdy
double precision,dimension(M)::         Kelvin, tmp

! initialize arrays to zero
u = 0
v = 0
h = 0

! other initialization
dx = 10000.d0   ; dy = 60000.d0 ; dt = 2400.d0
f0 = 8.d-5      ; beta = 2.d-11 ; g = 0.02d0    ; Href = 288.0d0
c = sqrt(g*Href)
Rd = c/f0
a = sqrt(Href/g)

! now define meridional dependence of Coriolis force 

if (F_PLANE) then
    f = f0                              ! constant f=f0
else if (B_PLANE) then
    if (BORDERS) then
        dely = (N-2*JWID)*dy
        f(1:M,1:JWID) = f0-beta*0.5d0*dely
        f(1:M,N-JWID:N) = f0 + beta*0.5d0*dely
        do j=JWID+1,N-(JWID+1)          ! linear except near the boundaries
            y = (j-(N/2))*dy
            f(1:M,j) = f0 + beta*y
        end do
        if (SMOOTH) then                ! beta goes smoothly to zero at bdrys
            do j=JWID,1,-1
                f(1:M,j) = f(1:M,j+1)-beta*(real(j-1)/real(JWID))*dy
            end do
            do j=N-JWID,N
                f(1:M,j) = f(1:M,j-1)+beta*(real(N-j)/real(JWID))*dy
            end do
        end if
    else
        do j=1,N                        ! b-plane all the way to boundary
            y = (j-(N/2))*dy
            f(1:M,j) = f0 + beta*y
        end do
    end if
end if

! define Kelvin wave structure in x to be balanced at the southern boundary
scale = (c/f(M,1))
do i=1,M
    Kelvin(i) = exp((i-M)*dx/scale)
end do

! run parameters
period = 0.6666666666d0*360.d0*86400.d0
omega = 2*Pi/period
v0 = 0.1d0

! Document parameters in output file

print *,'--------------------------------------------------------------------'
print *,'INFORMATION ABOUT DISCRETIZATION'
print *,'--------------------------------'
print *,'Grid size is ',M,N
print *,'dx, dy, dt = ',dx,dy,dt
print *,'domain size is ',dx*M/1000,dy*N/1000,' (km)'
print *
print *,'PHYSICAL PARAMETERS'
print *,'-------------------'
print *,'g, Href, c = ',g,Href,c
print *,'f0, fmin, fmax = ',f0,f(M,1),f(M,N)
print *,'beta = ',beta
Rd_max = c/f(M,1)       ; Rd_min = c/f(M,N)
print *,'Rd (mean, max, min) = ', c/f0/1000.,Rd_max/1000.,Rd_min/1000.,' (km)'
print *,'cfl(x,y) = ',c*dt/dx,c*dt/dy
print *
print *,'KELVIN WAVE PARAMETERS'
print *,'----------------------'
print *,'Incoming Kelvin wave amplitude: v0 = ',v0
print *,'Period = ',period,' (seconds) = ',period/(360.*86400.),' (years)'
print *,'f-plane Kelvin wave y scale = ',c/omega,' (m)'
print *,'                            = ',(c/omega)/dy,' (grid intervals)'
print *
print *,'OTHER INFORMATION'
print *,'-----------------'
print *,'Run length: NSTEPS = ',NSTEPS,' = ',NSTEPS*dt/period,' periods'
print *,'Plots every IPLOT steps = ',IPLOT,' = ',IPLOT*dt/period,' periods'
c_Rossby_max = beta*Rd_max**2
print *,'Maximum Rossby wave phase speed = ',c_Rossby_max
print *,'Distance travelled during simulation = ',c_Rossby_max*dt*NSTEPS,' (m)'
print *,'                                     = ',c_Rossby_max*dt*NSTEPS/dx, &
                                                ' (grid intervals)'
if (c_Rossby_max*dt*NSTEPS/(dx*M).gt.1.5d0) then
    print *,'*************************************************************'
    print *,'Rossby waves will travel ',c_Rossby_max*dt*NSTEPS/(dx*M), &
        ' basin widths in the simulation'
    print *,'You probably do not want this -- bailing out....'
    print *,'*************************************************************'
    stop
end if
print *,'--------------------------------------------------------------------'
print *,'Coriolis parameter = '
print *,'    j,f(j)       = ',1,f(M,1)
do j=2,N
    print *,'    j,f(j),df/dy = ',j,f(M,j),f(M,j)-f(M,j-1)
end do
print *,'--------------------------------------------------------------------'

! open and initialize output data file
if (NSIGHT) then
    open (unit=3,file='nsight.sw',status='unknown')
    write (3,*) 'scalars 1'
    write (3,*) "scalar 1 name 'Height'"
    write (3,*) 'vectors 1'
    write (3,*) "vector 1 name 'Velocity' size magnitude dimension 2"
    write (3,*) 'frames ',NSTEPS/IPLOT,' time'
else if (CONTOOL) then
    open (unit=3,file='contool.sw',status='unknown')
end if

old = 1
mid = 2
new = 3

!---------------------- BEGIN TIME MARCHING LOOP -------------------------
do istep=1,NSTEPS
    time = istep*dt

    ! ------ interior calculations ------ !

    dudx = ddx(u(:,:,mid))
    dvdy = ddy(v(:,:,mid))
    dhdx = ddx(h(:,:,mid))
    dhdy = ddy(h(:,:,mid))

    u(2:M-1,1:N,new) = u(2:M-1,1:N,old) &               ! interior u points
        +2.d0*dt*f(2:M-1,1:N)*v(2:M-1,1:N,mid) &
        -2.d0*dt/(2.d0*dx)*g*dhdx(2:M-1,1:N)

    v(1:M,2:N-1,new) = v(1:M,2:N-1,old) &               ! interior v points
        -2.d0*dt*f(1:M,2:N-1)*u(1:M,2:N-1,mid) &
        -2.d0*dt/(2.d0*dy)*g*dhdy(1:M,2:N-1)

    h(1:M,2:N-1,new) = h(1:M,2:N-1,old) &               ! interior h points
        -2.d0*dt/(2.d0*dx)*Href*dudx(1:M,2:N-1) &
        -2.d0*dt/(2.d0*dy)*Href*dvdy(1:M,2:N-1) 

    ! ------ boundary calculations ------ !

    ! calculate outgoing characteristic variable at southern boundary
    tmp(1:M) = (1.d0/(c*dt+dx))*( (Href*dt-a*dx)*v(1:M,1,old) &
                               +(dx-c*dt)*h(1:M,1,old) &
                               -2*Href*dt*v(1:M,2,mid) &
                               +2*c*dt*h(1:M,2,mid) )

    ! add in incoming Kelvin wave characteristic variable
    v(1:M,1,new) = v0*sin(omega*time)*Kelvin(1:M) &       ! southern v points
                    -(0.5d0/a)*tmp(1:M)
    h(1:M,1,new) = a*v0*sin(omega*time)*Kelvin(1:M) & ! southern h points
                    +0.5d0*tmp(1:M)

    ! calculate outgoing characteristic variable at northern boundary 
    tmp(1:M) = (1.d0/(c*dt+dx))*( (-Href*dt+a*dx)*v(1:M,N,old) & 
                               +(dx-c*dt)*h(1:M,N,old) &
                               +2*Href*dt*v(1:M,N-1,mid) &
                               +2*c*dt*h(1:M,N-1,mid) )
    v(1:M,N,new) = (0.5d0/a)*tmp(1:M)                   ! separate v 
    h(1:M,N,new) = 0.5d0*tmp(1:M)                               ! separate h

    iswap = old
    old = mid
    mid = new
    new = iswap

    if (mod(istep,IPLOT)==0) then
        call list ('h',time*omega/(2*Pi),h(1,1,mid),M,N)
!        call list ('u',time*omega/(2*Pi),u(1,1,mid),M,N)
!        call list ('v',time*omega/(2*Pi),v(1,1,mid),M,N)
        if (NSIGHT) then
            write (3,*) 'frame ',istep/IPLOT,' value ',time*omega/(2*3.14159)
            write (3,*) 'surfaces 1'
            write (3,*) 'surface 1 curves ',N,' nodes ',M, &
                    ' scalars 1 vectors 1 planar'
            write (3,*) "scalar 1 name 'Height'"
            write (3,*) "vector 1 name 'Velocity'"
            do j=1,N
                write (3,*) 'curve ',j
                do i=1,M
                    write (3,'(3(f9.0,1x),f8.3,1x,2(f8.4,1x))',err=9999) &
                            real(i-M)*dx,real(j)*dy,0.0, &
                            h(i,j,mid),u(i,j,mid),v(i,j,mid)
                end do
            end do
        else if (CONTOOL) then
            write (3,*) 'KELVIN-ROSSBY PROBLEM'
            write (3,*) M,N
            write (3,*) 1,1
            write (3,*) M,N
            write (3,*) -21.0,+21.0,2.0
            write (3,*) 0,-1,-31
            write (3,*) '(6(1pe12.5))'
            write (3,'(6(1pe12.5))') ((h(i,j,mid),i=1,M),j=1,N)
        end if
    end if

end do
!---------------------- END TIME MARCHING LOOP ---------------------------

if (NSIGHT .or. CONTOOL) then
    close(unit=3)
end if
stop

9999 print *,   'FORMAT ERROR at: ',i,j
print *,real(i),real(j),0.0,h(i,j,mid), &
                0.5*(u(i,j,mid)+u(i-1,j,mid)), &
                0.5*(v(i,j,mid)+v(i,j-1,mid))

!------------------------------------------------------------
contains
!------------------------------------------------------------
    function ddx(array)
    implicit double precision (a-h,o-z)
    double precision::          array(:,:)
    double precision::          ddx(size(array,dim=1),size(array,dim=2))

    I = size(array,dim=1)
    J = size(array,dim=2)

    ddx(2:I-1,1:J) = array(3:I,1:J)-array(1:I-2,1:J)    ! interior points

    ddx(1,1:J) = 2*(array(2,1:J)-array(  1,1:J))
    ddx(I,1:J) = 2*(array(I,1:J)-array(I-1,1:J))

    end function ddx

    function ddy(array)
    implicit double precision (a-h,o-z)
    double precision::          array(:,:)
    double precision::          ddy(size(array,dim=1),size(array,dim=2))

    I = size(array,dim=1)
    J = size(array,dim=2)

    ddy(1:I,2:J-1) = array(1:I,3:J)-array(1:I,1:J-2)    ! interior points

    ddy(1:I,1) = 2*(array(1:I,2)-array(1:I,  1))
    ddy(1:I,J) = 2*(array(1:I,J)-array(1:I,J-1))

    end function ddy
!------------------------------------------------------------
end program sw

!------------------------------------------------------------
subroutine list (label,time,array,M,N)
implicit double precision (a-h,o-z)
character*(*)::         label
integer::               M,N
double precision::                      time,array(M,N)

write (*,'(a,a,f8.4,a,1pe12.5)') label,' field at time = ',time, &
                                ' maxval = ',maxval(array)
do j=N,1,-2
    write (*,'(1x,25f5.1)') (array(i,j),i=M-14,M)
end do
end subroutine list
!------------------------------------------------------------
!------------------------------------------------------------
!interface
!    function ddx (array) 
!       double precision::              array(:,:)
!       double precision::              ddx(size(array,dim=1),size(array,dim=2))
!    end function ddx
!    function ddy (array) 
!       double precision::              array(:,:)
!       double precision::              ddy(size(array,dim=1),size(array,dim=2))
!    end function ddy
!end interface
!------------------------------------------------------------
