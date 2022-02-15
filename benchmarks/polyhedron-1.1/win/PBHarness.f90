!*==PBHarness.spg  processed by SPAG 6.55Dc at 14:31 on 26 Mar 2004
!  Polyhedron Benchmark Harness
!  Copyright (C) Polyhedron Software Ltd. 2004
!  Copyright (C) Polyhedron Solutions Ltd. 2015

      PROGRAM PBHarness
!-IF INTEL
      USE IFPOSIX
      USE IFPORT
!-ENDIF
      IMPLICIT NONE

!*** Start of declarations rewritten by SPAG

! PARAMETER definitions

      INTEGER , PARAMETER :: SUMMARY = 22

! Local variables

      CHARACTER(2048) :: allnames , cmdln , command , comment , compile ,&
     &                  exename , names , root , timeline , titleline , &
     &                  token , zcompiletimes , zexesizes , zruntimes
      REAL :: compiletime , ThisTime , AveTime , ErrPercent, gmean ,    &
     &        MaxTime , TargetErr
      INTEGER :: exesize , first , i , ios , ItNum , j , k , lbrac ,    &
     &           ldot , lfull , lname , lroot , ltoken , maxits ,       &
     &           minits , nbench , pname , pos
      LOGICAL :: qex , writeheader

!*** End of declarations rewritten by SPAG

      CALL GetCommandLine(cmdln)

!  get root of first .PAR file on command line

      ltoken = INDEX(cmdln,' ') - 1
      token = cmdln(1:ltoken)
      cmdln = ADJUSTL(cmdln(ltoken+1:))

      lbrac = INDEX(token,'(')
      ldot = INDEX(token,'.')
      IF ( lbrac>0 ) THEN
         lroot = LEN_TRIM(token) - lbrac - 1
         root = token(lbrac+1:lbrac+lroot)
         ltoken = lbrac - 1
         token(lbrac:) = ' '
      ELSEIF ( ldot>0 ) THEN
         root = token(1:ldot-1)
      ELSE
         root = token
      ENDIF

      IF ( ldot<=0 ) THEN
         token(ltoken+1:) = '.par'
         ltoken = ltoken + 4
      ENDIF
      OPEN (21,FILE=token(1:ltoken))

!  read compile command and benchmark names

      WRITE (*,'(A)') 'Polyhedron Benchmark Harness'
      WRITE (*,'(A)')                                                   &
     &'Copyright (C) Polyhedron Solutions Ltd - 2015 - All rights reserved'
!      WRITE (*,'(A)') 'Executed on '//DateText()
      WRITE (*,'(/A)') 'Test Name       : '//TRIM(root)
      CALL ReadLine(compile)
      WRITE (*,'(A)') 'Compile Command : '//TRIM(compile)
      CALL ReadLine(names)
      WRITE (*,'(A)') 'Benchmarks      : '//TRIM(names)
      allnames = names

!  read control params

      CALL ReadLine(timeline)
      READ (timeline,*) MaxTime , TargetErr , minits , maxits
      WRITE (*,'(A,F10.1)') 'Maximum Times   : ' , MaxTime
      WRITE (*,'(A,F10.3)') 'Target Error %  : ' , TargetErr
      WRITE (*,'(A,I5)') 'Minimum Repeats : ' , minits
      WRITE (*,'(A,I5)') 'Maximum Repeats : ' , maxits


!  Make sure output files deleted so we can be sure output is new

      CALL DeleteFile(TRIM(root)//'.cmp')
      CALL DeleteFile(TRIM(root)//'.run')
      CALL DeleteFile(TRIM(root)//'.sum')

!  Start .SUM file

      OPEN (SUMMARY,FILE=TRIM(root)//'.sum')
      WRITE (SUMMARY,'(A)') REPEAT('=',80)
      WRITE (SUMMARY,'(A)') 'Date & Time     : '//DateText()
      WRITE (SUMMARY,'(A)') 'Test Name       : '//TRIM(root)
      WRITE (SUMMARY,'(A)') 'Compile Command : '//TRIM(compile)
      WRITE (SUMMARY,'(A)') 'Benchmarks      : '//TRIM(names)
      WRITE (SUMMARY,'(A,F10.1)') 'Maximum Times   : ' , MaxTime
      WRITE (SUMMARY,'(A,F10.3)') 'Target Error %  : ' , TargetErr
      WRITE (SUMMARY,'(A,I5)') 'Minimum Repeats : ' , minits
      WRITE (SUMMARY,'(A,I5)') 'Maximum Repeats : ' , maxits
      WRITE (SUMMARY,*)
      WRITE (SUMMARY,'(A)')                                             &
     &    '   Benchmark   Compile  Executable   Ave Run  Number   Estim'
      WRITE (SUMMARY,'(A)')                                             &
     &    '        Name    (secs)     (bytes)    (secs) Repeats   Err %'
      WRITE (SUMMARY,'(A)')                                             &
     &    '   ---------   -------  ----------   ------- -------  ------'

      pos = 1
      nbench = 0
      gmean = 1.0D0
      zcompiletimes = ' '
      zruntimes = ' '
      titleline = ' '
      zexesizes = ' '

!  loop over benchmarks

      DO WHILE ( names/='' )
         names = ADJUSTL(names)
         lfull = INDEX(names,' ') - 1
         lname = INDEX(names(1:lfull),'.') - 1
         IF ( lname<0 ) lname = lfull
         IF ( names(1:1)=='-' ) THEN    ! specified benchmark is to be skipped
            WRITE (*,'(A)') TRIM(root)
            WRITE (*,'(A)') 'Skipping '//names(2:lname)
            compiletime = 0.0
            exesize = 0
            AveTime = -1.0
         ELSE
!-IF UNIX
!-            ExeName = './'//names(1:lname)
!-ELSE
            exename = names(1:lname)//'.exe'
!-ENDIF
            CALL DeleteFile(TRIM(exename))  ! to be sure we don't use an old executable

!  Compile & Link

            comment = DateText()//' '//TRIM(root)//' - Compile '//      &
     &                names(1:lfull)
            CALL Separator(TRIM(root)//'.cmp',comment)
            WRITE (*,'(/A)') TRIM(comment)

            command = compile
            IF ( LEN_TRIM(command)>0 ) THEN
               CALL ExpandTokens(command)
               WRITE (*,'(A)') 'command='//TRIM(command)
               WRITE (*,*)
               CALL SYSCMD(TRIM(command)//' >>'//TRIM(root)             &
     &                     //'.cmp 2>&1',compiletime)
            ENDIF

            INQUIRE (FILE=TRIM(exename),EXIST=qex)
            exesize = FileSize(TRIM(exename))

! Execute

            IF ( .NOT.qex ) THEN
               exesize = 0
               AveTime = -1.0
               comment = DateText()//' '//TRIM(root)                    &
     &                   //' - No executable found for '//names(1:lfull)
               CALL Separator(TRIM(root)//'.run',comment)
               WRITE (*,'(A)') TRIM(comment)
            ELSE
               comment = DateText()//' '//TRIM(root)//' - Execute '//   &
     &                   names(1:lfull)
               CALL Separator(TRIM(root)//'.run',comment)
               WRITE (*,'(A)') TRIM(comment)
               DO ItNum = 1 , maxits
                  IF ( ItNum==1 ) THEN  !  capture output from 1st run
                     command = TRIM(exename)//'>>'//TRIM(root)//'.run'
                  ELSE
                     command = TRIM(exename)//'>dummyfile'
                  ENDIF
                  CALL SYSCMD(command,ThisTime)
                  CALL EstimateError(ItNum,ThisTime,AveTime,ErrPercent)
                  WRITE (*,'(2A,I4,2F12.5,A,F8.4,A)') names(1:lname) ,  &
     &                   ' Run #' , ItNum , ThisTime , AveTime ,        &
     &                   ' - Error=' , ErrPercent , '%'
                  IF ( (ErrPercent<TargetErr .AND. ItNum>=minits) .OR.  &
     &                 AveTime*ItNum>=MaxTime .OR. ItNum==maxits ) EXIT
               ENDDO
            ENDIF
         ENDIF
         WRITE (SUMMARY,'(A12,F10.2,I12,F10.2,I8,F8.4)') names(1:lname) &
     &          , compiletime , exesize , AveTime , ItNum , ErrPercent
         WRITE (zcompiletimes(pos:pos+6),'(F7.1)',IOSTAT=ios)           &
     &          compiletime
         WRITE (zruntimes(pos:pos+6),'(F7.2)',IOSTAT=ios) AveTime
         WRITE (titleline(pos:pos+6),'(1X,A6)',IOSTAT=ios)              &
     &          names(1:lfull)
         WRITE (zexesizes(pos:pos+6),'(I7)',IOSTAT=ios) exesize/1000
         nbench = nbench + 1
         IF ( AveTime>0 ) THEN
            gmean = gmean*AveTime
         ELSE
            gmean = 1000.0*gmean
         ENDIF
         pos = pos + 7
         names(1:lfull) = ''
      ENDDO                       !  loop over benchmarks

      gmean = ABS(gmean)**(1.0D0/REAL(nbench))
      WRITE (SUMMARY,'(/A,F10.2,A/)') 'Geometric Mean Execution Time = '&
     &                                , gmean , ' seconds'
      WRITE (*,'(/A,F10.2,A/)') 'Geometric Mean Execution Time = ' ,    &
     &                          gmean , ' seconds'

      WRITE (SUMMARY,'(A)') REPEAT('=',80)
      CLOSE (SUMMARY)

      CALL AppendToSummary('bldtimes.txt',TRIM(titleline),root(1:16)    &
     &                     //TRIM(zcompiletimes)//' '//TRIM(compile))
      CALL AppendToSummary('exesizes.txt',TRIM(titleline),root(1:16)    &
     &                     //TRIM(zexesizes)//' '//TRIM(compile))

      WRITE (zruntimes(pos:pos+6),'(F7.2)',IOSTAT=ios) gmean
      WRITE (titleline(pos:),'(1X,A7)',IOSTAT=ios) 'GEOMEAN'
      CALL AppendToSummary('runtimes.txt',TRIM(titleline),root(1:16)    &
     &                     //TRIM(zruntimes)//' '//TRIM(compile))

!  Perform tidy up commands

      DO
         CALL ReadLine(command)
         IF ( LEN_TRIM(command)==0 ) EXIT
         CALL ExpandTokens(command)
         WRITE (*,'(/2A)') 'Tidy command= ' , TRIM(command)
         CALL SYSCMD(TRIM(command))
      ENDDO
      STOP

      CONTAINS
!=======================================================================
      SUBROUTINE SYSCMD(Command,Time)
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      CHARACTER(*) , INTENT(IN) :: Command
      REAL , OPTIONAL , INTENT(OUT) :: Time
!
! Local variables
!
      INTEGER :: ios , maxticks , tbase , ticks , tickspersec
!
!*** End of declarations rewritten by SPAG
!
      CALL SYSTEM_CLOCK(tbase,tickspersec,maxticks)

!  There seems to be a real problem getting consistent timing results
!  under RedHat 9 on my 3GHz P4. The Intel SYSTEM call spawns a new C-shell,
!  whereas Lahey's starts the process in the current shell.  For a few
!  benchmarks  (e.g. DRAG when compiled with lf95) this makes a significant
!  (~10%) difference.
!
!  The effect can be reproduced using the "time" command. It's also
!  noticeable that timings for DRAG degrade if there are nested bash shells.
!  My solution for this is to use the Intel compiled version of this
!  harness, or, if it is compiled with lf95, to run it from csh.

!-IF FTN95
!-      integer(2) ios
!-      call cissue@(command,ios)
!-ELSEIF DVF,INTEL
      ios = SYSTEMQQ(Command)
!-ELSEIF LF95
!-      call system(command)
!-ENDIF
      CALL SYSTEM_CLOCK(ticks,tickspersec,maxticks)
      IF ( PRESENT(Time) ) then
         Time = REAL(ticks-tbase)/REAL(tickspersec)
         if ( Time<0.0 ) Time = Time + REAL(maxticks)/REAL(tickspersec)
      ENDIF
      END SUBROUTINE SYSCMD

!=======================================================================
!  FileSize function is required if we want to record size of
!  executable - otherwise, just set to say 10000
!
      INTEGER FUNCTION FileSize(Filename)
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      CHARACTER(*) , INTENT(IN) :: Filename
!
!*** End of declarations rewritten by SPAG
!
!-IF LF95
!-      inquire(file=TRIM(FileName),flen=FileSize)
!-ELSEIF FTN95
!      INTEGER ios
!      call file_size@(TRIM(FileName),FileSize,ios)
!-ELSEIF DVF,INTEL
      TYPE (FILE$INFO) :: fileinfo
      INTEGER (KIND=INT_PTR_KIND()) ::  handle
      INTEGER :: l
      handle = file$first
      l = GETFILEINFOQQ(TRIM(Filename),fileinfo,handle)
      FileSize = fileinfo%length
!-ELSEIF ABSOFT
!-      integer statarray(13) ,l , lstat
!-      l = lstat(TRIM(FileName),statarray)
!-      FileSize = statarray(8)
!-ELSE
!-      FileSize = 10000
!-ENDIF
      END FUNCTION FileSize
!=======================================================================
      SUBROUTINE DeleteFile(Filename)
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      CHARACTER(*) , INTENT(IN) :: Filename
!
!*** End of declarations rewritten by SPAG
!
!-IF UNIX
!-       CALL SYSCMD('rm '//FileName//' 2>/dev/null')
!-ELSE
      CALL SYSCMD('del '//Filename//' 2>nul')
!-ENDIF
      END SUBROUTINE DeleteFile
!=======================================================================
      SUBROUTINE ReadLine(Line)
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      CHARACTER(*) , INTENT(OUT) :: Line
!
! Local variables
!
      CHARACTER(255) :: file
      INTEGER :: ios , lfile
!
!*** End of declarations rewritten by SPAG
!
      DO
         READ (21,'(A)',IOSTAT=ios) Line
         IF ( ios==0 ) THEN  !  Get next .PAR file from command line
            Line = ADJUSTL(Line)
            IF ( Line(1:1)=='#' .OR. Line==' ' ) CYCLE
            IF ( Line(1:1)=='@' ) THEN     !  take input from specified file
               CLOSE (21)
               OPEN (21,FILE=TRIM(Line(2:)),IOSTAT=ios,STATUS='old')
!            print*,ios
               CYCLE
            ENDIF
         ELSEIF ( CMDln==' ' ) THEN
            Line = ' '
         ELSE
            lfile = INDEX(CMDln,' ') - 1
            file = CMDln(1:lfile)
            CMDln = ADJUSTL(CMDln(lfile+1:))

            IF ( INDEX(file,'.')==0 ) THEN
               file(lfile+1:) = '.par'
               lfile = lfile + 4
            ENDIF
            OPEN (21,FILE=TRIM(file),IOSTAT=ios,STATUS='old')
            CYCLE
         ENDIF
         EXIT
      ENDDO
      END SUBROUTINE ReadLine
!=======================================================================
      SUBROUTINE ExpandTokens(Line)
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      CHARACTER(*) , INTENT(INOUT) :: Line
!
! Local variables
!
      INTEGER :: ptoken
!
!*** End of declarations rewritten by SPAG
!
!
!  expand %1 to full name of benchmark
!         %n to root name of benchmark (no extension)
!         %r to root name of .PAR file (e.g. LF95)
!         %a to list of benchmark names
!
      ptoken = INDEX(Line,'%1')
      DO WHILE ( ptoken>0 )
         Line = Line(1:ptoken-1)//NAMes(1:LFUll)//Line(ptoken+2:)
         ptoken = INDEX(Line,'%1')
      ENDDO

      ptoken = INDEX(Line,'%n')
      DO WHILE ( ptoken>0 )
         Line = Line(1:ptoken-1)//NAMes(1:LNAme)//Line(ptoken+2:)
         ptoken = INDEX(Line,'%n')
      ENDDO

      ptoken = INDEX(Line,'%r')
      DO WHILE ( ptoken>0 )
         Line = Line(1:ptoken-1)//TRIM(ROOt)//Line(ptoken+2:)
         ptoken = INDEX(Line,'%r')
      ENDDO

      ptoken = INDEX(Line,'%a')
      DO WHILE ( ptoken>0 )
         Line = Line(1:ptoken-1)//TRIM(ALLnames)//Line(ptoken+2:)
         ptoken = INDEX(Line,'%a')
      ENDDO
      END SUBROUTINE ExpandTokens
!=======================================================================
      SUBROUTINE GetCommandLine(Cmdln)
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      CHARACTER(*) , INTENT(OUT) :: Cmdln
!
! Local variables
!
      CHARACTER(72) :: cmd
      INTEGER :: err , i , l
      INTEGER :: IPXFARGC
!
!*** End of declarations rewritten by SPAG
!
!-IF INTEL
      Cmdln = ''
      DO i = 1 , IPXFARGC()
         CALL PXFGETARG(i,cmd,l,err)
         IF ( i==1 ) THEN
            Cmdln = cmd
         ELSE
            Cmdln = TRIM(Cmdln)//' '//cmd
         ENDIF
      ENDDO
!-ELSEIF LF95
!-      call getcl(cmdln)
!-ENDIF
      END SUBROUTINE GetCommandLine
!=======================================================================
      SUBROUTINE AppendToSummary(Summaryfile,Titleline,Dataline)
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      INTEGER , PARAMETER :: SFUNIT = 23
!
! Dummy arguments
!
      CHARACTER(*) , INTENT (IN) :: Dataline , Summaryfile , Titleline
!
! Local variables
!
      CHARACTER(255) :: line
      LOGICAL :: writeheader
!
!*** End of declarations rewritten by SPAG
!

!  Append TitleLine (if it is not already there) and DataLine to SummaryFile


      writeheader = .TRUE.
      OPEN (SFUNIT,FILE=Summaryfile,IOSTAT=IOS)
      READ (SFUNIT,'(a)',IOSTAT=IOS) line
      DO WHILE ( IOS==0 )
         IF ( line(1:16)==' ' ) writeheader = line(17:)/=Titleline
         READ (SFUNIT,'(a)',IOSTAT=IOS) line
      ENDDO
      CLOSE (SFUNIT)

      OPEN (SFUNIT,FILE=Summaryfile,POSITION='APPEND')
      IF ( writeheader ) WRITE (SFUNIT,'(2A)') '                ' ,     &
     &                          TRIM(Titleline)
      WRITE (SFUNIT,'(8A)') TRIM(Dataline)
      CLOSE (SFUNIT)
      END SUBROUTINE AppendToSummary
!=======================================================================
      SUBROUTINE Separator(Outputfile,Commentline)
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      INTEGER , PARAMETER :: OFUNIT = 24
!
! Dummy arguments
!
      CHARACTER(*) , INTENT(IN) :: Commentline , Outputfile
!
!*** End of declarations rewritten by SPAG
!

!  Append CommentLine to OutputFile


      OPEN (OFUNIT,FILE=Outputfile,POSITION='APPEND')
      WRITE (OFUNIT,'(A)') ' '
      WRITE (OFUNIT,'(A)') REPEAT('=',LEN_TRIM(Commentline))
      WRITE (OFUNIT,'(A)') TRIM(Commentline)
      WRITE (OFUNIT,'(A)') REPEAT('=',LEN_TRIM(Commentline))
      WRITE (OFUNIT,'(A)') ' '
      CLOSE (OFUNIT)
      END SUBROUTINE Separator
!=======================================================================
      FUNCTION DateText()
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      CHARACTER(3) , PARAMETER , DIMENSION(12)                          &
     &            :: MONTHS = (/'Jan','Feb','Mar','Apr','May','Jun',    &
     &                          'Jly','Aug','Sep','Oct','Nov','Dec'/)
!
! Dummy arguments
!
      CHARACTER(20) :: DateText
!
! Local variables
!
      CHARACTER(8) :: date
      INTEGER , DIMENSION(8) :: dtvals
      CHARACTER(10) :: time
      CHARACTER(5) :: zone
!
!*** End of declarations rewritten by SPAG
!

      CALL DATE_AND_TIME(date,time,zone,dtvals)
      WRITE (DateText,'(I2,A4,I5,I3,":",I2.2,":",I2.2)') dtvals(3) ,    &
     &       MONTHS(dtvals(2)) , dtvals(1) , dtvals(5) , dtvals(6) ,    &
     &       dtvals(7)
      END FUNCTION DateText
!=======================================================================
      SUBROUTINE OldEstimateError(ItNum,ThisTime,AveTime,ErrPercent)
      IMPLICIT NONE
      INTEGER :: ItNum
      REAL :: ThisTime , AveTime , ErrPercent

      REAL , SAVE , DIMENSION(10000) :: IterTime
      INTEGER , SAVE :: ActiveTimes , WorstIt , j , k
      INTEGER , DIMENSION(7) :: count
      REAL :: SumSqDev , MaxDev , EstRMSDev , tmin , tmax

      IF ( ItNum==1 ) THEN
         IterTime(1) = ThisTime
         ActiveTimes = 1
         AveTime = ThisTime
         ErrPercent = 100.0
         RETURN
      ELSEIF ( ItNum==5 ) THEN   !  After a while take first iteration out of average.  The first run
                               !  is sometimes unrepresentative, and can slow convergence a lot.
         IterTime(1:3) = IterTime(2:4)
         IterTime(4) = ThisTime
      ELSE
         ActiveTimes = ActiveTimes + 1
         IterTime(ActiveTimes) = ThisTime
      ENDIF

      DO  !  Compute estimate of run time, and error in estimate
          !  Also identify possible rogue results
         AveTime = MAX(SUM(IterTime(1:ActiveTimes))/ActiveTimes,0.01)
         SumSqDev = 0.0
         MaxDev = 0.0 ; WorstIt = 1
         DO j = 1 , ActiveTimes
            SumSqDev = SumSqDev + (IterTime(j)-AveTime)**2
            IF ( ABS(IterTime(j)-AveTime)>MaxDev ) THEN
               MaxDev = ABS(IterTime(j)-AveTime)
               WorstIt = j
            ENDIF
         ENDDO
         EstRMSDev = SQRT(SumSqDev/((ActiveTimes-1)*ActiveTimes))
         ErrPercent = 100*EstRMSDev/AveTime

!  If >10 iterations and there is a rogue result, eliminate it

         IF ( ActiveTimes<=10 .OR. MaxDev<3*EstRMSDev ) exit
         WRITE(*,'(A,F12.5,A,F5.1,A)')                              &
     &       'Eliminate Outlying Result ',IterTime(WorstIt) ,       &
     &       ' at ' , MaxDev/EstRMSDev ,' standard deviations'
         IterTime(WorstIt:ActiveTimes-1) = IterTime(WorstIt+1:ActiveTimes)
         ActiveTimes = ActiveTimes - 1
      ENDDO

!  display simple histogram on iterations 30, 60 etc.

      IF ( MOD(ItNum,30)==0 ) THEN
         tmin = MINVAL(IterTime(1:ActiveTimes))
         tmax = MAXVAL(IterTime(1:ActiveTimes))
         count = 0
         DO j = 1 , ActiveTimes
            k = 1 + (6.99999D0*(IterTime(j)-tmin))/(tmax-tmin)
            count(k) = count(k) + 1
         ENDDO
         DO k = 1 , 7
            WRITE (*,'(70A1)') ('*',j=1,count(k))
         ENDDO
      ENDIF
      END SUBROUTINE OldEstimateError
!=======================================================================
      SUBROUTINE EstimateError(ItNum,ThisTime,AveTime,ErrPercent)
      IMPLICIT NONE
      INTEGER :: ItNum
      REAL :: ThisTime , AveTime , ErrPercent

      REAL , SAVE , DIMENSION(10000) :: IterTime
      INTEGER , SAVE :: ActiveTimes , j , nelim
      INTEGER , DIMENSION(7) :: count
      REAL :: SumSqDev ,EstRMSDev

      IF ( ItNum==1 ) THEN
         IterTime(1) = ThisTime
         ActiveTimes = 1
         AveTime = ThisTime
         ErrPercent = 100.0
         RETURN
      ELSE
         do i = 1 , ActiveTimes
            if ( ThisTime<IterTime(i) ) exit
         enddo
         IterTime(i+1:ActiveTimes+1) = IterTime(i:ActiveTimes)
         IterTime(i) = ThisTime
         ActiveTimes = ActiveTimes + 1
      ENDIF

!  How many outliers to eliminate at either end of distribution?

      nelim = max(0,(ActiveTimes-8)/4)
      AveTime = MAX(SUM(IterTime(1+nelim:ActiveTimes-nelim))/(ActiveTimes-2*nelim),0.01)
      SumSqDev = 0.0
      DO j = 1+nelim , ActiveTimes-nelim
         SumSqDev = SumSqDev + (IterTime(j)-AveTime)**2
      ENDDO
      EstRMSDev = SQRT(SumSqDev/(   (ActiveTimes-1-2*nelim) * (ActiveTimes-2*nelim)  ) )
      ErrPercent = 100*EstRMSDev/AveTime

      IF ( ActiveTimes>11 .AND. mod(ActiveTimes,4)==0 )&
         write(*,'(4x,A,i3,a/4x,a,F12.4,A,F12.4)') 'Discount ',nelim,' from either end of distribution','Range is now',IterTime(1+nelim),' - ',IterTime(ActiveTimes-nelim)
      END SUBROUTINE EstimateError
      END PROGRAM PBHarness
