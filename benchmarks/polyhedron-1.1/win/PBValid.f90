   PROGRAM PBValidator
!-IF CVF,INTEL
      USE IFPORT
      IMPLICIT NONE
!-ENDIF
      CHARACTER(260) :: testfile , instructionfile
      CHARACTER(1024) :: line , str , text , CmdLine=' ' , CmdText
      CHARACTER(32) :: TestName
      CHARACTER(1) :: cmd
      INTEGER :: npass , nfail , benchpasses , benchfails , cnum , &
     &           lnum , n , p , markl , markc , lstr , ios
      REAL(KIND(1D0)) :: target , tolerance , val , tol
      LOGICAL inttarg , ok
      
      IF ( NARGS().NE.3 ) THEN
!      IF ( IPXFARGC().NE.2 ) THEN
         PRINT '(A)' , 'PBValid needs 2 arguments'
         PRINT '(A)' , 'PBValid file-to-be-checked  instruction-file'
         STOP
      ENDIF
!      CALL PXFGETARG(1,testfile,l,err)
      CALL GETARG(1,testfile)
      OPEN(11,file=testfile)
!      CALL PXFGETARG(2,instructionfile,l,err)
      CALL GETARG(2,instructionfile)
      OPEN(12,file=instructionfile)

      WRITE (*,'(A)') 'Polyhedron Benchmark Validator'
      WRITE (*,'(A)')                                                   &
     &'Copyright (C) Polyhedron Software Ltd - 2004 - All rights reserve&
     &d'
!      WRITE (*,'(A)') 'Executed on '//DateText()
      WRITE (*,'(/A)') 'Test File        : '//TRIM(testfile)
      WRITE (*,'(A/)') 'Instruction File : '//TRIM(instructionfile)
      npass = 0 ; nfail = 0 ; benchpasses = 0 ; benchfails = 0
      lnum = 0 ; cnum = 0 ; TestName = ' '
      
      do                                              !  loop over commands
         CALL GetCmd
         
         if     ( cmd=='N' ) then                     !  N Next - forward n lines
            CALL NextLine(n)                          
         elseif ( cmd=='P' ) then                     !  P Prev - back n lines
            CALL PrevLine(n)                          
         elseif ( cmd=='+' ) then                     !  + forward n columns
            cnum = MIN(cnum+n,LEN(line))
         elseif ( cmd=='-' ) then                     !  - back n cloumns
            cnum = MAX(1,cnum-n)                      
         elseif ( cmd=='W' ) then                     !  W move forward within current line to next Whitespace
            do cnum = cnum , len(line)
               if ( INDEX(' '//char(8),line(cnum:cnum))>0 ) exit
            enddo
         elseif ( cmd=='B' ) then                     !  B move forward within current line to next "Blackspace"
            do cnum = cnum , len(line)
               if ( INDEX(' '//char(8),line(cnum:cnum))==0 ) exit
            enddo
         elseif ( cmd=='#' ) then                     !  . move forward within current line to next number 
                                                      !    (including -,+,.)
            do cnum = cnum , len(line)
               if ( INDEX('1234567890-+.',line(cnum:cnum))>0 ) exit
            enddo
         elseif ( cmd=='F' ) then                     !  F Find string
            do
               p = index(line(cnum:),str(1:lstr))
               if ( p>0 ) then
                  cnum = cnum + p + lstr - 1
                  exit
               else
                  CALL NextLine(1)
               endif
            enddo
         elseif ( cmd=='<' ) then                     ! < mark start of test string at current char
            markl = lnum
            markc = cnum
         elseif ( cmd=='>' ) then                     ! > grab numeric string starting at mark
                                                      !   and ending at character before current char.
                                                      !   Test against specified numeric target and tolerance
            IF ( lnum/=markl ) CALL Error('Line Change')
            text = line(markc:cnum-1)
            READ(text,*,IOSTAT=ios) val
            IF ( ios/=0 ) CALL Error(text)
            if ( inttarg ) THEN
               ok = ABS(NINT(val)-NINT(target))<=NINT(tol)
               print '(A,I17,A,I17,A,I17)' , '> Value=',NINT(val),' Target=',NINT(target),' Tolerance=',NINT(tol)
            else
               ok = ABS(val-target)<tol
               print '(A,G17.11,A,G17.11,A,G17.11)' , '> Value=',val,' Target=',target,' Tolerance=',tol
            endif
            IF ( ok ) THEN
               npass = npass + 1
            ELSE
               print '(A)','FAIL <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'
               nfail = nfail + 1
            ENDIF
         elseif ( cmd=='=' ) then                     ! = grab character string starting at mark
                                                      !   and ending at character before current char.
                                                      !   Test against specified string
            IF ( lnum/=markl ) CALL Error('Line Change')
            print '(4A)' , '= Value=',line(markc:cnum-1),' Target=',str(1:lstr)
            IF ( line(markc:cnum-1)==str(1:lstr) ) THEN
               npass = npass + 1
            ELSE
               print '(A)','FAIL <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'
               nfail = nfail + 1
            ENDIF
         elseif ( cmd=='?' ) then                     ! ? write character string starting at mark
                                                      !   and ending at character before current char.
            print '(A)', line(markc:cnum-1)
         elseif ( cmd=='R' ) then                     ! R produce report with specified test name
            TestName = str
            CALL Report1 
         elseif ( cmd=='X' ) then                     ! finish
            print '(A,I4,A,I4,A,I4,A)','Finished Testing',benchpasses+benchfails,' benchmarks -', &
                                        benchpasses,' passed, and',benchfails,' failed'
            exit
         endif
      enddo
      
   CONTAINS
!=======================================================================
      SUBROUTINE NextLine(n)
      INTEGER  i , n
      DO i = 1 , n
         READ(11,'(A)',IOSTAT=ios) line
         IF ( ios/=0 ) CALL Error('Next Line')
         lnum = lnum + 1
         cnum = 1
      ENDDO
      END SUBROUTINE NextLine
!=======================================================================
      SUBROUTINE PrevLine(n)
      INTEGER i , n
      n  = MIN(lnum-1,n)
      DO i = 1 , n+1
         BACKSPACE(11,IOSTAT=ios)
         IF ( ios/=0 ) CALL Error('Prev Line')
         lnum = lnum - 1
         cnum = 1
      ENDDO
      CALL NextLine(1)
      END SUBROUTINE PrevLine
!=======================================================================
      SUBROUTINE GetCmd
      CHARACTER(1) :: delim
      INTEGER :: p
      INTEGER,PARAMETER::L2U=ICHAR('A')-ICHAR('a')
      
 100  DO WHILE ( cmdline==' ' )
         READ(12,'(A)',IOSTAT=ios) cmdline
         IF ( ios/=0 ) CALL ERROR('End of Instruction Stream')
         cmdline = ADJUSTL(cmdline)
      ENDDO
      cmd = cmdline(1:1)
      if ( cmd>='a' .AND. cmd<='z' ) cmd = CHAR(ICHAR(cmd)+L2U)
      cmdtext = cmd//cmdline(2:)
      if ( cmd=='!' ) THEN
         cmdline = ' '
         goto 100
      elseif ( INDEX('+-NP',cmd)>0 ) THEN
         DO p = 2 , len(cmdline)
            IF ( INDEX('0123456789',cmdline(p:p))==0 ) EXIT
         ENDDO
         READ(cmdline(2:p-1),*,IOSTAT=ios) n
         cmdtext(p:) = ' '
         cmdline = ADJUSTL(cmdline(p:))
      elseif ( INDEX('F=R',cmd)>0 ) THEN
         delim = cmdline(2:2)
         cmdline(1:2) = '  '
         p = INDEX(cmdline,delim)
         str = cmdline(3:p-1)
         lstr = p - 3
         cmdtext(p+1:) = ' '
         cmdline = ADJUSTL(cmdline(p+1:))
      elseif ( cmd=='>' ) THEN
         delim = cmdline(2:2)
         cmdline(1:2) = '  '
         p = INDEX(cmdline,delim)
         READ(cmdline(1:p-1),*) target
         inttarg = INDEX(cmdline(1:p-1),'.')==0
         cmdline(1:p) = ' '
         p = INDEX(cmdline,delim)
         READ(cmdline(1:p-1),*) tol
         cmdtext(p+1:) = ' '
         cmdline = ADJUSTL(cmdline(p+1:))        
      else
         cmdtext = cmd
         cmdline = ADJUSTL(cmdline(2:))
      endif
!      print * , trim(cmdtext)
      END SUBROUTINE GetCmd
!=======================================================================
      SUBROUTINE Report1
      PRINT *
      IF ( nfail>0 ) THEN
         PRINT '(2A,I4,A,I4,A)' , TRIM(TestName), ' FAILED ',nfail,' fails and ',npass,' passes'
         benchfails = benchfails + 1
      ELSEIF ( npass+nfail==0 ) THEN
         PRINT '(2A,I4,A,I4,A)' , TRIM(TestName), ' ????? - no tests'
      ELSE
         PRINT '(2A,I4,A,I4,A)' , TRIM(TestName), ' PASSED ',npass,' tests - no failures'
         benchpasses = benchpasses + 1
      ENDIF
      npass = 0
      nfail = 0
      PRINT *
      END SUBROUTINE Report1  
!=======================================================================
      SUBROUTINE Error(str)
      CHARACTER(*) str
      WRITE(*,*) 'Error executing '//TRIM(CmdText)//' - '//TRIM(str)
      STOP 8
      END SUBROUTINE Error 
!=======================================================================
   END PROGRAM PBValidator
