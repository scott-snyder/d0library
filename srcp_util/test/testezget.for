      PROGRAM TESTEZGET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test of EZGET etc..
C-
C-   Inputs  : SRCP        DEFINEd to be TESTSRCP.DAT
C-   Outputs : FOR001.DAT
C-   Controls:
C-
C-   Created  6-JAN-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN
      PARAMETER( LUN = 1 )
      LOGICAL BOOLE
      INTEGER LSRCP,IDATUM,NUM,I,ERROR,WRDIDS,LBANK,IZLINK
      INTEGER IARRAY(10)
      REAL    RARRAY(10)
      CHARACTER*4 CARRAY(10)
      EQUIVALENCE ( RARRAY(1), IARRAY(1), CARRAY(1) )
C
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C ****  Initialize ZEBRA
C
      CALL MZEBRA(0)
C
C ****  Initialize STP
C
      CALL INZSTP
C
C ****  Read in Text file and create SRCP bank
C       named FIRST_COPY as a stand-alone bank
C
      WRDIDS = 8        ! 32-Characters/record
      LBANK  = 0        ! Create stand-alone
      IZLINK = 0
      OPEN (UNIT=LUN,FILE='SRCP',STATUS='OLD',ERR=999)
      CALL EZREAD (LUN,'FIRST_COPY',WRDIDS,LBANK,IZLINK)
      CLOSE (UNIT=LUN)
C
C
C ****  Read in copies of the text file into different SRCP banks
C
      OPEN (UNIT=LUN,FILE='SRCP',STATUS='OLD',ERR=999)
      CALL EZREAD (LUN,'SECOND_COPY',WRDIDS,LBANK,IZLINK)
      CLOSE (UNIT=LUN)
C
      OPEN (UNIT=LUN,FILE='SRCP',STATUS='OLD',ERR=999)
      CALL EZREAD (LUN,'THIRD_COPY',WRDIDS,LBANK,IZLINK)
      CLOSE (UNIT=LUN)
C
C
C ****  Move third copy to first link below bank STPO
C
      LBANK = LC(LSTPH-IZSTPO) ! Get address of bank STPO
      IZLINK = 1
      CALL EZSHUNT ('THIRD_COPY',LBANK,IZLINK)
C
C
C ****  Print out contents of SRCP bank in original order
C       Note the use of zero in the second argument; this means
C       use the currently selected SRCP bank.
C
      CALL EZDUMP (LUN,0,0)
C
C ****************************************************
C ****  Change some parameters in each SRCP bank  ****
C ****************************************************
C
      CALL EZPICK ('FIRST_COPY')                ! Select bank FIRST_COPY
      CALL EZSET  ('MIXTUR(7:7)',5555,ERROR)    ! Set element 7 to 5555
      CALL EZRNAM ('FIRST_COPY','MAIN_COPY')    ! Rename bank
C
      CALL EZPICK ('SECOND_COPY')               ! Select bank SECOND_COPY
      CARRAY(1) = 'HELL'
      CARRAY(2) = 'FIRE'
      CALL EZSETA ('HOLLER',2,3,1,IARRAY(1),ERROR)! Modify elements 2, 3
C
      CALL EZPICK ('THIRD_COPY')                ! Select bank THIRD_COPY
      RARRAY(1) = 333.0
      RARRAY(2) = 222.0
      RARRAY(3) = 111.0
      CALL EZSET  ('DATA(1:6)2',RARRAY(1),ERROR)! Modify elements 1, 3, 5
C
C ****  Print out names and addresses of SRCP banks
C
      CALL EZDBUG (6)
      CALL EZDBUG (LUN)
C
C **********************************************
C ****  Get some parameters from SRCP bank  ****
C **********************************************
C
C
C ****  PARAMETER       MIXTUR(*)       SELECT BANK MAIN_COPY
C
      CALL EZPICK ('MAIN_COPY')
      CALL EZGET  ('MIXTUR(7:7)',IDATUM,ERROR)
      WRITE(6,90)   IDATUM
      WRITE(LUN,90) IDATUM
C
C ****  PARAMETER       LOGICAL
C
      CALL EZGET ('LOGICAL',BOOLE,ERROR)
      WRITE(6,100)   BOOLE
      WRITE(LUN,100) BOOLE
C
      CALL EZSET ('LOGICAL',.NOT. BOOLE,ERROR) ! Flip truth value of flag
C
      CALL EZGET ('LOGICAL',BOOLE,ERROR)
      WRITE(6,100)   BOOLE
      WRITE(LUN,100) BOOLE
C
C
C ****  PARAMETER       HOLLER(*)       SELECT BANK SECOND_COPY
C
      CALL EZPICK ('SECOND_COPY')
      CALL EZGET  ('HOLLER()',NUM,ERROR)        ! Get length of array HOLLER
      WRITE(6,110)   NUM
      WRITE(LUN,110) NUM
C
      CALL EZGET ('HOLLER',IARRAY(1),ERROR)    ! Get array HOLLER
      WRITE(6,120)   (CARRAY(I),I=1,NUM)
      WRITE(LUN,120) (CARRAY(I),I=1,NUM)
C
C
C ****  PARAMETER       DATA(*)         SELECT BANK THIRD_COPY
C
      CALL EZPICK ('THIRD_COPY')                ! Select bank THIRD_COPY
      CALL EZGETA ('DATA',1,6,2,RARRAY(1),ERROR)
      WRITE(6,130)   (RARRAY(I),I=1,3)
      WRITE(LUN,130) (RARRAY(I),I=1,3)
C
C
C ****  PARAMETER       WINOS(*)
C
      CARRAY(1) = 'DREI'
      CARRAY(2) = 'FIER'
      CARRAY(3) = 'FUNF'
      CALL EZSET ('WINOS(3:5)',IARRAY(1),ERROR)! Modify elements 3 thru 5
      CALL EZGETA('WINOS',1,6,1,IARRAY(1),ERROR)! Get elements 1 thru 6
      WRITE(6,140)   (CARRAY(I),I=1,6)
      WRITE(LUN,140) (CARRAY(I),I=1,6)
C
   90 FORMAT(//,1X,'MIXTUR(7:7)       > ',I10)
  100 FORMAT(   1X,'LOGICAL           > ',L10)
  110 FORMAT(   1X,'Length of HOLLER  > ',I10)
  120 FORMAT(   1X,'HOLLER(*)         > ',4A10)
  130 FORMAT(   1X,'DATA(1:6) step 2  > ',3F10.0)
  140 FORMAT(   1X,'WINOS(1:6)        > ',3A10,/,
     &          1X,'                    ',3A10/)
C
  999 CONTINUE
      END
