      SUBROUTINE CC_MG_DIV
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output CC Massless Gap Divisions to SRCP_UCAL.DAT
C-   Follows convention of WRTZDIV in writing Massless Gap Divisions
C-
C-   Inputs  : none
C-   Outputs : none (SRCP file)
C-   Controls: none
C-
C-   Created  28-JUN-1989   Chip Stewart
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Characters
      CHARACTER*32 CCMGZDV(2)
      CHARACTER*32 NMSRCP,NAME
C  Include Files
      INCLUDE 'D0$INC:WRITE_UNIT.INC'
C  Integers
      INTEGER IZ
      INTEGER LSR(1000)
      INTEGER LN
      INTEGER IPT,I,K
C  Reals
      REAL RSR(1000)
      EQUIVALENCE (LSR,RSR)
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C  DATA for SRCP Names
      DATA CCMGZDV/'CCMG_DIVISIONS+Z',    !Massless gap Z divisions
     &            'CCMG_DIVISIONS-Z'/
C----------------------------------------------------------------------
      DO 10 IZ = 1, 2
        NAME = CCMGZDV(IZ)
C       CALL ADDSTR(NAME,'(1:1)',NMSRCP,LN)
C       CALL GTSRCP(NMSRCP,LSR,1)
        CALL EZGSET(NAME,LSR,1)
C
        LN = LEN(NAME)
        WRITE(OUT_VOL,1)NAME(1:LN)
    1   FORMAT('\ARRAY ',A)
        WRITE(OUT_VOL,2)LSR(1),LSR(2),RSR(3)
    2   FORMAT(2I8,F8.2,' !Num Z div, Mat no,Stagger of module ')
        IPT = 3
        DO 8 I = 1,LSR(1)
          WRITE(OUT_VOL,3)LSR(IPT+1),CM_PER_INCH*RSR(IPT+2),
     &    CM_PER_INCH*RSR(IPT+3),
     &    LSR(IPT+4),(CM_PER_INCH*RSR(IPT+4+K),K=1,LSR(IPT+4))
    3     FORMAT(2X,'''',A4,'''',2F10.3,/,I10,/,(7F10.3))
          IPT = IPT + 3 + LSR(IPT+4)+ 1
          WRITE(OUT_VOL,5) LSR(IPT+1),(RSR(IPT+1+K),K=1,LSR(IPT+1))
    5     FORMAT(I10,/,(7F10.3))
          IPT = IPT + LSR(IPT+1) + 1
          WRITE(OUT_VOL,6)LSR(IPT+1),(LSR(IPT+1+K),K=1,LSR(IPT+1))
    6     FORMAT(I10,/,(7I10))
          IPT = IPT + LSR(IPT+1) + 1
          WRITE(OUT_VOL,7)
    7     FORMAT('\END ')
    8   CONTINUE
   10 CONTINUE
  999 RETURN
      END
