      SUBROUTINE PXLRUN(XPOS,YPOS,ZPOS)
C======================================================================
C
C  Description:  Labels display with run number and event number
C                or run number and beam crossing number for DAQ     
C  ============
C
C
C  Input Arguments:  XPOS - x value in world coord. of start of label
C                    YPOS - y value in world coord. of start of label
C                    ZPOS - z value in world coord. of start of label
C  ================
C
C
C  Output Arguments:
C  =================
C  None
C
C
C  Author:
C  =======
C  Written by S. Hagopian - Sept. 9, 1987
C======================================================================

      IMPLICIT NONE
C
C  Include Statements:
C  ===================
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INCLUDE 'D0$INC:GRAPHF77.INC/LIST'
C
C  Local Declarations:
C  ===================
C
      REAL XPOS,YPOS,ZPOS             ! Starting position of label on screen
      INTEGER NDIG
      CHARACTER*40 MESS
      INTEGER NEVT1,NEVT2  ! Event number or beam crossing numbers
      CHARACTER*6 CRUN
      INTEGER IRUN
      CHARACTER*8 CEVT1,CEVT2 ! Character represtation of event number
      CHARACTER*8 CBLANK
      DATA NDIG/8/
      DATA NEVT1,NEVT2/0,0/
      DATA CBLANK/'        '/
C
C
C  Executable Code:
C  ================
C
      CALL JOPEN
      CALL JSIZE(10.,5.)
      CALL J3MOVE(XPOS,YPOS,ZPOS)
      NEVT1=IQ(LHEAD+9)
      IF(NEVT1.NE.0)THEN
        NEVT2=0
      ELSE
        NEVT1=IQ(LHEAD+7)
        NEVT2=IQ(LHEAD+8)
      ENDIF
      CALL H3ITOC(NEVT1,NDIG,CEVT1)
      CALL H3ITOC(NEVT2,NDIG,CEVT2)
      IRUN=IQ(LHEAD+6)
      CALL H3ITOC(IRUN,NDIG,CRUN)
      MESS=' RUN'//CRUN//CEVT1//CEVT2
      CALL J1STRG(MESS)
      CALL JCLOSE
C
C
  900 CONTINUE
      RETURN
      END
