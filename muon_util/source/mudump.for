C DEC/CMS REPLACEMENT HISTORY, Element MUDUMP.FOR
C *1    28-APR-1989 18:10:54 TAMI "Dump an event in EXAMINE"
C DEC/CMS REPLACEMENT HISTORY, Element MUDUMP.FOR
C  
C======================================================================
      SUBROUTINE MUDUMP
C======================================================================
C
C  Description:  Checks Parameters for which banks one would like 
C  ============  dumped, and dumps them to EVENT_DUMP.DAT or to screen
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - January 2,1989
C   DH 3/92 use D0OPEN
C=======================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INTEGER LIN,I,ERR,LIBREP,DDBANK
      INTEGER JDUM
      LOGICAL OK
      CHARACTER*1 CDUM
      DATA JDUM/0/
      DATA CDUM/'.'/
C
C  Executable Code:
C  =================
C
      CALL GTSRCP('DDBANK',DDBANK,1)
      IF(DDBANK.LT.0) RETURN
      CALL GTUNIT(2,LIN,ERR)
      CALL D0OPEN(LIN,'EVENT_DUMP.DAT','OF',OK)
      IF (DDBANK .EQ. 0) THEN               
         CALL PRMUD1(LIN,JDUM,JDUM,CDUM,JDUM)
      ELSEIF (DDBANK .EQ. 1) THEN
         CALL PRMUOH(LIN,JDUM,JDUM,CDUM,JDUM)
      ELSEIF (DDBANK .EQ. 10) THEN 
         CALL MUPRT(LIN)
      ENDIF
      CALL RLUNIT(2,LIN,ERR)
      CALL INTMSG(' EVENT WAS DUMPED')
C      CALL HDSRES
      IF (DDBANK .EQ. 0) THEN               
         CALL PRMUD1(6,JDUM,JDUM,CDUM,JDUM)
      ELSEIF (DDBANK .EQ. 1) THEN
         CALL PRMUOH(6,JDUM,JDUM,CDUM,JDUM)
      ELSEIF (DDBANK .EQ. 10) THEN 
         CALL MUPRT(6)
      ENDIF
      CALL WAIBIT(2)
      I = LIBREP()
      RETURN
      END
