      SUBROUTINE BKMHIT(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MHIT'.
C-
C-   Inputs  :
C-      LSUP    I   address of support bank.
C-      NDAT    I   number of data words
C-
C-   Outputs :
C-      LADD    I   address of bank, MHIT.
C-
C-   Controls:
C-
C-   Created   22-MAR-1990   KUNORI
C-   Modified  05-DEC-1991   S. Abachi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMHIT.LINK'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR
C  -- local variables...
      INTEGER LSUP1,IREF,IOH
      LOGICAL FIRST
C  -- external...
C  -- initialize data...
      DATA FIRST/.TRUE./
      SAVE FIRST,IOH
C----------------------------------------------------------------------
C
C     -- save supprot bank address...
      IF(LSUP.NE.0) THEN
        CALL GRLINK('BKMHIT  ',IREF)
        LRLINK(IREF)=LSUP
      ENDIF
C
      IF(FIRST) THEN
        CALL MZFORM('MHIT','4I/3F',IOH)
        FIRST = .FALSE.
      ENDIF
C
      IF(NDAT .EQ. 0) THEN
        CALL ERRMSG('MUON','BKMHIT',
     +    'CANNOT BOOK MHIT, Number of data words is 0','F')
        GO TO 999
      ENDIF
C
      LADDR=0
C
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('MUON','BKMHIT',
     +    'CANNOT BOOK MHIT, SUPPORT BANK ADDRESS IS 0.','F')
        GO TO 999
      ELSE
        LSUP1=LSUP
      ENDIF
C
C  -- Book bank...
C
      CALL MZBOOK(IXMAIN,LADDR,LSUP,-IZMHIT,
     &                      'MHIT',1,1,NDAT,IOH,-1)
C
      IQ(LADDR+1)=1      ! version number
C
C     -- restore supprot bank address...
      IF(LSUP.NE.0) THEN
        LSUP=LRLINK(IREF)
        CALL RRLINK('BKMHIT  ',IREF)
      ENDIF
C
  999 RETURN
      END
