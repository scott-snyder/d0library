      SUBROUTINE BKTPRL(LTPRL,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank TPRL
C-
C-   Inputs  : LAYER integer  Layer number (1 to 3)
C-   Outputs : LTPRL integer  Link of Booked TPRL Bank
C-
C-   Controls: None
C-
C-   Created  30-OCT-1989 17:56:44.87  A. Zylberstejn
C-   Updated  14-SEP-1991   A. Zylberstejn  : Change bank format for COSMIC 1
C-                                            run
C-   Updated  19-JAN-1993   Alain PLUQUET   New TPRL structure (version>=2.0)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LAYER,LTPRL,LTRDT,GZTRDT,IXIO
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL DOPRINT,TRD_DO_PRINT
      INTEGER LOUT,TRUNIT,LTPRLI
      INTEGER NEED,N_IN_THIT,NWDS_USED, NWDS_MAX
      LOGICAL FIRST
      CHARACTER*1 C1(3)
      DATA FIRST/.TRUE./
      DATA C1/'1','2','3'/
      DOPRINT=TRD_DO_PRINT()
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        CALL MZFORM('TPRL','2F 9I 2F -I',IXIO)
      ENDIF
      LTPRL=0
      LTRDT=GZTRDT()
C  Check if enough room in ZEBCOM
      NEED = 1
      CALL MZNEED(IXMAIN, NEED, 'G')
      NWDS_USED = IQUEST(12)
      NWDS_MAX = IQUEST(13)
      IF(DOPRINT)WRITE(LOUT,*)'ltrdt in bktprl',LTRDT,' layer',
     &  LAYER,' nwds_used,nwds_max',NWDS_USED,NWDS_MAX
      IF ((NWDS_MAX - NWDS_USED) .LT. 1000) THEN
        CALL ERRMSG(' not enough room for bank TPRL layer '
     &        //C1(LAYER),'BKTPRL',' ','W')
        GO TO 999
      END IF
C      if(doprint)print*,' appel a mzbook dans bktrdt'
C
      CALL MZBOOK
     &    (IXMAIN,LTPRL,LTRDT,-LAYER,'TPRL',0,0,200,IXIO,0)
  999 RETURN
      END
