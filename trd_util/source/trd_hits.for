      SUBROUTINE TRD_HITS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Steering routine for unpacking TRD data (either CDD4
C-   or THIT)
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-APR-1994   A. Zylberstejn
C-   Updated  18-JUL-1995   Lewis Taylor Goss  modified to call TREAD_MONITOR
C-   Updated  24-NOV-1995   A. Zylberstejn: do not call data base for MC events
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTHIT,GZTHIT
      INTEGER LOUT,NEVOLD,TRUNIT
      LOGICAL FIRST,DO_URANIUM,DB_OPENED
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      DATA NEVOLD/0/
C----------------------------------------------------------------------
      IF(IQ(LHEAD+9).EQ.NEVOLD)GO TO 999
      NEVOLD=IQ(LHEAD+9)
      IF(LQ(LHEAD-IZCDD4).NE.0)THEN
        DB_OPENED  = .FALSE.
        DO_URANIUM = .FALSE.
        IF(IQ(LHEAD+1) .LT. 1000)
     +    CALL TREAD_MONITOR(DB_OPENED,DO_URANIUM)! do not call for MC
        CALL TSETWC ! Fill bank THIT for all the TRD coded wires
C          IF(DOPRINT)THEN
C          LTHIT=GZTHIT()
C            IF(LTHIT.NE.0)THEN
C              WRITE(LOUT,*)' nb. of words in THIT bank',
C     &          IQ(LTHIT-1),' total nb. of hits',IQ(LTHIT+2)
C            ELSE
C              WRITE(LOUT,*)' lthit=0 im ttraks after tsetwc'
C            END IF
      ELSE ! No CDD4 bank: look at THIT
        LTHIT=GZTHIT()
        IF(LTHIT.EQ.0)THEN
          CALL ERRMSG(' Missing TRD information','TTRAKS',' ','W')
          GO TO 999
        END IF
        CALL THIT_GET
      END IF
  999 RETURN
      END
