C DEC/CMS REPLACEMENT HISTORY, Element NOI_CAD_PICK.FOR
C *1     2-JUN-1992 17:09:03 STEWART "update from Allen Mincer for new CAHITS"
C DEC/CMS REPLACEMENT HISTORY, Element NOI_CAD_PICK.FOR
      SUBROUTINE NOI_CAD_PICK(NOITYP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SELECT the type of NOISY produced CAD
C-                          BANK to analyze
C-
C-   Inputs  : NOITYP = 0 for no noise
C-                      1 for level 1
C-                      2 for level 2 and offline
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-MAY-1992   Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INTEGER LCAD1,LCAD2,NOITYP,ICOUNT,OLDTYP,NOIVER
      INTEGER MCAD1
      LOGICAL FOUND
      DATA OLDTYP/-99999/
C----------------------------------------------------------------------
      IF (OLDTYP.NE.NOITYP)THEN
        IF(NOITYP.EQ.0)THEN
          WRITE(*,*)' NOI_CAD_PICK: NOISE FREE BANKS SELECTED'
        ELSEIF(NOITYP.EQ.1)THEN
          WRITE(*,*)' NOI_CAD_PICK: LEVEL 1 BANKS SELECTED'
        ELSEIF(NOITYP.EQ.2)THEN
          WRITE(*,*)' NOI_CAD_PICK: LEVEL 2 BANKS SELECTED'
        ELSE
          CALL ERRMSG('NOI_CAD_PICK','NOISY',
     &                     'ERROR IN SELECTED BANK','W')
        ENDIF
        ICOUNT=-1
        MCAD1=LHEAD-IZCAD1
10      CONTINUE
        LCAD1=LQ(MCAD1)
        IF (LCAD1.LE.0)THEN
          FOUND=.FALSE.
        ELSE
          ICOUNT=ICOUNT+1  
          NOIVER=IBITS(IQ(LCAD1+4),20,2)
          IF(NOIVER.EQ.NOITYP)THEN
            FOUND=.TRUE.
            OLDTYP=NOITYP
          ELSE
            MCAD1=LCAD1
            GOTO 10
          ENDIF
        ENDIF
        IF(.NOT.FOUND)THEN
          CALL ERRMSG('NOI_CAD_PICK','NOISY',
     &                     'SELECTED BANK NOT FOUND','W')
        ENDIF
      ENDIF
      IF(FOUND)THEN
        IF(ICOUNT.GT.0)THEN
          LCAD1=LQ(LHEAD-IZCAD1)
          LCAD1=LQ(LCAD1)
          IF(ICOUNT.GT.1)LCAD1=LQ(LCAD1)
          CALL ZSHUNT(IXMAIN,LCAD1,LHEAD,-IZCAD1,0)
          LCAD2=LQ(LHEAD-IZCAD2)
          LCAD2=LQ(LCAD2)
          IF(ICOUNT.GT.1)LCAD2=LQ(LCAD2)
          CALL ZSHUNT(IXMAIN,LCAD2,LHEAD,-IZCAD2,0)
        ENDIF
      ENDIF
  999 RETURN
      END
