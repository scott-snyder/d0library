      SUBROUTINE PXCOLN(IDET,INDX,ITYP,CALFLG,KCOLOR,KINTEN,KFILL,
     X                  KSTYL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call routines PXCOL1 (for device 1) or 
C-                        PXCOL2 (for device 2) to set up correct color table.
C-
C-   Inputs  : IDET   - detector code
C-                      [C*3]: CDC, MUO, CAL
C-             INDX   - index corresponding to the element of the color,
C-                      line style or line width table. (1 - 17 )
C-             ITYP   - 0 for line style,
C-                      1 for Color table and
C-                      2 for line width styles (VAX Workstations ONLY)
C-                      3 for fill if color dev or line width styles if non col
C-                      4 for fill if color dev or line style if non color
C-             CALFLG - Flag determining if the user wants to let the
C-                      subroutine make the JCOLOR, JLSTY, calls
C-                      for him/her.  .FALSE. - will not make the call for you
C-                                    .TRUE.  - will make the calls
C-                                       (it assumes a segment is open)
C-
C-   Outputs : KCOLOR - color code
C-             KINTEN - Color intensity code
C-             KFILL  - fill pattern code
C-             KSTYL  - line style
C-
C-  18-AUG-1991 Created by S. Hagopian, to simplify use of the 
C-              color Postscript hardcopier               
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXCOMK.INC/LIST'
      INTEGER  INDX, ITYP,KCOLOR, KINTEN, KFILL,
     X        KSTYL,DSPDEV, CODE, LIST, ITRY, ICTAB(17), ILTAB(17),
     X        IFTAB(17), I
      LOGICAL CALFLG,FLGVAL,FIRST
      CHARACTER*3 IDET
      CHARACTER*3 CDRV
      DATA FIRST /.FALSE./
C----------------------------------------------------------------------
C
      IF ( (IDEV .NE. 1) .AND. (IDEV .NE. 2) )GO TO 999
      IF(IDEV.EQ.1)THEN
        CALL PXCOL1(IDET,INDX,ITYP,CALFLG,KCOLOR,KINTEN,KFILL,KSTYL)
      ELSE IF (IDEV.EQ.2)THEN
C  Find out what printer driver one is using....
C  =======================================================
C
        CALL D0HDRV(2,CDRV)
        IF( CDRV.EQ. '   ' .OR. CDRV .EQ. 'DRV' ) THEN
          CALL INTMSG('0Hardcopy can not be done without a driver '//
     &               'for DI3000 device 2' //CHAR(7))
          GO TO 999
        ENDIF  
        CALL PXCOL2(IDET,INDX,ITYP,CALFLG,KCOLOR,KINTEN,KFILL,KSTYL)
      ENDIF
  999 RETURN
      END
