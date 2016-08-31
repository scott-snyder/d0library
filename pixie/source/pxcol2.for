      SUBROUTINE PXCOL2(IDET,INDX,ITYP,CALFLG,KCOLOR,KINTEN,KFILL,
     X                  KSTYL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Creates a color, a line style or line width table.
C-               for DEVICE 2, the optional HARDCOPY device.
C-               The color table will give a series of grey shades to
C-               simulate  color in non-color devices or filling patterns
C-               in those devices that does not support grey.
C-               The line width styles available in this table are only in VAX
C-               work stations, other devices do not support thjis feature.
C-               This routine will return the INDXth element on the table.
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
C-   Created   3-APR-1989   LUPE ROSAS
C-   Updated  14-SEP-1989   Lupe Rosas Extra parameter added KINTEN,special
C-                                case consider when black is requested.
C-   Updated   5-OCT-1989   Lupe Rosas Foreground color setting
C-   Updated   3-JAN-1990   Lupe Howell Then index number greather than 17
C-                          will be accepted and equivalent index will be
C-                          given. When CALFLG is TRUE will make the JCOLR
C-                           call only.
C-   Updated   1-MAR-1990   Lupe Howell a UIS call was made to distinguish from
C-                          a color VAX station and an intensity VAX Sta.
C-   Updated   4-APR-1990   Lupe Howell  XDW driver.
C-   Updated  10-APR-1991   Lupe Howell   The color table intesity setting
C-                          and special colors done here.
C-   Updated  19-AUG-1991   Sharon Hagopian, for color postscript device.
C-   Updated  21-JAN-1992   Lupe Howell  The call to COLOR routine changed to
C-                          PX_COLOR 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXCOMK.INC/LIST'
      INTEGER  INDX, ITYP, OLDITYP,KCOLOR, KINTEN, KFILL,
     X        KSTYL,DSPDEV, CODE, LIST, ITRY, ICTAB(17), ILTAB(17),
     X        IFTAB(17), I
      INTEGER GREYSCALE,DEVCOL
      PARAMETER( GREYSCALE = 36 )
      CHARACTER*3 IDET
      CHARACTER*3 DRVNAM
      CHARACTER*1 ANSWER
      LOGICAL COLDEV, RETAIN
      LOGICAL CALFLG,FLGVAL,FIRST,INTENSITY
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C **** Checking valid input
C
      IF ( INDX.LT.1 )
     X  GO TO 999
      IF ( (ITYP .LT. 0) .OR. (ITYP .GT.4) )
     X  GO TO 999
c      IF ( (CALFLG .NE. .TRUE.) .AND. (CALFLG .NE. .FALSE.) )
c     X  GO TO 999
      IF (INDX .GT.17) THEN
        IF ((ITYP .NE. 0).OR.(ITYP.NE.4) ) THEN
          INDX = MOD ( INDX,17 )
          IF (INDX .EQ. 0) INDX = 17
        ELSE
          INDX = MOD (INDX,10)
          IF (INDX .EQ. 0) INDX = 10
        ENDIF
      ENDIF
      IF (ITYP .NE. OLDITYP )   ITRY = 0
C
C ****  Setting up tables
C
      IF ( ITRY .EQ. 0 ) THEN
C
C ****  Cleaning arrays
C
        DO 10 I=1, 16
          IFTAB(I) = 0
   10   CONTINUE
        DO 20 I=1, 17
          ICTAB(I) = 0
          ILTAB(I) = 0
   20   CONTINUE
C
C ****  Define driver and number of colors
C
        OLDITYP = ITYP
        CODE = 1
        DSPDEV = 2
        CALL JIQDEV( DSPDEV, CODE, LIST )
        CALL D0HDRV( DSPDEV, DRVNAM )
        ITRY = ITRY + 1
C
C
C ****  Defining Tables
C
        IF((DRVNAM .EQ. 'PST').AND.(LIST.GT.GREYSCALE)) THEN !  Color device
C
C **** Color Device
C
          COLDEV = .TRUE.
          IF ( (ITYP.EQ.1).OR.(ITYP.EQ.3).OR.(ITYP.EQ.4) ) THEN
            CALL PX_COLOR( DRVNAM, ICTAB, IFTAB )          ! Setting colors
          ELSEIF(ITYP .EQ. 2) THEN                      ! Setting Lines width
            CALL LINWID(DRVNAM,ILTAB)
          ELSE
            CALL LSTYL(DRVNAM, ILTAB)         ! Line styles in color dev
          ENDIF
C
C *****  Non Color Device
C
        ELSEIF ( ITYP .EQ. 1 ) THEN
            IF(LIST.LE.2)THEN
              CALL HRDCPY( DRVNAM, ICTAB, IFTAB )
            ELSE
              CALL GREY( IFTAB, ICTAB, DRVNAM )           ! B/W fill
            ENDIF
          COLDEV = .FALSE.
        ELSEIF ( (ITYP .EQ. 2).OR.(ITYP .EQ. 3) ) THEN
          CALL LINWID(DRVNAM,ILTAB)                   ! B/W line width
        ELSE
          CALL LSTYL(DRVNAM, ILTAB)                   ! B/W line style
          COLDEV = .FALSE.
        ENDIF
      ENDIF
C
C ****  Setting outputs
C
      IF ((ITYP.EQ.1).OR.(ITYP .EQ.3).OR.(ITYP.EQ.4)) THEN  ! Fills
        KCOLOR = ICTAB(INDX)
        KINTEN = IFTAB(INDX)
        IF( (( INDX .EQ. 2 ).OR.(INDX.EQ.17).OR.(INDX.EQ.1))
     &         .AND.(.NOT.COLDEV) ) THEN
          KFILL = 1               ! Special case for black in non-col dev
        ELSE
          KFILL = IFTAB(INDX)
        ENDIF
        KSTYL = 0
        IF (((ITYP.EQ.3).OR.(ITYP.EQ.4)).AND.(.NOT.COLDEV)) THEN
          KSTYL  = ILTAB(INDX)   ! Line width style if none col dev.
          KFILL  = 0
          KCOLOR = 0
        ENDIF
      ELSEIF ( ITYP .EQ. 0 )  THEN ! non-color line style
        KSTYL  = ILTAB(INDX)
        KCOLOR = 0
        KFILL  = 0
      ELSEIF ( (ITYP .EQ. 2) )THEN  ! line widths
        KSTYL  = ILTAB(INDX)
        KCOLOR = 0
        KFILL  = 0
      ENDIF
C
C ****  Calls
C
      IF ( CALFLG ) THEN
        IF ( ITYP .EQ. 0 ) THEN  ! Line styles
          CALL JLSTYL( KSTYL )
        ELSEIF ( ITYP .EQ. 1 ) THEN ! Fill
          CALL JCOLOR( KCOLOR )
        ELSEIF ( ITYP .EQ. 2 ) THEN  ! Line width
          CALL JLWIDE( KSTYL )
        ELSEIF ( ITYP .EQ. 3 ) THEN  ! Fill if color dev line width if not
          IF( COLDEV ) THEN
            CALL JCOLOR( KCOLOR )
          ELSE
            CALL JLWIDE( KSTYL )
          ENDIF
        ELSEIF ( ITYP .EQ. 4 ) THEN  ! Fill if color dev line style if
          ! not
          IF ( COLDEV ) THEN
            CALL JCOLOR( KCOLOR )
          ELSE
            CALL JLSTYL( KSTYL )
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
