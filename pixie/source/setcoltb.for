      SUBROUTINE SETCOLTB(DRIVER,DEVCOL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will set the colors in the color table given that
C-   the device used has color. This routine hould be performed only once.
C-
C-   Inputs  : DRIVER - [C*] Driver name
C-             DEVCOL - [I ] Number of color the device has
C-
C-   Outputs : None
C-
C-   Created   4-APR-1990   Lupe Howell
C-   Updated  23-APR-1990   Lupe Howell  Only do JCOLTB when GPV or XDW Drivers
C-   Updated  16-MAY-1990   Lupe Howell  Color VAX 2000 with 4 bit map using
C-                          XDW driver case had to be added
C-   Updated  13-JUN-1990   Lupe Howell  The definition of colors was made by
C-                      leaving the original default color definitions and
C-                      defining the extra color using indexes higher than 9
C-                      Any device with maximum number of colors less or equal
C-                      than 11 will not have any of the new colors.
C-   Updated   9-JUN-1992   Lupe Howell  Use DEFTERM parameter to determine type
C-                      of terminal 
C-   Updated  23-MAR-1993   Lupe Howell  Replace IDEV for 1 in JCOTBL call
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DRIVER
      INTEGER DEVCOL
C
      INCLUDE 'D0$INC:PXCOMK.INC'
c
      INTEGER ICNT, MONOCR,NUMCOL,COLORS,MAPS,RBITS,GBITS,BBITS,IBITS,
     &        RES_INDICES,REGEN, ISATS(256),ILIGHTS(256),IHUES(256),
     &        ICTAB(256),GPV_HUEANG(4), XDW_HUEANG(9),I,J,DEFTERM
      REAL   RLEVEL
C
      INTEGER NUM_XDW_NEWCOL
      PARAMETER( NUM_XDW_NEWCOL = 9 ) ! Purple,orange,blue green,new
C
      INTEGER NUM_NEWCOL
      PARAMETER( NUM_NEWCOL = 4 )       ! Purple,blue green,orange,new cyan
C
      INTEGER NUM_DI3COL
      PARAMETER( NUM_DI3COL = 10 )      ! Number of colors available in DI3000
      LOGICAL ITRY
      DATA ITRY /.TRUE./
C----------------------------------------------------------------------
      DATA GPV_HUEANG/36,288,144,320/
      DATA XDW_HUEANG/36,36,0,320,240,288,72,120,144/
C----------------------------------------------------------------------
      IF ( ITRY ) THEN
C
C ****  Checking for Evans & Sutherland and Seiko devices
C
        CALL JIQDIL(RLEVEL)
        IF(( RLEVEL.EQ.-2 ).OR.( DRIVER.EQ.'S04' )) GO TO 999
        ICNT = DEVCOL - 1               ! Initializing Color count
C
C ****  Getting Default Terminal value from PX_SYSTEM_RCP
C
        CALL PUGETV('DEFTERM',DEFTERM)
C
C ****  Exit if the Default terminal vaule is 
C ****  2 - For Black/White
C
        IF ( DEFTERM .EQ. 2 ) 
     &    GOTO 800
C
        J = 0
C
C ****  Setting colors for XDW driver (no dark shades available so those have
C ****  to be defined)
C ****  XDW driver in color monitor with 8 bit plane The number of colors
C ****  available should be at least 18
C
        IF( (DRIVER .EQ. 'XDW').AND.
     &     (ICNT .GE. (NUM_XDW_NEWCOL+NUM_DI3COL)) )THEN
          ICNT = NUM_XDW_NEWCOL
          DO 200 I=10, 18 
            J = J + 1
            ICTAB(J) = I
            IHUES(J) = XDW_HUEANG(J)
            ISATS(J) = 32767
            ILIGHTS(J) = 16383
            IF ( (I.EQ.10).OR.(I.EQ.12).OR.(I.EQ.14).OR.(I.EQ.16).OR.
     &     (I.EQ.17) )   THEN
              ILIGHTS(J) = 8000
            ENDIF
  200     CONTINUE
C
C ****  Setting color for GPV drivers with 4 bit maps (more than eleven colors
C ****  available) Checking if there is enough colors avialable to declare more
C
        ELSEIF((DRIVER .NE. 'S04').AND.(DRIVER.NE.'240').AND.
     &         (ICNT.GE.(NUM_NEWCOL+NUM_DI3COL))  ) THEN
          ICNT = NUM_NEWCOL
          DO 100 I=10, 13
            J = J + 1
            ICTAB(J) = I
            IHUES(J) = GPV_HUEANG(J)
            ISATS(J) = 32767
            ILIGHTS(J) = 16383
  100     CONTINUE
        ELSE
          GO TO 999
        ENDIF
C
C ****  Down load color table
C
        CALL JCOTBL(IDEV, ICNT, ICTAB, IHUES, ISATS, ILIGHTS)
  800   ITRY = .FALSE.
      ENDIF
  999 RETURN
      END
