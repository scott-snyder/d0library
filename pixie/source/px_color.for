      SUBROUTINE PX_COLOR( DRVNAM, ICTAB, IFTAB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test 14 diff colors.  The first two colors are for
C-                foreground and backround (white and black).
C-                For GPV and SEIKO drivers:
C-                GPV - using JCOTBL create 10 different colors and
C-                      using JPIDEX shades create four more colors
C-                      Colors are:
C-                           White, Black,
C-                           Dark purple, Purple,     Dark blue,  Blue,
C-                           Cyan, Dark green, Green,
C-                           Blue green, Dark red,   Red,
C-                           Dark Magenta, Magenta, Orange, Yellow and
C-                           Complement
C-               SEIKO- Using the six original color in DI3000 and using
C-                      JPIDEX shades to create six more.
C-                           The same color as above only that there is no
C-                           kelly green or brown in their pplace the color
C-                           dark red and green is repeated.
C-
C-   Inputs  : DEVNAM - driver name in the device
C-
C-   Created  21-JAN-1992   Lupe Howell This rotuine is the old COLOR routine 
C-                          The name was changed to avoid any cofusion.
C-   Updated   8-MAR-1993   Lupe Howell  Add pst hardcopy 
C-   Updated  23-MAR-1993   Lupe Howell  Replace 1 for IDEV in JIQDEV call 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXCOMK.INC/LIST'
C----------------------------------------------------------------------
      INTEGER ICTAB(*), IFTAB(*), I, 
     X        CORDER(17), FORDER(17), 
     X        CSKORD(16), FSKORD(16), VORDER(16), VFORDR(16),ENSCOL(17),
     X        XCORDER(17),PCORDER(17),UORDER(17),HPCORDER(17)
      INTEGER DEVCOL,DEFTERM
      REAL        RLEVEL
      CHARACTER*(*)DRVNAM
C----------------------------------------------------------------------
C  GPV drive's order
      DATA CORDER/ 7,8,10,10, 4,4,13, 2,2,11, 5, 1,1,5,12,3,0/
      DATA FORDER/ 1,1,52, 1,52,1, 1,52,1, 1,52,52,1,1, 1,1,1/
C----------------------------------------------------------------------
C UISDI3 Color order
      DATA UORDER/7,8,4,4,4,4,6,2,2,2,1,1,1,5,3,3,7/
C----------------------------------------------------------------------
C PST driver for GREY scale's order
      DATA HPCORDER/
     &  7, 8,40,  ! White, Black, D. Purple
     &  20,39, 3, ! Purple, D. Blue, Blue
     &  6,51,2,   ! Cyan, D. Green, Green
     &  34,41,43, ! Blue Green, D. Magenta, D. Red,
     &  4,5,27,1,0/
C----------------------------------------------------------------------
C PST color driver order
      DATA PCORDER/ 7,8,40,20,39,4,6,51,2,34,41,43,1,5,27,3,8/
C----------------------------------------------------------------------
C XDW driver's order
      DATA XCORDER/7,8, 10,11,12,4,13,14,2,15,16,17,1,5,18,3,0/
C----------------------------------------------------------------------
C  S04 driver
      DATA CSKORD/ 7, 8, 5, 5,  4,  4,  6,  2,  2,  2,  5,  1,  1, 5,
     X              1, 3/
      DATA FSKORD/ 1, 1, 10, 1, 10,  1, 1,  10, 1,  1,  10, 10, 1, 1,
     X            10, 1/
C----------------------------------------------------------------------
C  240 driver
      DATA VORDER/ 0, 8, 7*2, 7*1 /
      DATA VFORDR/ 16*1 /
C----------------------------------------------------------------------
C Eans & sutherland device
      DATA        ENSCOL/0, 0, 13, 31, 4, 15, 6, 2, 17, 11,
     &            5, 1, 14, 20, 16, 3, 0/
C----------------------------------------------------------------------

      CALL JIQDEV(IDEV, 1, DEVCOL)
C
C *** Cleaning the arrays
C
      DO 20 I=1, 16
        IFTAB(I) = 0
   20 CONTINUE
C
C ****  Checking device
C
      CALL JIQDIL(RLEVEL)               ! To test E&S device
      IF( DRVNAM .EQ. 'S04' ) THEN
        GO TO 145   ! SEIKO
      ELSEIF (DRVNAM .EQ. '240') THEN
        GO TO 245    ! VT240
      ELSEIF((DRVNAM .EQ. 'XDW').AND.(DEVCOL.NE.10)) THEN
        DO 30 I=1, 17
          CORDER(I) = XCORDER(I)
          FORDER(I) = 1
   30   CONTINUE
C
C ****  Fill the table if Postscrip driver use and 
C ****  defterm eq 4.
C
      ELSEIF((DRVNAM .EQ. 'PST').AND.(DEVCOL.GT.36)) THEN
        CALL EZPICK('PX_SYSTEM_RCP')
        CALL PUGETV('DEFTERM',DEFTERM)
        CALL EZRSET
C GREY SHADE PST
        IF ( ( IDEV .EQ. 2 ) .AND. ( DEFTERM .EQ. 4 ) )THEN
          DO 40 I=1, 17
            ICTAB(I) = HPCORDER(I)
            IFTAB(I) = 1
 40       CONTINUE
          GOTO 270
        ENDIF
c color PST
        IF ( ( IDEV .EQ. 2 ) .AND. ( DEFTERM .EQ. 1 ) )THEN
          DO 41 I=1, 17
            ICTAB(I) = PCORDER(I)
            IFTAB(I) = 1
   41     CONTINUE
          GOTO 270
        ENDIF
      ELSEIF ( RLEVEL.LT.0 ) THEN           ! Evens & Sutherland machine
        GO TO 260 
      ENDIF
C
C ****  Set color table for UISDI3
C
      IF (DEVCOL.EQ.12 ) THEN
        DO 130 I=1, 17
          ICTAB(I) = UORDER(I)
          IFTAB(I) = FORDER(I)
  130   CONTINUE
        GO TO 270
      ENDIF
C
C ****  Setting color tables for devices with number of colors greather than 11
C
      IF (DEVCOL.GT.11 ) THEN
        DO 140 I=1, 17
          ICTAB(I) = CORDER(I)
          IFTAB(I) = FORDER(I)
  140   CONTINUE
        GO TO 270
      ENDIF
C
C ****  Setting Color and Fill tables for GPV,
C ****  SEIKO,MAC drives and devices with less
C ****  than 12 colors available
C ****  If the MAC driver is being used do not use
C ****  the draker versions because they do not work there
C
  145 IF ( DRVNAM .EQ. 'MAC' ) THEN ! Intensity fill 
        Do 143 I=1, 16
          IFTAB(I) = 1
  143   CONTINUE
      ELSE
        DO 200 I=1, 16
          IFTAB(I) = FSKORD(I)
  200   CONTINUE
      ENDIF
      DO 225 I=1, 16    ! Color fill
        ICTAB(I) = CSKORD(I)
  225 CONTINUE
      IFTAB(17) = 1 
      ICTAB(17) = 0                     ! Foreground
      GO TO 999
C
C ****  Setting color and fill tables for VT240 drivers
C

  245 CONTINUE
      DO 248 I=1,16
        IFTAB(I) = VFORDR(I)
        ICTAB(I) = VORDER(I)
  248 CONTINUE
      GO TO 270
C
C ****  Setting color and fill table for Evans & Sutherland devices
C
  260 CONTINUE
      DO 261 I = 1, 17
        IFTAB(I) = 32767 
        ICTAB(I) = ENSCOL(I)
  261 CONTINUE
      GO TO 999
  270 CONTINUE
  999 RETURN
      END
