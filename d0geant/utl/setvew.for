      SUBROUTINE SETVEW(NMSRCP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Serts up views from SRCP file
C-
C-   Inputs  : Name of VDzero view to be made
C-   Outputs : SRCPR.INC contains SRCP array
C-   Controls: None
C-
C-   Created  20-OCT-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:SRCPR.INC'
C
      CHARACTER*(*) NMSRCP
      CHARACTER*32 NMSRC1
      CHARACTER*4 CHRACT
      INTEGER IVOLU,LEN3,IOF
      CHARACTER*4 POSTYP
C----------------------------------------------------------------------
      CALL ADDSTR(NMSRCP,'(1)',NMSRC1,LEN3)   !Makes it into array format
      CALL GTSRCP(NMSRC1,SRCPAR(1),1)
C----------------------------------------------------------------------
!Format of View Parameters is as follows
! Array(1) = View number
! Array(2) = IDHEAD . Controls plotting of HEader. See Geant manual
! Array(3) = Title as Hollerith in Header
! Array(4) = Ditto
! Array(5) = Ditto
! Array(6) = DHEAD . Character size of Heading
! Array(7) = DMAN . U co-ordinate of center of Man
! Array(8) = DMAN . V co-ordinate of center of Man
! Array(9) = DAXIS1. X0 of origin of axis
! Array(10) = DAXIS2. Y0 of origin of axis
! Array(11)= DAXIS3. Z0 of origin of axis
! Array(12)= DAXIS4. Axis size.
! Array(13)= U co-ordinate of center of scale
! Array(14)= V co-ordinate of center of scale
! Array(15)= DRWC: DRAWC will be called.=DRWX DRWX will be called.
! Array(16) = Name of Volume to be cut
! following parameters have differing meaning depending on DRWC or DRWX.
! first DRWC meaning will be quoted. Then DRWX meaning will be quoted
! Array(17) = Axis  to be cut| Theta  of line normal to cut plane.
! Array(18) = Distance from origin of cut.|Phi angle of line normal
! Array(19) = U co-ordinate of origin on screen.|Dist. from origin of cut plane
! Array(20) = V co-ordinate of origin on screen.|Viewing angle Theta
! Array(21) = scale factor for U co-ordinates.|Viewing angle Phi
! Array(22) = scale factor for V co-ordinates.|U co-ordinate on screen of volume origin
! Array(23) = NOOP  |V co-ordinate on screen of volume origin
! Array(24) = NOOP  |scale factor of U co-ordinates
! Array(25) = NOOP  |scale factor of V co-ordinates
!
      CALL GDOPEN(SRCPAR(1))
      CALL GDSCAL(SRCPAR(13),SRCPAR(14))
      CALL GSATT(SRCPAR(16),'SEEN',1)
      CALL UHTOC(SRCPAR(15),4,CHRACT,4)
      IOF = 15
      IF(CHRACT.EQ.'DRWC')THEN
        CALL GDRAWC(SRCPAR(IOF+1),SRCPAR(IOF+2),SRCPAR(IOF+3),
     &    SRCPAR(IOF+4),SRCPAR(IOF+5),SRCPAR(IOF+6),SRCPAR(IOF+7))
      ELSEIF(CHRACT.EQ.'DRWX')THEN
        CALL GDRAWX(SRCPAR(IOF+1),SRCPAR(IOF+2),SRCPAR(IOF+3),
     &    SRCPAR(IOF+4),SRCPAR(IOF+5),SRCPAR(IOF+6),SRCPAR(IOF+7),
     &    SRCPAR(IOF+8),SRCPAR(IOF+9),SRCPAR(IOF+10))
      ENDIF
      CALL GDMAN(SRCPAR(7),SRCPAR(8))
      CALL GDAXIS(SRCPAR(9),SRCPAR(10),SRCPAR(11),SRCPAR(12))
      CALL GDHEAD(SRCPAR(2),SRCPAR(3),SRCPAR(6))
      CALL GSATT(SRCPAR(16),'SEEN',1)
      CALL GDCLOS
  999 RETURN
      END
