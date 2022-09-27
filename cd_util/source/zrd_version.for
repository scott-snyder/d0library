      SUBROUTINE ZRD_VERSION(NCDDN,VERSION,VTYPE,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode CDDn Bank Version Number from Data.
C-
C-   Inputs  : NCDDN = CDDn data cable identifying CD subdetector.
C-                   VTX = 1, CDC = 2, FDC = 3, TRD = 4
C-   Outputs : VERSION = Version Number (most sig. 29 bits)
C-             VTYPE := Describes Data.
C-                      = 0 for D0 Real Data, = 1 for D0 Monte Carlo Data
C-                      = 2 for TB Real Data, = 3 for TB MC data.
C-             IERR    = Error Status.
C-                     = 0  if ok.
C-                     = -1, Bank does not exist.
C-                     = -2, Invalid Data Cable.
C-   Controls: none
C-
C-   Created  14-SEP-1989   Srini Rajagopalan
C-   Updated  27-OCT-1989   Qizhong Li-Demarteau   many bugs removed
C-                                 and add Stony Brook cosmic specials
C-   Updated   3-JAN-1990   Qizhong Li-Demarteau   changes made for version 3
C-                                                 format data
C-   Updated   9-MAR-1990   Qizhong Li-Demarteau   change made for new
C-                                 definition of the version number word
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
C
      INTEGER LCDDN
C
      INTEGER VERSION,IERR,VTYPE
      INTEGER MASK,SYNC,NCDDN, LENGTH
      data MASK / z'FFFF' /
C
C----------------------------------------------------------------------
C
      IF (NCDDN.EQ.1) THEN
        LCDDN = LQ(LHEAD-IZCDD1)        ! VTX
      ELSE IF (NCDDN.EQ.2) THEN
        LCDDN = LQ(LHEAD-IZCDD2)        ! CDC
      ELSE IF (NCDDN.EQ.3) THEN
        LCDDN = LQ(LHEAD-IZCDD3)        ! FDC
      ELSE IF (NCDDN.EQ.4) THEN
        LCDDN = LQ(LHEAD-IZCDD4)        ! TRD
      ELSE
        IERR = -2
        RETURN
      ENDIF
C
      IF (LCDDN.EQ.0) THEN
        IERR = -1
        RETURN
      ENDIF
C
C *** Assume version number as zero. Overwrite if not correct
C
      VERSION = 0
C
C *** Require the lower 16 bits of the second word to be the SYNC word
C     for higher Versions
C
      SYNC = IAND(IQ(LCDDN+2), MASK)
      IF (SYNC .EQ. MASK) THEN
        IF (IQ(LCDDN+1).EQ.1) THEN
          VERSION = 1
        ELSE
          IF (IQ(LCDDN+1).EQ.5) THEN
            VTYPE = IBITS(IQ(LCDDN+5),29,2)
            VERSION = IBITS(IQ(LCDDN+5),0,29)
          ENDIF
          IF (IQ(LCDDN+1).EQ.3) THEN
            VTYPE = IBITS(IQ(LCDDN+4),29,2)
            VERSION = IBITS(IQ(LCDDN+4),0,16)
          ENDIF
        ENDIF
      ENDIF
C
C    following part is special for Stony Brook 1989 cosmic ray data
C
      IF (NCDDN .EQ. 2 .AND. VERSION .EQ. 0) THEN
        LENGTH = IQ(LCDDN-1)
        IF ((LENGTH-2 .EQ. IQ(LCDDN+LENGTH)) .AND. (IQ(LCDDN+1) .EQ. 1))
     &    VERSION = 1
      ENDIF
C
  999 RETURN
      END
