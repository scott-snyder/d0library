      SUBROUTINE CL1PHET(L1IPHI,L1IETA,EM_ET,TOT_ET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get FADC ET for this Trigger Tower from
C-                         the TRGR block.
C-
C-   Inputs  : L1IPHI,L1IETA,
C-   Outputs : EM_ET = Transverse EM Energy
C-             TOT_ET = Transverse (EM+HAD) Energy
C-   Controls:
C-
C-   Created   5-FEB-1990          Dale A. Ross
C-   Modified  18 Spetember 1990   Dale Ross
C-   Modified  30-APR-1991 Rich Astur " Put TRGR unpack params in include
C-                                      file: L2J_TRGRUNP.PARAMS "
C-   Modified  4-MAY-1991 R. Astur: Change arguments to return EM,TOT ET
C-   Modified 11-JUN-1991 R.A.: FADC counts to gev conversion set to defaults
C-                              if not set earlier. More user friendly.
C-            15-MAR-1992 R.A.: Get proper crate inside trigger block
C-   Updated  17-Mar-1992  Herbert Greenlee UNIX version (fix byte order).
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L2J_TRGRUNP.PARAMS'    ! TRGR parameters
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER L1IETA,L1IPHI
      REAL EM_ET,TOT_ET
      INTEGER IZWORD
      LOGICAL FIRST
      INTEGER IEN,IEN1,LTRGR,IZBYTE,BYTENUM
      INTEGER GZTRGR, GZFIND_CRATE
C...this is a slick way of getting the energy byte out of a word
      BYTE INERGY(4)
      EQUIVALENCE (IEN,INERGY)
      INTEGER IAND
      REAL CTS_TO_GEV_EM,CTS_TO_GEV_HD,CTS_EM_OFF,CTS_HD_OFF
      SAVE CTS_TO_GEV_EM,CTS_TO_GEV_HD,CTS_EM_OFF,CTS_HD_OFF
      REAL CONV_EM,CONV_HD,OFF_EM,OFF_HD
C...Initialize FADC count conversion numbers to something nonsensical
      DATA CTS_TO_GEV_EM / 0. /
      DATA CTS_TO_GEV_HD / 0. /
      DATA CTS_EM_OFF    / -100. /
      DATA CTS_HD_OFF    / -100. /
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C---First see if our conversion variables have been set by somone to
C---a nonzero value. If not, set them to the default.
      IF (FIRST) THEN
        IF (CTS_TO_GEV_EM .LE. 0.) THEN
          CTS_TO_GEV_EM = CTS_TO_GEV_EM_DEF
        END IF
        IF (CTS_TO_GEV_HD .LE. 0.) THEN
          CTS_TO_GEV_HD = CTS_TO_GEV_HD_DEF
        END IF
        IF (CTS_EM_OFF .LT. 0.) THEN
          CTS_EM_OFF = CTS_EM_OFF_DEF
        END IF
        IF (CTS_HD_OFF .LT. 0.) THEN
          CTS_HD_OFF = CTS_HD_OFF_DEF
        END IF
        FIRST = .FALSE.
      END IF
C     ---We want to find the byte number index in the L-1 trigger ---
C        data-block. According to the L-1 data-block bible, D0
C        note #967 the 16 bit word index into the block is given
C        as 16e+p+518 = (NPHIL1/2)e+p+(LEN_EM1-NPHIL1/2-1), where
C        (right now) NPHL1=32 and LEN_EM1=535. Doubling this index
C        vaule, plus 1, gives the byte index number (BYTENUM). For
C        p this index too much by NPHIL1-1. If IETA is negative
C        its ADC vaule is lower in the data-block, in bytes,
C        NETAL1*NPHIL1.
*
C:  Get crate 11 (Level 1 calorimeter crate) from TRGR
      LTRGR = GZFIND_CRATE('TRGR', GZTRGR(), 11) - 1
      IF ( LTRGR .LE. 0 ) THEN
        EM_ET = 0.0
        TOT_ET = 0.0
        CALL ERRMSG('No TRGR bank','CL1PHET',
     &    'Link to TRGR not found-zero Et returned','W')
      END IF
      BYTENUM = NPHIL1*IABS(L1IETA)+2*L1IPHI+ 2*(LEN_EM1-NPHIL1/2-1)-1
      IF (L1IPHI.GT.16) BYTENUM = BYTENUM + 1 - NPHIL1
      IF (L1IETA.LT.0)  BYTENUM = BYTENUM + NETAL1*NPHIL1
      IZWORD = (BYTENUM-1)/4 + 1 + (IQ(LTRGR+1) + 2)
      IZBYTE = IAND(BYTENUM-1,3) + 1 ! I use the IAND function in place
C                                    ! of using a modulus function.
C
C- Reverse the byte order on Big Endian machines -- HBG
      IF(BYTE1.NE.1)IZBYTE = 5-IZBYTE
*
C     !!!!!!>Now find the ADC value in the zebra bank.<!!!!!!
*
*
C     ---The location of the word that contains the byte we want is---
C        given by LTRGR+IZWORD. To get the byte wanted the byte is
C        shifted into the position of the lowest order byte. The
C        IAND operation zeros all bits except the lowest eight, the
C        low order byte. The resulting vaule is the ADC vaulue
C        sought. The Hadronic ADC values are NETAL1*NPHIL1 4-byte
C        _words_ away from the EM ADC vaules.
*
      IEN = IQ( LTRGR + IZWORD ) ! Get the integer packed ET for this link.
      IEN1 = INERGY(IZBYTE)      ! Extract the desired byte.
*
C        But if the 8th bit is set, the vax maps this to - numbers since
C        it defines bytes as from -128 to 127 with all bits set = -1
C        so if ADC counts >127 we will get a negative number: fix this
      IF (IEN1 .LT. 0) IEN1 = IEN1 + 128 + 128
C
C---The trigger has no EM calorimetry at eta=20. And currently, they do
C---not fill the words with the pedestal 8 counts. So we must handle here.
C
      IF (ABS(L1IETA) .EQ. 20) THEN
        EM_ET = 0.0
      ELSE
        EM_ET = (FLOAT(IEN1)- CTS_EM_OFF)/CTS_TO_GEV_EM
      END IF
*
C     -We now have the EM contribution of the trigger tower.
C      we want to add the hadronic as well to get the total
      IZWORD = IZWORD + NETAL1*NPHIL1/2 !inc. by the # of zebra WORDS between
C                                         ! em ttower and corr. hadronic tower
      IEN = IQ( LTRGR + IZWORD )
      IEN1= INERGY(IZBYTE)
      IF (IEN1 .LT. 0) IEN1 = IEN1 + 256
      TOT_ET = EM_ET + (FLOAT(IEN1) - CTS_HD_OFF)/CTS_TO_GEV_HD
*
  999 RETURN

      ENTRY CL1PAR_SET(CONV_EM,CONV_HD,OFF_EM,OFF_HD)

      IF (CONV_EM .GT. 0.) THEN
        CTS_TO_GEV_EM = CONV_EM
      ELSE
        CTS_TO_GEV_EM = CTS_TO_GEV_EM_DEF
      END IF
      IF (CONV_HD .GT. 0.) THEN
        CTS_TO_GEV_HD = CONV_HD
      ELSE
        CTS_TO_GEV_HD = CTS_TO_GEV_HD_DEF
      END IF
      IF (OFF_EM .GT. 0.) THEN
        CTS_EM_OFF = OFF_EM
      ELSE
        CTS_EM_OFF = CTS_EM_OFF_DEF
      END IF
      IF (OFF_HD .GT. 0.) THEN
        CTS_HD_OFF = OFF_HD
      ELSE
        CTS_HD_OFF = CTS_HD_OFF_DEF
      END IF
      RETURN

      END
