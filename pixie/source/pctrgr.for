      SUBROUTINE PCTRGR(L1IPHI,L1IETA,ITYPE,ET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get FADC ET for this Trigger Tower from
C-                         the TRGR block for PCTRxx routines.
C-
C-   Inputs  : L1IPHI  - PHI indices (1:32).
C-             L1IETA  - ETA indices (-20:20).
C-             ITYPE   - ITYPE=1 for EM, 0 for Total.
C-   Outputs : ET      - Transverse Energy
C-   Controls:
C-
C-   Modified  6-APR-1992  Nobuaki Oshima( add "CALL GZFIND_CRATE to find
C-                          Calorimeter crate 11. )
C-   Created   5-APR-1991  Nobuaki Oshima( Ref. CL1PHET.FOR )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
C.N.O.        INCLUDE 'D0$INC:L2JETS_TRGR.INC'   ! pointers into TRGR
      INTEGER LINK,ITYPE,L1IETA,L1IPHI
      INTEGER IZWORD, TRGROK
      INTEGER IBYTE,IEN,IEN1,IEN2,IE,IP,LTRGR,IZLINK,IZBYTE,BYTENUM
      INTEGER LEN_EM1,GZFIND_CRATE
C&IF VAXVMS
      BYTE INERGY(4)
      EQUIVALENCE (IEN,INERGY)
C&ENDIF
      REAL ET
      REAL CTS_TO_GEV,CTS_HD_OFF,CTS_EM_OFF
C-
C--- Set CTS_HD_OFF and CTS_EM_OFF by request of M. Abolins
C--- ( 2 GeV = 8 ADC counts.       Nobu. Oshima 31-JAN-92 )
C-
      DATA CTS_TO_GEV,CTS_HD_OFF,CTS_EM_OFF / 4., 2., 2./
      DATA LEN_EM1 /535/
      DATA TRGROK /1/
C----------------------------------------------------------------------
C-
C--- Check TRGR Bank is available or not...
C-
      LTRGR = LQ(LHEAD - IZTRGR) ! The location of the TRGR bank.
C 
      IF(LTRGR.GT.0) LTRGR = GZFIND_CRATE('TRGR',LTRGR,11) - 1 ! FIND CRATE 11 
C
      IF(LTRGR .LE. 0) THEN
        IF (TRGROK .EQ. 1)   CALL PUMESS(
     &    ' - PCTRGR, TRGR BANK DOES NOT EXIST IN THIS EVENT!!!')
        TRGROK = 0
        GO TO 999
      ELSE
        TRGROK = 1
      ENDIF
C-
      BYTENUM = NPHIL1*IABS(L1IETA)+2*L1IPHI+ 2*(LEN_EM1-NPHIL1/2-1)-1
      IF (L1IPHI.GT.16) BYTENUM = BYTENUM + 1 - NPHIL1
      IF (L1IETA.LT.0)  BYTENUM = BYTENUM + NETAL1*NPHIL1
      IZWORD = (BYTENUM-1)/4 + 1 + 7
CNEW..IZWORD = (BYTENUM-1)/4 + 1 + (IQ(LTRGR+1) + 2)
      IZBYTE = IAND(BYTENUM-1,3) + 1 ! I use the IAND function in place
                                     ! of using a modulus function.
C-
C--- Now find the ADC value in the zebra bank
C-
      IEN = IQ( LTRGR + IZWORD ) ! Get the integer packed ET for this link.
C&IF VAXVMS
      IEN1 = INERGY(IZBYTE)      ! Extract the desired byte.
C-
      IF (IEN1 .LT. 0) IEN1 = IEN1 + 256
C&ELSE
C&      CALL ABORT('IEN1 extraction not written for this routine')
C&ENDIF
      ET = FLOAT(IEN1)/CTS_TO_GEV - CTS_EM_OFF
C--- We now have the EM contribution of the trigger tower. If ITYPE=0 then
C--- we want to add the hadronic as well.
      IF (ITYPE .EQ. 0) THEN
        IZWORD = IZWORD + NETAL1*NPHIL1/2 !inc. by the # of zebra WORDS between
C-                                        ! em ttower and corr. hadronic tower
        IEN = IQ( LTRGR + IZWORD )
C&IF VAXVMS
        IEN1= INERGY(IZBYTE)
        IF (IEN1 .LT. 0) IEN1 = IEN1 + 256
C&ELSE
C&      CALL ABORT('IEN1 extraction not written for this routine')
C&ENDIF
        ET = ET + (FLOAT(IEN1)/CTS_TO_GEV - CTS_HD_OFF)
      END IF
C-
  999 RETURN
      END
