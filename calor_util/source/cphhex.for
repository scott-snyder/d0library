      SUBROUTINE CPHHEX(IETAC,IPHIC,CRATE,IADDR,ITSOK) 
C----------------------------------------------------------------------
C-
C-   CPHHEX = Convert PHysics indicies to HEX format
C-
C-   Purpose and Methods : It takes offline (IETAC,IPHIC) indexes
C-                         and converts them to the hex address
C-                         format, modulo DEPTH (Ie bits 2-5 are 0).
C-                         However, most of the work done right in
C-                         this routine does nothing more than select a
C-                         layer that will insure a PH to HEX conversion 
C-                         for an *ARBITRAY* IETAC-IPHIC pair.
C-
C-   Inputs  : IETAC and IPHIC are the offline indexes.
C-
C-   Outputs : CRATE and IADDR give the crate number and hex address.
C-             ITSOK flags an incomplete conversion error.
C-
C-   Controls: None.
C-
C-   Created   6-JAN-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
*
C     Passed Variables:
*
        INTEGER  IETAC,IPHIC  !  offline Eta, Phi indexes
        INTEGER  CRATE        !  The crate (cabale) number.
        INTEGER  IADDR        !  The packed hex address.
        LOGICAL  ITSOK        !  Conversion "IS OK" flag.
*
C     Local Variables:
*
        INTEGER  ADC          !  ADC card number in crate.
        INTEGER  BLS          !  BLS card number.
        INTEGER  DEPTH        !  Depth in the readout tower.
        INTEGER  ICOND        !
        INTEGER  LAYERC       !  Offline radial index.
        INTEGER  NEGLIM       !  NEGative LIMit bit (bit 0) of IADDR.
        INTEGER  ROTOW        !  Readout tower in the BLS.
        INTEGER  SCALE        !  Bit 1 (2nd bit) of IADDR (ignored here).
*
C     Function "Variable(s)"
*
        LOGICAL  CEXIST       !  Serban's routine to check the 
                              !  existance of indices (ietac,iphic,layer)
*
C     And now the local starring attraction:
*
C     ...first convert iphic and ietac to valuse akin to those used in 
C        D0geant (this is beacause Andy White's progam munches
C        on these value types).
*
*
      ITSOK = .TRUE.
      IF (ABS(IETAC) .EQ. 37) THEN    ! There is a fine hadronic tower
           LAYERC = 15               ! everywhere except at IETAC=37.
         ELSE                       ! So, to insure no conversion error
           LAYERC = 11             ! we teporarily use layer 15. 
      ENDIF                       ! This is in accordance with Calorimeter
C                                ! Addressing V1.12 (D0 note 774).
*
      IF (CEXIST(IETAC,IPHIC,LAYERC)) THEN
          CALL CPHAD(IETAC,IPHIC,LAYERC,CRATE,ADC,BLS,ROTOW,DEPTH,ICOND)
          DEPTH = 0 ! Don't care what these bits are.
          CALL CADPAK(ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM,IADDR)
C                ! Am ignoring SCALE and NEGLIM; thus they are set to 0.
          IF (ICOND .NE. 0) ITSOK=.FALSE.
        ELSE 
          ITSOK = .FALSE.
      ENDIF
*
*
  999 RETURN
      END
