      SUBROUTINE GTESUM (STYP,IDWANT,IWANT,ET,ETA,ETAD,PHI,FLAG,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return data for ESUM bank for one object
C-    Inputs:
C-             STYP     [C*4]   Summary type desired:
C-                                'FILT','TRGR','RECO','ISAE'
C-             IDWANT   [I]     ID of object to look for:
C-                              See ESUM.PARAMS for known object types:
C-                                INCLUDE 'D0$PARAMS:ESUM.PARAMS'
C-                                        possible values:
C-                              ID_VERTEX, ID_JET, ID_MUON, ID_ELECTRON
C-                              ID_PHOTON, ID_ETMISS, ID_ETSUM, ID_TAU
C-                              See ESUM_INIT for ascii labels for types
C-             IWANT    [I]     serial number object to look for:
C-                              range is 1:NFOUND(IDWANT), NFOUND from
C-                              GTESUM_COUNTS
C-
C-    Outputs:
C-             IER      [I]     Error code; 0 --- OK
C-                              -1 --- No ESUM bank of this type.
C-                              -2 --- Requested object not found
C-             ET       [R]     Et or Pt of object
C-             ETA      [R]     Physics Eta of object       (or x of vertex)
C-             ETAD     [R]     Detector Eta of object     (or y of vertex)
C-             PHI      [R]     Phi of object       (or z of vertex)
C-             FLAG     [I]     32 bit flag word defined by whatever 
C-                              created the object
C-
C-   Controls: none
C-   Created  10-DEC-1991   Richard V. Astur
C-   Updated  11-DEC-1991   James T. Linnemann   allow return by type; swap args
C-                                               add FLAGS
C-   Updated   6-JAN-1992   James T. Linnemann   rename to ESUM; add STYP
C-   Updated  19-JUN-1992   RVA: Detector eta in place of theta
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      CHARACTER*(*) STYP
      INTEGER IDWANT, IWANT, IFOUND
      INTEGER ID, FLAG 
      REAL ETA, ET, ETAD, PHI
      INTEGER IER, LESUM, GZESUM, POINT, I
      INTEGER NFIX,NR,NUM_OBJ
C----------------------------------------------------------------------
C: Set error flag
      IER = 0                   ! OK
C: Get link
      LESUM = GZESUM(STYP)
      IF ( LESUM .LE. 0 ) THEN
        IER = -1  ! couldn't find bank
        GO TO 999
      END IF
      NFIX = IQ( LESUM + 2)
      NR   = IQ( LESUM + 3)
      NUM_OBJ = IQ( LESUM + 4)
      IFOUND = 0
C: Look for objects desired
      DO I = 1, NUM_OBJ ! Cycle over known objects
        POINT = (I-1)*NR + NFIX + LESUM
        IF ( IDWANT.EQ.IQ(POINT+JESUM_ID) )THEN
          IFOUND = IFOUND + 1
          IF (IFOUND.EQ.IWANT) THEN
            ID    =IQ( POINT + JESUM_ID )
            ET    = Q( POINT + JESUM_PT )
            ETA   = Q( POINT + JESUM_ETA )
            ETAD  = Q( POINT + JESUM_ETA_DET )
            PHI   = Q( POINT + JESUM_PHI )
            FLAG  =IQ( POINT + JESUM_FLAG)
            GO TO 999
          ENDIF
        ENDIF
      END DO
      IER = -2  ! requested object never found
  999 RETURN
      END
