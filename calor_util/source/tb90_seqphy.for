      SUBROUTINE TB90_SEQPHY(SEQV,NUMG,ETAV,PHIV,LAYERV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the PHYSICS address(es) of channel(s)
C-                         corresponding to a sequential ADC address.
C-                         
C-   Inputs  : Sequential ADC address - SEQV
C-   Outputs : NUMG - number of corresponding PHYSICS address(es)
C-             ETAV(), PHIV(), LAYERV().
C-             Arrays of values for PHYSICS address(es)
C-   Controls:none
C-
C-   Created  11-DEC-1989   Andrew P. White
C-   Updated  17-APR-1990   Chip Stewart - Eliminated search through TB_SORT
C-                          arrays by using TB90_PHYS_ADDR array.
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:TB90_PHYS_ADDR.INC'
      INTEGER PAKADR,I,POINT
C&IF VAXVMS,ULTRIX,SIUNIX,ALFOSF
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
C&ENDIF
      INTEGER SEQV,NUMG,ETAV(20),PHIV(20),LAYERV(20)
C----------------------------------------------------------------------
C
      NUMG=0
C
C- Extract the ganged channels (if any).
C
      NUMG = 1
      POINT = SEQV 
      PAKADR = TB90_PHYS_ADDR(2,SEQV)
   50 POINT = TB90_PHYS_ADDR(1,POINT)
      IF(POINT.GT.10000 ) THEN
C
C ****  TB90_PHYS_ADDR table corrupted
C
        CALL ERRMSG('TB90','TB90_SEQPHY',' BAD POINT ','W')
      ELSE IF (POINT.EQ.0) THEN
C
C ****  Some ADC channels are not used.
C
        NUMG = 0
        ETAV(1) = 0
        PHIV(1) = 0
        LAYERV(1) = 0
      ELSE
C
C ****  Good addresses
C
C&IF VAXVMS,ULTRIX,SIUNIX,ALFOSF
        ETAV(NUMG)  = BYTES(3)
        PHIV(NUMG)  = BYTES(2)
        LAYERV(NUMG)= BYTES(1)
C&ENDIF
C&IF IBMAIX
C&        ETAV(NUMG)  = ISHFT(ISHFT(PAKADR,8),-24)
C&        PHIV(NUMG)  = ISHFT(ISHFT(PAKADR,16),-24)
C&        LAYERV(NUMG)= ISHFT(ISHFT(PAKADR,24),-24)
C&ENDIF
        IF (POINT.GT.SEQV) THEN
C
C ****  Loop back to pick up next ganging
C
          PAKADR = TB90_PHYS_ADDR(2,POINT)
          NUMG = NUMG + 1
          GOTO 50
        END IF
      END IF
C
  999 RETURN
      END
