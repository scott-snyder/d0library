      SUBROUTINE GET_TRD_COR_ELE (PLANE,WIRE,CORRECTION,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculates electronic gain
C-
C-   Inputs  : PLANE      integer   1,2,3 (anodes) or 4,5,6 (cathodes)
C-             WIRE       integer   in [1,256]
C-   Outputs : CORRECTION real      electronic gain
C-             ERROR      integer   0 = OK
C-                                  1 = correction not required in TRD.RCP
C-                                  2 = plane>3 (cathodes)
C-   Controls: TRD.RCP
C-
C-   Created  15-JAN-1993   Alain PLUQUET
C-   Updated   6-MAY-1993   Alain PLUQUET   correction-->1./correction
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'

      REAL GAIN_TRD(NW1+NW2+NW3)
C      REAL GAIN_TRD(3,256)
      REAL GAIN,CORRECTION
      INTEGER PLANE,SECTOR,WIRE,ERROR,IER,P,W,TCHNB
      LOGICAL FIRST,DO_CORRECTION
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('TRD_RCP')
        CALL EZGET('COR_ELE',DO_CORRECTION,IER)
        CALL EZRSET
C         print*,' in GET_TRD_COR_ELE, do_correction',DO_CORRECTION
        IF(DO_CORRECTION) THEN
          IF(NWIRE_PER_LAYER(1).LE.0)CALL TRD_NWIRE_PER_LAYER
          DO P=1,3
C            print*,'layer',p,'  NWIRE_PER_LAYER',NWIRE_PER_LAYER(P)
            DO W=1, NWIRE_PER_LAYER(P)
C              if(p.eq.1)print*,' appel a TRGGN'
              CALL TRGGN(' ',W,P,GAIN,IER)
C              if(p.eq.1)print*,' appel a TRGGN,ier',ier,' gain',
C     &          gain
              IF(IER.EQ.0.) THEN
                IF (GAIN.GT.0.) THEN
                  GAIN_TRD(TCHNB(W,P))=1./GAIN
                ELSE
                  GAIN_TRD(TCHNB(W,P))=0.
                ENDIF
              ELSE
                GAIN_TRD(TCHNB(W,P))=1.
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDIF
      IF(DO_CORRECTION) THEN
        IF (PLANE.LE.3) THEN
          CORRECTION=GAIN_TRD(TCHNB(WIRE,PLANE))
          ERROR=0
        ELSE
          CORRECTION=1.
          ERROR=2
        END IF
      ELSE
        CORRECTION=1.
        ERROR=1
      ENDIF
      END
