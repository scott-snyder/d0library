      SUBROUTINE VDTMFL(LAYER,CATEG,WIRE_POS,POINTERS,MAP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill VDTM bank for LAYER, CATEG
C-
C-   Inputs  : LAYER: VTX Layer
C-             CATEG: Drift field category number
C-             WIRE_POS: Array of wire positions (local)
C-             POINTERS: pointers to dtm maps for each wire
C-             MAP: the dtm maps
C-   Outputs : Puts data into VDTM bank
C-
C-   Created  27-JUN-1992   Peter Grudberg
C-   Updated  25-DEC-1992   Ed Oltman  Fill bank at end of linear structure 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVDTM.LINK'
C
      INTEGER LAYER, CATEG, MAXMAP, MXITEM, MXWIRE
      PARAMETER ( MAXMAP = 3000 ) 
      PARAMETER ( MXWIRE = 8 )
      PARAMETER ( MXITEM = 5 )
      REAL WIRE_POS(2*MXWIRE), MAP(MAXMAP)
      INTEGER POINTERS(4*MXITEM*MXWIRE)
C
      REAL SCALE
      INTEGER NWIRE, ITEMS, SIZE, POINT, NUM, I
      INTEGER LVTMW, GZVTMW, LVDTM
      DATA SCALE / 1.0 /
C----------------------------------------------------------------------
      LVTMW = GZVTMW(LAYER)
      IF (LVTMW .EQ. 0) THEN
        CALL ERRMSG('No VTMW bank', 'VDTMFL',
     &    'Supporting bank not present', 'F')
        GO TO 999
      ENDIF
      LVDTM = LC(LVTMW - (IZVDTM+CATEG) )
      IF (LVDTM .EQ. 0) THEN
        CALL ERRMSG('VDTM not booked', 'VDTMFL',
     &    'Attempt to fill unbooked bank', 'F')
        GO TO 999
      ENDIF
C..HAVE A LINEAR STRUCTURE NOW -- GO TO THE END
      DO WHILE(LC(LVDTM) .GT. 0)
        LVDTM = LC(LVDTM)
      ENDDO

      ITEMS = IC(LVDTM+3)
      NWIRE = IC(LVDTM+4)
      SIZE = IC(LVDTM+5)
C
C ****  Now fill the bank
C
      C(LVDTM+6) = SCALE                ! v_drift-(E/P) scale factor
      POINT = LVDTM + 6
      NUM = 2 * NWIRE
      DO I = 1, NUM
        C(POINT+I) = WIRE_POS(I)
      ENDDO
      POINT = POINT + NUM
      NUM = 4 * ITEMS * NWIRE
      DO I = 1, NUM
        IC(POINT+I) = POINTERS(I)
      ENDDO
      POINT = POINT + NUM
      NUM = 2 * SIZE
      DO I = 1, NUM
        C(POINT+I) = MAP(I)
      ENDDO
  999 RETURN
      END
