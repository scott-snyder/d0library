      SUBROUTINE CALC_HOT_ET(HOT_E,HOT_ET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      calculate E,ET of hot cells
C-
C-   Outputs:
C-     HOT_E  = sum of E in hot cells
C-     HOT_ET = vector sum of ET in hot cells
C-     
C-   Created  22-MAR-1994   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    HOT_E,HOT_ET
      REAL    HOT_EX,HOT_EY
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER PACKED_ADDRESS
      BYTE BYTES(4)
      EQUIVALENCE (PACKED_ADDRESS,BYTES)
      INTEGER LVERT,GZVERT,I,NFOUND
      INTEGER LCAID,GZCAID,LPOINT,IETA,IPHI,LAYER,IOK
      REAL    XC,YC,ZC,XV,YV,ZV,DIST,E,EX,EY
C----------------------------------------------------------------------
      HOT_ET=0
      HOT_E=0
      HOT_EX=0
      HOT_EY=0
      LCAID=GZCAID()
      IF(LCAID.NE.0) THEN
        NFOUND=IQ(LCAID+4)
        LPOINT=LCAID+7
        IF(NFOUND.GT.0) THEN
          LVERT=GZVERT(1)
          XV=Q(LVERT+3)
          YV=Q(LVERT+4)
          ZV=Q(LVERT+5)
          DO I=1,NFOUND
            PACKED_ADDRESS=IQ(LPOINT+1)
            E=Q(LPOINT+2)
            IETA = BYTES(BYTE4)
            IPHI = BYTES(BYTE3)
            LAYER= BYTES(BYTE2)
            IF(IETA*IPHI*LAYER*E.NE.0) THEN  ! protect against garbage
              CALL CELXYZ(IETA,IPHI,LAYER,XC,YC,ZC,IOK)
              DIST=SQRT((XC-XV)**2+(YC-YV)**2+(ZC-ZV)**2)
              EX=E*XC/DIST
              EY=E*YC/DIST
              HOT_EX=HOT_EX+EX
              HOT_EY=HOT_EY+EY
              HOT_E=HOT_E+E
            ENDIF
            LPOINT=LPOINT+2
          ENDDO
          HOT_ET=SQRT(HOT_EX**2+HOT_EY**2)
        ENDIF
      ENDIF
  999 RETURN
      END
