      SUBROUTINE MUHWIR(NMOD,NCEL,IPAD,IADC,IWIR,WIRE)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Convert hit values to times
C-                         and match to cells
C-
C-    Input  :  NMOD   - Module ID
C-              NCEL   - Cell number
C-              IPAD(2)- Flag bits for pad hits
C-              IADC(8)- Raw ADC counts
C-
C-    Output :  IWIR   - Flag for wire hit (negative for odd cell match)
C-                       = 0  Good single drift time
C-                       = 8  Two good drift times
C-                       = 16  Wire refire: use delta 1
C-                       = 32  Wire refire: use delta 2
C-                       = 64  Wire refire: use 1/2 delta 2
C-                       = 128  Delta 1 unphysical
C-                       = 256  Delta 2 unphysical
C-                       = 512  Drift 1 unphysical
C-                       = 1024  Drift 2 unphysical
C-              WIRE(4)- Corrected wire values
C-
C-    Created :  2-SEP-93  M. Fortner
C-    Modified:  2/94 MF remove error messages, use average DT on refire
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMOD,NCEL,IPAD(2),IADC(8)
      INTEGER IWIR
      INTEGER I,IREF,ICEL(2),NPLN,NWIR
      INTEGER MUDVER
      EXTERNAL MUDVER
      LOGICAL FIRST
      REAL TZER(4),TSLP(4),WIRE(4)
      REAL WIRPED,WIROVF,WIRDIS
      REAL TMIN,TMAX,DLIM,DELMIN
      DATA FIRST/.TRUE./
      DATA WIRPED,WIROVF,WIRDIS/400.,4090.,150./
      DATA TMIN,TMAX,DLIM,DELMIN/-100.,1200.,60.,5./
C----------------------------------------------------------------------
C               Initialize
C----------------------------------------------------------------------
      IF (FIRST) THEN
          IF (MUDVER(0).EQ.20) TMAX=1400.
          FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
C               Load raw data into real array
C----------------------------------------------------------------------
      WIRE(1)=FLOAT(IADC(1))               ! Drift Time 1
      WIRE(2)=FLOAT(IADC(2))               ! Drift Time 2
      WIRE(3)=FLOAT(IADC(6))               ! Delta Time 1
      WIRE(4)=FLOAT(IADC(5))               ! Delta Time 2
C----------------------------------------------------------------------
C               Get calibration constants and adjust data
C----------------------------------------------------------------------
      NWIR = NCEL/4
      NPLN = NCEL - NWIR*4
      CALL MUGTCN(NMOD,NPLN,NWIR,TZER(1),TSLP(1),TZER(2),
     &            TSLP(2),TZER(3),TSLP(3),TZER(4),TSLP(4))
C
      TSLP(1) = -TSLP(1)
      TSLP(2) = -TSLP(2)
      DO I = 1,4
          IF (WIRE(I).GT.WIROVF) THEN
              WIRE(I) = 50000.
          ELSE IF (WIRE(I).LT.WIRPED) THEN
              WIRE(I) = -50000.
          ELSE
              WIRE(I) = (WIRE(I)-TZER(I))*TSLP(I)
          ENDIF
      ENDDO
C-------------------------------------------------------------------
C               Test for physical drift and delta times
C-------------------------------------------------------------------
      DO I=1,2
          ICEL(I) = 0
          IF (WIRE(I).GT.TMIN.AND.WIRE(I).LT.TMAX) THEN
              IF (ABS(WIRE(I+2)).GT.DLIM) THEN
                  ICEL(I) = -1
              ELSE
                  ICEL(I) = 1
                  IF (WIRE(I+2).LT.0) ICEL(I) = 2
              ENDIF
          ENDIF
      ENDDO
C-------------------------------------------------------------------
C               Test for bad drift times
C-------------------------------------------------------------------
      IWIR = 8
      IF (ICEL(2).EQ.0) IWIR=0
      IF (ICEL(1).EQ.0) THEN
          IF (ICEL(2).NE.0) THEN
              IWIR = 0
              ICEL(1) = ICEL(2)
              ICEL(2) = 0
              WIRE(1) = WIRE(2)
              WIRE(3) = WIRE(4)
          ELSE
              IWIR = IWIR + 512 + 1024
          ENDIF
      ELSE
C-------------------------------------------------------------------
C               Test for wire refiring
C-------------------------------------------------------------------
          IF (ICEL(2).NE.0.AND.WIRE(2)-WIRE(1).LT.WIRDIS) THEN
              IWIR = 16
              WIRE(2) = -50000.
              IF (ICEL(2).GT.0) THEN
                  ICEL(1) = ICEL(2)
                  IF (ABS(WIRE(3)-WIRE(4)).LT.DELMIN) THEN
                      WIRE(3) = (WIRE(3)+WIRE(4))/2
                  ELSE
                      WIRE(3) = WIRE(4)
                  ENDIF
                  IWIR = 32
                  IF (ICEL(1).EQ.1.AND.IPAD(1).EQ.3) THEN
                      WIRE(3) = (FLOAT(IADC(5))-WIRPED)/2+WIRPED
                      WIRE(3) = (WIRE(3)-TZER(4))*TSLP(4)
                      ICEL(1) = 2
                      IWIR = 64
                  ENDIF
              ENDIF
              ICEL(2) = 0
              WIRE(4) = -50000.
          ENDIF
      ENDIF
C-------------------------------------------------------------------
C               Flag bad delta times
C-------------------------------------------------------------------
      DO I=1,2
          IF (ICEL(I).LT.0) THEN
              IWIR = IWIR + 128*I
          ENDIF
      ENDDO
C
      RETURN
      END
