      SUBROUTINE MUHPAD(NMOD,NCEL,NLAT,IADC,IPAD,PAD)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Convert pad values to pulse heights
C-                         and match to cells
C-
C-    Input  :  NMOD   - Module ID
C-              NCEL   - Cell number
C-              NLAT   - Latch bits
C-              IADC(8)- Raw ADC counts
C-
C-    Output :  IPAD(2)- Flag for hit cell
C-                       1 = no pad latch
C-                       2 = no pad pulse
C-                       4 = pad pulse overflow
C-              PAD(4) - Corrected pad values
C-
C-    Created :  2-SEP-93  M. Fortner
C-    Modified:  2/94 MF Remove error messages
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IADC(8),NMOD,NCEL,NLAT,IPAD(2)
      INTEGER I,NWIR,NPLN
      REAL GAIN(4),PED(4),PSUM,PAD(4)
      REAL PADPED,PADOVF,PADMIN,PADMAX
      DATA PADPED,PADOVF,PADMIN,PADMAX/50,3200,100,10000/
C----------------------------------------------------------------------
C               Load raw data into real array
C----------------------------------------------------------------------
      PAD(1)=FLOAT(IADC(3))               ! PAD EA
      PAD(2)=FLOAT(IADC(4))               ! PAD EB
      PAD(3)=FLOAT(IADC(7))               ! PAD OA
      PAD(4)=FLOAT(IADC(8))               ! PAD OB
C----------------------------------------------------------------------
C               Get calibration constants and adjust data
C----------------------------------------------------------------------
      NWIR = NCEL/4
      NPLN = NCEL - NWIR*4
      CALL MUGGAN(NMOD,NPLN,NWIR,GAIN(1),GAIN(2),GAIN(3),GAIN(4),
     &            PED(1),PED(2),PED(3),PED(4))
      DO I = 1,4
          IF (PAD(I).GT.PADOVF) THEN
              PAD(I) = 50000.
          ELSE
              PAD(I) = (PAD(I)-PED(I))/GAIN(I)
          ENDIF
          IF (PAD(I).LT.PADPED) PAD(I) = 0
      ENDDO
C-------------------------------------------------------------------
C               Test hit topology for single or double hits
C-------------------------------------------------------------------
      DO I=1,2
          IPAD(I) = 0
          IF (IBITS(NLAT,I-1,1).EQ.0) IPAD(I)=1
          PSUM = PAD(I*2-1) + PAD(I*2)
          IF (PAD(I*2-1).EQ.0.OR.PAD(I*2).EQ.0) IPAD(I)=IPAD(I)+2
          IF (PSUM.GT.PADMAX) IPAD(I)=IPAD(I)+4
      ENDDO
C
      RETURN
      END
