      SUBROUTINE FILL_GSHLB(GSHLB,NGSHLB,NSHDATA,PAR_MASS,ITRA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FILLS THE GSHLB ARRAY WITH GCAH INFO
C-
C-   Inputs  : LGCAH (FROM ZLINKC)
C-   Outputs : GSHLB = ARRAY FOR SHOWERLIBRARY USAGE
C-             NGSHLB = STORAGE IN GSHLB ARRAY LEFT
C-             NSHDATA = Number of data points
C-             PAR_MASS = mass of the partical in GCAH
C-             ITRA = Track number. ITRA=-1 means no track.
C-   Controls: 
C-
C-   Created  10-MAY-1990   Rajendran Raja
C-   Updated  14-APR-1992   W.G.D.Dharmaratna, CHANGES FOR VERSION 2.0 
C-                          gshlb(10) set to 9999 or -9999 for bad tracks
C-                          as flagged in gcah p(4,2). 
C-   Updated   2-DEC-1992   W. Dharmaratna   GOING BACK TO ENERGY from KE.
C- 
C-   Updated  11-DEC-1992   W. Dharmaratna, corrected the GCAH bad flagg
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:SHLCON.INC'
      INCLUDE 'D0$INC:SHLDAT.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
      INCLUDE 'D0$LINKS:IZGHIT.LINK'
      INCLUDE 'D0$LINKS:IZGCAH.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      REAL    GSHLB(*)
      INTEGER NGSHLB,NSHDATA,ITRA
C
      REAL VERT(3,3),MVERT
C
      INTEGER IE,IPH,IL,ITAG
      INTEGER I,J,JQ,II,IJ,IX,NPLIVE
      REAL XT,PT,EM,MASS_IN,PAR_MASS
      INTEGER VERSION
      INTEGER IREJ_MOM
      INTEGER NDATA_MAX
      REAL    EFR_KEEP
C
      INTEGER IER
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      INTEGER SSUNIT
      INTEGER MAX_HIT_CAL,MAX_HIT_MG1,MAX_HIT_ICD,MAX_HIT_MG2
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')
        CALL EZGET('ENERGY_FRACTION_KEEP',EFR_KEEP,IER)
        CALL EZGET('MAX_HIT_CAL',MAX_HIT_CAL,IER)
        CALL EZGET('MAX_HIT_MG1',MAX_HIT_MG1,IER)
        CALL EZGET('MAX_HIT_ICD',MAX_HIT_ICD,IER)
        CALL EZGET('MAX_HIT_MG2',MAX_HIT_MG2,IER)
        CALL EZRSET
C
      ENDIF
C
      MASS_IN = 0.0
      PAR_MASS = 0.0
      CALL GHGCAH(LGCAH,ITRA,VERT,P,IDATA,TOTAL,NPOINT)
C
      IF(ITRA.LT.0)GOTO 999         ! No more tracks: return
C Check the flag for bad tracks
C
      IF (P(4,2) .GT. 9990.0 ) THEN ! Skip the track
        GSHLB(10)= 9999
        GOTO 999
      ELSE IF (P(4,2) .LT. -9990.0) THEN  ! This was flaged incorrectly
        P(4,2) = SQRT(P(1,2)**2+P(2,2)**2+P(3,2)**2) ! recorrect the bug 
      ENDIF                                          ! neglecting mass
C
C ****  Extrapolate back to notional primary vertex z-position
C
      XT=SQRT(VERT(1,2)**2 + VERT(2,2)**2)
      PT=SQRT(P(1,2)**2 + P(2,2)**2)
      IF (PT.EQ.0.)PT=0.000001
C
C ****  Determine key vector values
C
      MVERT=SQRT(VERT(1,2)**2+VERT(2,2)**2+VERT(3,2)**2)
      IF (MVERT.NE.0.)THEN
        PX=VERT(1,2)
        PY=VERT(2,2)
        PZ=VERT(3,2)
        EIN=P(4,2)
        NVTX=VERT(3,2)-XT*P(3,2)/PT
      ELSE                              ! MVERT=0 for old datafiles
        PX=P(1,1)
        PY=P(2,1)
        PZ=P(3,1)
        EIN=P(4,1)
        NVTX=VERT(1,3)
      ENDIF
C
C ****  evaluate how far particle travelled in calorimeter
C ****  (needed for muon punchthrough estimation)
C
      DIST=(VERT(1,3)-VERT(1,2))**2 + (VERT(2,3)-VERT(2,2))**2 +
     &  (VERT(3,3)-VERT(3,2))**2
      DIST=SQRT(DIST)
C
C ****  Zero arrays of hits and cell indices
C
      IF(NPOINT.GT.NHMAX)THEN
        WRITE(SSUNIT(),*)
     &  'SHLEVT: Warning --- track has more than', NHMAX,' hit cells;',
     &  '/        Some will be skipped'
        NPOINT=NHMAX
      ENDIF
C
      CALL UZERO(EALL,1,4)
      CALL UZERO(CELLE,1,NPOINT)
      CALL UZERO(MG1E,1,NPOINT)
      CALL UZERO(ICDE,1,NPOINT)
      CALL UZERO(MG2E,1,NPOINT)
      ETOT=TOTAL(1)
      CALL UZERO(IETA,1,NPOINT)
      CALL UZERO(IPHI,1,NPOINT)
      CALL UZERO(LAYER,1,NPOINT)
C
C ****  Loop over all hits for this track
C
      J=0
      DO 200 I=1,NPOINT
        CALL GDGCAH(LGCAH,I,CELLER,IE,IPH,IL,ITAG)
C
        IF(IL.GE.MXLYCH+1) GOTO 200           ! dead layers
C
        J=J+1
        LAYER(J)=IL
        IETA(J)=IE
        IPHI(J)=IPH
C
        IF(IL.EQ.MNLYMG)THEN
          MG1E(J)=CELLER(1)
          EALL(2)=EALL(2)+CELLER(1)
        ELSEIF(IL.EQ.LYICD)THEN
          ICDE(J)=CELLER(1)
          EALL(3)=EALL(3)+CELLER(1)
        ELSEIF(IL.EQ.MXLYMG)THEN
          MG2E(J)=CELLER(1)
          EALL(4)=EALL(4)+CELLER(1)
        ELSE
          CELLE(J)=CELLER(1)
          EALL(1)=EALL(1)+CELLER(1)
        ENDIF
C
 200  CONTINUE
      NPLIVE=J
C
C ****  All hits in arrays: now process hits
C
C ****  Remove calorimeter hits until 20 remain
C       Remove MG and ICD hits until 4 remain in each
C       (or fewer in each case if they contain 95% of the energy)
C
      CALL UZERO(NHIT,1,4)
      IF(NPLIVE.EQ.0)THEN
        CALL UZERO(EMISS,1,4)
      ELSE
        CALL SORTZV(CELLE,INDE,NPLIVE,1,1,0)
        EM=0.
        IF(CELLE(INDE(1)).EQ.0.)GOTO 221
        DO 220 I=1,MAX_HIT_CAL
          EM=EM+CELLE(INDE(I))
          NHIT(1)=NHIT(1)+1
          IF(EM.GE.EFR_KEEP*EALL(1))GOTO 221
  220   CONTINUE
  221   CONTINUE
        EMISS(1)=EALL(1)-EM
C
        CALL SORTZV(MG1E,INDMG1,NPLIVE,1,1,0)
C
        EM=0.
        IF(MG1E(INDMG1(1)).EQ.0.)GOTO 223
        DO 222 I=1,MAX_HIT_MG1
          EM=EM+MG1E(INDMG1(I))
          NHIT(2)=NHIT(2)+1
          IF(EM.GE.EFR_KEEP*EALL(2))GOTO 223
  222   CONTINUE
  223   CONTINUE
        EMISS(2)=EALL(2)-EM
C
        CALL SORTZV(ICDE,INDICD,NPLIVE,1,1,0)
        EM=0.
        IF(ICDE(INDICD(1)).EQ.0.)GOTO 225
        DO 224 I=1,MAX_HIT_ICD
          EM=EM+ICDE(INDICD(I))
          NHIT(3)=NHIT(3)+1
          IF(EM.GE.EFR_KEEP*EALL(3))GOTO 225
  224   CONTINUE
  225   CONTINUE
        EMISS(3)=EALL(3)-EM
C
        CALL SORTZV(MG2E,INDMG2,NPLIVE,1,1,0)
        EM=0.
        IF(MG2E(INDMG2(1)).EQ.0.)GOTO 227
        DO 226 I=1,MAX_HIT_MG2
          EM=EM+MG2E(INDMG2(I))
          NHIT(4)=NHIT(4)+1
          IF(EM.GE.EFR_KEEP*EALL(4))GOTO 227
  226   CONTINUE
  227   CONTINUE
        EMISS(4)=EALL(4)-EM
      ENDIF
C
C ****  Get the shower library entry for the given KEY value
C
      NHITS=NHIT(1)+NHIT(2)+NHIT(3)+NHIT(4)! number of hits to store
C
        NSHDATA=12+2*NHITS                  ! number of data words
        IF ( NSHDATA.GT.NGSHLB ) THEN
          CALL ERRMSG('SHOWERLIBRARY','SHLEVT_MAKE_LIB',
     &      'DATA IN GSHLB OVERFLOWS DIMENSIONS--REJECT INFO','W')
          GSHLB(4) = P(4,2)
          GO TO 999
        ENDIF
C
C ****  Store information in the library entry
C
        GSHLB(1)=VERT(1,2)                   ! x
        GSHLB(2)=VERT(2,2)                   ! y
        GSHLB(3)=VERT(3,2)                   ! z
        MASS_IN = SQRT(P(1,2)**2+P(2,2)**2+P(3,2)**2) ! momentum
        MASS_IN = SQRT((P(4,2)+MASS_IN)*ABS(P(4,2)-MASS_IN)) ! mass
        GSHLB(4)=P(4,2)                      !  Energy
        PAR_MASS = MASS_IN
        GSHLB(5)=IDATA(1)              ! Geant particle ID
        GSHLB(6)=EMISS(1)               ! Cal energy - sum of hits
        GSHLB(7)=EMISS(2)               ! MG1 energy - sum of hits
        GSHLB(8)=EMISS(3)               ! ICD energy - sum of hits
        GSHLB(9)=EMISS(4)               ! MG2 energy - sum of hits
        GSHLB(10)=TOTAL(4)              ! Total dead material energy
        GSHLB(11)=DIST
        GSHLB(12)=NHITS
C
        DO 600 II=1,NHIT(1)
          IJ=INDE(II)
          GSHLB(13+2*(II-1))=CELLE(IJ)
          IX=
     &      ISIGN(10000*ABS(IETA(IJ))+100*IPHI(IJ)+LAYER(IJ),IETA(IJ))
          GSHLB(14+2*(II-1))=IX
  600   CONTINUE
C
        DO 601 II=1,NHIT(2)
          IJ=INDMG1(II)
          GSHLB(13+2*(NHIT(1)+II-1))=MG1E(IJ)
          IX=
     &      ISIGN(10000*ABS(IETA(IJ))+100*IPHI(IJ)+LAYER(IJ),IETA(IJ))
          GSHLB(14+2*(NHIT(1)+II-1))=IX
  601   CONTINUE

        DO 602 II=1,NHIT(3)
          IJ=INDICD(II)
          GSHLB(13+2*(NHIT(1)+NHIT(2)+II-1))=ICDE(IJ)
          IX=
     &      ISIGN(10000*ABS(IETA(IJ))+100*IPHI(IJ)+LAYER(IJ),IETA(IJ))
          GSHLB(14+2*(NHIT(1)+NHIT(2)+II-1))=IX
  602   CONTINUE

        DO 603 II=1,NHIT(4)
          IJ=INDMG2(II)
          GSHLB(13+2*(NHIT(1)+NHIT(2)+NHIT(3)+II-1))=MG2E(IJ)
          IX=
     &      ISIGN(10000*ABS(IETA(IJ))+100*IPHI(IJ)+LAYER(IJ),IETA(IJ))
          GSHLB(14+2*(NHIT(1)+NHIT(2)+NHIT(3)+II-1))=IX
  603   CONTINUE
C
  999 RETURN
      END
