      SUBROUTINE MUANLZ(IERR,SKIP_LEVEL,TRIG_LEVEL,DET_REGION)
C---------------------------------------------------------------------------
C-    Purpose:    CONTROLLING ROUTINE FOR ANALYZING MUON DRIFT TUBES
C-
C-    Outputs:    IERR=0 OK; =1,2 FATAL; 3 OR MORE WARNING
C-
C-    Inputs:     SKIP_LEVEL = 0 do everything
C-                           = 3 skip after hit finding
C-                           = 5 skip stubs
C-                           = 6 skip after trigger unpacking
C-                           = 7 skip after data unpacking
C-                TRIG_LEVEL = 0 ignore muon trigger info
C-                           = 1 require level 1 bit
C-                           = 2 require level 1.5 low pt
C-                           = 3 require level 1.5 high pt
C-                DET_REGION = 1 (CF)=(Y1)
C-                           = 2 (CF+WN+WS)=(Y2)
C-                           = 3 (CF+WN+WS+ON+OS)=(Y3)
C-                           = 4 (CF+WN+WS+ON+OS+SN+SS)=(Y4)
C-                           =-1,-2,-3 inverse of 1,2,3 (X1,X2,X3)
C-
C-    Created DH 9-8-85
C-    Updated DH 9/86, 4/87;  TK 11/87, 1/88;  DH 7/89, 4/90, 5/90, 9/90
C-    Updated DZ 9/91, 10/91, 1/92, 5/92   Add SAMUS and fixes
C-    Updated DH 10/91, 11/91, 2/92, 5/92, 6/92, 7/92, 11/92, 12/92
C-    Updated HTD 1/93, AT 1/93  CALL FLVSN_MTRH for STP version book keeping
C-    Updated 2/93 A Kozelov  add track cleaning in SAMUS-WAMUS overlap
C-    Updated MF 9/93 change to 1B unpacking
C-    Updated HTD,SCE 11/93 add call to MU_ASTUB
C-    Updated MF 12/93 add TRIG_LEVEL and DET_REGION parameters
C-                     remove EZGET calls
C-    Updated MF 2/94 add trigger unpacking, centroids, new track calls
C-    Updated DW 5/94 add call to MTRHFL_FLAGS
C-    Updated MF 6/94 include flag calls in track routines MUTWAM,...
C---------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IERR,SKIP_LEVEL,TRIG_LEVEL,DET_REGION
      INTEGER IVMUHT,LMUHT,MUDVER,JERR,ITRG,IREG,ITYPE
      INTEGER GZMTRH,LMTRH,NTRACKS,MTRACKS
      INTEGER I,IQUAD,NQUAD,JQUAD(18),NMOD,JMOD(200)
C
C         Create muon hit bank header
C
      IERR=0
      IVMUHT = MUDVER(0)
      IF (IVMUHT.EQ.-1) THEN
          IERR = -4
          GO TO 999
      ENDIF
      IF (IVMUHT.EQ.0) CALL BKMUHT(0,0,LMUHT)
C
C         Create pointer banks, unpack raw data
C
      CALL MUDPAK(JERR)
      IF (JERR.LT.0) THEN
          IERR = JERR
          GO TO 999
      ENDIF
C
C         Calculate centroids for trigger simulator
C
      IF (SKIP_LEVEL.EQ.7) THEN
          ITYPE = 1
          CALL MUCENT(ITYPE,DET_REGION)
          GO TO 999
      ENDIF
C
C         Book/fill muon trigger bank
C
      CALL MOTPAK(JERR)
      IF (JERR.LT.0) THEN
          IERR = JERR
          GO TO 999
      ENDIF
      IF (SKIP_LEVEL.EQ.6) GO TO 999
C
C         Select list of modules based on trigger and fill centroids
C
      ITRG = TRIG_LEVEL
      IREG = DET_REGION
      IF (IREG.EQ.0) IREG = 4           ! full detector
      IF (SKIP_LEVEL.EQ.4) IREG = 2     ! WAMUS only (no overlap)
      CALL MUMSET(ITRG,IREG,NMOD,JMOD,NQUAD,JQUAD)
C
C         Create hit banks, process raw data in WAMUS, SAMUS and SCINT
C
      CALL MUHITS(NMOD,JMOD,JERR)
      IF (JERR.LT.0) THEN
          IERR = JERR
          GO TO 999
      ENDIF
      IF (SKIP_LEVEL.EQ.3) GO TO 999
C
C         Begin tracking, create processed event bank
C
      IF (GZMTRH(0).EQ.0) CALL BKMTRH(0,0,LMTRH)
      CALL MTRHFL
      CALL FLVSN_MTRH

C
C         Find tracks by quadrant
C
      DO I=1,NQUAD
          IQUAD=JQUAD(I)
          IF (IQUAD.GT.0.AND.IQUAD.LE.12) CALL MUTWAM(IQUAD)
          IF (IQUAD.EQ.17.OR.IQUAD.EQ.18) CALL MUTSWW(IQUAD)
          IF (IQUAD.EQ.15.OR.IQUAD.EQ.16) CALL MUTSSW(IQUAD)
          IF (IQUAD.EQ.13.OR.IQUAD.EQ.14) CALL MUTSAM(IQUAD)
      ENDDO
C
C         Look for a-layer stubs
C
      IF (SKIP_LEVEL.NE.5) THEN
          ITYPE = 2
          CALL MUCENT(ITYPE,DET_REGION)
          CALL GTMTRH(NTRACKS)
          CALL MU_ASTUB
          CALL GTMTRH(MTRACKS)
          DO I=NTRACKS+1,MTRACKS
              CALL MUIFW4(I)
          ENDDO
          CALL MUCENT(1,4)
      ENDIF
C
  999 RETURN
      END
