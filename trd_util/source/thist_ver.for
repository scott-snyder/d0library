      SUBROUTINE THIST_VER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Fill histograms for TRD informations using TRD banks created
C-      with the Electron candidate
C-   Created  31-Aug-1990  Y. Ducros:  subroutine TRD_ELEC
C-   Updated  18-NOV-1991   A. Zylberstejn: change the name and adapt to
C-                                                reconstuction program
C-   Updated  16-JAN-1992   A. Zylberstejn   REPLACE A CALL TO GZPELC
C-   Updated   9-MAY-1992   A. Zylberstejn  Check that tracks entering the
C-                                  histos have not been already accumulated
C-   Updated  29-JAN-1993   Alain PLUQUET   new version of TPRL
C-   Updated   5-DEC-1993   A. Zylberstejn
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'
      REAL VAR,VERSION
      INTEGER I,IK,IM,J,JK,K,GZPELC,GZZTRH,LDTRK,IER
      INTEGER ICH,GZTRDT,LTRDT,LTPRL,FIRST_VER,NHIT(6),NTMAX
      PARAMETER(NTMAX=100)
      INTEGER NTR
      INTEGER LPELC, LZTRK,LZFIT, LZTRH,NEVT0
      INTEGER LOUT,TRUNIT
      INTEGER NA,NC,NCLA,NCLC
      INTEGER NWORD
      PARAMETER (NWORD=300)
      INTEGER INTEGER_WORD(NWORD)
      REAL REAL_WORD(NWORD)
      LOGICAL FIRST,DOPRINT
      INCLUDE 'D0$INC:FIRSHT.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        DOPRINT=SWTDBG.EQ.1
        FIRST_VER=FIRSHT+1000
        NEVT0=0
        NTR=NTMAX
      ENDIF
      IF(IQ(LHEAD+9).NE.NEVT0)THEN
        NEVT0=IQ(LHEAD+9)
        NTR=0
      END IF
      CALL HCDIR('//PAWC/TRD',' ')  ! go to TRD directory
C
C
      LZTRH=GZZTRH()
      IF(LZTRH.LE.0)GO TO 999
      LZTRK=LQ(LZTRH-1)
      IF(LZTRK.EQ.0)GO TO 999  ! request a track
      DO WHILE(LZTRK.NE.0) ! loop on tracks
        LTRDT=LQ(LZTRK-9)
        IF(LTRDT.EQ.0)GO TO 100  ! request a TRD information
        IM=1
        IF(LQ(LZTRK-4).NE.0)IM=2  ! If PELC
        DO IK=1,IM ! Ik=1 all tracks, ik=2 electrons
          J=(IK-1)*100
          CALL HF1(FIRST_VER+39+J,Q(LTRDT+16),1.)   ! effic. Likelihood E tot
          VAR=FLOAT(IQ(LTRDT+3))
          CALL HF1(FIRST_VER+50+J,VAR,1.)! nb. of hit  anode layers
          VAR=10+IQ(LTRDT+23)
          CALL HF1(FIRST_VER+50+J,VAR,1.)! nb. of hit  cathode layers
          DO 50 ICH=1,3
            LTPRL=LQ(LTRDT-ICH)
            NHIT(ICH)=0
            NHIT(ICH+3)=0
            IF(LTPRL.LE.0)GO TO 50
            CALL UNPACK_TPRL(LTPRL,VERSION,REAL_WORD,INTEGER_WORD,IER)
            NA=INTEGER_WORD(4)
            NC=INTEGER_WORD(5)
            NCLA=INTEGER_WORD(6)
            NCLC=INTEGER_WORD(7)
            DO I=1,NA                       ! E ANODE PER LAYER
              CALL HF1(FIRST_VER+6+ICH+J  ,REAL_WORD(50+I)+0.005,1.)
            END DO
            DO I=1,NC   ! E CATH. PER LAYER
              CALL HF1(FIRST_VER+6+ICH+J+3,REAL_WORD(50+NA+I)+0.005,1.)
            END DO
C            print*,' dans thist_ver layer',ich
C            call print_version
            IF(NA.NE.0)NHIT(ICH)=1
            IF(NC.NE.0)NHIT(ICH+3)=1
   50     CONTINUE
          DO JK=1,2  ! Loop on anode/cathode
            K=(JK-1)*3
            IF(NHIT(1+K)+NHIT(2+K)+NHIT(3+K) .GE.2)THEN
              DO ICH =  1, 3 ! Plot of NHIT/NMISS
                CALL HF1(FIRST_VER+54+J  ,FLOAT(ICH+K),
     &            FLOAT(NHIT(ICH+K)))
              ENDDO
            END IF
          ENDDO
        END DO
  100   CONTINUE
        LZTRK=LQ(LZTRK)
      END DO
  999 RETURN
      END
