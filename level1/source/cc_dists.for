      SUBROUTINE CC_DISTS(L1BIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : L1BIT    - L1.0 muon raw bits from simulator
C-   Outputs : none
C-   Controls:
C-
C-   Original    JUN-1992   Kamel Bazizi
C-   Created   2-SEP-1992   Guilherme Lima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

C-- From RCP
      INTEGER MULTBK,IER
      LOGICAL IPRMULT
C-- Counters
      INTEGER J,I,II,III
      INTEGER NUMCC,NCC1,NCC2
C-- Coarse centroid variables
      INTEGER MODNO,MODID,MODS(200),ISECT
      INTEGER ICRS(16),ITRUNC,ICENFL,TEMP
      INTEGER IND,IND1,NMULT(6),IERP
C-- Regions with triggre
      LOGICAL ON,OS,SN,SS,SSWN,SSWS,L1BIT(16)
      CHARACTER*80 STRING


      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.

        CALL EZPICK('MUSIM_RCP')
	CALL EZERR(IERP)     ! Check if error
        IF(IERP.EQ.0)THEN
          CALL EZGET('MULT_BK',MULTBK,IER)
          CALL EZGET('IPR_MULT',IPRMULT,IER)
          CALL EZRSET()
	ELSE
           CALL EZGET_ERROR_TEXT(IER,STRING)
           CALL ERRMSG(' CANNOT PICK MUSIM.RCP',
     &          'CC_DISTS',STRING,'F')
           GOTO 999
	ENDIF

C-- Get the module numbers
        CALL MU_TRIG_SECT(MODNO,MODID,ISECT,MODS)

C.. Fill SAMUS mult histos when IPRMULT flag is set
        IF(MULTBK.EQ.0 .AND. IPRMULT) MULTBK=1

      ENDIF

C- DO WAMUS
      IF(MULTBK.EQ.2) THEN
        DO I=1,164
          MODNO=MODS(I)
          NUMCC=0
          CALL MUMCRS(MODNO,ITRUNC,ICENFL,ICRS)
          IF(ICENFL.GT.0) THEN
            CALL MU_TRIG_SECT(MODNO,MODID,ISECT,MODS)
            IF (ABS(MODID).NE.MODID) THEN
              DO III = 1,8
                TEMP = ICRS(III)
                ICRS(III)= ICRS(17-III)
                ICRS(17-III)=TEMP
              ENDDO
            ENDIF

            DO J=1,16
              NUMCC=NUMCC+ICRS(J)
              IF(ICRS(J).NE.0)
     &            CALL HFILL(4000+MODNO,FLOAT(J),0.,1.)
            ENDDO
          ENDIF
          CALL HFILL(5000+MODNO,FLOAT(NUMCC),0.,1.)
        ENDDO
      ENDIF

      IF(MULTBK.GE.1) THEN

C.. Do SAMUS
        DO I=165,200
          MODNO=MODS(I)
          NUMCC=0
          CALL MUMCRS(MODNO,ITRUNC,ICENFL,ICRS)
          IF(MOD(I,2).EQ.0) MODNO=MODS(I-1)
          IF(ICENFL.GT.0) THEN
C--       Just to fill the righ histo
            DO J=0,15
              NUMCC=NUMCC+ICRS(J+1)
              II=J
              IF(MOD(I,2).EQ.0) II=31-J
              IF(ICRS(J+1).NE.0)
     &            CALL HFILL(4000+MODNO,FLOAT(II),0.,1.)
            ENDDO
          ENDIF
          IF(MOD(I,2).EQ.0) THEN
            NCC2=NUMCC
          ELSE
            NCC1=NUMCC
          ENDIF
          IF(MOD(I,2).EQ.0) THEN
            CALL HFILL(5000+MODNO,FLOAT(NCC1+NCC2),0.,1.)
C.. Print some mutiplicity info (these are the modules with cuts).
            IND1=MOD(MODS(I),10)               !  Last digit of SAMUS MAC's
            IF(IND1.EQ.0) THEN
              IND=(MODS(I)-400)/10             !  Second digit of SAMUS MAC's
              NMULT(IND+1)=NCC1+NCC2
            ENDIF
          ENDIF
        ENDDO

C.. Print out the SAMUS multiplicities
        IF(IPRMULT) THEN
          WRITE(*,*) '     SAM North mult:',(NMULT(I),I=1,3)
          WRITE(*,*) '     SAM South mult:',(NMULT(I),I=4,6)
        ENDIF

C.. Checks which histograms to fill
        ON=.FALSE.
        OS=.FALSE.
        SN=.FALSE.
        SS=.FALSE.
        SSWN=.FALSE.
        SSWS=.FALSE.
        IF(L1BIT( 8).OR.L1BIT( 9)) ON=.TRUE.
        IF(L1BIT(10).OR.L1BIT(11)) OS=.TRUE.
        IF(L1BIT(12).OR.L1BIT(13)) SN=.TRUE.
        IF(L1BIT(14).OR.L1BIT(15)) SS=.TRUE.
        IF(ON.OR.OS) CALL MU_SWCCT_SSW(SSWN,SSWS)

C.. Fills the veto histograms
        IF(ON) THEN
          CALL HFILL(1071,FLOAT(NMULT(1)),0.,1.)
          CALL HFILL(1072,FLOAT(NMULT(2)),0.,1.)
          IF(SSWN) CALL HFILL(1073,FLOAT(NMULT(2)),0.,1.)
        ENDIF

        IF(OS) THEN
          CALL HFILL(1074,FLOAT(NMULT(4)),0.,1.)
          CALL HFILL(1075,FLOAT(NMULT(5)),0.,1.)
          IF(SSWS) CALL HFILL(1076,FLOAT(NMULT(5)),0.,1.)
        ENDIF

        IF(SN) THEN
          CALL HFILL(1081,FLOAT(NMULT(1)),0.,1.)
          CALL HFILL(1082,FLOAT(NMULT(2)),0.,1.)
          CALL HFILL(1083,FLOAT(NMULT(3)),0.,1.)
        ENDIF

        IF(SS) THEN
          CALL HFILL(1084,FLOAT(NMULT(4)),0.,1.)
          CALL HFILL(1085,FLOAT(NMULT(5)),0.,1.)
          CALL HFILL(1086,FLOAT(NMULT(6)),0.,1.)
        ENDIF

      ENDIF

  999 RETURN
      END
