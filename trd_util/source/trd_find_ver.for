      INTEGER FUNCTION TRD_FIND_VER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find trd version for new M.C. pedestal and gains
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-SEP-1993   J.P. Cussonneau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
      INCLUDE 'D0$LINKS:IZHSTR.LINK'
C --
      CHARACTER*4 PATH
      INTEGER I,MB,LOUT,TRUNIT
      INTEGER LGEAN,LHSTR,ND
      INTEGER GZHSTR
      INTEGER IVERSION
      INTEGER MBYTET
      CHARACTER*6 CHAR6(2)
      LOGICAL MCDATA,DOPRINT,TRD_DO_PRINT
C----------------------------------------------------------------------
      LOUT=TRUNIT()
      DOPRINT=TRD_DO_PRINT()
      TRD_FIND_VER = 0
      CALL PATHGT(PATH)
      IF ( LHEAD.GT.0 ) THEN
        MCDATA =  IQ(LHEAD+1) .GT. 1000
        IF(DOPRINT)THEN
          WRITE(LOUT,*)' record type dans HEAD',IQ(LHEAD+1),
     &      ' micro name',
     &      Q(LHEAD+2),Q(LHEAD+3)
          CALL UHTOC(Q(LHEAD+2),6,CHAR6(1),6)
          WRITE(LOUT,*)' micro_name 1',CHAR6(1)
          CALL UHTOC(Q(LHEAD+2),6,CHAR6(2),6)
          WRITE(LOUT,*)' micro_name 2',CHAR6(2)
        END IF
      ELSE
        MCDATA =  .FALSE.
      ENDIF
      IF ( MCDATA ) THEN
C        IF(DOPRINT)WRITE(LOUT,*)' in TRD_FIND_VER,path ',PATH
        IF (PATH.EQ.'RECO')  THEN
C
C  First try word in Geant HSTR bank.
C
          LHSTR=0
          IF (LHEADR.GT.0) THEN
            LGEAN=LQ(LHEADR-IZGEAN)
            IF (LGEAN.GT.0) THEN
              LHSTR=LQ(LGEAN-IZHSTR)
            ENDIF
          ENDIF
C          IF(DOPRINT)WRITE(LOUT,*)' lgeant',LGEAN, ' lhstr',LHSTR
          IF (LHSTR.GT.0) THEN
            ND=IQ(LHSTR-1)        ! CHECK IF HSTR CONTAINS VERSION INFORMATION
            IF (ND.GE.22) THEN
              IF(DOPRINT)THEN
                WRITE(LOUT,*)' production pass ID',IQ(LHSTR+2)
                WRITE(LOUT,*)' version #,pass #',IQ(LHSTR+3),
     &            IQ(LHSTR+4)
                WRITE(LOUT,*)' production program name ',
     &            (Q(LHSTR+I),I=7,16)
                WRITE(LOUT,*)' creation site ', (Q(LHSTR+I),
     &            I=17,18)
              END IF
              IVERSION = IQ(LHSTR+21)         ! READ VERSION NUMBER
              MB = MBYTET(12,IVERSION,1,8) ! TRD VERSION is in
C                                                   ! BITS 3 & 4
              CALL CBYT(MB,3,TRD_FIND_VER,1,2)
              IF ( TRD_FIND_VER.NE.1 ) TRD_FIND_VER=0

            ELSE
              TRD_FIND_VER=0
            ENDIF
          ELSE
C
C  Else use word in EVENT_HEAD bank.
C
            IF(DOPRINT)WRITE(LOUT,*)' We take version number in lhead'
            IVERSION = IQ(LHEAD+13)         ! READ VERSION NUMBER
            MB = MBYTET(12,IVERSION,1,8) ! TRD VERSION is in
C                                                             ! BITS 3 & 4
            CALL CBYT(MB,3,TRD_FIND_VER,1,2)
            IF ( TRD_FIND_VER.NE.1 ) TRD_FIND_VER=0
          ENDIF
        ELSEIF ( PATH.EQ.'GEAN') THEN
          LHSTR = GZHSTR()
          IF (LHSTR.GT.0) THEN
            ND=IQ(LHSTR-1)        ! CHECK IF HSTR CONTAINS VERSION INFORMATION
            IF (ND.GE.22) THEN
              IVERSION = IQ(LHSTR+21)         ! READ VERSION NUMBER
              MB = MBYTET(12,IVERSION,1,8) ! TRD VERSION is in
C                                                   ! BITS 3 & 4
              CALL CBYT(MB,3,TRD_FIND_VER,1,2)
              IF ( TRD_FIND_VER.NE.1 ) TRD_FIND_VER=0

            ELSE
              TRD_FIND_VER=0
            ENDIF
          ENDIF
          TRD_FIND_VER = 1 ! CCC
        ENDIF
      ENDIF
C
      IF(DOPRINT)WRITE(LOUT,*)'  TRD_FIND_VER', TRD_FIND_VER
  999 RETURN
      END
