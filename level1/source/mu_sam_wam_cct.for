      SUBROUTINE MU_SAM_WAM_CCT(ICCT,SWCC,SWBITS,SWTRIG,SEGFLG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find roads in SAMUS-WAMUS overlap region.
C-
C-   Inputs  :
C-             ICCT           LOCATION:
C-           21,23,25,27    North Quadrant 0,2,4,6
C-           22,24,26,28    South Quadrant 0,2,4,6
C-
C-             SWCC(i,j) - Input bit map for the CCT
C-
C-   Outputs : SWTRIG    - A bit set if at least one good road is found
C-             SWBITS(i) - 16 bits for good roads
C-             SEGFLG   -  Flags for segments fired
C-
C-   Controls: None
C-
C-   Created  13-MAY-1992   Guilherme Lima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICCT, SWCC(0:15,6)
      INTEGER SWTRIG, SWBITS(0:15)
      INTEGER IND,I,II,X(0:31),Y(0:31),U(0:31)
      INTEGER IA,IB,IC
      LOGICAL SSWN,SSWS,SSW
      LOGICAL SSW_N,SSW_S
      LOGICAL SEGFLG(2),AUX
C----------------------------------------------------------------------

      DO II=1,2
        DO I=0,15
          IND=16*(II-1)+I
          X(IND)=0
          Y(IND)=0
          U(IND)=0
          X(IND)=SWCC(I,II)
          Y(IND)=SWCC(I,II+2)
          U(IND)=SWCC(I,II+4)
        ENDDO
      ENDDO

      IA=0
      IB=0
      IC=0
      DO I=0,31
        IF(X(I).NE.0) THEN
          IF(I.LE.15) IA=1
          IF(I.GE.16) IB=1
        ENDIF
        IF(Y(I).NE.0) IB=1
        IF(U(I).NE.0) IC=1
      ENDDO
c      CALL LAY_WRITE(ICCT,IA,IB,IC)


      IF(ICCT.EQ.21) SSWN=.FALSE.
      IF(ICCT.EQ.22) SSWS=.FALSE.

C.. Logic for top SAMUS-WAMUS overlap
      IF (ICCT.GE.21.AND.ICCT.LE.24)
     &    CALL MU_SWCCT_TOP(X,Y,U,SWBITS,SSW,SEGFLG)

C.. Logic for bottom SAMUS-WAMUS overlap
      IF (ICCT.GE.25.AND.ICCT.LE.28)
     &    CALL MU_SWCCT_BOT(X,Y,U,SWBITS,SSW,SEGFLG)

C.. Reverse segment flags for quadrants 2 and 4
      IF(ICCT.EQ.23.OR.ICCT.EQ.24.OR.ICCT.GE.27) THEN
        AUX=SEGFLG(1)
        SEGFLG(1)=SEGFLG(2)
        SEGFLG(2)=AUX
      ENDIF

      IF(MOD(ICCT,2).EQ.1) THEN
        IF(SSW) SSWN=.TRUE.
      ELSE
        IF(SSW) SSWS=.TRUE.
      ENDIF

      SWTRIG=0
      DO I=0,15
        IF (SWBITS(I).NE.0) SWTRIG=1
      ENDDO

      RETURN

C#######################################################################
      ENTRY MU_SWCCT_SSW(SSW_N,SSW_S)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Tells if there are overlap triggers from 
C-       S+S+W combinations in ON or OS
C-
C-   Inputs  : None
C-   Outputs : SSWN,SSWS = For North/South S+S+W combinations
C-   Controls: 
C-
C-   Created   3-MAR-1993   Guilherme Lima
C-
C----------------------------------------------------------------------
      SSW_N=SSWN
      SSW_S=SSWS
      RETURN
      END
