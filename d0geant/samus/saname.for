      SUBROUTINE SANAME ( NTY, TUBE_NAME,VOLU_NAME )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Give names of a tube and its drift volume for
C-                         certain tube's type
C-
C-   Inputs  : NTY - tube's type
C-   Outputs : TUBE_NAME - name of a tube
C-             VOLU_NAME - name of tube's drift volume
C-
C-   Created  18-OCT-1990   A.Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTY, I,J,K
      INTEGER NTYPES
      PARAMETER( NTYPES = 199 )
      CHARACTER*4 TUBE_NAME,VOLU_NAME,BL(20),TN(NTYPES),VN(NTYPES)
      LOGICAL FIRST
      SAVE FIRST,TN,VN
C
      DATA BL/'0','1','2','3','4','5','6','7','8','9',
     &        'A','B','C','D','E','F','G','H','I','J'/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  Define the arrays with names
C
      IF ( FIRST ) THEN
        DO 10 I=1,NTYPES
          TN(I)(1:2)='ST'
          VN(I)(1:2)='SV'
          J=I/10+1
          K=MOD(I,10)+1
          TN(I)(3:3)=BL(J)
          VN(I)(3:3)=BL(J)
          TN(I)(4:4)=BL(K)
          VN(I)(4:4)=BL(K)
   10   CONTINUE
        FIRST=.FALSE.
        GOTO 999
      ENDIF
C
C ****  Receive tube name and drift volume name
C
      TUBE_NAME = TN(NTY)
      VOLU_NAME = VN(NTY)
C
  999 RETURN
      END
