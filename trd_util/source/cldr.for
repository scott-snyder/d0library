        SUBROUTINE CLDR(IAN,IEVDAT,IEVTIM,QJR)
*
        IMPLICIT NONE
        INTEGER IEVDAT,IEVTIM
        INTEGER IY,IM,ID,IH,IN,IADD,IDD,IAN
        REAL QJR
        INTEGER NOFF(12)
        DATA NOFF/-1, 30, 58, 89, 119, 150, 180, 211, 242, 272, 303,
     &    333/
*
        IF (IAN.LE.0) IAN = 90
        IY = IEVDAT/10000
        IM = IEVDAT/100 - 100*IY
        ID = MOD(IEVDAT,100)
        IH = IEVTIM/10000
        IN = IEVTIM/100 - 100*IH
*
        IADD = 0
        IF(IY.EQ.92.AND.IM.GE.3)   IADD = 1
        IF(IY.GT.92.AND.IAN.EQ.92) IADD = 1
        IF(IY.EQ.96.AND.IM.GE.3)   IADD = 1
        IF(IY.GE.96.AND.IAN.EQ.96)   IADD = 1
        IDD = (IY-IAN)*365 + NOFF(IM) +ID + IADD
        QJR = IDD + (IH+IN/60.)/24.
*
        RETURN
        END
