C
C       Define NUMERIC variables for COMPACK
C
C	27-Oct-1989	Penelope Constanta-Fanourakis
C		Added variable COMLUN = LUN to open a command file. This LUN
C					is never released during program execution
C			       INPLUN = Variable that holds the current LUN that
C					input is comming from. To has the value
C					of either 5 (terminal input) or COMLUN
C					(command file input).
C				CMDOPEN = Logical indicating that a command file
C					  is open or not
C
      INTEGER MAXPOS,MAXCOM
      PARAMETER (MAXPOS=32)
      PARAMETER (MAXCOM=10)
      INTEGER POS,PF,MAXLIN(MAXLEV),NUMCOL,COMNUM,CURLEV
      INTEGER OLDLEV,SAVLEV,SAVPOS,NXTLEV,COMUNI,LINSPA,UPRLEV
      INTEGER MAILEV
      LOGICAL FULSCR,SETUP,EXIFLG,RDCOM,FSAVE,ONEFLG,ASTFLG,LOGUP
      LOGICAL TOPGO,NODISP,WAIFLG,SPLFLG,SAVSPL,TRMFLG,SAVTRM,ENDFLG
      LOGICAL CANFLG,QIOFLG,SMGON,BEGJOU,STAFLG,SETDON
      INTEGER COMLUN,INPLUN
      LOGICAL CMDOPEN
      COMMON /COMNUM/POS,PF,MAXLIN,CURLEV,NUMCOL,FULSCR,SETUP,COMNUM,
     *       TRMFLG,EXIFLG,RDCOM,FSAVE,SAVTRM,ONEFLG,ASTFLG,
     *       OLDLEV,SAVLEV,SAVPOS,LOGUP,NXTLEV,TOPGO,NODISP,WAIFLG,
     *       SPLFLG,SAVSPL,ENDFLG,CANFLG,QIOFLG,COMUNI,SMGON,BEGJOU,
     *       LINSPA,UPRLEV,MAILEV,STAFLG,SETDON,
     *       COMLUN,INPLUN,CMDOPEN
