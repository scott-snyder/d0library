C
      COMMON /ZEBIO/  IRDUNI,IWRUNI,IZBOPT(8),IRDREC,IWRREC
C
      INTEGER IRDUNI,IRDREC,IWRUNI,IWRREC,IZBOPT
C
C  Common block for I/O control.   see Zebra manual, S/R FZOPEN
C  Input/Ouput file names (character strings) are in /ZEBIOG/.
C
C  IRDUNI/IWRUNI =  input/output logical unit number.
C                     if =0, no input/output.   
C  IZBOPT        =  option words for future use.
C  IRDREC/IWRREC =  input/output record length.
C
