C  
      COMMON /ZEBIOG/ IRDCHR,IRDFLN,IWRCHR,IWRFLN
C
      CHARACTER*4 IRDCHR,IWRCHR
      CHARACTER*40 IRDFLN,IWRFLN
      INTEGER LRDCHR,LRDFLN(10),LWRCHR,LWRFLN(10)
      EQUIVALENCE (IRDCHR,LRDCHR)
      EQUIVALENCE (IRDFLN,LRDFLN(1))
      EQUIVALENCE (IWRCHR,LWRCHR)
      EQUIVALENCE (IWRFLN,LWRFLN(1))
C
C  Common block for D0 I/O for /ZEBCOM/ data structure.   
C  I/O logical unit numbers are stored in  /ZEBIO/.
C
C  IRDCHR/IWRCHR =  input/output options.
C  IRDFLN/IWRFLN =  input output file name.
C
