C ----------------------------------------------------------------------
C            Data Logger Volume Definition Declarations
C ----------------------------------------------------------------------
      PARAMETER MAX_TRIG_BITS = 32               
      PARAMETER Max_Files = 32
      PARAMETER MARKER = '!'                     ! Denotes string input
C                                                
      Structure /Volume_Attr/
        Character*12    Label
        Integer*4       Size                     
        Integer*4       File_Cnt
        Integer*4       Tot_Event_Cnt
        Integer*4       Trig_Cnts (Max_Trig_Bits)
      End Structure

      Structure /File_Attr/
        Integer*4       Open_Tim(2),Close_Tim(2)
        Character*32    FileName
        Integer*4       Size                     
        Integer*4       Run
        Integer*4       Beg_Cnt,End_Cnt,Other,Event_Cnt
        Integer*4       Trig_Cnts (Max_Trig_Bits)
        Character*72    Run_Comt, Trig_Log_Name (Max_Trig_Bits)
      End Structure
