******************************************************************************
*                                                                            *
*    File:  L_Tool.Inc                                  Rev.  26-JAN-1994    *
*                                                                            *
*                                                                            *
*  This is an Include file which is referenced by the Tool Code source       *
*  files:  Inner_T2...Inner_T5.  It is not referenced by any other modules   *
*  at this time.                                                             *
*                                                                            *
*  This file defines three symbols which describe the relative positions of  *
*  the 4 blocks of Trigger Tower data in C40 memory.                         *
*                                                                            *
*  It is not my choice to use an include file for this purpose but because   *
*  these symbols are used as immediate data in C40 instructions then they    *
*  need to be absolute symbols and they can not be external symbols.  Thus   *
*  they need to be defined in each of the 4 instances of Tool Code source    *
*  files (i.e. each of the 4 eta rings).                                     *
*                                                                            *
*  At lease having these symbols in an include file lets us edit only one    *
*  file to change their value in all 4 Tool Code files.                      *
*                                                                            *
*                                                                            *
* This file also contains some constants used in all Tool Code source files  *
******************************************************************************

******************************************************************************
*   "Relative Data Positioning" constants                                    *
******************************************************************************

Rack_1_Tot_Minus_EM_Value   .set         40h       ; Difference between
Rack_2_Tot_Minus_EM_Value   .set         40h       ; Base Address of the
                                                   ; EM data and the Tot
                                                   ; data for each rack.

Rack_2_Minus_Rack_1_Value   .set         80h       ; Difference between
                                                   ; the
                                                   ; Base Address of
                                                   ; Rack #2
                                                   ; data and the Base
                                                   ; Address of Rack #1
                                                   ; data.

Neighbor_1                    .set 00100h       ; Specifies "key" to
                                                ; 1st neighbor scanned.
                                                ; MSByte = delta phi (+1)
                                                ; LSByte = delta eta (0)

Neighbor_2                    .set 00001h       ; Specifies "key" to
                                                ; 2nd neighbor scanned.
                                                ; MSByte = delta phi (0)
                                                ; LSByte = delta eta (+1)

Neighbor_3                    .set 0ff00h       ; Specifies "key" to
                                                ; 3rd neighbor scanned.
                                                ; MSByte = delta phi (-1)
                                                ; LSByte = delta eta (0)

Neighbor_4                    .set 000ffh       ; Specifies "key" to
                                                ; 4th neighbor scanned.
                                                ; MSByte = delta phi (0)
                                                ; LSByte = delta eta (-1)

Tool_Accept_Flag              .set   000h       ; Flag value indicating
                                                ; that seed is accepted.

Tool_Reject_Flag              .set   0ffh       ; Flag value indicating
                                                ; that seed is rejected.

