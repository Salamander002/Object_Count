;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               Object counting system assembly code                 ;
;                     Using PIC 16F877A                              ;
;         Version 0 by Mohammad Salameh on May 10th 2023             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Version 0 : built the basic structure for the code, not a final or  ;
;            testing version.                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TODO:                                                               ;
;       Initial subroutine                                           ;
;       Delay subroutine                                             ;
;       InRange subroutine                                           ;
;       OutRange subroutine                                          ;
;       all Interrupts                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;System has to count/uncount objects that pass through the two infra-;
;red sensors. displays the count value on two 7 segments displays.   ;
;produce a buzzing sound after each count and if the count is outside;
;a certain range.                                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           Hardware used                            ;
;       2 multiplexed 7-segments displays connected to portD         ;
;           Select display using pins RB3, RB4                       ;
;       2 infrared sensors connected to pin RB0 in OR configuration  ;
;           also connected to pins RB1 and RB2 separately            ;
;       1 Buzzer connected to pin RB5                                ;
;       1 LED light connected to pin RB6                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           Code convention                          ;
;Label		INST		field1, field2								 ;
;Labels use PascalCase (first letter of each word is capitalized)    ;
;Instructions have all letters capitalized                           ;
;Variable names and pins defined also use PascalCase                 ;
;FSR and Pin names defined by the header file are all capitalized    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           Start of Code                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         Declaring Variables                        ;
            cblock 0x20
            Count
            State
            CMin
            CMax
            Units
            Tens
            Temp
;version 0 Variables ends here, next versions add below and add comment

            endc
;                                                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          Declaring pin names                       ;
;pins are named to increase code readability                         ;
SenseRight  equ         1           ;Pin 1 of PortB == RB1
SenseLeft   equ         2           ;Pin 2 of PortB == RB2
UnitsSelect equ         3           ;Pin 3 of PortB == RB3
TensSelect  equ         4           ;Pin 4 of PortB == RB4
Buzzer      equ         5           ;Pin 5 of PortB == RB5
Led         equ         6           ;Pin 6 of PortB == RB6
RTL         equ         0           ;bit 0 of State
LTR         equ         1           ;bit 1 of State
Detected    equ         2           ;bit 2 of State
VarL        equ         3           ;bit 3 of State
VarR        equ         4           ;bit 4 of State
Pause       equ         7           ;bit 7 of State
;version 0 pins ends here, next versions add below and add comment

;                                                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         Start of Program                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            org 0x00
            CALL        Initial
            GOTO        0x06

            org 0x04
            GOTO        ISR


            org 0x06
Main        CALL        UpdateDisplay
            BTFSC       State, Detected         ;skip if no object
            CALL        ObjectFound
            GOTO        Main

;;;;;;;;;;;;;;;;;;;;;;;;;;Initial;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Intializes the configuration bits and ports for the program to start;

Initial


;;;;;;;;;;;;;;;;;;;;;;;;;;UpdateDisplay;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;finds the units value and Tens value to be displayed and displays them
UpdateDisplay
            CALL        GenBCD
            CALL        Display
            RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;GenBCD;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Converts counter's 8-bit value to 2 BCD digits, Units and Tens      ;
;Copied from Exp7's labSheet                                         ;
GenBCD
            BANKSEL		PORTA
            MOVF        Count,f
            MOVWF       Temp
gen_tens
            MOVLW     	.10             ;sub 10,result keep in W
            SUBWF     	Temp,w
            BTFSS     	STATUS,C        ;judge if the result bigger than 10
            GOTO      	gen_ones        ;no,get the Entries bit result
            MOVWF     	Temp            ;yes,result keep in TEMP
            INCF      	Tens,f          ;ten bit add 1
            GOTO      	gen_tens        ;turn  to continue get ten bit
gen_ones
            MOVF      	Temp,W
            MOVWF     	Units           ;the value of Entries bit
            RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;Display;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;displays units first, waits for 1ms (may be increased later) then   ;
;displays Tens. finds 7 segments code using a look-up table          ;
Display     BANKSEL     PORTB
            BSF         PORTB,UnitsSelect
            BCF         PORTB,TensSelect
            MOVF        Units,w
            Call        Table
            MOVWF       PORTD
            CALL        Delay               ;Delay using timers
            BCF         PORTB,UnitsSelect
            BSF         PORTB,TensSelect
            MOVF        Tens,w
            Call        Table
            MOVWF       PORTD
            CALL        Delay
            RETURN
;************************** Lookup Table ************************
Table
		ADDWF  	 	PCL,1
		RETLW		B'11000000'		;'0'
		RETLW		B'11111001'		;'1'
		RETLW		B'10100100'		;'2'
		RETLW		B'10110000'		;'3'
		RETLW		B'10011001'		;'4'
		RETLW		B'10010010'		;'5'
		RETLW		B'10000010' 	;'6'
		RETLW		B'11111000'		;'7'
		RETLW		B'10000000'		;'8'
		RETLW		B'10010000'		;'9'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Delay;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Using timer2 for delay. this subroutine is blocking which is        ;
;necessary to ensure that the displays are in sync                   ;
Delay



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ObjectFound;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this subroutine determines whether to Decrement or Increment the count
ObjectFound BTFSC       State, LTR           ;skip if Right to left
            CALL        Decrement
            BTFSC       State, RTL           ;skip if left to right
            CALL        Increment
            RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Decrement;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Decrements Count , checks if under CMin
Decrement   DECF        Count, f
            MOVF        CMin , w
            SUBWF       Count, w
            BTFSS       STATUS,C        ;skip if count is in range
            CALL        OutRange
            MOVF        CMin , w        ;have to redo the calculation
            SUBWF       Count, w        ;to know if it is actually in range
            BTFSC       STATUS,C        ;or returned from OutRange subroutine
            Call        InRange
            RETURN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Increment;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Increments Count , checks if Over CMax
Increment   DECF        Count, f
            MOVF        CMax , w
            SUBWF       Count, w
            BTFSS       STATUS,C        ;skip if count is out of range
            CALL        InRange
            MOVF        CMax , w        ;have to redo the calculation
            SUBWF       Count, w        ;to know if it is actually out of range
            BTFSC       STATUS,C        ;or returned from INRange subroutine
            Call        OutRange
            RETURN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;InRange;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Turns on buzzer and LED and timer1 module.
;timer1 interrupt code turns off the buzzer and the LED
InRange


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OutRange;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Turns on buzzer and LED and timer1 module.
;timer1 interrupt code turns the buzzer and the LED off and on repeatedly
OutRange


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                   Interrupts code starts here                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ISR
