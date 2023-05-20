;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               Object counting system assembly code                 ;
;                     Using PIC 16F877A                              ;
;         Version 0 by Mohammad Salameh on May 10th 2023             ;
;         Version 1 by Laith Hamdan on May 15th 2023                 ;
;	  Version 1.1 by Laith Hamdan on May 16th 2023		     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Version 0 : built the basic structure for the code, not a final or  ;
;            testing version.                                        ;
;Version 1 : Testing version.					     ;
;Version 1 : Semi-Final version.				     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TODO:                                                               ; 
;	Make better comments					     ;
;	Detail the subroutines					     ;
;	there's probably ton of comments syntax error...	     ;
;	Naming of variables and subroutines			     ;
;	Optimization						     ;
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
;Label		INST		field1, field2			     ;
;Labels use PascalCase (first letter of each word is capitalized)    ;
;Instructions have all letters capitalized                           ;
;Variable names and pins defined also use PascalCase                 ;
;FSR and Pin names defined by the header file are all capitalized    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           Start of Code                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;			Header files				     ;
	    include		"p16f877A.inc"
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;			Configuration word			     ;
	    ; CONFIG
	    ; __config 0x3F39
	    __CONFIG _FOSC_XT & _WDTE_OFF & _PWRTE_OFF & _BOREN_OFF & _LVP_OFF & _CPD_OFF & _WRT_OFF & _CP_OFF
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
	    WTemp
	    StatusTemp
	    HalfSecCounter
	    OverflowCount
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
Display     equ         0           ;bit 0 of State
LTR         equ         1           ;bit 1 of State
Detected    equ         2           ;bit 2 of State
VarL        equ         3           ;bit 3 of State
VarR        equ         4           ;bit 4 of State
Pause       equ         7           ;bit 7 of State
;version 0 pins ends here, next versions add below and add comment
PauseButton equ         7           ;Pin 7 of PortB == RB7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;			    Macro Assignments			     ;
;Copied from Exp8's labSheet 
push	    macro
	    movwf		WTemp		;WTemp must be reserved in all banks
	    swapf		STATUS,W	;store in W without affecting status bits
	    banksel		StatusTemp	;select StatusTemp bank
	    movwf		StatusTemp	;save STATUS
	    endm


pop	    macro
	    banksel		StatusTemp	;point to StatusTemp bank
	    swapf		StatusTemp,W	;unswap STATUS nibbles into W	
	    movwf		STATUS		;restore STATUS (which points to where W was stored)
	    swapf		WTemp,F		;unswap W nibbles
	    swapf		WTemp,W		;restore W without affecting STATUS
	    endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         Start of Program                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Start of executable code
            org 0x00
            GOTO        Main
            org 0x04
            GOTO        ISR

;;;;;;;;;;;;;;;;;;;;;;;;;;Initial;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Initial	    
	    BANKSEL	TRISD
	    CLRF	TRISD			;Output
	    MOVLW	b'10000111'		
	    MOVWF	TRISB

	    MOVLW	b'11110000'
	    MOVWF	INTCON
;(GIE, PEIE, TMR0IE, INTE) is set, (RBIE, TMR0IF, INTF, RBIF) is cleared

	    MOVLW	b'11000001'
	    MOVWF	OPTION_REG
;Interrupt on rising edge of RB0, 1:4 prescale
	    
	    BCF		PIR1, TMR1IF
	    BCF		PIR1, TMR2IF		;All Flags cleared
	    BSF		PIE1, TMR2IE		;enable timer2
	    BSF		PIE1, TMR1IE

	    MOVLW	.244			;244 + 1 = 245
	    MOVWF	PR2
;to create just over a 1 sec delay, 1s and 3520us

	    BANKSEL	TMR0			
	    MOVLW	0X06			;256 - 6 = 250
	    MOVWF	TMR0
	    
	    MOVLW	b'01111010'
	    MOVWF	T2CON			
;1:16 prescale, 1:16 postscale, TMR2ON is cleared.

	    MOVLW	b'00110000'
	    MOVWF	T1CON
;1:8 prescale, TMR1ON is cleared.
	    
;timer1 0.5s, 65,536 - 62,500 = 2856, 0x0B28	    
	    MOVLW	0x28
	    MOVWF	TMR1L
	    MOVLW	0X0B
	    MOVWF	TMR1H
	    
	    MOVLW	b'00001000'		;one of the displays has to be on
	    MOVWF	PORTB
	    
	    MOVLW	.5
	    MOVWF	CMin
	    MOVLW	.10
	    MOVWF	CMax
	    INCF	CMax			;CMax still 10
;but since we check the carry if set, to determince if we're out of in range,
;both Count > CMax, and Count = CMax, will result in carry = 1, when checking
;the easiest way to solve this is to increment CMax in Initial.
	    
	    CLRF	TMR2
	    CLRF	Count
            CLRF	State
	    CLRF	OverflowCount
	    RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;;Main;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Main        
	    CALL	Initial
MainLoop    
            BTFSC       State, Detected         ;skip if no object
            CALL        ObjectFound
            GOTO        MainLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ObjectFound;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this subroutine determines whether to Decrement or Increment the count                                        
;Checks if Under CMin or Over CMax	
;Turn on buzzer, LED and timer1 module.
ObjectFound 
	    BCF		State, Detected
	    BTFSC       State, LTR		;skip if Right to left
            GOTO        Decrement
	    
Increment   
	    INCF        Count, F
	    MOVLW	.100
	    SUBWF       Count, W
	    BTFSC       STATUS,C        
            GOTO        Decrement
	    GOTO	BCD
	    
Decrement
	    DECF        Count, F
	    BTFSC	Count, .7		;Check if the MSB is set
	    GOTO	Increment
	    
BCD
	    CALL	GenBCD
	    
Test
            MOVF        CMax , W
            SUBWF       Count, W
            BTFSC       STATUS,C        
            GOTO        OutRange	
	    
TestIfInRange
	    MOVF        CMin , W
            SUBWF       Count, W
            BTFSS       STATUS,C         
            GOTO        OutRange
	    
InRange
	    MOVLW	.1
	    GOTO	TURNON
	    
OutRange
	    MOVLW	.3
;If we want the Led to Flash more times than 3, we don't make it flash more than
;1.5s, it's already long, we edit timer 1 to be 0.25 sec,
;HalfSecCounter and Timer1 ISR.
	    
TURNON					
	    MOVWF	HalfSecCounter
	    BSF		PORTB, Buzzer
	    BSF		PORTB, Led
	    BSF		T1CON, TMR1ON
	    RETURN				;Returns to Main
	    
;;;;;;;;;;;;;;;;;;;;;;;;;GenBCD;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Converts counter's 8-bit value to 2 BCD digits, Units and Tens      ;
;Copied from Exp7's labSheet                                         ;
GenBCD
            BANKSEL	PORTA
            MOVF        Count,W			
            MOVWF       Temp
	    CLRF	Tens			;Must Clear the Tens.

gen_tens
            MOVLW     	.10			;sub 10,result keep in W
            SUBWF     	Temp,W
            BTFSS     	STATUS,C	        ;judge if the result bigger than 10
            GOTO      	gen_ones	        ;no,get the Entries bit result
            MOVWF     	Temp		        ;yes,result keep in TEMP
            INCF      	Tens,F			;ten bit add 1
            GOTO      	gen_tens		;turn  to continue get ten bit
gen_ones
            MOVF      	Temp,W
            MOVWF     	Units			;the value of Entries bit
            RETURN				;returns to ObjectFound
	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                   Interrupts code starts here                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ISR
	    push
	    BTFSC	INTCON, INTF		;External Interrupt has higher priority
	    CALL	Sensors
	    BTFSC	INTCON, TMR0IF		
	    CALL	DisplayUpdate
	    BTFSC	PIR1, TMR1IF
	    CALL	HalfSecDelay
	    BTFSC	PIR1, TMR2IF
	    CALL	LeftRightClear		
	    pop
	    RETFIE 
	    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Sensors;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Checks SenseLeft, Complement VarL/VarR
;Enable Timer2 module or update the state
Sensors
	    BCF		INTCON, INTF
	    BTFSC	PORTB, SenseLeft	;Check if SensorLeft is ON
	    GOTO	Left
	    
Right	    
	    BSF		State, VarR		;Complementing VarR without checking PORTB SenseRight
	    BTFSS	State, VarL	
	    GOTO	EnableTimer2
	    BSF		State, LTR
	    GOTO	DisableTimer2
	   
Left	    
	    BSF		State, VarL		;Complementing VarL
	    BTFSS	State, VarR
	    GOTO	EnableTimer2
	    BCF		State, LTR
	    GOTO	DisableTimer2
	    	    
EnableTimer2
	    BSF		T2CON, TMR2ON		
	    RETURN				;Returns to ISR
	    
DisableTimer2
	    BCF		T2CON, TMR2ON		;Disable Timer2
	    CLRF	TMR2
	    CLRF	OverflowCount
	    
	    BSF		State, Detected
	    BCF		State, VarL
	    BCF		State, VarR
	    RETURN				;Returns to ISR
	    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DisplayUpdate;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Checks Display, Send Units/Tens, Complement Display
DisplayUpdate
	    BCF		INTCON, TMR0IF		;Clear TMR0 Flag
	    MOVLW	0X06			;Reinitialize TMR0
	    MOVWF	TMR0
	    MOVLW	b'00011000'		;Complement Display
	    XORWF	PORTB, F
;the complement HAS to be here for the 7 segment to work probably
;2us between the MOVWF PORTD and the complement is not enough for the
;7segment to display accurate reslults.
	    BTFSC	PORTB, UnitsSelect
	    GOTO	UnitsDisplay
	    MOVF	Tens, W
	    GOTO	Send
	    
UnitsDisplay	    
	    MOVF	Units, W
	    
Send	    
	    CALL	Table
	    MOVWF	PORTD
	    RETURN
	    
;************************** Lookup Table ************************
Table
	    ADDWF  	PCL,1
	    RETLW	B'11000000'		;'0'
	    RETLW	B'11111001'		;'1'
	    RETLW	B'10100100'		;'2'
	    RETLW	B'10110000'		;'3'
	    RETLW	B'10011001'		;'4'
	    RETLW	B'10010010'		;'5'
	    RETLW	B'10000010'		;'6'
	    RETLW	B'11111000'		;'7'
	    RETLW	B'10000000'		;'8'
	    RETLW	B'10010000'		;'9'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HalfSecDelay;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Decrements HalfSecCounter, Complement Led and Buzzer If zero,
;Complement Led Only if not, and Turn off timer1
HalfSecDelay
	    BCF		PIR1, TMR1IF
	    BANKSEL	TMR1L				    
	    MOVLW	0x28
	    MOVWF	TMR1L
	    MOVLW	0X0B
	    MOVWF	TMR1H
	    DECFSZ	HalfSecCounter, F
	    GOTO	Continue 
	    
Zero	    
	    MOVLW	b'01100000'		;Complement Led and Buzzer
	    XORWF	PORTB, F
	    BCF		T1CON, TMR1ON
	    RETURN
	    
Continue	    
	    MOVLW b'01000000'			;Complement Led
	    XORWF PORTB, F
	    RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LeftRightClear;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Just over 1ms Delay with 16 Software postscale, when done Clear VarL/VarR
;and Turn off timer2
LeftRightClear
	    BCF		PIR1, TMR2IF		;Clear TMR2 Flag
	    INCF	OverflowCount, F
	    MOVLW	.16			;Assuming a clock of 4MHz, we need 
	    SUBWF	OverflowCount, W	;245 * 16 * 16 * 16 = 1003520us
	    BTFSS	STATUS, Z
	    RETURN				

	    CLRF	OverflowCount
	    BCF		State, VarL
	    BCF		State, VarR
	    BCF		T2CON, TMR2ON
	    RETURN
	    
	    END
