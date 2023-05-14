;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               Object counting system assembly code                 ;
;                     Using PIC 16F877A                              ;
;         Version 2 by Laith Hamdan on May 14th 2023                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Version 0 : built the basic structure for the code, not a final or  ;
;            testing version.                                        ;
;Version 1 : I can probably call it a testing version		     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TODO:                                                               ;
;	Test the code					             ;  
;	Do we want Hardware/Software pause button		     ;
;	Make better comments					     ;
;	Cap Cmin to 00 and Cmax to 99				     ;
;	Detail the subrotines					     ;
;	there's probably ton of comments syntax error...	     ;
;	Last few lines of intitial				     ;
;	Naming of variables and subroutiens			     ;
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
	    include		"p16f877A.inc"
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
	    ContextW
	    ContextS
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
;
;                                                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         Start of Program                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            org 0x00
            GOTO        Main
            org 0x04
            GOTO        ISR

Main        
	    CALL	Initial
MainLoop    
            BTFSC       State, Detected         ;skip if no object
            CALL        ObjectFound
            GOTO        MainLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;Initial;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Intializes the configuration bits and ports for the program to start;

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
	    BCF		PIR2, TMR2IF		;All Flags cleared
	    BSF		PIE2, TMR2IE		;enable timer2, we want 32 post*pre scale
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
	    
	    ;Do I have to clear PORTB?
	    CLRF	TMR2
	    CLRF	Count			;Start From 0?			
            CLRF	State			;the only one that have to be cleared
            CLRF	CMin			;what is CMin
            CLRF	CMax			;what is CMax
;the program must always start from 00 not Cmin with our implementaion, because BCD
;doesn't update unless we increment/decrement, we can initialize the Units/Digits/Count
;to be Cmin for it to work



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ObjectFound;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this subroutine determines whether to Decrement or Increment the count                                        
;Checks if Under CMin or Over CMax	
;Turn on buzzer, LED and timer1 module.
ObjectFound 
	    BCF		State, Detected
	    BTFSC       State, LTR		;skip if Right to left
            GOTO        Decrement
	    
Increment   
	    INCF        Count, f
	    GOTO	BCD
	    
Decrement
	    DECF        Count, f
	    
BCD
	    CALL	GenBCD
	    
Test
            MOVF        CMax , w
            SUBWF       Count, w
            BTFSC       STATUS,C        
            GOTO        OutRange	
	    
TestIfInRange
	    MOVF        CMin , w
            SUBWF       Count, w
            BTFSS       STATUS,C         
            GOTO        OutRange
	    
InRange
	    MOVLW	.1
	    GOTO	TURNON
	    
OutRange
	    MOVLW	.3
;If we want the Led to Flash more times than 3, we don't make it flash more than
;1.5s, it's already long, we edit timer 1 to be 0.25 sec
	    
TURNON					
	    MOVWF	HalfSecCounter
	    BSF		PORTB, Buzzer		;check if you need to banksel
	    BSF		PORTB, Led
	    BSF		T1CON, TMR1ON		;check the banks
	    RETURN				;returns to main
	    
;;;;;;;;;;;;;;;;;;;;;;;;;GenBCD;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Converts counter's 8-bit value to 2 BCD digits, Units and Tens      ;
;Copied from Exp7's labSheet                                         ;
GenBCD
            BANKSEL	PORTA
            MOVF        Count,w			
            MOVWF       Temp			
gen_tens
            MOVLW     	.10			;sub 10,result keep in W
            SUBWF     	Temp,w
            BTFSS     	STATUS,C	        ;judge if the result bigger than 10
            GOTO      	gen_ones	        ;no,get the Entries bit result
            MOVWF     	Temp		        ;yes,result keep in TEMP
            INCF      	Tens,f			;ten bit add 1
            GOTO      	gen_tens		;turn  to continue get ten bit
gen_ones
            MOVF      	Temp,W
            MOVWF     	Units			;the value of Entries bit
            RETURN				;returns to ObjectFound
	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                   Interrupts code starts here                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ISR
	    MOVWF	ContextW		;Context Saving
	    SWAPF	STATUS, W
	    MOVWF	ContextS
	    BTFSC	INTCON, INTF		;External Interrupt has higher priority
	    CALL	Sensors
	    BTFSC	INTCON, TMR0IF		
	    CALL	DisplayUpdate
	    BTFSC	PIR1, TMR1IF
	    CALL	HalfSecDelay
	    BTFSC	PIR2, TMR2IF
	    CALL	LeftRightClear		
	    SWAPF	ContextS, W		;Context Retrieval
	    MOVWF	STATUS
	    MOVF	ContextW, W
	    RETFIE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Sensors;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Checks SenseLeft, Complement VarL/VarR
;Enable Timer2 module or update the state
Sensors
	    BCF		INTCON, INTF
	    BTFSC	PORTB, SenseLeft	;Check if SensorLeft is ON
	    GOTO	Left
	    
Right	    
	    MOVF State, W			;Complementing VarR without checking PORTB SenseRight
	    XORLW b'00010000'
	    ANDWF State, F
	    BTFSS	State, VarL	
	    GOTO	EnableTimer2		
	    BCF		State, LTR
	    BSF		State, Detected
	    RETURN				;return to ISR
	   
Left	    
	    MOVF State, W			;Complement VarL
	    XORLW b'00001000'
	    ANDWF State, F	
	    BTFSS	State, VarR
	    GOTO	EnableTimer2
	    BSF		State, LTR
	    BSF		State, Detected
	    RETURN				;return to ISR
	    	    
EnableTimer2
	    BSF		T2CON, TMR2ON		;check the banks
	    Return				;return to ISR
	    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DisplayUpdate;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Checks Display, Send Units/Tens, Complement Display
DisplayUpdate
	    BCF		INTCON, TMR0IF		;Clear TMR0 Flag
	    MOVLW	0X06			;Reinitialize TMR0
	    MOVWF	TMR0
	    
	    btfss	State, Display
	    GOTO	UnitsDisplay
	    MOVF	Tens, W
	    GOTO	Send	    
UnitsDisplay	    
	    MOVF	Units, W
Send	    
	    CALL	Table
	    MOVWF	PORTD
	    MOVF State, W			;Complement Display
	    XORLW b'00000001'
	    ANDWF State, F
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

	    BCF		PIR1, TMR1IF		;Check tha banks
	    BANKSEL	TMR1L				    
	    MOVLW	0x28
	    MOVWF	TMR1L
	    MOVLW	0X0B
	    MOVWF	TMR1H
	    DECFSZ	HalfSecCounter, F
	    GOTO	Zero
	    MOVF PORTB, W			;Complement Led
	    XORLW b'01000000'
	    ANDWF PORTB, F
	    RETURN
	    
Zero	    
	    MOVF PORTB,				;Complement Led and Buzzer
	    XORLW b'01100000'
	    ANDWF PORTB, F
	    BCF		T1CON, TMR1ON		;check the banks
	    RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LeftRightClear;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Just over 1ms Delay with 16 Software postscale, when done Clear VarL/VarR
;and Turn off timer2
LeftRightClear

	    BCF		PIR2, TMR2IF		;Clear TMR2 Flag

	    INCF	OverflowCount, F
	    MOVLW	.16			;Assuming a clock of 4MHz, we need 
	    SUBWF	OverflowCount, W	;245 * 16 * 16 * 16 = 1003520us
	    BTFSS	STATUS, Z
	    RETURN				

	    BCF		State, VarL
	    BCF		State, VarR
	    BCF		T2CON, TMR2ON		;check the banks
	    Return
	    
	    end