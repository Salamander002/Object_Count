;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               Object counting system assembly code                 ;
;                     Using PIC 16F877A                              ;
;         Version 0    by Mohammad Salameh on May 10th 2023          ;
;         Version 1    by Laith Hamdan     on May 15th 2023          ;
;	  Version 1.1  by Laith Hamdan     on May 16th 2023	     ;
;         Version 2    by Mohammad Salameh on May 20th 2023          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Version 0 : built the basic structure for the code, not a final or  ;
;            testing version.                                        ;
;Version 1 : Testing version.					     ;
;Version 1.1 : Semi-Final version.				     ;
;version 2 : added better descriptive comments and cleaned up the    ;
;            file, changed locations of state bits, removed code     ;
;            for unimplemented Pause button as it's implemented in   ;
;            Hardware. Renamed OverflowCount to TMR2PostScale        ;
;            Renamed HalfSecCounter to FlashCount                    ;
;            Renamed LeftRightClear to FalseSense                    ;
;            Renamed HalfSecDelay to Flash                           ;
;            Renamed DisplayUpdate to Display                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TODO: optimize the code                                             ;
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
;                                                                    ;
;                          Internal modules                          ;
;       Timer0 : Asynchronous Update for the display every 1ms       ;
;       Timer1 : Buzzer and LED control                              ;
;       Timer2 : Clear State if object did not trigger both sensors  ;
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
	    include	"p16f877A.inc"
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;			Configuration word			     ;
	    ; CONFIG
	    ; __config 0x3F39
	    __CONFIG	_FOSC_XT & _WDTE_OFF & _PWRTE_OFF & _BOREN_OFF & _LVP_OFF & _CPD_OFF & _WRT_OFF & _CP_OFF
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
            WTemp
            StatusTemp
            FlashCount
            TMR2PostScale
            endc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          Declaring pin/bits names                  ;
;pins are named to increase code readability and make changes easier ;
;PORTB pins
SenseRight  equ         1			;Pin 1 of PortB == RB1
SenseLeft   equ         2			;Pin 2 of PortB == RB2
UnitsSelect equ         3			;Pin 3 of PortB == RB3
TensSelect  equ         4			;Pin 4 of PortB == RB4
Buzzer      equ         5			;Pin 5 of PortB == RB5
Led         equ         6			;Pin 6 of PortB == RB6

;State Register pins
Detected    equ         0			;bit 0 of State
LTR         equ         1			;bit 1 of State
VarL        equ         2			;bit 2 of State
VarR        equ         3			;bit 3 of State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;			            Macro Assignments		     ;
;Copied from Exp8's labSheet                                         ;
;Used to save WReg and STATUS Register into other temporary registers;
PUSH	    macro
	    movwf	WTemp			;ContextW must be reserved in all banks
	    swapf	STATUS, W		;store in W without affecting status bits
	    movwf	StatusTemp		;save STATUS
	    endm

;Used to load WReg and STATUS Register into other temporary registers;
POP	    macro
	    swapf	StatusTemp, W		;unswap STATUS nibbles into W
	    movwf	STATUS			;restore STATUS (which points to where W was stored)
	    swapf	WTemp, F		;unswap W nibbles
	    swapf	WTemp, W		;restore W without affecting STATUS
	    endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         Start of Program                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start of executable code                                           ;
	    org         0x00
	    GOTO        Main
	    org         0x04
	    GOTO        ISR

;;;;;;;;;;;;;;;;;;;;;;;;;;Initial;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;INTCON Register : Initializing PORTD as output.                     ;
;Enable Global Interrupts, Peripheral Interrupt, TMR0 Overflow       ;
;interrupt, External Interrupt.                                      ;
;Disable Port Change Interrupt.                                      ;
;Clear interrupt flags to get rid of any unwanted behavior           ;
Initial
	    BANKSEL	TRISD			;Bank1 selected
	    CLRF	TRISD			;Output
	    MOVLW	b'10000111'
	    MOVWF	TRISB
	    MOVLW	b'11110000'
	    MOVWF	INTCON

;Prepare timer0 to interrupt after 1 millisecond		     ;
;OPTION_REG REGISTER: Interrupt on rising edge of RB0, 1:4 prescaler ;
;                     internal clock for TMR0                        ;
	    MOVLW	b'11000001'
	    MOVWF	OPTION_REG


;Prepare timer2 to interrupt after 1.00352 Seconds                   ;
;1:16 prescale, 1:16 postscale, TMR2ON is cleared.                   ;
;Software postscaling by 16                                          ;
	    BSF		PIE1, TMR2IE
	    BSF		PIE1, TMR1IE
	    MOVLW	.244			;244 + 1 = 245
	    MOVWF	PR2

	    BANKSEL	T2CON			;Bank0 selected
	    MOVLW	b'01111010'
	    MOVWF	T2CON

;Prepare timer1 to interrupt after .5 Seconds                        ;
;1:8 prescale, TMR1ON is cleared.                                    ;
;timer1 0.5s, 65, 536 - 62, 500 = 2856, 0x0B28                       ;
	    MOVLW	0X06			;256 - 6 = 250
	    MOVWF	TMR0
	    MOVLW	b'00110000'
	    MOVWF	T1CON

	    MOVLW	0x28
	    MOVWF	TMR1L
	    MOVLW	0X0B
	    MOVWF	TMR1H
	    
	    CLRF	PORTB

;Turn on any display because selecting displays works by complement  ;
	    BSF		PORTB, UnitsSelect

;Initialize CMin and CMax to 5 and 10, CMax has one added so that the;
;carry flag works correctly after subtraction                        ;
	    MOVLW	.5
	    MOVWF	CMin
	    MOVLW	.11
	    MOVWF	CMax

	    BCF		PIR1, TMR1IF
	    BCF		PIR1, TMR2IF		;All Flags cleared
	    CLRF	TMR2
	    CLRF	Count
	    CLRF	State
	    CLRF	TMR2PostScale
	    RETURN				;Returns to Main
;;;;;;;;;;;;;;;;;;;;;;;;;;Main;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Main
            CALL    	Initial
MainLoop
            BTFSC       State, Detected		;skip if no object detected
            CALL        ObjectFound
            GOTO        MainLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ObjectFound;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this subroutine determines whether to Decrement or Increment the count
;Checks if Under CMin or Over CMax
;Turn on buzzer, LED and timer modules.
ObjectFound
	    BCF		State, Detected
	    BTFSC	State, LTR		;skip if Right to left
	    GOTO	Decrement
;Object moved from Right to left, count increments, count capped at 100
Increment
	    INCF	Count, F
	    MOVLW	.100
	    SUBWF	Count, W
	    BTFSC	STATUS, C
	    GOTO	Decrement
	    GOTO	BCD

;Object moved from left to right, count decrements
Decrement
	    DECF	Count, F
	    BTFSC	Count, .7		;Check if the MSB is set
	    GOTO	Increment

;Generate decimal digits from binary value of Count
BCD
	    CALL	GenBCD

TestAboveRange
	    MOVF	CMax, W
	    SUBWF	Count, W
	    BTFSC	STATUS, C
	    GOTO	OutRange

TestUnderRange
	    MOVF	CMin, W
	    SUBWF	Count, W
	    BTFSS	STATUS, C
	    GOTO	OutRange


;FlashCount Register                                                  ;
;The number put in WReg here is the number of flashes done            ;
;the number must be an odd number so that it will always turn off     ;
;it's decremented after every change of on/off state                  ;
;turns off the timer1 module when it reaches 0                        ;
InRange
	    MOVLW	.1
	    GOTO	TURNON
OutRange
	    MOVLW	.3
TURNON
	    MOVWF	FlashCount
	    BSF		PORTB, Buzzer
	    BSF		PORTB, Led
	    BSF		T1CON, TMR1ON
	    RETURN				;Returns to Main

;;;;;;;;;;;;;;;;;;;;;;;;;GenBCD;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Converts counter's 8-bit value to 2 BCD digits, Units and Tens      ;
;Copied from Exp7's labSheet                                         ;
;Divides Count by ten to find Tens                                   ;
;Remainder is Units                                                  ;
GenBCD
	    BANKSEL	PORTA
	    MOVF	Count, W
	    MOVWF	Temp
	    CLRF	Tens			;Must Clear the Tens.

gen_tens
	    MOVLW	.10			;sub 10, result keep in W
	    SUBWF	Temp, W
	    BTFSS	STATUS, C		;judge if the result bigger than 10
	    GOTO	gen_ones		;no, get the Entries bit result
	    MOVWF	Temp			;yes, result keep in TEMP
	    INCF	Tens, F			;ten bit add 1
	    GOTO	gen_tens		;turn  to continue get ten bit
gen_ones
	    MOVF	Temp, W
	    MOVWF	Units			;the value of Entries bit
	    RETURN				;returns to ObjectFound

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                   Interrupts code starts here                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ISR
	    PUSH
	    BTFSC	INTCON, INTF
	    CALL	Sensors			;any Sensor detected something
	    BTFSC	INTCON, TMR0IF
	    CALL	Display			;Updating the display value
	    BTFSC	PIR1, TMR1IF
	    CALL	Flash			;LED and Buzzer control
	    BTFSC	PIR1, TMR2IF
	    CALL	FalseSense		;Clean State register if object did not fully go through
	    POP
	    RETFIE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Sensors;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Checks SenseLeft, Complement VarL/VarR
;Enable Timer2 module or update the state
;uses Complements to account for objects that go between the sensors and returns from where it came from
Sensors
	    BCF		INTCON, INTF
	    BTFSC	PORTB, SenseLeft	;skip if Left Sensor did not detect any thing
	    GOTO	Left

Right
	    BSF		State, VarR		;no need to check SenseRight
	    BTFSS	State, VarL		;skip if an object has already gone through the other sensor
	    GOTO	EnableTimer2		;clear VarR after 1 second
	    BSF		State, LTR		;Object has moved from Left to right
	    GOTO	DisableTimer2

Left
	    BSF		State, VarL		;Set VarL
	    BTFSS	State, VarR		;skip if an object has already gone through the other sensor
	    GOTO	EnableTimer2		;clear VarL after 1 second
	    BCF		State, LTR		;object has moved from Right to left
	    GOTO	DisableTimer2

EnableTimer2
	    BSF		T2CON, TMR2ON
	    RETURN				;Returns to ISR

DisableTimer2
	    BCF		T2CON, TMR2ON
	    CLRF	TMR2
	    CLRF	TMR2PostScale

	    BSF		State, Detected		;Object detected flag so that Main can process the change
	    BCF		State, VarL
	    BCF		State, VarR
	    RETURN				;Returns to ISR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Display;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this subroutine gets executed every 1 mS
;Checks Display, Send Units or Tens, Complements UnitsSelect and TensSelect
;Works by complement so that every time this subroutine is executed
;either units or tens would be displayed since the data bus is shared
Display
	    BCF		INTCON, TMR0IF		;Clear TMR0 Flag
	    MOVLW	0X06			;Reinitialize TMR0
	    MOVWF	TMR0
	    MOVLW	b'00011000'		;Complement Display
	    XORWF	PORTB, F

	    BTFSC	PORTB, UnitsSelect	;skip if TensDisplay selected
	    GOTO	UnitsDisplay
	    MOVF	Tens, W
	    GOTO	Send
UnitsDisplay
	    MOVF	Units, W
Send
	    CALL	Table
	    MOVWF	PORTD
	    RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Flash;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this subroutine gets executed every .5 Seconds
;Decrements FlashCount, Complement Led and Buzzer If zero,
;Complement Led Only if not, and Turn off timer1
Flash
	    BCF		PIR1, TMR1IF
	    BANKSEL	TMR1L			;same bank as TMR1H and PORTB
	    MOVLW	0x28			;Reinitialize TMR1
	    MOVWF	TMR1L
	    MOVLW	0X0B
	    MOVWF	TMR1H

	    DECFSZ	FlashCount, F		;skips if LED changed state FlashCount times
	    GOTO	Continue

Zero
	    MOVLW	b'01100000'		;Turn off the LED and buzzer
	    XORWF	PORTB, F
	    BCF		T1CON, TMR1ON		;Turn off Timer1 module
	    RETURN

Continue
	    MOVLW	b'01000000'		;Complement Led
	    XORWF	PORTB, F
	    RETURN				;Returns to ISR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FalseSense;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This subroutine is to stop objects that did not go through both sensors;
;from contaminating the count                                           ;
;1.003520 S Delay with 16 Software postscale, when done Clear VarL/VarR
;and Turn off timer2
;245 * 16 * 16 * 16 = 1003520us
FalseSense
	    BCF		PIR1, TMR2IF		;Clear TMR2 Flag
	    INCF	TMR2PostScale, F	;Software postscaling
	    MOVLW	.16
	    SUBWF	TMR2PostScale, W
	    BTFSS	STATUS, Z		;skip if 1 second has passed
	    RETURN

	    CLRF	TMR2PostScale		;clear Software postscaling
	    BCF		State, VarL
	    BCF		State, VarR
	    BCF		T2CON, TMR2ON		;turn off Timer2 until another sensors update
	    RETURN

;************************** Lookup Table ************************
;7-segments display code for numbers
Table
	    ADDWF	PCL, 1
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

	    END
