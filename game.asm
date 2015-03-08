; Game of life (Possibly)
; Ref: http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
; Rules are simple:
; Any live cell with fewer than two live neighbours dies
; Any live cell with two or three live neighbours lives on
; Any live cell with more than three live neighbours dies
; Any dead cell with exactly three live neighbours becomes a live cell
.MODEL compact
.STACK 256

;-- Includes


;-- EQU and = equates
LF equ 0Dh;
CR equ 0Ah;
DEF_PX_COLOUR equ 02d;

;-- Error codes

;-- Variable declarations
.DATA

; Initialize the videoBuffer that we will use to compute the next state of the
; game of life before copying it to the video buffer.
videoBuffer db 64000 dup(0);

; Variables for putting pixels on to the screen using the putPixelArrayToScreen
; routine after loading the arrays with a putPattern routine.
xPixelArray dw 64 dup(0);
yPixelArray dw 64 dup(0);
pixelArrayCounter dw 0;

; Variables for use as input for putPattern routines.
x0Value dw 0;
y0Value dw 0;

;-- Extern variables

;-- Extern procedures

;-- Program macros
.CODE

;-- Program subroutines

;;;;;;
; For each pixel that exist in the current video buffer located at
; ES:((y*320)+x) we want to investigate it's neighbours and evaluate if the
; cell should come alive or stay dead.
; The cell in the buffer is updated to it's new state and will wait until
; all cells have been evaluated before the buffer is copied to the video
; buffer.
; For ease in order to handle border cases we will count from (x,y) at 1,1
; to 318,198. (Screen is 320,200 but since the counting is sensible we start
; at 0 and end at 319/199 - want to exclude outermost pixels so hence the
; 1,1 to 318,198 evaluated pixels)
;;;;;;
computeNextGeneration:
    ; Save the stack
    push AX;
    push BX;
    push CX;
    push DX;
    push DS;
    push DI;

    ; Initiate counters in CX, DX and DI
    ; CX will keep track of the current y position
    ; DX will keep track of the current x position
    ; DI will keep track of the linear offset from where videoBuffer is stored
    ; and the corresponding offset in the current video buffer in ES

    mov CX, 0d; y0 = 0
    mov DI, 0d; 320*y + x

    mov AX, offset videoBuffer; Load base address of video buffer into AX
    mov DX, DS;
    add AX, DX;
    mov DS, AX; Store the base address of videoBuffer in DS

    ; This loop will be used to increment the Y position
    outerCyclePx:
        mov DX, 0d; x0 = 0
        inc CX; y = y + 1
        cmp CX, 0199d; is y at max?
        jne innerCyclePx; if not at max, continue iteration
        jmp endComputeNextGeneration

        ; This loop will be used to increment the X position and perform the
        ; required computations
        innerCyclePx:
            inc DX; x = x + 1
            cmp DX, 0319d; at end of x range?
            je outerCyclePx; if so go to outer loop


            mov BX, CX; BX = y
            dec BX; BX = y-1
            mov AX, 0320d;

            push DX; Store DX as mul will overwrite it

            mul BX; AX = 320 * (y-1)

            pop DX; Retreive DX after mul

            mov BX, DX; BX = x
            dec BX; BX = x-1
            add AX, BX; 320 * (y-1) + (x-1)
            mov DI, AX; --=--

            mov AX, 0d; used to store number of live pixels around x,y

            y_min_one_x_min_one:
                cmp byte ptr ES:[DI], 0d; current screen memory == 1 at x-1,y-1?
                jz y_min_one_x;
                    inc AX; if not zero increment AX

            y_min_one_x:
                inc DI; 320(y-1) + (x)

                cmp byte ptr ES:[DI], 0d; current screen memory == 1 at x,y-1?
                jz y_min_one_x_plus_one;
                    inc AX; if not zero increment AX

            y_min_one_x_plus_one:
                inc DI; 320(y-1) + (x+1)

                cmp byte ptr ES:[DI], 0d; current screen memory == 1 at x+1,y-1?
                jz y_x_plus_one
                    inc AX; if not zero increment AX

            y_x_plus_one:
                add DI, 0320d; 320(y) + (x+1)

                cmp byte ptr ES:[DI], 0d; current screen memory == 1 at x+1,y?
                jz y_x_min_one
                    inc AX; if not zero increment AX

            y_x_min_one:
                dec DI; 320(y) + (x)
                dec DI; 320(y) + (x-1)

                cmp byte ptr ES:[DI], 0d;
                jz y_plus_one_x_min_one
                    inc AX; if not zero increment AX

            y_plus_one_x_min_one:
                add DI, 0320d; 320(y+1) + (x-1)

                cmp byte ptr ES:[DI], 0d;
                jz y_plus_one_x
                    inc AX; if not zero increment AX

            y_plus_one_x:
                inc DI; 320(y+1) + (x)

                cmp byte ptr ES:[DI], 0d;
                jz y_plus_one_x_plus_one
                    inc AX; if not zero increment AX

            y_plus_one_x_plus_one:
                inc DI; 320(y+1) + (x+1)

                cmp byte ptr ES:[DI], 0d;
                jz evaluatePixel
                    inc AX; if not zero increment AX

            evaluatePixel:
                sub DI, 0320d; 320(y) + (x+1)
                dec DI; 320(y) + (x)

                ; Remember the rules:
                ; 1 - Live cell with fewer than two live nighbours -> dies
                ; 2 - Live cell with two or three live neighbours -> lives
                ; 3 - Live cell with more than three live neighbours -> dies
                ; 4 - Dead cell with exactly three live neighbours -> lives

                cmp byte ptr ES:[DI], 0d; Check if current cell is alive
                jz evaluateDead; If 0 evaluate pixel as dead

                ; If not dead evaluate as living.
                evaluateLiving:
                    cmp AX, 02d; Rule 1
                    jl killPx; jump if less than 2

                    cmp AX, 03d; Rule 2
                    jle resurrectPx; jump if less or equal to 3
                    jmp killPx; if above two conditions are not met the cell dies

                evaluateDead:
                    cmp AX, 03d; Rule 4
                    je resurrectPx;
                    jmp innerCyclePx;

                resurrectPx:
                    mov byte ptr DS:[DI], DEF_PX_COLOUR;
                    jmp innerCyclePx;

                killPx:
                    mov byte ptr DS:[DI], 0d;
                    jmp innerCyclePx;

endComputeNextGeneration:
    call copyBufferToScreen;

    ; Retrieve the stack
    pop DI;
    pop DS;
    pop DX;
    pop CX;
    pop BX;
    pop AX;

ret;

;;;;;;
; Use the rep MOVSB instruction to copy the contents of the videoBuffer into the
; video memory.
; Before this routine is called ES shall contain the address of the video memory
; and DS shall contain the address of the videoBuffer array.
;;;;;;
copyBufferToScreen:

    push CX;
    push DI;
    push SI;

    ; ES already stores the address for the screen buffer, so we can use this
    ; here without modification.
    ; The address of videoBuffer is already stored in DS, so we can use this for
    ; the string move operation.

    mov DI, 0d;
    mov SI, 0d;

    mov CX, 64000d; Set the number of operations we want to perform
    cld; Clear the direction flag for string operations using rep movsb

    ; Call ES:[DI] = DS:[SI] CX number of times using movsb, copying the data
    ; in the videoBuffer to the video memory.
    rep MOVSB;

    pop SI;
    pop DI;
    pop CX;

ret;


;;;;;;
; For each x,y pair contained in yPixelArray[] and xPixelArray[] we want to bring
; the cell at the locations alive.
; The arrays will be filled by other routines with the number of pairs being
; contained in pixelArrayCounter.
; The routine shall bring cells to life from BX = 0 and run until the breakpoint
; at BX = pixelArrayCounter is reached.
;;;;;;
putPixelArrayToScreen:
    push AX;
    push BX;
    push DX;

    mov BX, 0;

    putPixelLoop:
        cmp BX, pixelArrayCounter; Check if are done
        jg putPixelExit

        mov AX, 0320d;
        mov DX, yPixelArray[BX];
        mul DX; AX = 320*y
        mov DX, xPixelArray[BX];
        add AX, DX; AX = 320*y + x

        push BX; Save counter

        mov BX, AX; BX = 320*y + x
        mov byte ptr ES:[BX], DEF_PX_COLOUR; Set pixel at (x,y)

        pop BX; Restore counter

        inc BX; Increment counter
        inc BX;
        jmp putPixelLoop

putPixelExit:
    pop DX;
    pop BX;
    pop AX;

ret;


;;;;;;
; Pattern:
;    x0
; y0 - X - - - - -
;    - - - X - - -
;    X X - - X X X
; Inputs:
;   x0Value
;   y0Value
; Outputs:
;   yPixelArray[]
;   xPixelArray[]
;   pixelArrayCounter
;;;;;;
putAcornPattern:
    push BX;
    push CX;
    push DX;

    mov BX, 0d; This is our counter for the PixelArrays

    mov CX, x0Value;
    mov DX, y0Value;

    ; First pixel at (x0+1, y0)
    mov xPixelArray[BX], CX;
    mov yPixelArray[BX], DX;
    add xPixelArray[BX], 01d;

    ; Second pixel at (x0+3, y0+1)
    inc BX;
    inc BX;
    mov xPixelArray[BX], CX;
    mov yPixelArray[BX], DX;
    add xPixelArray[BX], 03d;
    add yPixelArray[BX], 01d;

    ; Third pixel at (x0, y0+2)
    inc BX;
    inc BX;
    add DX, 02d; DX = y0 + 2, Will use this y-value for all remaining pixels
    mov xPixelArray[BX], CX;
    mov yPixelArray[BX], DX;

    ; Fourth pixel at (x0+1, y0+2)
    inc BX;
    inc BX;
    inc CX; CX = x0+1;
    mov xPixelArray[BX], CX;
    mov yPixelArray[BX], DX;

    ; Fifth pixel at (x0+4, y0+2)
    inc BX;
    inc BX;
    inc CX; CX = x0+2
    inc CX; CX = x0+3
    inc CX; CX = x0+4
    mov xPixelArray[BX], CX;
    mov yPixelArray[BX], DX;

    ; Sixth pixel at (x0+5, y0+2)
    inc BX;
    inc BX;
    inc CX; CX = x0+5
    mov xPixelArray[BX], CX;
    mov yPixelArray[BX], DX;

    ; Seventh pixel at (x0+6, y0+2)
    inc BX;
    inc BX;
    inc CX; CX = x0+6
    mov xPixelArray[BX], CX;
    mov yPixelArray[BX], DX;

    mov pixelArrayCounter, BX; Store the counter

    pop DX;
    pop CX;
    pop BX;

ret;


;-- Program code
START:

initialization:

    ; Clear all general purpose registers
    xor AX, AX;
    xor BX, BX;
    xor CX, CX;
    xor DX, DX;

    ; Initialize the data segment
    mov AX, @DATA;
    mov DS, AX;

    ; Initialize ES to point to the graphics memory
    mov AX, 0A000h;
    mov ES, AX;

    ; Set keyboard to no delay
    ; ref: http://www.ctyme.com/intr/rb-1757.htm
    mov AX, 0305h;
    xor BX, BX;
    int 016h;


enterVgaMode:

    mov AX, 013h;
    int 010h;


setupLoop:

    ;;;;;;
    ; First we want to clear the video memory and the videoBuffer - that is
    ; setting all values to zero to ensure proper operation.
    ;;;;;;

    mov BX, 0d; Counter = 0

    clearBuffer:
        mov byte ptr videoBuffer[BX], 0d;
        mov byte ptr ES:[BX], 0d; Clear ES+BX
        inc BX; Counter = Counter + 1
        cmp BX, 064001d;
        jne clearBuffer

    setInitialScreen:

    ; Want to put an acorn at the middle of the screen
    ; It is 7 x 3 pixels big so we want
    ; x0 = ((320/2) - 7/2) - 1 = 156.5 - 1 ~= 156
    ; y0 = ((200/2) - 3/2) - 1 = 98
    mov x0Value, 0156d;
    mov y0Value, 098d;

    call putAcornPattern

    call putPixelArrayToScreen

    mov ah,00
    int 16h

mainLoop:

call computeNextGeneration;

    mov ah,00
    int 16h

jmp mainLoop;


EXIT:

exitVgaMode:
    mov AX, 03h;
    int 10h;

    mov ax, 4c00h
    int 21h;

END START
