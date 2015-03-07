; Game of life (Possibly)
; Ref: http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
; Rules are simple:
; Any live cell with fewer than two live neighbours dies, as if caused by
; under-population.
; Any live cell with two or three live neighbours lives on to the next
; generation.
; Any live cell with more than three live neighbours dies, as if by overcrowding.
; Any dead cell with exactly three live neighbours becomes a live cell, as if by
; reproduction.
.MODEL compact
.STACK 256

;-- Includes


;-- EQU and = equates
LF equ 0Dh
CR equ 0Ah
DEF_PX_COLOUR equ 02d;

;-- Error codes

;-- Variable declarations
.DATA

; Initialize the videoBuffer that we will use to compute the next state of the
; game of life before copying it to the video buffer.
videoBuffer db 64000 dup(0);

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
                ; Live cell with fewer than two live nighbours -> dies
                ; Live cell with two or three live neighbours -> lives
                ; Live cell with more than three live neighbours -> dies
                ; Dead cell with exactly three live neighbours -> lives

                cmp byte ptr ES:[DI], 0d; Check if current cell is alive
                jz evaluateDead; If 0 evaluate pixel as dead

                ; If not dead evaluate as living.
                evaluateLiving:
                    cmp AX, 02d;
                    jl killPx; jump if less than 2

                    cmp AX, 03d;
                    jle resurrectPx; jump if less or equal to 3
                    jmp killPx; if above two conditions are not met the cell dies

                evaluateDead:
                    cmp AX, 03d;
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
;
;;;;;;

copyBufferToScreen:

    push AX;
    push DX;
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
    pop DX;
    pop AX;

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

        ; Setup linear gun!
        mov byte ptr ES:[100d*320d+150d+0d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+1d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+2d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+3d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+4d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+5d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+6d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+7d], DEF_PX_COLOUR;

        mov byte ptr ES:[100d*320d+150d+9d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+010d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+011d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+012d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+013d], DEF_PX_COLOUR;

        mov byte ptr ES:[100d*320d+150d+17d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+18d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+19d], DEF_PX_COLOUR;

        mov byte ptr ES:[100d*320d+150d+26d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+27d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+28d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+29d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+30d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+31d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+32d], DEF_PX_COLOUR;

        mov byte ptr ES:[100d*320d+150d+34d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+35d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+36d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+37d], DEF_PX_COLOUR;
        mov byte ptr ES:[100d*320d+150d+38d], DEF_PX_COLOUR;

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
