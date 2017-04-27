; RSA fingerprint
; Author: Jakub "MordiJC" Czapiga
; 
; RSA fingerprint is 80286 assembly project which I did for Assembly classes. 
; Its purpose is to generate RSA fingerprint whih is ASCII ART based on input 
; hash. 
;
; Parameters template:
; 
; <executable> <modification-flag> <hash>
;
; <executable>        - program executable name
; <modification-flag> - 0: normal behaviour. 1: modified behaviour.
; <hash>              - 32 characters long hash string containing only 0-9 and
;                       a-f characters
;
; Modified behaviour
; Instead of sliding on borders, modified version of program is bouncing.
; (Tip: To see effect use normal and modified behavioud on e.g. 0-filled hash)

.286

assume CS:code, SS:stack, DS:data

; DATA SEGMENT

EOLS equ 13, 10, '$'
CRLF equ 13, 10

BOARD_W equ 17
BOARD_H equ 9

data segment

	no_args_str			db "No arguments!", EOLS
	not_enough_args_str		db "Not enough arguments!", EOLS
	too_much_args_str		db "Too much arguments!", EOLS
	
	arg1_wrong_len_str		db "The first argument must be a single character!", EOLS
	arg1_wrong_char_str		db "The first argument must be 0 or 1!", EOLS

	arg2_wrong_len_str		db "The second argument must be 32 characters long!", EOLS
	arg2_wrong_char_str		db "The second argument can only contain characters 0-9 and a-f!", EOLS

	fp_chars			db " .o+=*BOX@%&#/^"

	border_top_bot			db "+-----------------+", EOLS
	border_side			db 00h, "|"

	eol				db EOLS

	help_string			db "Usage: ", CRLF
					db "<program> <MOD> <HASH>", CRLF, CRLF
					db "MOD  - modification flag.", CRLF
					db "   0 - normal behaviour.", CRLF
					db "   1 - bouncing behaviour.", CRLF
					db "HASH - hash in the form of 32-character string containing characters 0-9 and a-f.", CRLF
					db "Autor: Jakub Czapiga", EOLS

	; Modification flag
	mod_flag			dw 0

	; behavioud function pointer
	behaviour_ptr			dw 0

	; Buffer for hash converted to binary form
	hash_hex			db 16 dup(0h)

	; Bishop position. We are starting from center of board.
	bishop_pos_Y			dw 4 ; (9 - 1) // 2
	bishop_pos_X			dw 8 ; (17 - 1) // 2

	; Fingerprint board: 17 columns, 9 rows
	board				db 153 dup(0)

	; Input command buffer
	data_buf			db 128 dup('$'), EOLS

data ends

; MACROS

SETUP_STACK macro top_ptr
	mov ax, seg top_ptr
	mov ss, ax
	lea sp, top_ptr
endm

SETUP_DATA macro data_seg
	mov ax, seg data_seg
	mov ds, ax
endm



; CODE

code segment

; Program setup and configuration. [Entry point]
setup_:
	SETUP_STACK stk_top
	SETUP_DATA data
	jmp main

PAUSE:
	push ax

	xor ax, ax
	mov ah, 01h
	int 21h

	pop ax
	ret

; Main program function
main:

	call process_arguments

	call hash_to_ASCII_ART

	call print_board

	jmp exit

;------------------------------;
;           UTILITY            ;
;------------------------------;

; void exit()
exit:
	mov ah, 4ch ; Exit function
	mov al, 00h ; Error code
	int 21h

; void print(char*)
print:
	push bp
	mov bp, sp

	push ax
	push dx

	mov dx, [bp + 4] ; ds:dx = string
	mov ah, 9	; Dos print function
	int 21h

	pop dx
	pop ax
	pop bp

	ret

; void printn(char *, int<max: 127>)
printn:
	push bp
	mov bp, sp
	pusha

	xor ax, ax

	mov dx, [bp + 4]
	mov cx, [bp + 6]
	mov bx, 1 ; STDOUT
	mov ah, 40h
	int 21h

	popa
	pop bp
	ret

; void putchar(char)
putchar:
	push bp
	mov bp, sp
	push dx
	push ax

	xor dx, dx
	mov dl, [bp + 4]
	mov ah, 02h
	int 21h

	pop ax
	pop dx
	pop bp
	ret

; void memset(byte *, char, uint)
memset:
	push bp
	mov bp, sp
	pusha
	push es

	xor ax, ax
	mov al, [bp + 6]

	mov bx, ds
	mov es, bx

	xor cx, cx
	mov cl, [bp + 8]

	mov bx, [bp + 4]
	mov di, bx

	rep stosw

	pop es
	popa
	pop bp
	ret

; void memcopy(char *, int, char *)
; Accepting DS offsets only
memcopy:
	push bp
	mov bp, sp
	pusha
	push es

	xor cx, cx
	mov cx, [bp + 6] ; Set counter value

	mov si, [bp + 4] ; Source
	mov di, [bp + 8] ; Destination

	mov ax, ds
	mov es, ax

	rep movsb ; Repeat cx-times byte moving operation

	pop es
	popa
	pop bp
	ret

; <ax: 0/1> is_space(char)
is_space:
	push bp
	mov bp, sp

	xor ax, ax
	mov al, [bp + 4]
	
	cmp al, 9
	jle is_space_L2 ; Jump if < 10	

	cmp al, 11
	je is_space_L2 ; Jump if == 11

	cmp al, 12
	je is_space_L2 ; Jump if == 12

	cmp al, 13
	jle is_space_L3 ; Jump if <= 13

	cmp al, 32
	jg is_space_L3 ; Jump if > 32

	is_space_L2:
	mov ax, 1 ; Return true
	jmp is_space_L4

	is_space_L3:
	xor ax, ax ; Return false

	is_space_L4:
	pop bp
	ret

; <ax: 0/1> is_space_or_end(char)
is_space_or_end:
	push bp
	mov bp, sp
	push bx

	xor ax, ax
	xor bx, bx
	mov bl, byte ptr [bp + 4]

	; Check if it is space
	push bx
	call is_space
	add sp, 2
	
	test ax, ax
	jnz is_space_or_end_L2 ; Jump if true

	cmp bl, 10
	je is_space_or_end_L2 ; Jump if == \n

	cmp bl, 13
	je is_space_or_end_L2 ; Jump if == \r

	cmp bl, '$'
	je is_space_or_end_L2 ; Jump if == '$'

	xor ax, ax
	jmp is_space_or_end_L3

	is_space_or_end_L2:
	mov ax, 1

	is_space_or_end_L3:
	pop bx
	pop bp
	ret

; <ax: 0/1> is_num(char)
is_num:
	push bp
	mov bp, sp

	xor ax, ax
	mov al, [bp + 4]

	cmp al, ('0' - 1)
	jle is_num_L2 ; Jump if value is lover than '0'

	cmp al, '9'
	jg is_num_L2 ; Jump if value is greater than '9'

	mov ax, 1
	jmp is_num_L3

	is_num_L2:
	xor ax, ax

	is_num_L3:
	pop bp
	ret

; <ax: 0/1> is_af(char)
is_af:
	push bp
	mov bp, sp

	mov ax, [bp + 4]

	cmp ax, 96 ; 'a' - 1
	jle is_af_L2 ; Jump if character code is lover than 'a'

	cmp ax, 102 ; 'f'
	jg is_af_L2 ; Jump if character code is greater than 'f'
	
	mov ax, 1 ; Return true

	jmp is_af_L3

	is_af_L2:
	xor ax, ax ; Return false

	is_af_L3:
	pop bp
	ret

; <ax: 0/1> is_hash_char(char)
is_hash_char:
	push bp
	mov bp, sp
	push bx

	xor bx, bx
	mov bl, al

	; Check if it is number
	push bx
	call is_num
	add sp, 2

	test ax, ax
	jnz is_hash_char_L2 ; Jump if true

	; Check if it is a-f character
	push bx
	call is_af
	add sp, 2
	
	test ax, ax
	jz is_hash_char_L3 ; Jump if not true

	is_hash_char_L2:
	mov ax, 1 ; True
	jmp is_hash_char_L4

	is_hash_char_L3:
	xor ax, ax ; False

	is_hash_char_L4:
	pop bx
	pop bp
	ret

; <ax: 0/1> is_bool_char(char)
is_bool_char:
	push bp
	mov bp, sp

	mov al, [bp + 4]

	cmp al, '0'
	je is_bool_char_L2 ; Jump if == '0'

	cmp al, '1'
	jne is_bool_char_L3 ; Jump if != '1'

	is_bool_char_L2:
	mov ax, 1 ; Return true
	jmp is_bool_char_L4

	is_bool_char_L3:
	xor ax, ax ; Return false

	is_bool_char_L4:
	pop bp
	ret

; <ax: char*> omit_spaces(char *)
omit_spaces:
	push bp
	mov bp, sp
	push bx
	push di

	mov di, [bp+4]

	omit_spaces_L2:
	
	xor bx, bx
	mov bl, byte ptr [di] ; Get character

	; Check if it is space
	push bx
	call is_space
	add sp, 2

	test ax, ax
	je omit_spaces_L3 ; If not, then return

	inc di

	jmp omit_spaces_L2

	omit_spaces_L3:
	mov ax, di ; Return pointer to first non-space character (beginning of argument)

	pop di
	pop bx
	pop bp
	ret

; <ax: length, bx: start_ptr> length_of_next(char *)
length_of_next:
	push bp
	mov bp, sp
	push si
	push dx
	push cx

	xor cx, cx
	xor ax, ax
	mov si, [bp + 4]

	; Omit spaces and get pointer to the next argument
	push si
	call omit_spaces
	add sp, 2

	mov bx, ax ; Store value to return
	mov si, ax
	
	length_of_next_L2:

	xor dx, dx
	mov dl, byte ptr [si] ; Get next character

	; Check if it is argument-terminating character
	push dx
	call is_space_or_end
	add sp, 2

	test ax, ax
	jnz length_of_next_L3 ; If it is arg-term char, then return

	; Next character
	inc cx
	inc si

	jmp length_of_next_L2

	length_of_next_L3:
	mov ax, cx ; Return length (and argument-start in bx)

	pop cx
	pop dx
	pop si
	pop bp
	ret

; <ax: 0/1> is_hash_string(char *, int)
is_hash_string:
	push bp
	mov bp, sp
	push di
	push cx

	mov ax, [bp + 4]
	mov di, ax

	mov cx, [bp + 6]

	is_hash_string_L2:
	xor ax, ax
	mov al, byte ptr[di] ; Get character

	; Check if character is hash-type character
	push ax
	call is_hash_char
	add sp, 2
	
	test ax, ax
	jz is_hash_string_L4 ; If not then return false

	inc di

	loop is_hash_string_L2

	is_hash_string_L3:
	mov ax, 1
	jmp is_hash_string_L5
	
	is_hash_string_L4:
	xor ax, ax
	
	is_hash_string_L5:
	pop cx
	pop di
	pop bp
	ret

; <ax: num> hash_char_to_num(char)
hash_char_to_num:
	push bp
	mov bp, sp
	push bx

	mov bx, [bp + 4] ; Get character

	; Check if it's number
	push bx
	call is_num
	add sp, 2

	test ax, ax
	jnz hash_char_to_num_L3 ; If it is number, then jump to corresponding code

	; Check if it is a-f character
	push bx
	call is_af
	add sp, 2

	test ax, ax
	jnz hash_char_to_num_L4 ; If it is, then jump to corresponding code

	xor bx, bx
	jmp hash_char_to_num_L5 ; If it is not any of then then return 0

	hash_char_to_num_L3:
	sub bx, '0' ; Convert to binary
	jmp hash_char_to_num_L5

	hash_char_to_num_L4:
	sub bx, ('a'-10) ; Convert to binary a -> 10, b -> 11...

	hash_char_to_num_L5:
	mov ax, bx

	pop bx
	pop bp
	ret

; void hash_to_binary(char * source, uint len, char * dest)
; WARNING: Len accepted only if even
hash_to_binary:
	push bp
	mov bp, sp
	pusha

	mov ax, [bp + 8] ; Destination
	mov di, ax
	mov ax, [bp + 4] ; Source
	mov si, ax

	xor dx, dx

	; We need only half of iterations
	mov cx, [bp + 6]
	shr cx, 1

	hash_to_binary_L2:
	xor ax, ax
	xor bx, bx
	xor dx, dx
	
	; Get character from string
	mov dl, byte ptr [si]

	; Convert character to binary
	push dx
	call hash_char_to_num
	add sp, 2

	; Add value and move it to the left
	mov bl, al
	shl bx, 4

	inc si ; Increase source

	mov dl, byte ptr [si] ; Get next character

	; Convert it to binary
	push dx
	call hash_char_to_num
	add sp, 2

	add bl, al ; Add value to prevoius

	mov [di], bl ; Write to destination
	
	; Next character/byte
	inc si
	inc di

	loop hash_to_binary_L2

	popa
	pop bp
	ret

;------------------------------;
;    APPLICATION  FUNCTIONS    ;
;------------------------------;


; [ Errors ]

print_help_and_exit macro msg
	push offset msg
	call print
	add sp, 2
	push offset help_string
	call print
	add sp, 2
	jmp exit
endm

; void print_help_and_exit()
;print_help_and_exit:
;	push offset help_string
;	call print
;	add sp, 2
;	jmp exit

; void no_arguments()
no_arguments:
	print_help_and_exit no_args_str	

; void not_enough_arguments()
not_enough_arguments:
	print_help_and_exit not_enough_args_str

; void too_much_arguments()
too_much_arguments:
	print_help_and_exit too_much_args_str

; void first_argument_too_long()
first_argument_too_long:
	print_help_and_exit arg1_wrong_len_str

; void first_argument_wrong_type()
first_argument_wrong_type:
	print_help_and_exit arg1_wrong_char_str

; void second_argument_wrong_length()
second_argument_wrong_length:
	print_help_and_exit arg2_wrong_len_str

; void second_argument_wrong_char()
second_argument_wrong_char:
	print_help_and_exit arg2_wrong_char_str

; [ Parser ]

; void print_all_arguments()
print_all_arguments:
	push bp
	mov bp, sp
	pusha

	; Copy arguments to buffer
	call copy_arguments_to_buffer

	mov bx, offset data_buf
	
	print_all_arguments_L2:
	; Get next argument
	push bx
	call length_of_next
	add sp, 2

	; If no arguments left, then return
	test ax, ax
	jz print_all_arguments_L3

	; Print argument
	push ax
	push bx
	call printn
	add sp, 4

	; Increase pointer
	add bx, ax

	; New line
	push offset eol
	call print
	add sp, 2

	jmp print_all_arguments_L2 ; Loop

	print_all_arguments_L3:
	popa
	pop bp
	ret

; <ax: int> num_of_args(char *)
num_of_args:
	push bp
	mov bp, sp
	push bx
	push cx

	xor cx, cx
	mov bx, [bp + 4]
	
	num_of_args_L2:
	; Get next argument
	push bx
	call length_of_next
	add sp, 2

	; If no arguments left, then return
	test ax, ax
	jz num_of_args_L3

	; Increment number of arguments and move pointer
	inc cx
	add bx, ax

	jmp num_of_args_L2

	; Return number of arguments
	num_of_args_L3:
	mov ax, cx

	pop cx
	pop bx
	pop bp
	ret

; void process_arguments()
process_arguments:
	push bp
	mov bp, sp
	pusha

	; Check command line length
	call check_command_length
	test ax, ax
	je no_arguments

	; Copy command line to buffer for parsing purposes
	call copy_arguments_to_buffer

	; Get number of arguments
	push offset data_buf
	call num_of_args
	add sp, 2

	; If zero, then error
	test ax, ax
	jz no_arguments

	; If one, then error, bcs we need 2 
	cmp ax, 1
	je not_enough_arguments

	; If 3 or more, then error, because we need only 2 arguments
	cmp ax, 2
	jg too_much_arguments

	; First argument
	; Omit spaces, get argument starting pointer and its length
	push offset data_buf
	call length_of_next
	add sp, 2
	mov cx, ax

	; If argument is longer than 1 character, then error. We need 0 or 1
	cmp ax, 1
	jg first_argument_too_long

	; Get character
	mov di, bx
	xor dx, dx
	mov dl, byte ptr [di]

	; Check if it is 0 or 1
	push dx
	call is_bool_char
	add sp, 2

	; If not, then error.
	test ax, ax
	jz first_argument_wrong_type

	; Now we know that it's 0 or 1, so we can convert it to boolean value
	sub dl, '0'
	mov mod_flag, dx

	; Jump to character after argument
	add bx, cx

	; Second argument
	; Omit spaces, get argument starting pointer and its length
	push bx
	call length_of_next
	add sp, 2

	; Store length
	mov cx, ax

	; If argument is not 32 characters long, then error.
	cmp ax, 32
	jne second_argument_wrong_length

	; Check if it contains only 0-9 and a-f
	push ax
	push bx
	call is_hash_string
	add sp, 4

	; If not then error
	test ax, ax
	jz second_argument_wrong_char

	; Convert hash to binary form
	push offset hash_hex
	push cx
	push bx
	call hash_to_binary
	add sp, 6

	popa
	pop bp
	ret

; <ax: 0/1> check_arguments_length()
check_command_length:
	push bp
	mov bp, sp

	xor ax, ax
	
	; Get command length from PSP
	mov al, byte ptr es:[80h]

	; If length is above 127, then error
	cmp al, 127
	ja check_arguments_length_L2

	; If length is 0, then error
	test al, al
	jz check_arguments_length_L2

	; Length is good. True
	mov ax, 1
	jmp check_arguments_length_L3

	; Length is not good. False
	check_arguments_length_L2:
	xor ax, ax

	check_arguments_length_L3:
	pop bp
	ret

; void copy_arguments_to_buffer()
copy_arguments_to_buffer:
	push bp
	mov bp, sp
	pusha

	xor cx, cx

	; Get command length from PSP
	mov cl, byte ptr es:[80h]
	sub cl, 1

	; Swap ES<=>DS
	mov ax, ds
	mov bx, es
	mov es, ax
	mov ds, bx

	mov di, offset data_buf
	mov si, 82h

	; Copy command to data_buf
	rep movsb

	; Swap ES<=>DS
	mov ax, ds
	mov bx, es
	mov es, ax
	mov ds, bx

	popa
	pop bp
	ret

; [ ASCII ART GENERATOR ]

; <ax: int> get_direction(byte, int step<0-3>)
get_direction:
	push bp
	mov bp, sp
	push bx
	push cx

	xor ax, ax
	xor bx, bx

	mov al, byte ptr [bp+4]
	mov bl, byte ptr [bp+6]

	; step * 2
	shl bl, 1

	; Count of left shifts: 8 - 2 - step * 2 -> 6 - step*2
	mov cl, 6
	sub cl, bl

	shl al, cl ; Truncate left bits
	shr al, 6 ; Truncate right bits

	pop cx
	pop bx
	pop bp
	ret

; void move_bishop(byte direction)
move_bishop:
	push bp
	mov bp, sp
	push ax

	mov ax, word ptr [behaviour_ptr]

	push [bp+4]
	call ax
	add sp, 2

	pop ax
	pop bp
	ret 

; void normal_move(byte direction)
normal_move:
	push bp
	mov bp, sp
	push ax
	push cx
	push dx
	push di

	xor cx, cx
	mov cl, byte ptr [bp + 4]

	test cl, cl ; 00
	jz normal_move_L2

	cmp cl, 1 ; 01
	je normal_move_L3

	cmp cl, 2 ; 10
	je normal_move_L4

	cmp cl, 3 ; 11
	je normal_move_L5

	jmp normal_move_L6

	normal_move_L2:
	call normal_move_LU
	jmp normal_move_L6

	normal_move_L3:
	call normal_move_RU
	jmp normal_move_L6

	normal_move_L4:
	call normal_move_LD
	jmp normal_move_L6

	normal_move_L5:
	call normal_move_RD
	jmp normal_move_L6

	normal_move_L6:

	mov ax, [bishop_pos_Y]
	mov dx, 17
	mul dx
	add ax, [bishop_pos_X]
	add ax, offset board

	mov di, ax

	add byte ptr [di], 1

	pop di
	pop dx
	pop cx
	pop ax
	pop bp
	ret

; void normal_move_LU()
normal_move_LU:
	call normal_move_L
	call normal_move_U
	ret

; void normal_move_RU()
normal_move_RU:
	call normal_move_R
	call normal_move_U
	ret

; void normal_move_LD()
normal_move_LD:
	call normal_move_L
	call normal_move_D
	ret

; void normal_move_RD()
normal_move_RD:
	call normal_move_R
	call normal_move_D
	ret

; void normal_move_L()
normal_move_L:
	push ax

	mov ax, [bishop_pos_X]

	test ax, ax
	jz normal_move_L_L2 ; == 0

	dec [bishop_pos_X]

	normal_move_L_L2:

	pop ax
	ret

; void normal_move_R()
normal_move_R:
	push ax

	mov ax, [bishop_pos_X]

	cmp ax, 15
	ja normal_move_R_L2 ; >= 16

	inc [bishop_pos_X]

	normal_move_R_L2:

	pop ax
	ret

; void normal_move_U()
normal_move_U:
	push ax

	mov ax, [bishop_pos_Y]

	test ax, ax
	jz normal_move_U_L2 ; == 0

	dec [bishop_pos_Y]

	normal_move_U_L2:

	pop ax
	ret

; void normal_move_D()
normal_move_D:
	push ax

	mov ax, [bishop_pos_Y]

	cmp ax, 7
	ja normal_move_D_L2 ; >= 8

	inc [bishop_pos_Y]

	normal_move_D_L2:

	pop ax
	ret

; void modified_move(byte direction)
modified_move:
	push bp
	mov bp, sp
	push ax
	push cx
	push dx
	push di

	xor cx, cx
	mov cl, byte ptr [bp + 4]

	test cl, cl ; 00
	jz modified_move_L2

	cmp cl, 1 ; 01
	je modified_move_L3

	cmp cl, 2 ; 10
	je modified_move_L4

	cmp cl, 3 ; 11
	je modified_move_L5

	jmp modified_move_L6

	modified_move_L2:
	call modified_move_LU
	jmp modified_move_L6

	modified_move_L3:
	call modified_move_RU
	jmp modified_move_L6

	modified_move_L4:
	call modified_move_LD
	jmp modified_move_L6

	modified_move_L5:
	call modified_move_RD
	jmp modified_move_L6

	modified_move_L6:

	mov ax, [bishop_pos_Y]
	mov dx, 17
	mul dx
	add ax, [bishop_pos_X]
	add ax, offset board

	mov di, ax

	add byte ptr [di], 1

	pop di
	pop dx
	pop cx
	pop ax
	pop bp
	ret

; void modified_move_LU()
modified_move_LU:
	call modified_move_L
	call modified_move_U
	ret

; void modified_move_RU()
modified_move_RU:
	call modified_move_R
	call modified_move_U
	ret

; void modified_move_LD()
modified_move_LD:
	call modified_move_L
	call modified_move_D
	ret

; void modified_move_RD()
modified_move_RD:
	call modified_move_R
	call modified_move_D
	ret

; void modified_move_L()
modified_move_L:
	push ax

	mov ax, [bishop_pos_X]

	test ax, ax
	jz modified_move_L_L2 ; == 0

	dec [bishop_pos_X]
	jmp modified_move_L_L3

	modified_move_L_L2:
	inc [bishop_pos_X]

	modified_move_L_L3:
	pop ax
	ret

; void modified_move_R()
modified_move_R:
	push ax

	mov ax, [bishop_pos_X]

	cmp ax, 15
	ja modified_move_R_L2 ; >= 16

	inc [bishop_pos_X]
	jmp modified_move_R_L3

	modified_move_R_L2:
	dec [bishop_pos_X]

	modified_move_R_L3:
	pop ax
	ret

; void modified_move_U()
modified_move_U:
	push ax

	mov ax, [bishop_pos_Y]

	test ax, ax
	jz modified_move_U_L2 ; == 0

	dec [bishop_pos_Y]
	jmp modified_move_U_L3

	modified_move_U_L2:
	inc [bishop_pos_Y]

	modified_move_U_L3:
	pop ax
	ret

; void modified_move_D()
modified_move_D:
	push ax

	mov ax, [bishop_pos_Y]

	cmp ax, 7
	ja modified_move_D_L2 ; >= 8

	inc [bishop_pos_Y]
	jmp modified_move_D_L3

	modified_move_D_L2:
	dec [bishop_pos_Y]

	modified_move_D_L3:
	pop ax
	ret

; <al: char> board_field_to_ASCII(byte field)
board_field_to_ASCII:
	push bp
	mov bp, sp
	push bx
	push di

	xor ax, ax
	xor bx, bx

	mov bl, byte ptr [bp + 4]

	cmp bl, 0FFh ; E
	je board_field_to_ASCII_L2

	cmp bl, 0FEh ; S
	je board_field_to_ASCII_L3

	cmp bl, 13 ; 0-13
	jle board_field_to_ASCII_L4

	mov bx, offset fp_chars
	mov al, byte ptr [bx + 14] ; 14+ visits = ^

	jmp board_field_to_ASCII_L5

	board_field_to_ASCII_L2:
	mov al, 'E'
	jmp board_field_to_ASCII_L5

	board_field_to_ASCII_L3:
	mov al, 'S'
	jmp board_field_to_ASCII_L5
	
	board_field_to_ASCII_L4:
	xor ax, ax
	mov di, offset fp_chars
	add di, bx
	mov al, byte ptr [di] ; Store corresponding ASCII code

	board_field_to_ASCII_L5:
	pop di
	pop bx
	pop bp
	ret

; void board_to_ASCII()
board_to_ASCII:
	pusha

	mov cx, 153 ; 153 iterations

	mov [board+76], 0FEh ; S

	mov ax, [bishop_pos_Y]
	mov dx, 17
	mul dx
	add ax, [bishop_pos_X]
	add ax, offset board

	mov di, ax

	mov byte ptr [di], 0FFh ; E

	mov di, offset (board + 152)

	board_to_ASCII_L2:

	xor ax, ax
	mov al, byte ptr [di] ; Get byte

	push ax
	call board_field_to_ASCII ; Convert visits count to ASCII code
	add sp, 2

	mov byte ptr[di], al ; Store ascii code

	dec di ; Next (previous, because we are going from 152 to 0)

	loop board_to_ASCII_L2

	popa
	ret

; void print_board()
print_board:
	push ax
	push cx

	mov cx, 9
	mov ax, offset board

	push offset border_top_bot
	call print
	add sp, 2

	print_board_L2:

	push '|'
	call putchar
	add sp, 2

	push 17
	push ax
	call printn
	add sp, 4

	add ax, 17

	push '|'
	call putchar
	add sp, 2

	push offset eol
	call print
	add sp, 2

	loop print_board_L2

	push offset border_top_bot
	call print
	add sp, 2

	pop cx
	pop ax
	ret

; void hash_to_ASCII_ART()
; Hash in `hash_hex`
; ASCII-ART output in `board`
hash_to_ASCII_ART:
	push bp
	mov bp, sp
	pusha

	call set_behaviour_ptr

	mov di, offset hash_hex
	mov cx, 16
	
	hash_to_ASCII_ART_L2: ; Loop 1
	mov dx, cx ; Store cx

	mov cx, 4 ; 4 pairs of bits

	hash_to_ASCII_ART_L3: ; Loop 2
	xor ax, ax
	mov al, byte ptr [di] ; Get next byte

	mov bx, 4
	sub bx, cx ; Calculate step

	push bx
	push ax
	call get_direction ; Get direction bit pair
	add sp, 4

	push ax
	call [behaviour_ptr] ; Execute move
	add sp, 2

	loop hash_to_ASCII_ART_L3 ; Loop 2

	inc di ; Next byte

	mov cx, dx ; Restore cx
	loop hash_to_ASCII_ART_L2 ; Loop 1

	call board_to_ASCII ; Convert visit board to ASCII codes board

	popa
	pop bp
	ret

; void set_behaviour_ptr()
set_behaviour_ptr:
	push ax

	mov ax, word ptr [mod_flag]

	test ax, ax
	jz set_behaviour_ptr_L2

	lea ax, modified_move
	mov behaviour_ptr, ax
	jmp set_behaviour_ptr_L3

	set_behaviour_ptr_L2:
	lea ax, normal_move
	mov behaviour_ptr, ax
	
	set_behaviour_ptr_L3:
	pop ax
	ret

code ends

stack segment STACK

        dw 255 dup(0) ; 510 NULL bytes
stk_top dw 0          ; Now 512. stack start point

stack ends

end setup_
