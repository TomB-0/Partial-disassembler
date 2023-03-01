;Apdoroti SUB, AAS, DAS, PUSH, INC, SHR, ROR, AAA, DAA


.model small
.stack 100h

.data

msg_Output_Create db "Nepavyko sukurti irasymo failo", '$'
msg_Input_Open db "Nepavyko nuskaityti skaitymo failo", '$'
msg_Reset_Read db "Nepavyko nuskaityti  irasymo failo", '$'
msg_Help db "pirma ivestis failo .com pavadinimas, antraa .txt arba .asm pavadinimas", '$'
msg_Writing_Error db "Nepavyko rasyti i file", '$'
msg_Unknown db "Neatpazintas byte ", 0
msg_No_Input db "Nera ivesties ", '$'
;kintamieji
bovb db ?
left_Bytes dw 0
word_Lenght db 0
byte_First db 0
code_Poz dw 0FFh 
d db ?   
w db ?
md db ?
back_md db ?
reg db ?
rm db ?
sr db ?
bojb db ?
finished db 0
word_Ptr db "word ptr ", 0
byte_Ptr db "byte ptr ", 0


;komandu vardai

c_AAA db 'AAA', 0
c_AAS db 'AAS', 0
c_DAA db 'DAA', 0
c_DAS db 'DAS', 0
c_SUB db "SUB ", 0
c_SHR db "SHR ", 0
c_ROR db "ROR ", 0
c_INC db "INC ", 0
c_PUSH db "PUSH ", 0

;registru vardai

r16_AX db 'ax', 0
r16_CX db 'cx', 0
r16_DX db 'dx', 0
r16_BX db 'bx', 0
r16_SP db 'sp', 0
r16_BP db 'bp', 0
r16_SI db 'si', 0
r16_DI db 'di', 0

r8_al db "al", 0
r8_cl db "cl", 0
r8_dl db "dl", 0
r8_bl db "bl", 0
r8_ah db "ah", 0
r8_ch db "ch", 0
r8_dh db "dh", 0
r8_bh db "bh", 0

sr_ES db " ES", 0
sr_CS db " CS", 0
sr_SS db " SS", 0
sr_DS db " DS", 0


md00_Rm000 db "bx+si", 0
md00_Rm001 db "bx+di", 0
md00_Rm010 db "bp+si", 0
md00_Rm011 db "bp+di", 0
md00_Rm100 db "si", 0
md00_Rm101 db "di", 0
md00_Rm110 db "bp", 0
md00_Rm111 db "bx", 0

;charai
space db ' ', 0
comma db ',', 0
endl db 13, 10, 0
leftbr db '[', 0
rightbr db ']', 0  

;ivesties parametrai
;read_End dw 81h 
buff db 512 dup(?)  ;skaitymas is failo 
write_Buff db 512 dup(?)
handle_Input dw 0
handle_Output dw 0
name_Input db  255 dup(?)
name_Output db 255 dup(?)
buff_Output_Pos db 0
byte_Buff_Poz dw 0
byte_Buff db 10h


.code
fput_Str macro string
	lea bx, string
	call fPut_String	
endm
fput_Ch macro char
	mov al, char
	call fput_Char
endm


start:
	mov ax, @data
	mov ds, ax
	get_Command_Line_Parameters:	
	xor cx, cx
	mov cl, es:[80h]
	cmp cx, 0
	jne set_Input
	mov dx, offset msg_No_Input
	call error_Output
	jmp prog_End
	set_Input:
	mov bx, 81h 
    xor si, si
    call skip_Spaces
	helpOut:  
        cmp byte ptr es:[bx], '/'       ; patikrinama ar nebuvo prasoma informacijos /?
		jne get_Read_Name
		cmp byte ptr es:[bx+1], '?'     ; patikrinama ar nebuvo prasoma informacijos /?
		jne get_Read_Name
		mov ah, 09h
	    mov dx, offset msg_Help
		int 21h
	    jmp prog_End    ;isvedama informacijos zinute ir pasibaigia programa
	get_Read_Name:
	mov al, es:[bx+si]
	cmp al, ' '
	je read_Name
	mov name_Input[si], al
	inc si
	loop get_Read_Name

	read_Name:
	mov name_Input[si], 0
    call input_Open
	call skip_Spaces

	reset_Index:
	add bx, si ;naujas bx
	xor si, si
			
	get_File_Name:
	mov al, es:[bx+si]
	cmp al, ' '
	je file_Name
	mov name_Output[si], al
	inc si
	loop get_File_Name
	
	file_Name:
    call output_Create_Open
	xor si,si
	xor dx, dx
    mov di, 512


read_Loop:
cmp finished, 1
jne not_Finished
fput_Ch space
fput_Ch ';'
call write_byte_Buff
fput_Str endl

not_Finished:
mov finished, 0 
call get_Byte
call get_First_Byte_Info
mov byte_First, dh
call check_If_One_Byte_Opperation
cmp finished, 1
jne not_One_Byte
jmp loop_End  
not_One_Byte:
call check_If_Left_One_Byte_Opperation
cmp finished, 1
jne not_Left_One_Byte
jmp loop_End
not_Left_One_Byte:
call get_Byte
dec code_Poz
call get_Second_Byte_Info
mov dh, byte_First
call check_If_Two_Byte_Opperation
inc code_Poz
cmp finished, 1 
je loop_End
call no_Byte   ; reikia padaryti isvedima y faila kad neatpazinta
loop_End:
call is_Full 
loop read_Loop


;PABAIGA----------------------------------
	EOF:
	call write_To_File
	mov ah, 3eh
	mov bx, handle_Input
	int 21h           
	mov bx, handle_Output
	int 21h
	jmp prog_End
;proceduros ----------------------------------------------------
proc error_Output 
mov ah, 9
int 21h

ret
endp error_Output 

proc no_Byte
dec code_Poz
mov dh, byte_First
call hex_To_Dec
fPut_Str msg_Unknown 
call dec_To_Hex
fput_Ch space
fput_Str endl
mov byte_Buff_Poz, 0
dec di
inc left_Bytes
ret
endp no_Byte

proc write_byte_Buff
push bx cx dx
mov cx, byte_Buff_Poz
mov bx, 0
loop_Byte:
mov dh, byte_Buff[bx]
inc bx
call dec_To_Hex
fput_Ch space
call is_Full
loop loop_Byte

mov byte_Buff_Poz, 0
pop dx cx bx
ret
endp write_byte_Buff

proc find_Md_Rm
md11:
cmp md, 11b
jne not_md11  
mov ah, rm
call print_Reg
jmp find_Md_Rm_End
not_md11:
cmp w, 0
jne not_w0
fput_Str byte_Ptr
fput_Ch leftbr
jmp md_00
not_w0:
fput_Str word_Ptr
fput_Ch leftbr
md_00:
cmp md, 00b 
jne not_Direct
cmp rm, 110b
jne not_Direct
direct_Address:
call get_Byte
mov byte_First, dh
call get_Byte
call dec_To_Hex
mov dh, byte_First
call dec_To_Hex
fput_Ch 'h'
fput_Ch rightbr  
jmp find_Md_Rm_End
not_Direct:
cmp rm, 000b
jne not_rm000
fput_Str md00_Rm000
jmp rm_Check_End
not_rm000:
cmp rm, 001b
jne not_rm001
fput_Str md00_Rm001
jmp rm_Check_End
not_rm001:
cmp rm, 010b
jne not_rm010
fput_Str md00_Rm010
jmp rm_Check_End
not_rm010:
cmp rm, 011b
jne not_rm011
fput_Str md00_Rm011
jmp rm_Check_End
not_rm011:
cmp rm, 100b
jne not_rm100
fput_Str md00_Rm100
jmp rm_Check_End
not_rm100:
cmp rm, 101b
jne not_rm101
fput_Str md00_Rm101
jmp rm_Check_End
not_rm101:
cmp rm, 110b
jne not_rm110
fput_Str md00_Rm110
jmp rm_Check_End
not_rm110:
cmp rm, 111b
fput_Str md00_Rm111
rm_Check_End:
cmp md, 00b
jne md_01b
fput_Ch rightbr
jmp find_Md_Rm_End
md_01b:
cmp md, 01b 
jne not_Md01
fput_Ch '+'
call get_Byte
call dec_To_Hex
fput_Ch 'h'
fput_Ch rightbr
jmp find_Md_Rm_End
not_Md01:
cmp md, 10b
jne find_Md_Rm_End
fput_Ch '+'
call get_Byte
mov byte_First, dh
call get_Byte
call dec_To_Hex
mov dh, byte_First
call dec_To_Hex
fput_Ch 'h'
fput_Ch rightbr
find_Md_Rm_End:
ret
endp find_Md_Rm

proc check_If_Two_Byte_Opperation
check_If_ROR_Or_SHR:
cmp dh, 0D0h
jnb maybe_ROR1
jmp not_SHR
maybe_ROR1:
cmp dh, 0D3h
jna maybe_ROR
jmp not_SHR 
maybe_ROR:
cmp reg, 001b
je its_ROR
jmp check_If_SHR
its_ROR:
call hex_To_Dec
fput_Str c_ROR
inc finished
call find_Md_Rm
fput_Ch comma
fput_Ch space
cmp d, 0
jne ror_Cl
fput_Ch '1'
jmp check_If_Two_Byte_Opperation_End
ror_Cl: 
fput_Str r8_cl 
jmp check_If_Two_Byte_Opperation_End
check_If_SHR:
cmp reg, 101b
je its_SHR
jmp not_SHR
its_SHR:
call hex_To_Dec
fput_Str c_SHR
inc finished
call find_Md_Rm
fput_Ch comma
fput_Ch space
cmp d, 0
jne shr_Cl
fput_Ch '1'
jmp check_If_Two_Byte_Opperation_End
shr_Cl:
fput_Str r8_cl 
jmp check_If_Two_Byte_Opperation_End
not_SHR:
cmp dh, 0FFh
je maybe_Rm_PUSH
jmp maybe_Rm_INC
maybe_Rm_PUSH:
cmp reg, 110b
je rm_PUSH
jmp maybe_Rm_INC1
rm_PUSH:
call hex_To_Dec
fput_Str c_PUSH
call find_Md_Rm
inc finished
jmp check_If_Two_Byte_Opperation_End
maybe_Rm_INC:
cmp dh, 0FEh
je maybe_Rm_Inc1
jmp not_INC
maybe_Rm_Inc1:
cmp reg, 000b
je rm_INC
jmp not_INC
rm_INC: 
call hex_To_Dec
fput_Str c_INC
call find_Md_Rm
inc finished
jmp check_If_Two_Byte_Opperation_End
not_INC:
cmp dh, 28h
jnb maybe_rm_SUB
jmp rm_Sub_End
maybe_rm_SUB:
cmp dh, 2Bh
jna rm_SUB
jmp rm_Sub_End
rm_SUB:
inc finished
call hex_To_Dec
fput_Str c_SUB
cmp d, 0
je rm_SUBd1 
rm_SUBd0:
mov ah, reg
call print_Reg
fput_Ch comma
fput_Ch space
call find_Md_Rm
jmp rm_Sub_End
rm_SUBd1:
call find_Md_Rm
fput_Ch comma
fput_Ch space
mov ah, reg
call print_Reg
rm_Sub_End:
cmp dh, 80h
jnb maybe_SUB_Exception
jmp check_If_Two_Byte_Opperation_End
maybe_SUB_Exception:
cmp dh, 83h
jna exception_SUB
jmp check_If_Two_Byte_Opperation_End
exception_SUB:
inc finished
call hex_To_Dec
fput_Str c_SUB
call find_Md_Rm
fput_Ch comma
fput_Ch space
call get_Byte
cmp w, 0
je exception_SUB_w0
jmp exception_SUB_w1
exception_SUB_w0:
call dec_To_Hex
fput_Ch 'h'
jmp check_If_Two_Byte_Opperation_End
exception_SUB_w1:
cmp d, 0
jne exception_SUB_w1_s1
jmp exception_SUB_w1_s0
exception_SUB_w1_s1:
cmp dh, 7Fh
ja exception_SUB_w1_FF
fput_Ch '0'
fput_Ch '0'
call dec_To_Hex
fput_Ch 'h'
jmp check_If_Two_Byte_Opperation_End
exception_SUB_w1_FF:
fput_Ch 'F'
fput_Ch 'F'
call dec_To_Hex
fput_Ch 'h'
jmp check_If_Two_Byte_Opperation_End
exception_SUB_w1_s0:
mov dh, byte_First
call dec_To_Hex
call get_Byte
call dec_To_Hex
fput_Ch 'h'
check_If_Two_Byte_Opperation_End:
ret
endp check_If_Two_Byte_Opperation

proc get_Second_Byte_Info
md_Find:
mov ah, dh
and ah, 11000000b
mov cl, 6
shr ah, cl
mov md, ah
reg_Find2:
mov ah, dh
and ah, 00111000b
mov cl, 3
shr ah, cl
mov reg, ah
rm_Find:
mov ah, dh
and ah, 00000111b
mov rm, ah
ret
endp get_Second_Byte_Info




proc dec_To_Hex
    push ax 
    xor ax, ax
	mov al, dh
    push bx
	push cx
	push dx
	mov bx, 16
	xor cx,cx
	get_Hex:
	xor dx,dx
	div bx
	add dx, 30h
	push dx
	inc cx
	cmp ax, 0
    je get_Hex_End
	jmp get_Hex
	
	get_Hex_End:
	call is_Full
	pop dx
	hex10:
	cmp dl, 3Ah
	jne hex11
	mov dl, 41h
	hex11:
	cmp dl, 3Bh
	jne hex12
	mov dl, 42h
	hex12:
	cmp dl, 3Ch
	jne hex13
	mov dl, 43h
	hex13:
	cmp dl, 3Dh
	jne hex14
	mov dl, 44h
	hex14:
	cmp dl, 3Eh
	jne hex15
	mov dl, 45h
	hex15:
	cmp dl, 3Fh
	jne normal_Hex
	mov dl, 46h
	
	normal_Hex:
	fput_Ch dl
	loop get_Hex_End
    pop dx
	pop cx
	pop bx
	pop ax

ret 
endp dec_To_Hex

proc print_Sr
cmp sr, 00b
jne sr01
fput_Str sr_ES
jmp print_Sr_End
sr01:
cmp sr, 01b
jne sr10
fput_Str sr_CS
jmp print_Sr_End
sr10:
cmp sr, 10b
jne sr11
fput_Str sr_SS
jmp print_Sr_End
sr11:
cmp sr, 11b
jne print_Sr_End
fput_Str sr_DS

print_Sr_End:
ret
endp print_Sr
  
proc print_Reg
cmp w, 0b 
je reg8
jmp reg16
reg8:

cmp ah, 000b 
jne reg0010
fput_Str r8_al
jmp print_Reg_End
reg0010:
cmp ah, 001b
jne reg0100
fput_Str r8_cl
jmp print_Reg_End
reg0100:
cmp ah, 010b
jne reg0110
fput_Str r8_dl
jmp print_Reg_End
reg0110:
cmp ah, 011b
jne reg1000
fput_Str r8_bl
jmp print_Reg_End
reg1000:
cmp ah, 100b
jne reg1010
fput_Str r8_ah
jmp print_Reg_End
reg1010:
cmp ah, 101b
jne reg1100
fput_Str r8_ch
jmp print_Reg_End
reg1100:
cmp ah, 110b
jne reg1110
fput_Str r8_dh
jmp print_Reg_End
reg1110:
fput_Str r8_bh
jmp print_Reg_End
reg16:
cmp ah, 000b 
jne reg001
fput_Str r16_AX
jmp print_Reg_End
reg001:
cmp ah, 001b
jne reg010
fput_Str r16_CX
jmp print_Reg_End
reg010:
cmp ah, 010b
jne reg011
fput_Str r16_DX
jmp print_Reg_End
reg011:
cmp ah, 011b
jne reg100
fput_Str r16_BX
jmp print_Reg_End
reg100:
cmp ah, 100b
jne reg101
fput_Str r16_SP
jmp print_Reg_End
reg101:
cmp ah, 101b
jne reg110
fput_Str r16_BP
jmp print_Reg_End
reg110:
cmp ah, 110b
jne reg111
fput_Str r16_SI
jmp print_Reg_End
reg111:
cmp ah, 111b
jne print_Reg_End
fput_Str r16_DI

print_Reg_End:
ret
endp print_Reg

proc check_If_Left_One_Byte_Opperation
cmp dh, 06h  ; nuo 6, 0E, 16 iki 1E
jnb sr_Push1
jmp bojb_Sub0
sr_Push1:
cmp dh, 1Eh
jna sr_Push
bojb_Sub0:
cmp dh, 2Ch
je bojb_Sub
cmp dh,2Dh
je bojb_Sub
word_Inc0:
cmp dh,40h
jnb word_Inc1
jmp not_Word_Inc
word_Inc1:
cmp dh, 47h
jna word_Inc2
jmp not_Word_Inc
word_Inc2:
jmp word_Inc
not_Word_Inc:
cmp dh, 50h
jnb word_Push1
jmp not_Word_Push
word_Push1:
cmp dh, 57h
jna word_Push2
jmp not_Word_Push
word_Push2:
jmp word_Push 
not_Word_Push:
jmp check_If_Left_One_Byte_Opperation_End

sr_Push:
call hex_To_Dec   ; sr pushas
inc finished 
fput_Str c_PUSH
call print_Sr 
jmp check_If_Left_One_Byte_Opperation_End

bojb_Sub:
call hex_To_Dec
inc finished 
fput_Str c_SUB

first_Byte_w_0:
cmp w, 0
jne first_Byte_w_1
call get_Byte
fput_Str r8_al
fput_Ch comma
fput_Ch space
call dec_To_Hex
fput_Ch 'h'
jmp check_If_Left_One_Byte_Opperation_End
first_Byte_w_1:
call get_Byte
mov bojb, dh
call get_Byte
fput_Str r16_AX
fput_Ch comma
fput_Ch space
call dec_To_Hex
mov dh, [bojb]
call dec_To_Hex
fput_Ch 'h'

jmp check_If_Left_One_Byte_Opperation_End

word_Inc:
mov w, 1
call hex_To_Dec
inc finished 
fput_Str c_INC
mov ah, reg
call print_Reg
jmp check_If_Left_One_Byte_Opperation_End

word_Push:
mov w, 1
call hex_To_Dec
inc finished 
fput_Str c_PUSH
mov ah, reg
call print_Reg
jmp check_If_Left_One_Byte_Opperation_End

check_If_Left_One_Byte_Opperation_End:

check_If_Left_One_Byte_Opperation_End2: 
ret
endp check_If_Left_One_Byte_Opperation


proc get_First_Byte_Info
push ax
push cx
sr_Find:
mov ah, dh
and ah, 00011000b
mov cl, 3
shr ah, cl
mov sr, ah  ;gaunamas sr
w_Find:
mov ah, dh
and ah, 00000001b
mov w, ah    ;gaunamas w_Find
d_Find:
mov ah, dh
and ah, 00000010b
mov cl, 1
shr ah, cl
mov d, ah
reg_Find:
mov ah, dh
and ah, 00000111b
mov reg, ah
pop ax
pop cx
ret
endp get_First_Byte_Info

proc check_If_One_Byte_Opperation
cmp dh, 37h
je byte_AAA
cmp dh, 3Fh
je byte_AAS
cmp dh, 27h
je byte_DAA
cmp dh, 2Fh
je byte_DAS
jmp check_If_One_Byte_Opperation_End
byte_AAA:
call hex_To_Dec
fput_Str c_AAA
inc finished
call is_Full
jmp check_If_One_Byte_Opperation_End 

byte_AAS:
call hex_To_Dec
fput_Str c_AAS
inc finished
call is_Full
jmp check_If_One_Byte_Opperation_End

byte_DAA:
call hex_To_Dec
fput_Str c_DAA
inc finished
jmp check_If_One_Byte_Opperation_End

byte_DAS:
call hex_To_Dec
fput_Str c_DAS
inc finished
check_If_One_Byte_Opperation_End:

ret
endp check_If_One_Byte_Opperation



proc write_To_File
	push ax bx cx dx  
	
	mov ah, 40h
	mov bx, handle_Output
	mov cx, si
	mov dx, offset write_Buff
	int 21h
	jnc not_Error5
	mov dx, offset msg_Writing_Error
    call error_Output
	jmp prog_End	
	not_Error5:

	pop dx cx bx ax
		ret

endp write_To_File

proc fPut_String
	push ax
	into_Write_Buff:
		mov al, [bx]
		cmp al, 0
		je into_Write_Buff_end	
		mov write_Buff[si], al
		inc si
		call is_Full
		inc bx
		jmp into_Write_Buff
	into_Write_Buff_end:
	pop ax
		ret
endp fPut_String

proc is_Full
cmp si, 512
ja full
cmp si, 512
jne not_Full
full:
call write_To_File
xor si,si
not_Full:
ret
endp is_Full
proc fput_Char
	mov write_Buff[si], al
	inc si
	call is_Full
	ret
endp fput_Char

proc get_Byte
	
	cmp di, 512
	je reset_Read
	cmp left_Bytes, 0
	jne notEOF
	jmp EOF
	notEOF:
	mov dh, buff[di]
	push bx
	mov bx, byte_Buff_Poz
	mov byte_Buff[bx], dh
	pop bx
	inc byte_Buff_Poz
	inc di;
	dec left_Bytes 
	inc code_Poz
	ret

reset_Read:
	push ax bx cx dx
	mov bx, handle_Input
	mov ah, 3fh
	mov cx, 512
	mov dx, offset buff
	int 21h
	jnc not_Error4
	mov dx, offset msg_Reset_Read
	call error_Output
	jmp prog_End
	not_Error4:
	xor di, di
	mov left_Bytes, ax
	pop dx cx bx ax
	call get_Byte
	ret
endp get_Byte

proc output_Create_Open
 
    out_Create: 
    mov dx, offset name_Output
	mov cx, 2
	mov ah, 3Ch
	int 21h
	jnc no_Error2
	mov dx, offset msg_Output_Create 
	call error_Output
	jmp prog_End
	no_Error2:
	mov [handle_Output], ax
	ret
endp output_Create_Open

proc hex_To_Dec
    push ax bx cx dx
	mov ax, code_Poz
	mov bx,16  ; hex i dec
	xor cx,cx
	get_Digit:

	xor dx, dx
	div bx
	add dx, 30h
	push dx
	inc cx
	cmp ax, 0
	je get_Digit_End
	jmp get_Digit
    
get_Digit_End:
    call is_Full
	pop dx
	dec10:
	cmp dl, 3Ah
	jne dec11
	mov dl, 41h
	dec11:
	cmp dl, 3Bh
	jne dec12
	mov dl, 42h
	dec12:
	cmp dl, 3Ch
	jne dec13
	mov dl, 43h
	dec13:
	cmp dl, 3Dh
	jne dec14
	mov dl, 44h
	dec14:
	cmp dl, 3Eh
	jne dec15
	mov dl, 45h
	dec15:
	cmp dl, 3Fh
	jne normal_Dec
	mov dl, 46h
	normal_Dec:
	mov write_Buff[si], dl
	inc si
	call is_Full
	loop get_Digit_End
	mov write_Buff[si], ':'
    inc si
	call is_Full
	mov write_Buff[si], 20h 
	inc si
	call is_Full
	pop dx cx bx ax
    ret	
endp hex_To_Dec	
	
proc input_Open
	mov ax, 3D00h
	mov dx, offset name_Input
    int 21h
	jnc no_Error
	mov dx, offset msg_Input_Open
	call error_Output
	jmp prog_End
	no_Error:
	mov [handle_Input], ax
	ret
endp input_Open
	
proc skip_Spaces
skip_Space:
    mov al, es:[bx+si]
	cmp al, ' '   ; praleisdineja tarpus iki kol ju nera
	jne skip_Space_End
	inc bx       
	loop skip_Space
	skip_Space_End:
	ret
endp skip_Spaces	


prog_End:	
	mov ax, 4c00h
	int 21h
end start