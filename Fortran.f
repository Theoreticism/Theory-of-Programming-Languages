! Author: Christopher Lee

program Past
	implicit none
	
	! Key can be changed to any numeric key
	! Max length of sentence must be changed to match length of sentence
	character(50) :: sentence, encrypted, decrypted
	integer :: key, max_shift_value
	
	
	write(*,*) "Enter the sentence to be encrypted: "
	read(*,"(A)") sentence
	write(*,*) "Enter the shift value of the encryption: "
	read(*,*) key
	write(*,*) "Enter the max shift value to assist in solving the Caesar Cipher: "
	read(*,*) max_shift_value
	
	write(*,*) "Original sentence = ", sentence
	encrypted = encrypt(sentence, key)
	write(*,*) "Encrypted sentence = ", encrypted
	decrypted = decrypt(encrypted, key)
	write(*,*) "Decrypted sentence = ", decrypted
	
	call solve(decrypted, max_shift_value)
	! Due to the annoying nature of this shift function (and how it returns MULTIPLE outputs,
	! I decided to use a subroutine here instead. Works much better for multiple outputs.
	
contains

character(len(sentence)) function encrypt(input, shift)
	implicit none
	! Encrypt a sentence with a Caesar Cipher

	character(len=*), intent(inout) :: input
	integer, intent(in) :: shift
	integer :: i, ic

	call lower_case(input)

	do i = 1, len(input)
		ic = iachar(input(i:i))
		
		if (ic >= 97 .and. ic <= 122) then
			input(i:i) = achar(modulo(iachar(input(i:i)) – 97 + shift, 26) + 97)
		end if
		
	end do
	
	encrypt = input
end function encrypt

character(len(sentence)) function decrypt(input, shift)
	implicit none
	! Decrypt a sentence with a Caesar Cipher

	character(len=*), intent(inout) :: input
	integer, intent(in) :: shift
	integer :: i, ic

	call lower_case(input)

	do i = 1, len(input)
		ic = iachar(input(i:i))
	
		if (ic >= 97 .and. ic <= 122) then
			input(i:i) = achar(modulo(iachar(input(i:i)) - 97 – shift, 26) + 97)
		end if
		
	end do
	
	decrypt = input
end function decrypt

subroutine solve(input, shift)
	implicit none
	! Solve the Caesar Cipher encryption
	integer :: shift, i, ic, counter
	integer :: j = 0
	character(len=*) :: input
	character(len=50) :: temp
	temp = input
	counter = shift
	
	do while(j <= shift)
		do i = 1, len(input)
			ic = iachar(input(i:i))
			
			if (ic >= 97 .and. ic <= 122) then
				temp(i:i) = achar(modulo(iachar(input(i:i)) - 97 - j, 26) + 97)
			end if
		end do
		
		write(*,*) "Caesar", counter, ": ", temp
		j = j + 1
		counter = counter - 1
	end do

end subroutine solve

subroutine lower_case(text)
	implicit none
	! Convert a sentence to lower case
	character(len=*), intent(inout) :: text
	integer :: i, ic

	do i = 1, len(text)
		ic = iachar(text(i:i))
		if (ic >= 65 .and. ic <= 90) then
			text(i:i) = char(ic+32)
		end if
	end do

end subroutine lower_case

end program Past