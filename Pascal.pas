{ Author: Christopher Lee }

program ProgrammingPast(output);
 
var
	sentence, encrypted, decrypted: string;
	key, maxshiftvalue: integer;

procedure encrypt(var sen: string; key: integer);
	var
		i: integer;
	begin
		for i := 1 to length(sen) do
			case sen[i] of
				'a'..'z': sen[i] := chr(ord('a') + (ord(sen[i]) - ord('a') + key) mod 26);
				{Chr(X) returns the char that has ASCII value X
				Ord('a') returns 97, the ASCII location of lowercase a}
			end;
	end;

procedure decrypt(var sen: string; key: integer);
	var
		i: integer;
	begin
		for i := 1 to length(sen) do
			case sen[i] of
				'a'..'z': sen[i] := chr(ord('a') + (ord(sen[i]) - ord('a') - key + 26) mod 26);
				{'a'..'z' ensures no spaces or non-lowercase alphabetical characters are converted}
			end;
	end;
	
procedure solve(var sen: string; shift: integer);
	var
		i, countdown: integer;
	begin
		countdown := shift;
		for i := 0 to shift do
		begin
			writeln('Caesar ', countdown, ': ', sen);
			decrypt(sen, 1);
			countdown := countdown - 1;
		end;
	end;	

begin
	key := 3;										{Change the shift value of the encryption here}
	maxshiftvalue := 26;							{Change the max shift value of the cipher solver here}
	sentence := 'Hello world';						{Change the sentence to be encrypted here}
	writeln('Original sentence: ', sentence);
	writeln('Shift amount: ', key);
	encrypted := lowercase(sentence);
	encrypt(encrypted, key);
	writeln('Encrypted sentence: ', encrypted);
	decrypted := encrypted;
	decrypt(decrypted, key);
	writeln('Decrypted sentence: ', decrypted);
	writeln('Solving...');
	solve(decrypted, maxshiftvalue);
end.