%% Caesar cipher Erlang program.
%% Author: Christopher Lee

-module(caesarcipher).
-export([encrypt/2, decrypt/2, solve/2]).

%% The rotate function is essentially the encrypt/decrypt functions I used in Fortran.
%% It splices the sentence into characters and applies the Shift value on each Char
%% individually after first removing the Offset, then adds it back after taking the 
%% modulo with 26.
rotate(Char,Shift) when (Char >= $a) and (Char =< $z) or (Char >= $A) and (Char =< $Z) ->
	Offset = $A + Char band 32,
	Temp = Char - Offset,
	Offset + (Temp + Shift) rem 26;
rotate(Char,_Shift) ->
	Char.

%% Modulo implementation, there is no inherent Erlang function for modulo with neg numbers.
mod(X,Y) when X > 0 ->
	X rem Y;
mod(X,Y) when X < 0 ->
	Y + X rem Y;
mod(0,_) ->
	0.

%% Encrypt function
encrypt(Sentence,Shift) ->
	Encryptshift = mod(Shift,26),

	io:format("Original sentence: ~s.~n", [Sentence]),
	io:format("Shift amount: ~B.~n", [Shift]),
	
	Encrypted = lists:map(fun(Char) -> rotate(Char,Encryptshift) end, Sentence),
	
	io:format("Encrypted sentence: ~s.~n", [Encrypted]).

%% Decrypt function
decrypt(Sentence,Shift) ->
	Decryptshift = mod(-Shift,26),
	
	io:format("Original sentence: ~s.~n", [Sentence]),
	io:format("Shift amount: ~B.~n", [Shift]),
	
	Decrypted = lists:map(fun(Char) -> rotate(Char,Decryptshift) end, Sentence),
	
	io:format("Decrypted sentence: ~s.~n", [Decrypted]).

%% Recursive solve function
%% I decided not to use the existing decrypt function because it held innate IO.
%% Instead, I just copied the decrypt lines to solve recursively.
solve(Sentence,0) ->
	io:format("Caesar 0: ~s~n", [Sentence]);
solve(Sentence,Count) when Count > 0 ->
	Decrypted = lists:map(fun(Char) -> rotate(Char,Count) end, Sentence),
	io:format("Caesar ~B: ~s~n", [Count,Decrypted]),
	solve(Sentence,Count-1).
	
	
	